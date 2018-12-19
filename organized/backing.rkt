#lang racket

(require "base.rkt")
(provide (all-defined-out))

;; I want a fast way to know everyone around an agent.
;; The agent knows it's [x,y] and I can also convert
;; that to a patch-id, which is essentially a
;; linear combination of [x,y] that could serve as a
;; vector reference.

(define prev-world (make-vector 1 false))
(define curr-world (make-vector 1 false))
(define world-leng 0)

;; We need to be able to set this when
;; the world is created.
(define (setup-backing-world! x y)
  (set! prev-world (make-vector (* x y) 0))
  (set! curr-world (make-vector (* x y) 0))
  (set! world-leng (* x y))
  
  (for ([ndx (range world-leng)])
    (vector-set! prev-world ndx (make-hash))
    (vector-set! curr-world ndx (make-hash))))

;; An agent is either going to be
;; 1. set at a location, or
;; 2. move to a location.
;; The agent should know its prev patch-id and current patch-id.
;; So, given that, I can move the agent.
(define (update-backing! ag)
  (define prev (hash-ref (agent-fields ag) 'prev-patch-id))
  (define curr (hash-ref (agent-fields ag) 'patch-id))
  (define id (agent-id ag))
  
  ;; Remove the old prev.
  (when (hash-has-key? (vector-ref prev-world prev) id)
    (hash-remove! (vector-ref prev-world prev) id))
  
  ;; Make new prev the old current.
  (hash-set! (vector-ref curr-world curr) id ag)
  )

;; Return everyone at the location of the given agent.
(define (get-backing-from-agent ag)
  (vector-ref curr-world (hash-ref (agent-fields ag) 'patch-id)))

(define (get-backing-from-patch-id pid)
  (vector-ref curr-world pid))
  