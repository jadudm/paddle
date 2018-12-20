;;; Copyright 2018 Matt Jadud <matt@jadud.com>
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
;;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#lang racket

(require "types.rkt"
         "base.rkt")
(provide (all-defined-out))

;; I want a fast way to know everyone around an agent.
;; The agent knows it's [x,y] and I can also convert
;; that to a patch-id, which is essentially a
;; linear combination of [x,y] that could serve as a
;; vector reference.

;(define prev-world (make-vector 1 false))
(define curr-world (make-vector 1 false))
(define world-leng 0)

;; We need to be able to set this when
;; the world is created.
(define (setup-backing-world! x y)
  ;(set! prev-world (make-vector (* x y) 0))
  (set! curr-world (make-vector (* x y) 0))
  (set! world-leng (* x y))
  
  (for ([ndx (range world-leng)])
    ;(vector-set! prev-world ndx (make-hash))
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
  (when (hash-has-key? (vector-ref curr-world prev) id)
    #;(printf "removing agent from x ~a y ~a ppid ~a~n"
            (hash-ref (agent-fields ag) 'xcor)
            (hash-ref (agent-fields ag) 'ycor)
            (hash-ref (agent-fields ag) 'prev-patch-id))
    (hash-remove! (vector-ref curr-world prev) id))
  
  ;; Make new prev the old current.
  ;; FIXME I should store booleans here, so it is just a
  ;; set of IDs, not a set of agents.
  (hash-set! (vector-ref curr-world curr) id true)
  )

(define (remove-from-backing! ag)
  (define prev (hash-ref (agent-fields ag) 'prev-patch-id))
  (define curr (hash-ref (agent-fields ag) 'patch-id))
  (define id (agent-id ag))
  (when (hash-has-key? (vector-ref curr-world prev) id)
    (hash-remove! (vector-ref curr-world prev) id))
  (when (hash-has-key? (vector-ref curr-world curr) id)
    (hash-remove! (vector-ref curr-world curr) id)))

;; Return everyone at the location of the given agent.
;; FIXME These need to be renamed to something like "get-agent-ids-at..."
(define (get-backing-from-agent ag)
  (vector-ref curr-world (hash-ref (agent-fields ag) 'patch-id)))

(define (get-backing-from-patch-id pid)
  (vector-ref curr-world pid))
  