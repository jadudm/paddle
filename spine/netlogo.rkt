#lang racket
(provide (all-defined-out))
(require "agentsets.rkt"
         "agents.rkt"
         "breeds.rkt"
         "state.rkt"
         "world.rkt"
         "util.rkt")

(define (get-agents as)
  (cond
    ;; If we're given a symbol, they're asking for the whole
    ;; agentset, and I should fetch it.
    [(symbol? as) (get-agentset as)]
    ;; Possible checks for things that are an error...
    ;; Or, check that it is the right kind of thing.
    ;; To be decided.
    [else as]))

(define-syntax-rule (ask as body ...)
  (let ()
    (for ([agent (get-agents as)])
     (parameterize ([current-agent agent])
       (when (current-agent)
         body ...))
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Movement
(define pi-conv (/ pi 180))

(define (offset x y direction magnitude)
  (define dir (* (+ direction 90) pi-conv))
  (define dy (* magnitude (sin dir)))
  (define dx (* magnitude (cos dir)))
  (values (+ x dx) (+ y dy)))


(define (move magnitude)
  (define direction (vector-ref (current-agent) agent-direction))
  (define-values (new-x new-y)
    (offset (vector-ref (current-agent) agent-x)
            (vector-ref (current-agent) agent-y)
            direction magnitude))
  (vector-set! (current-agent)
       agent-x
       (wrap new-x (get-global 'world-columns)))
  (vector-set! (current-agent)
       agent-y
       (wrap new-y (get-global 'world-rows)))

  #;(vector-set! (current-agent)
       prev-patch-id
       (get patch-id))
  
  #;(vector-set! (current-agent)
       patch-id
       (->patch (get xcor) (get ycor)))

  ;; For tracking agent locations.
  ;; (update-backing! (current-agent))

  )

(define (right d)
  (vector-set! (current-agent)
               agent-direction
               (+ (vector-ref (current-agent) agent-direction) d)))

(define (left d)
  (vector-set! (current-agent)
               agent-direction
               (- (vector-ref (current-agent) agent-direction) d)))


(module+ test
  (make-world 10 100)
  (create-breed turtle turtles)
  (create turtles 5)
  ;; (printf "~a~n" (get-agentset turtles))
  (ask turtles
    (printf "id: ~a~n" (vector-ref (current-agent) agent-id)))
  )