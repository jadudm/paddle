#lang racket

(require "netlogo.rkt"
         "world.rkt"
         "agents.rkt"
         "agentsets.rkt"
         "breeds.rkt"
         "get-set.rkt"
         )

(make-world 10 400)

(create-breed turtle turtles #:have eyes eyebrows)
(create turtles 2)

(define colors '(a b c d e))

(define (setup) 'pass)
(define (go)
  (ask turtles
       (set turtle eyes (random 1000000))
       (set turtle eyebrows (list-ref colors (random (length colors))))
       (move 1)
       )

  (ask turtles
       (sleep 1)
       (printf "~a ~a~n" (get turtle id) (get turtle eyes))
       (printf "~a ~a~n" (get turtle id) (get turtle eyebrows))
       ))

(run-world setup go)