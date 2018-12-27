#lang racket

(require "breeds.rkt"
         "agentsets.rkt"
         "agents.rkt"
         "world.rkt"
         "netlogo.rkt"
         "state.rkt"
         "get-set.rkt"
         racket/syntax
         )

(define WORLD 500)
(make-world WORLD 800)

(create-breed turtle turtles)
(create turtles 5000)

(define (setup)
  (ask turtles
    ;;(printf "Turtle: ~a~n" (vector-ref (current-agent) agent-id))
    (set turtle-x (random WORLD))
    (set turtle-y (random WORLD))
    )
  )

(define (go)
  ; (sleep 0.1)
  
  (ask turtles
    ;; (printf "Moving ~a~n" (vector-ref (current-agent) agent-id))
    (move 1)
    (right (random 10))
  )
  )

(run-world setup go)
