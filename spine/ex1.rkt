#lang racket

(require "breeds.rkt"
         "agentsets.rkt"
         "agents.rkt"
         "world.rkt"
         "netlogo.rkt"
         "state.rkt"
         )

(make-world 10 300)

(create-breed turtle turtles)
(create turtles 5)



(define (setup) 'pass)
(define (go)
  (ask turtles
    (printf "Moving ~a~n" (vector-ref (current-agent) agent-id))
    (move 1)
    (sleep 1)
  )
  )

(run-world setup go)
