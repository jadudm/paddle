#lang racket

(require "breeds.rkt"
         "agentsets.rkt"
         "agents.rkt"
         "world.rkt"
         )

(make-world 10 300)

(create-breed turtle turtles)
(create turtles 5)



(define (setup) 'pass)
(define (go) 'pass)

(run-world setup go)
