#lang racket

(require "netlogo.rkt"
         "world.rkt"
         "agents.rkt"
         "agentsets.rkt"
         "breeds.rkt"
         "get-set.rkt"
         "colors.rkt"
         "state.rkt"
         )

(make-world 400 800)

(create-breed turtle turtles #:have eyes eyebrows)
(create turtles 500)

(define (setup) 'pass)

(define max-rings 20)
(define count-rings 0)
(define colors (list (color (random 255) (random 255) (random 255))
                     (color (random 255) (random 255) (random 255))
                     (color (random 255) (random 255) (random 255))
                     (color (random 255) (random 255) (random 255))))
(define (random-color) (list-ref colors (random (length colors))))

(define (go)
  (ask turtles
    (move 1)
    ;; (set color (color (random 255) (random 255) (random 255)))
    )

  (when (and (zero? (modulo (ticker) 10))
             (< count-rings max-rings))
    (define new-c (random-color))
    (ask (create turtles 100)
      (set color new-c))
    (increment! count-rings))
  )

(run-world setup go)