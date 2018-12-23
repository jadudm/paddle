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

(make-world 100 800)

(create-breed turtle turtles #:have eyes eyebrows)
(create turtles 200)

(define (setup) 'pass)

(define max-rings 20)
(define count-rings 0)

(define colors (list (get-color "red") (get-color "blue") (get-color "yellow")))

(define next-color 0)
(define (get-next-color)
  (set! next-color (modulo (add1 next-color) (length colors)))
  (list-ref colors next-color))
  
(define (go)
  (sleep 1)
  (ask turtles
    (move 1)
    ;; (set color (color (random 255) (random 255) (random 255)))
    )

  (when (and (zero? (modulo (ticker) 10))
             (< count-rings max-rings))
    (define new-c (get-next-color))

    (define count 0)
    (ask (create turtles 100)
      (increment! count)
      (set color new-c))
    (printf "count: ~a~n" count)
    
    (increment! count-rings))
  )

(run-world setup go)