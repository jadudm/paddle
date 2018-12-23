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
(create turtles 100)

(define (setup)
  (ask (where turtles (even? (get id)))
    (set color (color 255 255 0))
    )

  (ask (where turtles (odd? (get id)))
    (set color (color 255 0 255))
    )
  (printf "Done running setup.~n")
  )

(define (go)
  (ask turtles
    (move .25))

  (when (> (ticker) 20)
    (define yellow-turtles (make-hash))
    (ask turtles
      (when (equal? (get color) (color 255 255 0))
        (hash-set! yellow-turtles (get id) (current-agent))))
    
    (ask (where turtles (even? (get id)))
      (when (not (hash-has-key? yellow-turtles (get id)))
        (printf "~a should be a yellow turtle.~n" (get id)))
      
      ;;(printf "~a sniffing: ~a~n" (get id) (hash-keys (agentset-agents ((sniff 3)))))
      (define neighbors (sniff turtles 3))
     
      (ask neighbors
        (define new-color (color (random 255) (random 255) (random 255)))
        (when (hash-has-key? yellow-turtles (get id))
          (printf "about to change yt ~a to ~a~n" (get id) new-color)
          (hash-remove! yellow-turtles (get id)))
        (set color new-color)
        ;;(printf "\t\tnow ~a~n" (get color))
        )))
  )

(run-world setup go)