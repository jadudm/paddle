#lang racket
(require "world.rkt"
         "turtle4.rkt")

(world-rows 40)
(world-cols 40)

;; This is code a user would write.
(define-breed turtle turtles)
(create-turtles 5000)
(tick-pause (/ 1 60))

(define (move-turtles)
  (ask-turtles
   (cond
     [(zero? (modulo (random 11) 2))
      (move 1)
      (right (random 15))]
     [else
      (move 1)
      (left (random 15))])
   ))

(define (go)
  (move-turtles))

(define (setup)
  (ask-turtles
   (set-color! (select-random-color))
   (set-x! (/ (world-rows) 2))
   (set-y! (/ (world-cols) 2))
   ;; FIXME The user should not have to tell the turtles
   ;; where the world boundary is. 
   (set-world-rows! (world-rows))
   (set-world-cols! (world-cols))
   ;; (set-direction! 0)
   ))


(define kill-proc (run-world setup go))
(shutdown
 (λ ()
   (printf "Killing computation threads.~n")
   (kill-proc)))