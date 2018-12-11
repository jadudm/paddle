#lang racket
(require "world.rkt"
         "turtle4.rkt")

;; This is code a user would write.
(define-breed turtle turtles)
(create-turtles 5000)

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
  (move-turtles)
  )

(define (setup)
  ;; (printf "Running setup~n")
  (ask-turtles
   #;(printf "setup ~a ~a~n"
           (agent-singular (current-agent))
           (agent-id (current-agent)))
   
   ;; (set-color! (color-name->color-obj "firebrick"))
   (set-color! (select-random-color))
   (set-x! (/ (world-rows) 2))
   (set-y! (/ (world-cols) 2))
   (set-world-rows! (world-rows))
   (set-world-cols! (world-cols))

   ;; (printf "init x ~a y ~a~n" (get-x) (get-y))
   ))


(define kill-proc (run-world setup go))
(shutdown
 (λ ()
   (printf "Killing computation threads.~n")
   (kill-proc)))