#lang racket
(require "world.rkt"
         "turtle4.rkt")

(world-rows 40)
(world-cols 40)

;; This is code a user would write.
(define-breed boid boids)
;;(boids-own flockmates nearest-neighbor)
;;(create-boids 30)

(tick-pause (/ 1 60))

(define (move-turtles)
  (ask-boids
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
  (ask-boids
   ;; Random shades of yellow?
   (set-color! (rgb (+ 128 (random 128))
                    (+ 128 (random 128))
                    0))
   
   ;; Random placement in the world.
   (set-x! (/ (random (* (world-cols) 10)) 10))
   (set-y! (/ (random (* (world-cols) 10)) 10))
   
   ;; Flockmates
   (boid-set-flockmates! no-boids)
   
   ;; FIXME The user should not have to tell the turtles
   ;; where the world boundary is. 
   (set-world-rows! (world-rows))
   (set-world-cols! (world-cols))
   
   ))


(define kill-proc (run-world setup go))
(shutdown
 (λ ()
   (printf "Killing computation threads.~n")
   (kill-proc)))