#lang racket
(require "base.rkt"
         "world.rkt"
         "forms.rkt")

(world-cols 20)
(world-rows 20)
(tick-pause (/ 1 20))

(define-breed fish fishes)
(create-fishes 5)

(define (setup)
  'pass)

(define (wiggle)
  (if (> (random 100) 50)
      (right (random 10))
      (left (random 10))))

(define (shimmer)
  (set-color! (rgb (+ 128 (random 64))
                   (+ 128 (random 64))
                   (+ 128 (random 64)))))

(define (go)
  (ask-fishes
   (move 1)
   (wiggle)
   (shimmer)
   ))

;;(run-world setup go)

(define stop
  (run-world setup go))