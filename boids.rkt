#lang racket
(require "world.rkt"
         "forms.rkt")

(define-breed fish fishes)
(create-fishes 5)

(define (setup)
  'pass)

(define (go)
  (ask-fishes
   (move 1)
   ))

;;(run-world setup go)

(define stop
  (run-world setup go))