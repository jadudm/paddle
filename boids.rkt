#lang racket
(require "world.rkt"
         "forms.rkt")

(define-breed fish fishes)
(create-fishes 5)

(define (setup)
  'pass)

(define (go)
  'pass)

(run-world setup go)
