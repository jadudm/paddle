#lang racket
(require "structs.rkt"
         "breeds.rkt")

(require rackunit)
  (create-breed turtle turtles)
  (define t1 (make-turtle 1 2 3 4 (color 0 0 0) 5))
  (check-equal? (turtle-get t1 id)            1)
  (check-equal? (turtle-get t1 singular)      'turtle)

  (create-breed fish fishes #:have eyes)
  (define f1 (make-fish 1 2 3 4 (color 0 0 0) 5 42))
  (check-equal? (fish-get f1 eyes)            42)