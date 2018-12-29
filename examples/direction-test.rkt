#lang racket
(require paddle)

(make-world 20 400)
(create-breed boid boids #:have velocity vision)
(create boids 1)

(define (setup)
  (ask boids
    (set boid-direction 0)))

(define (go)
  (ask boids
    (move 1)
    (right 1))
  (sleep (/ 1 20))
  )

(run-world setup go)