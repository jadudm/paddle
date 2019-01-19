#lang racket
;;(require (except-in paddle move))
;;(require (only-in "../src/netlogo.rkt" move))
(require paddle)

(define RxC 30)
(make-world RxC 800)
(set-edge-mode! bounce)

(create-breed particle particles #:have velocity)
(create particles 200)

(define (choice ls)
  (list-ref ls (random (length ls))))

(define (setup)
  (ask particles
    (set particle-velocity .05)
    (set particle-x ((choice (list + -)) (/ RxC 2) (* (random) (/ RxC 20))))
    (set particle-y ((choice (list + -)) (/ RxC 2) (* (random) (/ RxC 20))))
    (set particle-direction (random 360))
    (set particle-shape (shape:disk (/ 1 2) 20))
  )
  )

(define (go)
  (ask particles
    (jiggle)))

(define (jiggle)
  (define current-direction (get particle-direction))
  (set particle-direction ((choice (list + -)) current-direction (random 60)))
  ;;(set particle-direction (random 360))
  (move (/ (random) 10)))

(run-world setup go)