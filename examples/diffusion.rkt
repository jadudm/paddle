#lang racket
;;(require (except-in paddle move))
;;(require (only-in "../src/netlogo.rkt" move))
(require paddle)

(define RxC 30)
(make-world RxC 800)
(set-edge-mode! bounce)

(create-breed particle particles #:have velocity)
(create particles 20)


(define (setup)
  (ask particles
    (set particle-velocity .05)
    (set particle-x (/ RxC 2))
    (set particle-y (/ RxC 2))
    (set particle-direction (random 360))            
  )
  )

(define counter 1)
(define (go)
  (set! counter (add1 counter))
  (sleep 0.1)
  (ask particles
    (move 1)
    (left 5)
    )
  #;(when (zero? (modulo counter 10))
    (if (equal? (edge-case) 'bounce)
        (set-edge-mode! wrap)
        (set-edge-mode! bounce)))
  )

(run-world setup go)