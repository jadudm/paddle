#lang racket

(require paddle
         data-table)

(define RxC 200)
(make-world RxC 600)

(create-breed ball balls)
(create balls 5)
(set-edge-mode! bounce)

(define the-table (create-numeric-table bouncy id y vy))

(define (setup)
  (ask balls
    (set ball-x (/ RxC (add1 (get ball-id))))
    (set ball-y (/ RxC 2))
    (set ball-vy (+ 100 (random 300)))
    (printf "bd: ~a ~a ~a~n" (get ball-id) (get ball-vy) (get ball-direction))
    (set ball-shape (shape:disk 2 20))
    )
  )

(define (sign-of v)
  (if (positive? v) 1 -1))

(define (go)
  (sleep 0.01)

  (when (> (ticker) 500)
    (save the-table)
    (stop))
  
  (ask balls
    (insert the-table (get ball-id) (get ball-y) (get ball-vy)))
  
  (ask balls
    (define current-vy (get ball-vy))
    (set ball-vy (- (get ball-vy) 9.8 (sign-of current-vy)))
    (move 0.01)
    ))

(run-world setup go)