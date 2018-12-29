#lang racket

(require paddle "log.rkt")

(define RxC 50)
(make-world RxC 600)

(create-breed turtle turtles)
(create turtles 200)

(define (wiggle n)
  (define _n (random n))
  (if (even? _n)
      (right _n)
      (left _n)))

(define (setup)
  (ask (where turtles (= (get turtle-id) 0))
    (set turtle-color (color 255 255 0)))
  )

(define (go)
  (ask (where turtles (not (zero? (get turtle-id))))
    (move 1)
    (wiggle 10)
    )

   (ask (where turtles (and (> (ticker) 5)
                            (zero? (get turtle-id))))
     (ask (sniff turtles 3)
       (set turtle-color (color 255 255 255)))
     )
 
  )



(run-world setup go)