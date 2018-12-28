#lang racket

(require paddle)

(define RxC 100)
(make-world RxC 600)

(create-breed turtle turtles)
(create turtles 400)
(create-log yellow-turtle-counts)
(create-log other-turtle-counts)

(create-plot yellow-turtle-counts
             other-turtle-counts)


(define (wiggle n)
  (define _n (random n))
  (if (even? _n)
      (right _n)
      (left _n)))

(define (setup)
  (ask (where turtles (= (get turtle-id) 0))
    (set turtle-color (color 255 255 255)))
  )

(define (go)
  (ask (where turtles (not (zero? (get turtle-id))))
    (move 1)
    (wiggle 10)
    )

   (ask (where turtles (and (> (ticker) 5)
                            (zero? (get turtle-id))))
     (ask (sniff turtles 3)
       (set turtle-color (color 255 255 0)))
     )

  (when (zero? (modulo (ticker) 10))
    (log yellow-turtle-counts
         (where turtles (equal? (get turtle-color)
                                (color 255 255 0)
                                )))
    (log other-turtle-counts
         (where turtles (not (equal? (get turtle-color)
                                     (color 255 255 0)
                                     ))))
    )
       
  )

(run-world setup go)