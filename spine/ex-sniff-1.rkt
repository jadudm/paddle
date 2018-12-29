#lang racket

(require paddle)

(define RxC 300)
(make-world RxC 600)
(create-breed turtle turtles)
(create turtles 1000)

(define turtle-0
  (list (vector-ref (get-agentset 'turtles) 0)))

(define (setup)
  
  (ask turtles
    (set turtle-color (color 255 255 255)))
  
  (ask turtle-0
    (set turtle-color (color 255 255 0)))

  (ask turtles
    (set turtle-x (random RxC))
    (set turtle-y (random RxC))
    ))

(define (go)

  (define this-color (color (random 255) (random 255) (random 255)))
  (ask turtles
    (define count 0)
    (ask (sniff turtles 5)
      (set! count (add1 count)))
    (when (> count 10)
      ;;(printf "~a has ~a~n" (get turtle-id) count)
      (set turtle-color this-color)
      ))


  (ask turtles
    (move 1)
    (define n (random 10))
    (if (even? n)
        (right n)
        (left  n)))
  )

(run-world setup go)

