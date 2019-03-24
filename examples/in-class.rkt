#lang racket

(require paddle)

(define RxC 100)
(make-world RxC 800)

(create-breed turtle turtles)
(create turtles 6000)

(frames-per-second 30)

(define (setup)
  (ask turtles
    (set turtle-x (random 200))
    (set turtle-y (random 200))
    )

  (ask (where turtles (> (get turtle-x) 50))
    (set turtle-color (color 255 255 0)))

  (ask (where turtles (< (get turtle-x) 50))
    (set turtle-color (color 0 255 255)))
  )

(define turn -5)
(define increment +1)

(define (run)
  (ask turtles
    (move 1)
    (right turn)
    )
  
  (cond
    [(= turn 5)
     (set! increment -1)]
    [(= turn -5)
     (set! increment +1)])
  (set! turn (+ increment turn))
  )

(run-world setup run)