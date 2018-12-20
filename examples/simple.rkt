#lang racket

(require paddle)

(make-world 100 100 600 600)

(create-breed midge midges)

(create midges 100)

(define (setup)
  (ask midges
       (set xcor (random (get global world-cols)))
       (set ycor (random (get global world-rows)))
       (set direction (random 360))
       ))

(define (go)
  (ask (with midges (even? (get id)))
       (move .5)
       (set patch pcolor (get color)))
  (ask (with midges (odd? (get id)))
       (move .5)
       (clear-patch!))
  )

(run-world setup go)
