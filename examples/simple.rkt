;;; Copyright 2018 Matt Jadud <matt@jadud.com>. All rights reserved.
;;; This work is licensed under the terms of the MIT license.

#lang racket

(require paddle)

(make-world 100)

(create-breed midge midges)
(tick-pause (/ 1 30))

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
  #;(ask (with midges (odd? (get id)))
       (move .5)
       (clear-patch!))
  'pass
  )

(run-world setup go)
