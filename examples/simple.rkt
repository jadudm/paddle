;;; Copyright 2018 Matt Jadud <matt@jadud.com>
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
;;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

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
