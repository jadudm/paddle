;;; Copyright 2018 Matt Jadud <matt@jadud.com>
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
;;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#lang racket

(require paddle)

(make-world 200 200 600 600)
;(tick-pause (/ 1 30))

(define get-neighbors
  (case-lambda
    [(x y)
     (define n empty)
     (for ([x-offset (list -1 0 1)])
       (for ([y-offset (list -1 0 1)])
         (unless (and (zero? x-offset) (zero? y-offset))
           (define new-x (+ x x-offset))
           (define new-y (+ y y-offset))
           (set! n (cons (list (wrap new-x (get global world-cols))
                               (wrap new-y (get global world-rows)))
                         n)))))
       (map ->patch (map first n) (map second n))]
    [(pid)
     (parameterize ([current-patch pid])
       (get-neighbors (get patch pxcor)
                      (get patch pycor)))]
    ))

(define yellow (rgb 255 255 0))

(define (setup)
  (ask patches
       (set patch alive? false)
       (set patch neighbors (get-neighbors (current-patch)))
       (clear-patch! (current-patch))
       
       (when (zero? (random 13))
         (set patch pcolor yellow)
         (set patch alive? true))
       ))

(define (setup-one-glider)
  (ask patches
       (set patch neighbors (get-neighbors (current-patch))))
  
  (ask patches
       (set patch alive? false)
       (clear-patch!))

  (define points
    '((3 3) (4 3) (5 3) (5 4) (4 5)))
  (for ([p points])
    (current-patch (->patch (first p) (second p)))
    (set patch alive? true)
    (set patch pcolor yellow))
)

(define (neighbors-alive neighbors)
  (define found-alive 0)
  (for ([nid neighbors])
    (parameterize ([current-patch nid])
      (when (get patch alive?)
        (set! found-alive (add1 found-alive)))))
  found-alive)

(define (go)

  (ask patches
       (define na (neighbors-alive (get patch neighbors)))
       (set patch na na))

  (ask patches
       (define na (get patch na))
       (cond
         [(get patch alive?)
          (cond
            [(or (zero? na) (= na 1))
             (set patch alive? false)
             (clear-patch! (current-patch))]
            [(or (= na 2) (= na 3))
             'DieAnotherDay]
            [(> na 3)
             (set patch alive? false)
             (clear-patch! (current-patch))]
            )]
         [(not (get patch alive?))
          (when (= na 3)
            (set patch alive? true)
            (set patch pcolor yellow))])
       )
  )

(run-world setup go)


