;;; Copyright 2018 Matt Jadud <matt@jadud.com>. All rights reserved.
;;; This work is licensed under the terms of the MIT license.

#lang racket

(require paddle)

(make-world 200 200 800 800)
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


