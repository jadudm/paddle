;;; Copyright 2018 Matt Jadud <matt@jadud.com>. All rights reserved.
;;; This work is licensed under the terms of the MIT license.

#lang racket

(require paddle)

(make-world 200 200 800 800)

;; ================ SETUP ========================
(define (setup)
  (ask patches
       (set patch alive? false)
       (set patch neighbors (get-neighbors (current-patch)))
       (clear-patch! (current-patch))
       
       (when (zero? (random 13))
         (set patch pcolor yellow)
         (set patch alive? true))
       ))

;; ================= GO ==========================
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

;; I want my cells to be yellow, and I don't want to have
;; type the RGB values over and over.
(define yellow (rgb 255 255 0))

;; Given a patch, I want the neighbors.
;; This should become part of the standard library.
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

;; This is a function I wrote to set up a test case.
;; Specifically, a single glider in the world. If my
;; rules are implemented correctly, it will glide across the
;; universe.
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

;; This is an important part of the GoL algorithm.
;; It tells me how many of my immediate neighbors are
;; alive. 
(define (neighbors-alive neighbors)
  (define found-alive 0)
  (for ([nid neighbors])
    (parameterize ([current-patch nid])
      (when (get patch alive?)
        (set! found-alive (add1 found-alive)))))
  found-alive)

;; Run the world.
(run-world setup go)


