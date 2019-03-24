;;; Copyright 2018 Matt Jadud <matt@jadud.com>.
;;; All rights reserved.
;;; This work is licensed under the terms of the MIT license.
;; Based on the Termites model by Uri Wilensky
;; http://ccl.northwestern.edu/netlogo/models/Termites
;; GitHub: https://bit.ly/2CFVilN
#lang racket

(require paddle)

(define RxC 200)
(make-world RxC 600)
(create-patches)

(create-breed bug bugs #:have has-chip?)
(create bugs 80)

(define density 10)
(define jump-away 20)

(define white   (color 255 255 255))
(define orange  (color 255 140 0))
(define black   (color 0 0 0))
(define yellow  (color 255 255 0))
  
(define (setup)
  ;; Set some patches randomly to woodchips.
  (ask patches
    (when (< (random 100) density)
      (set patch color (color 255 255 0))))
  
  (ask bugs
    (set bug-x (random RxC))
    (set bug-y (random RxC))
    (set bug-color white)
    (set bug-has-chip? false)
    )
  )

(define (go)
  (ask bugs
    (search-for-chip)
    (find-new-pile)
    (put-down-chip))
  )


(define (search-for-chip)
  (cond
    [(and (equal? (get patch-here color) yellow)
          (not (get bug-has-chip?)))
     (set patch-here color black)
     (set bug-color orange)
     (set bug-has-chip? true)
     (fd jump-away)]
    [else
     (wiggle)
     (search-for-chip)])
  )

(define (find-new-pile)
  (when (not (equal? (get patch-here color) yellow))
    (wiggle)
    (find-new-pile)))

(define (put-down-chip)
  (cond
    [(and (equal? (get patch-here color) black)
          (get bug-has-chip?))
     (set patch-here color yellow)
     (set bug-color white)
     (set bug-has-chip? false)
     (get-away)]
    [else
     (rt (random 360))
     (fd 1)
     (put-down-chip)
     ]))

(define (get-away)
  (rt (random 360))
  (fd jump-away)
  ;; INTERESTING
  ;; If you forget the "not," there is almost no cohesion of
  ;; piles from the model. 
  (when (not (equal? (get patch-here color) black))
    (get-away))
  )

(define (wiggle)
  (fd 1)
  (rt (random 50))
  (lt  (random 50)))

(run-world setup go)