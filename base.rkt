#lang racket/base

(provide (all-defined-out))

;; I would like these globals somewhere else.
(define world-rows (make-parameter 80))
(define world-cols (make-parameter 80))
(define frame-width (make-parameter 600))
(define frame-height (make-parameter 600))

(define (pixels-per)
  (/ (* (frame-width) (frame-height))
     (* (world-rows) (world-cols))))

(define pass 'pass)