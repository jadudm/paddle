#lang racket/base

(provide (all-defined-out))

;; I would like these globals somewhere else.
(define world-rows (make-parameter 80))
(define world-cols (make-parameter 80))

(define pass 'pass)