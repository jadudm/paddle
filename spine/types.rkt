#lang racket

(provide (all-defined-out))

(struct color (red green blue) #:transparent)

;; Needed in multiple places, for syntax.
(define agent-base-fields '(breed plural id pid x y direction color))