#lang racket

(provide (all-defined-out))
(require "state.rkt")

(define (make-world cols pixels)
  (set-global! 'world-cols cols)
  (set-global! 'world-rows cols)
  (set-global! 'frame-width pixels)
  (set-global! 'frame-height pixels))
