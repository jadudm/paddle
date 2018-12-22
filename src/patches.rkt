#lang racket
(provide (all-defined-out))

(require "types.rkt"
         "util.rkt"
         "get-set.rkt"
         "state.rkt")


;; Given the world dimensions, create the patches
;; for this world. Every patch has a
;; hash table for its fields.
(define (create-patches rows cols)
  (the-world  (make-vector (* rows cols) false))
  (for ([col (range rows)])
    (for ([row (range cols)])
      (define pid (+ (* row rows) col))
      (vector-set! (the-world) pid (make-hash))
      ;; (printf "x ~a y ~a pid ~a~n" row col pid)
      (set-patch-field-no-dirty! pid 'pid pid)
      (set-patch-field-no-dirty! pid 'pxcor col)
      (set-patch-field-no-dirty! pid 'pycor row)
      (set-patch-field-no-dirty! pid 'pcolor (rgb-color 0 0 0))
      )))

;; values

(define (get-patch-coordinate pid)
  (coordinate (quotient  pid (get global world-cols))
              (remainder pid (get global world-rows))))