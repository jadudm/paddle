#lang racket
(provide (all-defined-out))

(require "types.rkt"
         "util.rkt"
         "get-set.rkt"
         "state.rkt")


;; Given the world dimensions, create the patches
;; for this world. Every patch has a
;; hash table for its fields.
(define (create-patches)
  (define rows (get-global 'world-rows))
  (define cols (get-global 'world-columns))
  (set-global! 'the-world (make-vector (* rows cols) false))
  (for ([col (range rows)])
    (for ([row (range cols)])
      (define pid (+ (* row rows) col))
      (vector-set! (get-global 'the-world) pid (make-hash))
      ;; (printf "x ~a y ~a pid ~a~n" row col pid)
      (set-patch-field-no-dirty! pid 'pid pid)
      (set-patch-field-no-dirty! pid 'patch-x col)
      (set-patch-field-no-dirty! pid 'patch-y row)
      (set-patch-field-no-dirty! pid 'patch-color (color 0 0 0))
      )))

;; values

#;(define (get-patch-coordinate pid)
  (coordinate (quotient  pid (get global world-cols))
              (remainder pid (get global world-rows))))