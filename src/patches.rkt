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
  ;; Initialize all of the patches in the microworld.
  (for ([col (range rows)])
    (for ([row (range cols)])
      (define pid (+ (* row rows) col))
      (vector-set! (get-global 'the-world) pid (make-hash))

      ;; -no-dirty!
      ;; This function allows me to update properties of the patches
      ;; without setting the 'dirty' bit. Otherwise, all of the patches
      ;; will be signified as dirty, they will all be drawn by routines in
      ;; 'wor'd, and the entire simulation will come to a screaming halt.
      (set-patch-field-no-dirty! pid 'pid pid)
      (set-patch-field-no-dirty! pid 'patch-x col)
      (set-patch-field-no-dirty! pid 'patch-y row)
      (set-patch-field-no-dirty! pid 'patch-color (color 0 0 0))
      )))
