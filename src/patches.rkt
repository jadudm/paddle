;;; Copyright 2018 Matt Jadud <matt@jadud.com>
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
;;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#lang racket
(provide (all-defined-out))
(require "types.rkt")

(define the-world  (make-vector 1 false))
;; I'll track dirtyness as a boolean.
(define dirty-bits (make-hash))

;; Given the world dimensions, create the patches
;; for this world. Every patch has a
;; hash table for its fields.
(define (create-patches rows cols)
  (set! the-world  (make-vector (* rows cols) false))
  (for ([col (range rows)])
    (for ([row (range cols)])
      (define pid (+ (* row rows) col))
      (vector-set! the-world pid (make-hash))
      ;; (printf "x ~a y ~a pid ~a~n" row col pid)
      (set-patch-field-no-dirty! pid 'pid pid)
      (set-patch-field-no-dirty! pid 'pxcor col)
      (set-patch-field-no-dirty! pid 'pycor row)
      (set-patch-field-no-dirty! pid 'pcolor (rgb-color 0 0 0))
      )))

(define dirty-patch!
  (case-lambda
    [(x y) (dirty-patch! (* (exact-floor x) (exact-floor y)))]
    [(pid) (hash-set! dirty-bits pid true)]))

(define clean-patch!
  (case-lambda
    [(x y) (clean-patch! (* (exact-floor x) (exact-floor y)))]
    [(pid) (when (hash-has-key? dirty-bits pid)
             (set-patch-field-no-dirty! pid 'pcolor (rgb-color 0 0 0))
             (hash-remove! dirty-bits pid))]))

(define patch-dirty?
  (case-lambda
    [(x y) (patch-dirty? (* (exact-floor x) (exact-floor y)))]
    [(pid) (hash-ref dirty-bits pid)]))

(define (get-dirty-patch-ids)
  (hash-keys dirty-bits))

(define set-patch-field-no-dirty!
  (case-lambda
    [(x y k v) (set-patch-field! (* (exact-floor x) (exact-floor y)) k v)]
    [(pid k v)
     ;; (dirty-patch! pid)
     (hash-set! (vector-ref the-world pid) k v)]))

(define set-patch-field!
  (case-lambda
    [(x y k v) (set-patch-field! (* (exact-floor x) (exact-floor y)) k v)]
    [(pid k v)
     ;; Setting something on a patch makes it dirty.
     (dirty-patch! pid)
     ;;(printf "set pid ~a k ~a v ~a~n" pid k v)
     (hash-set! (vector-ref the-world pid) k v)]))

(define get-patch-field
  (case-lambda
    [(x y k) (get-patch-field (* x y) k)]
    [(pid k)
     (define result (hash-ref (vector-ref the-world pid) k 'NoPatchFieldsFound))
     ;;(printf "get pid ~a k ~a v ~a~n" pid k result)
     result]
     ))