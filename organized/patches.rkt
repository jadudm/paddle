#lang racket
(provide (all-defined-out))
(require "types.rkt")

(define the-world  (make-vector 1 false))
;; I'll track dirtyness as a boolean.
(define dirty-bits (make-hash))

;; To give us something to detect...
(struct patches ())

;; Given the world dimensions, create the patches
;; for this world. Every patch has a
;; hash table for its fields.
(define (create-patches x y)
  (set! the-world  (make-vector (* x y) false))
  (for ([px (range x)])
    (for ([py (range y)])
      (define pid (* px py))
      (vector-set! the-world pid (make-hash))
      ;; (printf "x ~a y ~a pid ~a~n" px py pid)
      (set-patch-field-no-dirty! pid 'pid pid)
      (set-patch-field-no-dirty! pid 'pxcor px)
      (set-patch-field-no-dirty! pid 'pycor py)
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
     (printf "set pid ~a k ~a v ~a~n" pid k v)
     (hash-set! (vector-ref the-world pid) k v)]))

(define get-patch-field
  (case-lambda
    [(x y k) (get-patch-field (* x y) k)]
    [(pid k)
     (define result (hash-ref (vector-ref the-world pid) k 'NoPatchFieldsFound))
     (printf "get pid ~a k ~a v ~a~n" pid k result)
     result]
     ))