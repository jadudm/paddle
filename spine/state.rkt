#lang racket

(provide (all-defined-out))

(define globals (make-hash))
(define-values (get-global set-global!)
  (values
   (λ (k) (hash-ref globals k false))
   (λ (k v) (hash-set! globals k v))))
