#lang racket

(provide (all-defined-out))

(define current-agent (make-parameter false))

(define globals (make-hash))
(define-values (get-global set-global!)
  (values
   (λ (k) (hash-ref globals k false))
   (λ (k v) (hash-set! globals k v))))

(define (->pid x y)
  (+ (* (get-global 'world-rows) y) x))


(define ticker (make-parameter 0))
(define tick-pause (make-parameter (/ 1 60)))
(define (tick)
  (ticker (add1 (ticker)))
  (sleep (tick-pause)))