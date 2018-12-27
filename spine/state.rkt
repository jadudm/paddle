#lang racket

(provide (all-defined-out))

(define DEFAULT-AGENTSET-SIZE 10000)

(define current-agent    (make-parameter false))
(define current-agentset (make-parameter false))
(define current-quadtree (make-parameter false))

(define globals (make-hash))
(define-values (get-global set-global!)
  (values
   (λ (k) (hash-ref globals k false))
   (λ (k v) (hash-set! globals k v))))

;; Some defaults are a good idea, or "world.rkt" is
;; required everywhere...
(set-global! 'world-columns 20)
(set-global! 'world-rows    20)
(set-global! 'frame-width  400)
(set-global! 'frame-height 400)

(define (->pid x y)
  (+ (* (get-global 'world-rows) y) x))


(define ticker (make-parameter 0))
(define tick-pause (make-parameter (/ 1 60)))
(define (tick)
  (ticker (add1 (ticker)))
  (sleep (tick-pause)))