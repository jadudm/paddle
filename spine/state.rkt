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
(set-global! 'ticker 0)

(define (->pid x y)
  (+ (* (get-global 'world-rows) y) x))


(define ticker (thunk (get-global 'ticker)))
(define tick-pause (make-parameter (/ 1 60)))
(define (world-tick)
  (set-global! 'ticker (add1 (get-global 'ticker))))

;; For logging and plotting
(define log-conns (make-hash))

;; For cleanup of the world.
(define threads-to-kill '())
(define (add-thread-to-kill! t)
  (set! threads-to-kill (cons t threads-to-kill)))
(define stop (make-parameter false))