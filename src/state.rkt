#lang racket

(provide (all-defined-out))

;; For Patches / dirty backing
(define the-world  (make-parameter (make-vector 1 false)))
;; I'll track dirtyness as a boolean.
(define dirty-bits (make-hash))

(define next-agent-id 0)
(define (set-next-agent-id! v)
  (set! next-agent-id v))
(define (get-next-agent-id)
  next-agent-id)

;; Default globals
(define defaults '([world-cols  80]
                   [world-rows  80]
                   ))


(define ticker (make-parameter 0))
(define tick-pause (make-parameter (/ 1 60)))
(define (tick)
  (ticker (add1 (ticker)))
  (sleep (tick-pause)))



;; Globals
;; I'll store globals in a hash.
(define globals (make-hash))
(for ([p defaults])
  (hash-set! globals (first p) (second p)))

(define (get-global k)
  (hash-ref globals k false))
(define (set-global! k v)
  (hash-set! globals k v))

;; Calculated globals
(define (set-frame-dimensions)
  (set-global! 'frame-width  (quotient 600 (get-global 'world-cols)))
  (set-global! 'frame-height (quotient 600 (get-global 'world-rows))))


;; For drawing
(define draw-opengl (make-parameter false))
(define stop (make-parameter false))