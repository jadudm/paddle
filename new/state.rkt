#lang racket

(provide (contract-out
          [agentsets                      hash?]
          [current-agent                  parameter?]
          [current-agentset               parameter?]
          [get-global                     (-> symbol? any)]
          [set-global!                    (-> symbol? any/c any)]
          [is-quadtree-dirty?             boolean?]
          [quadtree-is-dirty!             procedure?]
          [quadtree-is-clean!             procedure?]
          [always-generate-quadtree?      boolean?]
          [use-precise-sniffing           procedure?]
          [tick                           procedure?]
          [ticker                         parameter?]
         ))

;; This will be a hash of all of the agentsets.
;; See agentsets.rkt for implementation.
(define agentsets     (make-hash))

;; In (ask ...) forms, I need to track the current agent.
;; There should only be one at a time. (This precludes
;; a parallel implementation at this time.)
;; FIXME: Perhaps this syntax can be injected by ask?
(define current-agent (make-parameter false))
(define current-agentset (make-parameter false))

;; Globals
(define globals (make-hash))
(define (get-global k)
  (hash-ref globals k false))
(define (set-global! k v)
  (hash-set! globals k v))

;; Quadtrees
;; I don't want to regenerate these constantly.
;; So, I'll have a dirty bit, and only regenerate if
;; absolutely necessary.
(define is-quadtree-dirty? true)
(define (quadtree-is-dirty!)
  (set! is-quadtree-dirty? true))
(define (quadtree-is-clean!)
  (set! is-quadtree-dirty? false))
;; And, a way to override
(define always-generate-quadtree? false)
(define (use-precise-sniffing) (set! always-generate-quadtree? true))

;; The ticker!

(define ticker (make-parameter 0))
(define tick-pause (make-parameter (/ 1 60)))
(define (tick)
  (ticker (add1 (ticker)))
  (sleep (tick-pause)))

;; For unique IDs
(set-global! 'last-id 0)