#lang racket

(provide (all-defined-out))
(require "util.rkt")

;; Agentsets map
;; plural-symbol -> vector
;; eg.
;; 'turtles -> (vector (vector ...) ...)
(define agentsets (make-hash))
;; Metadata is two deep. Every breed has its own metadata.
;; plural -> (hash (key value) ...)
(define agentsets-meta (make-hash))

(define-values (get-agentset set-agentset!)
  (values
   (λ (k)   (hash-ref  agentsets k false))
   (λ (k v) (hash-set! agentsets k v))))

(define-values (get-agentset-meta set-agentset-meta!)
  (values
   (λ (plural k)   (hash-ref  (hash-ref agentsets-meta plural) k false))
   (λ (plural k v) (hash-set! (hash-ref agentsets-meta plural) k v))))

(define (init-meta plural)
  (hash-set! agentsets-meta plural (make-hash)))

(define (get-max-id plural)
  (get-agentset-meta plural
                     (combine-to-symbol
                      (get-agentset-meta plural 'singular)
                      '-next-index)))

