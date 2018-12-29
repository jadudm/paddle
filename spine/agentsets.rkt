#lang racket

(provide (contract-out
          [get-agentset               (-> symbol? vector?)]
          [set-agentset!              (-> symbol? vector? any)]
          [get-agentset-meta          (-> symbol? symbol? any)]
          [set-agentset-meta!         (-> symbol? symbol? any/c any)]
          [init-meta                  (-> symbol? any)]
          [get-max-id                 (-> symbol? number?)]
          ))

(require "util.rkt"
         "state.rkt")

;; For setting and getting the agentset vectors.
(define-values (get-agentset set-agentset!)
  (values
   (λ (k)   (hash-ref  agentsets k false))
   (λ (k v) (hash-set! agentsets k v))))

;; For setting and getting metadata about a given breed.
(define-values (get-agentset-meta set-agentset-meta!)
  (values
   (λ (plural k)   (hash-ref  (hash-ref agentsets-meta plural) k false))
   (λ (plural k v) (hash-set! (hash-ref agentsets-meta plural) k v))))

;; Initialize the metadata. Speciically, create an empty
;; hash table for a new breed. Used in the create-breed macro.
(define (init-meta plural)
  (hash-set! agentsets-meta plural (make-hash)))

;; Returns the largest identifier used for agents of this breed.
;; FIXME
;; Should all agents, of all breeds, have unique IDs?
;; https://bitbucket.org/jadudm/paddle/issues/9/should-agent-ids-be-globally-unique
(define (get-max-id plural)
  (get-agentset-meta plural
                     (combine-to-symbol
                      (get-agentset-meta plural 'singular)
                      '-next-index)))

