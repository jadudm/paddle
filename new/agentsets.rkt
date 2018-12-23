#lang racket

(provide
 (struct-out agentset)
 (contract-out
  [get-agentset         (-> symbol? any)]
  [add-agentset!        (-> symbol? agentset? any)]
  ))
(require "state.rkt")

(struct agentset (breed plural agents fields special-fields) #:transparent #:mutable)

(define (get-agentset sym)
  (hash-ref (agentsets) sym false))

(define (add-agentset! sym as)
  (hash-set! (agentsets) sym as))