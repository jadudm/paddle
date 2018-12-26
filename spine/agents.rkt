#lang racket

(require "agentsets.rkt"
         "util.rkt"
         )

(provide create
         show-agents
         )

(define base-fields '(breed plural id pid x y direction))

(define (make-default-agent sing plur ndx)
  (vector sing plur ndx 0 0 (random 360)))


;; This creates new agent vectors and inserts them into
;; the correct breed's agentset.
(define (create plural-sym num)
  (define singular (get-agentset-meta plural-sym 'singular))
  (define sym (combine-to-symbol singular '-next-index))
  (define starting-ndx (get-agentset-meta plural-sym sym))
  (for ([ndx (range starting-ndx (+ num starting-ndx))])
    (let ([agent (make-default-agent singular plural-sym ndx)]
          [asvec (get-agentset plural-sym)])
      (vector-set! asvec ndx agent)
      )
    (set-agentset-meta! plural-sym (combine-to-symbol singular '-next-index) (+ num starting-ndx))
    ))


;; Once agents start dying, this will not work.
(define (show-agents plural)
  (define agentset (hash-ref agentsets plural))
  (define max-id (get-max-id plural))
  (for ([ndx (range max-id)])
    ;; FIXME: What happens when agents die? Set them to false?
    (define agent (vector-ref agentset ndx))
    (when (vector? agent)
      (printf "~a: ~a~n" ndx agent))))