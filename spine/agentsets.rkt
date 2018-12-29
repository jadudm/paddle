#lang racket

(provide (contract-out
          
          
          [get-max-id                 (-> symbol? number?)]
          [compact-agentset           (-> symbol? any)]
          [extend-agentset            (-> symbol? any)]
          ))

(require "util.rkt"
         "state.rkt")


;; Returns the largest identifier used for agents of this breed.
;; FIXME
;; Should all agents, of all breeds, have unique IDs?
;; https://bitbucket.org/jadudm/paddle/issues/9/should-agent-ids-be-globally-unique
(define (get-max-id plural)
  (get-agentset-meta plural
                     (combine-to-symbol
                      (get-agentset-meta plural 'singular)
                      '-next-index)))

(define (compact vec)
  (define insert 0)
  (define read 0)
  (define vleng (vector-length vec))
  (define new-end-index 0)
  (let loop ([read 0]
             [insert 0])
    (cond
      [(< read vleng)
       (define read-head (vector-ref vec read))
       ;; (printf "rh: ~a~n" read-head)
       (cond
         [(false? read-head)
          (loop (add1 read) insert)]
         [(= read insert)
          (loop (add1 read) (add1 insert))]
         ;; Otherwise, copy from read to insert, and increment both.
         [else
          ;; Copy to the insert location
          (vector-set! vec insert (vector-ref vec read))
          ;; False-out the read location
          (vector-set! vec read false)
          ;; Increment both.
          (loop (add1 read) (add1 insert))])]
      [else
       (set! new-end-index insert)]))
  ;; (printf "v*: ~a~n" vec)
  ;; (printf "nx: ~a~n" new-end-index)
  new-end-index
  )

(define (compact-agentset plural)
  ;; (printf "Compacting ~a~n" plural)
  (define singular (get-agentset-meta plural 'singular))
  (define as (get-agentset plural))
  (define new-end-index (compact as))
  ;; (printf "New end index for ~a: ~a~n" plural new-end-index)
  ;; Reset the max counter for this breed.
  (set-agentset-meta! plural (combine-to-symbol singular '-next-index) new-end-index)
  )

(define (extend-agentset plural)
  (printf "Extending ~a to ~a~n" plural (get-max plural))
  ;; (printf "Compacting ~a~n" plural)
  (define singular (get-agentset-meta plural 'singular))
  (define as (get-agentset plural))
  (define new-end-index (compact as))
  (define new-as (make-vector (get-max plural) false))
  (for ([n new-end-index])
    (vector-set! new-as n (vector-ref as n)))
  (set-agentset! plural new-as)
  (printf "New agentset: ~a~n" (vector-length (get-agentset plural)))
  ;; (printf "New end index for ~a: ~a~n" plural new-end-index)
  ;; Reset the max counter for this breed.
  (set-agentset-meta! plural (combine-to-symbol singular '-next-index) new-end-index)
  )

(module+ test
  (require rackunit)
  (define v (make-vector 10 false))
  
  (for ([n (vector-length v)])
    (when (even? n)
      (vector-set! v n n)))
  (define nex (compact v))

  (check-equal? nex 5)
  (check-equal? v #(0 2 4 6 8 #f #f #f #f #f))
  )
  