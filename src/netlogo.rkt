#lang racket
(provide (all-defined-out)
         (rename-out
          [move forward]
          [move fd]
          [right rt]
          [left lt])
         )
(require "agentsets.rkt"
         "agents.rkt"
         "breeds.rkt"
         "state.rkt"
         "world.rkt"
         "util.rkt"
         "quadtree.rkt"
         "types.rkt"
         )
(require (for-syntax syntax/parse))

(define (get-agents as)
  (cond
    ;; If we're given a symbol, they're asking for the whole
    ;; agentset, and I should fetch it.
    [(symbol? as) (agentset->list (get-agentset as))]
    ;; Possible checks for things that are an error...
    ;; Or, check that it is the right kind of thing.
    ;; To be decided.
    [else as]))

(define-syntax (ask stx)
  (syntax-parse stx
    [(_ask (~datum patches) bodies:expr ...)
     #`(for ([i (range (vector-length (get-global 'the-world)))])
         (parameterize ([current-patch i])
           bodies ...))]
    
    [(_ask as bodies:expr ...)
     #`(let ()
         (parameterize ([current-agentset (get-agents as)])
           ;; FIXME
           ;; Leaky abstraction on agentsets.
           (for ([agent (get-agents as)])
             (when (vector? agent)
               (parameterize ([current-agent agent])
                 bodies ...))
             )))]
    ))

(define (sniff plural distance)
  (when (vector? (current-agent))
    (define new-as
      (filter (λ (a)
                (and (symbol=? (vector-ref a agent-plural) plural)
                     (not (= (vector-ref a agent-id)
                             (vector-ref (current-agent) agent-id)))))
              (send (current-quadtree)
                    query
                    (make-rect (vector-ref (current-agent) agent-x)
                               (vector-ref (current-agent) agent-y)
                               distance distance))))
    new-as))


(define (any? as)
  (> (length (get-agents as)) 0))

;; We need a syntax so that the expression will be
;; passed through. However, all we need to do is filter over the
;; agents provided, and return the resulting list.
(define-syntax (where stx)
  (syntax-parse stx
    [(_where as expr)
     #`(let ([agents (get-agents as)])
         (filter (λ (a)
                   (parameterize ([current-agent a])
                     (and a expr)))
                 agents))]
    ))

(define die
  (case-lambda
    [()
     (die (current-agent))]
    [(ca)
     (define plural (vector-ref ca agent-plural))
     ;; We just set that agent to false, and they go away.
     ;; I think.
     (remove-agent! (hash-ref agentsets plural)
                    (vector-ref ca agent-id)
                    )
     ;; Update the quadtree when things die, so they
     ;; can't interact again.
     ;; (build-quadtree)
     (send (current-quadtree)
           remove!
           (make-point (vector-ref ca agent-x)
                       (vector-ref ca agent-y)
                       ca))
     ]))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Movement
(define pi-conv (/ pi 180))

(define (->rational v)
  (cond
    [(rational? v) v]
    [else 0]))

(define (offset x y direction magnitude)
  (define dir (* direction pi-conv))
  (define dy (* magnitude (sin dir)))
  (define dx (* magnitude (cos dir)))
  (values (->rational (+ x dx))
          (->rational (+ y dy))))


(define (move magnitude)
  (define direction (vector-ref (current-agent) agent-direction))
  (define-values (new-x new-y)
    (offset (vector-ref (current-agent) agent-x)
            (vector-ref (current-agent) agent-y)
            direction magnitude))
  
  (vector-set! (current-agent)
       agent-x
       (wrap new-x (get-global 'world-columns)))
  (vector-set! (current-agent)
       agent-y
       (wrap new-y (get-global 'world-rows)))
  (vector-set! (current-agent)
               agent-pid
               (->pid
                ;; Need to floor this, or we can't index into
                ;; the patch array...
                (exact-floor (vector-ref (current-agent) agent-x))
                (exact-floor (vector-ref (current-agent) agent-y))))
  )


(define (right d)
  (vector-set! (current-agent)
               agent-direction
               (+ (vector-ref (current-agent) agent-direction) d)))

(define (left d)
  (vector-set! (current-agent)
               agent-direction
               (- (vector-ref (current-agent) agent-direction) d)))


(module+ test
  (require rackunit)
  
  (make-world 10 100)
  (create-breed turtle turtles)
  (create turtles 5)
  ;; (printf "~a~n" (get-agentset turtles))
  (define found-set (make-hash))
  (ask turtles
    (define id (vector-ref (current-agent) agent-id))
    ;; (printf "id: ~a~n" id)
    (hash-set! found-set id true)
    )
  (check-equal? (hash-keys found-set) '(0 1 2 3 4))
  
  ;; (printf "----~n")
  (set! found-set (make-hash))
  (ask (where turtles (> (vector-ref (current-agent) agent-id) 2))
    (define id (vector-ref (current-agent) agent-id))
    ;; (printf "id: ~a~n" id)
    (hash-set! found-set id true)
    )
  (check-equal? (hash-keys found-set) '(3 4))
  )