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

(define edge-case (make-parameter 'wrap))
(define-syntax-rule (set-edge-mode! type)
  (edge-case (quote type)))

(define (move magnitude)
  (define cur-x (vector-ref (current-agent) agent-x))
  (define cur-y (vector-ref (current-agent) agent-y))
  (define cur-vx (vector-ref (current-agent) agent-vx))
  (define cur-vy (vector-ref (current-agent) agent-vy))
  (define dx (* magnitude cur-vx))
  (define dy (* magnitude cur-vy))
  (define new-x (+ cur-x dx))
  (define new-y (+ cur-y dy))
  
  (define cols (get-global 'world-columns))
  (define rows (get-global 'world-rows))

  (case (edge-case)
    [(wrap)
     (vector-set! (current-agent)
                  agent-x
                  (wrap new-x cols))
     (vector-set! (current-agent)
                  agent-y
                  (wrap new-y rows))]
    [(bounce)
     (cond
       [(> new-x cols)
        (vector-set! (current-agent) agent-x (- cols 0.001))
        (vector-set! (current-agent) agent-vx (- cur-vx))]
       [(< new-x 0)
        (vector-set! (current-agent) agent-x 0.001)
        (vector-set! (current-agent) agent-vx (- cur-vx))])

     (cond
       [(> new-y rows)
        (vector-set! (current-agent) agent-y (- rows 0.001))
        (vector-set! (current-agent) agent-vy (- cur-vy))]
       [(< new-y 0)
        (vector-set! (current-agent) agent-y 0.001)
        (vector-set! (current-agent) agent-vy (- cur-vy))])
     
     (define cur-x2  (vector-ref (current-agent) agent-x ))
     (define cur-y2  (vector-ref (current-agent) agent-y ))
     (define cur-vx2 (vector-ref (current-agent) agent-vx))
     (define cur-vy2 (vector-ref (current-agent) agent-vy))
     
     (vector-set! (current-agent) agent-direction (theta cur-vx2 cur-vy2))
     
     (define dx2 (* magnitude cur-vx2))
     (define dy2 (* magnitude cur-vy2))
     
     (define new-x2 (+ cur-x2 dx2))
     (define new-y2 (+ cur-y2 dy2))
     
     (vector-set! (current-agent) agent-x new-x2)
     (vector-set! (current-agent) agent-y new-y2)] ;; end bounce
    ) ;; End case

  (vector-set! (current-agent)
                   agent-pid
                   (->pid
                    ;; Need to floor this, or we can't index into
                    ;; the patch array...
                    (exact-floor (vector-ref (current-agent) agent-x))
                    (exact-floor (vector-ref (current-agent) agent-y))))
  )


(define (right d)
  (define new-d (- (vector-ref (current-agent) agent-direction) d))
  (vector-set! (current-agent) agent-direction new-d)
  (define-values (vx vy) (degrees->components new-d))
  (vector-set! (current-agent) agent-vx vx)
  (vector-set! (current-agent) agent-vy vy)
  )

(define (left d)
  (define new-d (+ (vector-ref (current-agent) agent-direction) d))
  (vector-set! (current-agent) agent-direction new-d)
  (define-values (vx vy) (degrees->components new-d))
  (vector-set! (current-agent) agent-vx vx)
  (vector-set! (current-agent) agent-vy vy)
)


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