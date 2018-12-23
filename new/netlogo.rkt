#lang racket

(provide ask create sniff move increment! where)

(require (for-syntax syntax/parse) racket/hash)
(require "agentsets.rkt"
         "agents.rkt"
         "colors.rkt"
         "quadtree.rkt"
         "state.rkt"
         "get-set.rkt"
         "util.rkt")

;; 'ask' is a query language. It operates over agentsets.
;;
;; (ask <agentset> bodies ...)
;;
;; There are query operators, like
;;
;; (here <agentset>)
;;
;; which look for agents that are on the current patch. Or,
;;
;; (sniff <radius>)
;;
;; which return agents of the same breed within the given radius
;; from the current agent.
;;
;; So, all forms that live in <agentset> have the contract
;;
;; fun : agentset -> agentset

(define-syntax (ask stx)
  (syntax-parse stx
    [(ask as:expr bodies:expr ...)
     #`(begin
         ;; Set the quadtree as dirty at the top of every ask.
         (quadtree-is-dirty!)
         (current-agentset (as))
         (let ([agent-hash (agentset-agents (as))])
           ;;(printf "Asking ~a agents.~n" (hash-count (agentset-agents (as))))
           (for ([id (hash-keys agent-hash)])
             ;; Set the current agent.
           (current-agent (hash-ref agent-hash id))
           ;; Run the bodies
           bodies ...))
         )]
    ))

;; sniff searches the nearest neighbors to see who is in-radius.
;; (make-quadtree width height agents)
;; (neighbors qt dim x y radius)
(define sniff
  (case-lambda
    [(as radius)
     (define qt (make-quadtree (get-global 'world-columns)
                               (get-global 'world-rows)
                               (agentset-agents (as))))
     
     ;;(printf "Recalculating NN~n")
     (define NN (nearest-neighbors qt (get-global 'world-columns)
                                   (agent-x (current-agent)) (agent-y (current-agent))
                                   radius))
     (for ([(id agent) NN])
       (when (= id (agent-id (current-agent)))
         (hash-remove! NN id)))
     
     ;; Return the nearest neighbors as a set.
     (define new-as (struct-copy agentset (as)))
     (set-agentset-agents! new-as NN)
 
     (make-parameter new-as)]
    [(radius)
     (sniff current-agentset radius)]))


(define (create as n)
  (define h (make-hash))
  ;; (printf "~a -> ~a~n" (get-global 'last-id) (+ (get-global 'last-id) n))
  
  (for ([id (range (get-global 'last-id) (+ (get-global 'last-id) n))])
    ;; (printf "creating agent ~a ~a~n" (agentset-breed (as)) id)
    
    (hash-set! h id
               ;; (id pid x y color direction singular plural ...))
               (apply vector
                      (append (list id 0
                                    (/ (get-global 'world-columns) 2)
                                    (/ (get-global 'world-rows) 2)
                                    (color 255 255 255)
                                    (random 360)
                                    (agentset-breed (as))
                                    (agentset-plural (as))
                                    default-draw-function
                                    )
                              (map (λ (field) 0) (agentset-special-fields (as)))))
               ))
  (set-global! 'last-id (+ (get-global 'last-id) n))

  ;; This is all some silly juggling to make sure that the turtles we just created
  ;; come back in their own agentset. There is almost certainly a cheaper way, but
  ;; I don't want sharing with the original set.
  ;;(breed plural agents fields special-fields)
  (define ta (as))
  (define new-as (agentset (agentset-breed ta)
                           (agentset-plural ta)
                           h
                           (agentset-fields ta)
                           (agentset-special-fields ta)))
  (define the-h (agentset-agents ta))
  (for ([(k v) h])
    (hash-set! the-h k v))
  (make-parameter new-as)
  )


(define pi-conv (/ pi 180))

(define (offset x y direction magnitude)
  (define dir (* (+ direction 90) pi-conv))
  (define dy (* magnitude (sin dir)))
  (define dx (* magnitude (cos dir)))
  (values (+ x dx) (+ y dy)))

;; Agent actions.
(define move
  (case-lambda
    [(magnitude)
     (move (current-agent) magnitude)]
    [(ag magnitude)
     (define direction (get direction))
     (define-values (new-x new-y)
       (offset (get x)
               (get y)
               direction magnitude))
     (vector-set! ag (index-of agent-fields 'x) (wrap new-x (get-global 'world-columns)))
     (vector-set! ag (index-of agent-fields 'y) (wrap new-y (get-global 'world-rows)))
     ]))
  

(define-syntax-rule (increment! id)
  (set! id (add1 id)))

(define-syntax-rule (where as expr)
  (let ([new-h (make-hash)]
        [new-as (struct-copy agentset (as))])
    (current-agentset (as))
    (for ([(id agent) (agentset-agents (as))])
      (parameterize ([current-agent agent])
        (when expr
          (hash-set! new-h id agent))))
    (set-agentset-agents! new-as new-h)
    ;; (current-agentset new-as)
    (make-parameter new-as))) 

