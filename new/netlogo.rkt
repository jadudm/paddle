#lang racket

(provide ask create)

(require (for-syntax syntax/parse))
(require "agentsets.rkt"
         "agents.rkt"
         "colors.rkt"
         "quadtree.rkt"
         "state.rkt")

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
         (for ([(id agent) (agentset-agents (as))])
           ;; Set the current agent.
           (current-agent agent)
           ;; Run the bodies
           bodies ...)
         )]
    ))

;; sniff searches the nearest neighbors to see who is in-radius.
;; (make-quadtree width height agents)
;; (neighbors qt dim x y radius)
(define qt (make-parameter false))
(define (sniff as radius)
  (when (or is-quadtree-dirty? always-generate-quadtree?)
    (qt (make-quadtree (get-global 'world-columns)
                       (get-global 'world-rows)
                       (agentset-agents (as))))
    (quadtree-is-clean!))
  
  (define NN (nearest-neighbors (qt) (get-global 'world-columns)
                                (agent-x (current-agent)) (agent-y (current-agent))
                                radius))
  ;; Return the nearest neighbors as a set.
  (agentset NN))


(define (create as n)
  (define h (make-hash))
  (for ([id (range (get-global 'last-id) (+ (get-global 'last-id) n))])
    (printf "creating agent ~a ~a~n" (agentset-breed (as)) id)
    
    (hash-set! h id
               ;; (id pid breed x y color direction))
               (apply (agentset-agent-maker (as))
                      (append (list id 0
                                    (/ (get-global 'world-columns) 2)
                                    (/ (get-global 'world-rows) 2)
                                    (get-random-color)
                                    (random 360))
                              (map (λ (field) 0) (agentset-agent-special-fields (as)))))
               ))
  (set-global! 'last-id (+ (get-global 'last-id) n))
  (set-agentset-agents! (as) h)
  )
