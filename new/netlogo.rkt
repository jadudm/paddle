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

;; The agentset is now a parameter containing a list of apairs (id plural)
(define-syntax (ask stx)
  (syntax-parse stx
    [(ask as:id bodies:expr ...)
     #`(begin
         (define the-as (if (agentset? as) (agentset-agents as) (agentset-agents (hash-ref agentsets (as)))))
         (for ([id (hash-keys the-as)])
           ;; Set the current agent.
           (current-agent (hash-ref the-as id))
           ;; Run the bodies
           bodies ...))
     ]
    [(ask as:expr bodies:expr ...)
     #`(begin
         (define the-as (agentset-agents as))
         (for ([id (hash-keys the-as)])
           ;; Set the current agent.
           (current-agent (hash-ref the-as id))
           ;; Run the bodies
           bodies ...))
     ]
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


(define (create plural-param n)
  ;; (printf "~a -> ~a~n" (get-global 'last-id) (+ (get-global 'last-id) n))
  (define as (hash-ref agentsets (plural-param)))
  (define this-set (make-hash))
  (define the-agents (agentset-agents as))
  
  (for ([id (range (get-global 'last-id) (+ (get-global 'last-id) n))])
    ;;(printf "creating agent ~a ~a~n" (agentset-breed as) id)
    (define new-agent
      (apply vector
             (append (list id 0
                           (/ (get-global 'world-columns) 2)
                           (/ (get-global 'world-rows) 2)
                           (color 255 255 255)
                           (random 360)
                           (agentset-breed as)
                           (agentset-plural as)
                           default-draw-function
                           (append agent-fields agent-control-fields (agentset-special-fields as))
                           )
                     (map (λ (field) 0) (agentset-special-fields as)))))
    
    (hash-set! this-set id new-agent)
    (hash-set! the-agents id new-agent))
  
  (set-global! 'last-id (+ (get-global 'last-id) n))
  (set-agentset-agents! as the-agents)
  
  (agentset (agentset-breed as)
            (agentset-plural as)
            this-set
            (agentset-fields as)
            (agentset-special-fields as))
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
        [the-as (agentset-agents as)])
    
    (for ([id (hash-keys (agentset-agents as))])
      (parameterize ([current-agent-id id])
        (define agent (hash-ref the-as id))
        (when expr
          (hash-set! new-h id agent))))
    
    (agentset (agentset-breed as)
              (agentset-plural as)
              new-h
              (agentset-fields as)
              (agentset-special-fields as))
    ))
