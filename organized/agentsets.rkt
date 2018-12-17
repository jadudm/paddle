#lang racket
(require syntax/parse
         racket/contract
         (only-in racket/draw color%))
(require "base.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;
;; BREEDS
;; Breeds suggest new agentsets.
(define-syntax (create-breed stx)
  (syntax-case stx ()
    [(_ breed plural)
     #`(begin
         (add-agentset (agentset (quote breed)
                                 (quote plural)
                                 (hash-keys (make-default-agent-fields
                                             'unused-id
                                             (quote breed)))
                                 (make-hash)))
         (define plural
           (λ () (hash-ref agentsets (quote plural))))
         )
     ]))

;; And, everything will depend on knowing the current agent.
;; This has moved to a separate module, because it is required
;; for syntax and runtime. The module boundary solves that phase issue.

;; A critical part of NetLogo is keeping track of all of the agents.
;; There are "default" agent sets, as well as new agent sets that the
;; user can introduce (eg. breed [fish fishes]). I need a way
;; to track all of these agent sets.
;;
;; agentsets are keyed by their plural.
(define agentsets (make-hash))

;; And, I need a way to manipulate agentsets. Better an ADT...
(define/contract (add-agentset as)
  (-> agentset? agentset?)
  (hash-set! agentsets (agentset-plural as) as)
  as)

;; Adding fields to a things...
(define (set-agentset-base-fields! as fields)
  ;; We have to add this to every agent.
  (define agents (for/hash ([(id agent)
                             (agentset-agents (as))])
                   ;; Hopefully my set macro works...
                   ;; FIXME handle boolean? defaults.
                   (set agent field 0)
                   (values id agent)))
  ;; And build a new agentset.
  (define new-agentset
    (agentset (agentset-breed (as))
              (agentset-plural (as))
              (combine (agentset-base-fields (as)) fields)
              agents))
  (hash-set! agentsets (agentset-plural (as)) new-agentset))

;; This has to happen regardless of when
(define-syntax (give stx)
  (syntax-case stx ()
    [(_ as fields ...)
     ;; FIXME
     ;; This sets the global params for the agentset. However,
     ;; it should also update all agents.
     #`(begin
         (set-agentset-base-fields! as (quote (fields ...)))
         (printf "Dealing with ~a~n" (as))
         (for ([(id agent) (agentset-agents (as))])
           (printf "Looking at agent ~a~n" id)
           ;; Unless they already have a value there, we'll
           ;; assign a value of zero, so every agent gets the key.
           (for ([k (quote (fields ...))])
             (printf "Adding field: ~a~n" k)
             (unless (hash-has-key? (agent-fields agent) k)
               (printf "~a added to ~a:~a~n" k (agentset-breed (as)) id)
               (hash-set! (agent-fields agent) k 0)))))
         ]))

;; FIXME: There are 14 core colors.
;; It will take some effort to build the NetLogo color table.
(define/contract (rgb r g b)
  (-> byte? byte? byte? (is-a?/c color%))
  (make-object color% r g b))

;; We need a default fieldset for agents.
(define/contract (make-default-agent-fields id breed)
  (-> (or/c symbol? number?) symbol? hash?) 
  (define h (make-hash))
  (hash-set! h 'id id)
  (hash-set! h 'breed breed)
  (hash-set! h 'xcor 0)
  (hash-set! h 'ycor 0)
  (hash-set! h 'direction (random 360))
  (hash-set! h 'color (rgb (+ 64 (random 128))
                           (+ 64 (random 128))
                           (+ 64 (random 128))))
  h)

;; Adding agents to an agentset should be easy.
(define/contract (add-to-agentset! λ:as ag)
  (-> procedure? agent? agent?)
  (define agents (hash-copy (agentset-agents (λ:as))))
  (hash-set! agents (agent-id ag) ag)
  (set-agentset-agents! (λ:as) agents)
  ag)

;; Those are static agentsets. Some agentsets are actually more
;; dynamic. For example, 'turtles-here' is an agentset, but
;; it queries the world and returns the turtles on the current
;; patch. These will have to be implemented differently.
;; Or, they're just functions.

;; CONTRACT
;; other : agentset -> agentset
;; PURPOSE
;; Returns an agentset with everything save for the current agent.
;; Must be executed in a context where the current agent is defined.
(define/contract (other as)
  (-> agentset? agentset?)
  (define current-agent-id (agent-id (current-agent)))
  (define h (make-hash))
  (for ([(id agnt) (agentset-agents (as))])
    ;; Only keep things that are not the current agent.
    (unless (= current-agent-id id)
      (hash-set! h id agnt)))
  ;; Return a new set of agents.
  (agentset (agentset-breed (as))
            (agentset-plural (as))
            h))

;; The 'ask' macro is easier now. It isn't 'ask-turtles' and 'ask-fishes,' but instead
;; just 'ask' followed by an agentset.
(define-syntax (ask stx)
  (syntax-case stx ()
    [(_ as bodies ...)
     #`(begin
         (for ([(id agent) (agentset-agents (as))])
           (current-agent agent)
           bodies ...)
         )]))

;; However, I can't test anything yet, because I can't create any agentsets.
;; I need to be able to do that. I could 'create-turtles', and then I need
;; 'create-fishes', and so on. So, I should have a way of specifying breeds
;; just like a specify agentsets. Perhaps the agentset is good enough,
;; because it carries the info I need?
;;
;; This also looks like it is a magic value, when really the agentset
;; is just the parameter.

;; CONTRACT
;; create : agentset number -> agentset
;; PURPOSE
;; Creates new agents in the agentset.
;; Returns the extended agentset.
(define/contract (create λ:as num)
  (-> procedure? number? procedure?)
  (define h (agentset-agents (λ:as)))
  (define breed (agentset-breed (λ:as)))
  (define start-id (hash-count h))
  (for ([id (range num)])
    (define offset-id (+ id start-id))
    ;; FIXME
    ;; I need a default set of values in the agent's fields.
    (define default-fields (make-default-agent-fields offset-id breed))
    ;; Add in the fields that have been added.
    (for ([f (agentset-base-fields (λ:as))])
      (unless (hash-has-key? default-fields f)
        ;; FIXME: Always give zero as value?
        (hash-set! default-fields f 0)))
    (define new-agent (agent offset-id breed default-fields))
    (add-to-agentset! λ:as new-agent)
    )
  λ:as)

;; Now, it would be nice to be able to have a "with" form
;; that is used as part of the agentset filtering.
(define-syntax (with stx)
  (syntax-case stx ()
    [(with as bool-exp)
     #`(let ()
         (define subset (make-hash))
         (for ([(id agent) (agentset-agents (as))])
           (current-agent agent)
           (when bool-exp
             (hash-set! subset
                        (agent-id (current-agent))
                        (current-agent))))
         ;; Agentsets need to come back as a thunk.
         (λ () (agentset (agentset-breed (as))
                         (agentset-plural (as))
                         (agentset-base-fields (as))
                         subset)))
     ]))

(define-syntax (have stx)
  (syntax-case stx ()
    [(_ var)
     #`(hash-ref (agent-fields (current-agent)) (quote var) false)]
    [(_ a var)
     #`(hash-ref (agent-fields a) (quote var) false)]
    ))


(define-syntax (agent-get stx)
  (syntax-case stx ()
    [(_ var)
     #`(hash-ref (agent-fields (current-agent)) (quote var) false)]
    [(_ a var)
     #`(hash-ref (agent-fields a) (quote var) false)]))

(define-syntax (agent-set! stx)
  (syntax-case stx ()
    [(_ var expr)
     #`(hash-set! (agent-fields (current-agent)) (quote var) expr)]
    [(_ a var expr)
     #`(hash-set! (agent-fields a) (quote var) expr)]))

(define-syntax (any? stx)
  (syntax-case stx ()
    [(_ as bool-exp)
     #`(let ()
         (define counter 0)
         (for ([(id agent) (agentset-agents (as))])
           (current-agent agent)
           (when bool-exp
             (set! counter (add1 counter))))
         ;; If the bool-exp was ever true, I incremented
         ;; the counter. So, I should return true.
         (> counter 0))
     ]))


;; What if I want to know if everyone has a property?
(define-syntax (all? stx)
  (syntax-case stx ()
    [(_ as bool-exp)
     #`(let ()
         (define flag true)
         (for ([(id agent) (agentset-agents (as))])
           (current-agent agent)
           (set! flag (and flag bool-exp)))
         flag)
     ]))

;; How about the ability to pull one agent at random?

;; CONTRACT
;; one-of : agentset -> agentset
;; PURPOSE
;; Selects one agent at random from the agentset, returning
;; an agentset with just one agent. This has an expensive
;; operation to turn the keys into a list... but, I don't know
;; a better way to guarantee that I'll get a valid id.
(define/contract (one-of as)
  (-> agentset? agentset?)
  (define h (agentset-agents (as)))
  (define keys (hash-keys h))
  (agentset (agentset-breed (as))
            (agentset-plural (as))
            (hash-ref h (list-ref keys (random (length keys))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Patches are not that different.

;; The world has to do with patches.
;; It requires the agentsets module, because patches are an agentset.

;;(define patches (agentset 'patch  'patches empty (make-hash)))
;;(add-agentset patches)

(create-breed patch patches)

;; If patches are indexed reasonably in the agentset, then it should be
;; possible to index into them quickly.
;;
;; If they are sequential, I can mathematically map to the correct patch.
;; This will all need to be done at run-time. The user needs to be able
;; to change the number or rows/cols, and as a result, all of this needs to be
;; dynamic.
(define (create-patches)
  (create patches (* (get world-cols) (get world-rows))))

;; Everything is in a coordinate system with OpenGL where the number of
;; columns and rows is scaled to the height and width of the viewport.
;; Therefore, [79.9, 79.9] will map to [79,79], which is less than 80x80.
;; Can we ever get a value outside the range? I don't know. This has to do with
;; whether we map by wrapping or not.
(define (->patch x y)
  (* ((get edge-x) x)
     ((get edge-y) y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Movement
(define pi-conv (/ pi 180))

(define (offset x y direction magnitude)
  (define dir (* (+ direction 90) pi-conv))
  (define dy (* magnitude (sin dir)))
  (define dx (* magnitude (cos dir)))
  (values (+ x dx) (+ y dy)))

(define (wrap v edge)
  (cond
    [(> v edge) 0]
    [(<= v 0) edge]
    [else v]))

(define (move magnitude)
  (define direction (agent-get direction (current-agent)))
  (define-values (new-x new-y)
    (offset (agent-get (current-agent) xcor)
            (agent-get (current-agent) ycor)
            direction magnitude))
  (set (current-agent)
       x
       (wrap new-x (get-global world-rows)))
  (set (current-agent)
       y
       (wrap new-y (get-global world-cols)))
  )

(define (right d)
  (set (current-agent) direction (+ (get (current-agent) direction) d)))

(define (left d)
  (set (current-agent) direction (- (get (current-agent) direction) d)))



;; TESTING

;; There are some default agentsets.
;; 'turtles' is one. 'patches' is another. For simplicity, it might
;; have to be that all agentsets 
;; (define turtles (agentset 'turtle 'turtles empty (make-hash)))
"Creating turtles"
(create-breed turtle turtles)
;; (add-agentset turtles)
"Giving them attributes"
(give turtles boogers)
"Creating 5 turtles."
(create turtles 5)

"Let us see what we have wrought"
(turtles)

"Asking turtles to do something"
(ask (with turtles (< (agent-id (current-agent)) 3))
     (printf "~a~n" (agent-id (current-agent))))

(ask (with turtles (and (> (have id) 3)
                        (> (have direction) 0)
                        (< (have direction) 90)))
       (printf "id ~a ~a~n" (agent-get id) (agent-get direction)))


(ask (with turtles (> (have id) 3))
       (printf "id ~a~n" (agent-id (current-agent))))


;; Now I can ask if an agentset has some property.
;; I would like it if I didn't have to wrap agent variables up
;; in a syntax, but otherwise I have to introduce users to symbols...
;; perhaps I should. That would, though, require that I interpret
;; the boolean expressions, and expand symbols out to hash-refs.
(when (any? turtles (> (have id) 10))
  (printf "At least one turtle has an id greater than 10!~n"))


(when (all? turtles (> (have id) -1))
  (printf "ALL turtles have ids greater than -1!~n"))
(when (all? turtles (> (have id) 5))
  (printf "ALL turtles have ids greater than 5!~n"))