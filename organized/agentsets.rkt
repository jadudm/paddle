#lang racket
(require syntax/parse
         racket/contract
         (only-in racket/draw color%))
(require "base.rkt")

;; For this to work, I need individual agents.
;; They can all be the same. They'll keep their fields
;; in a hash. I debated calling that field "has", so it would be
;; (agent-has ...) as the accessor...
(struct agent (id breed fields) #:transparent)

;; And, everything will depend on knowing the current agent.
  

;; A critical part of NetLogo is keeping track of all of the agents.
;; There are "default" agent sets, as well as new agent sets that the
;; user can introduce (eg. breed [fish fishes]). I need a way
;; to track all of these agent sets.
;;
;; agentsets are keyed by their plural.
(define agentsets (make-hash))
;; An agentset should be a struct, so I know when I'm working with it.
(struct agentset (breed plural agents) #:transparent)
;; And, I need a way to manipulate agentsets. Better an ADT...
(define/contract (add-agentset as)
  (-> agentset? agentset?)
  (hash-set! agentsets (agentset-plural as) as)
  as)

;; There are 14 core colors. Whatevs.
(define (rgb r g b)
  (make-object color% r g b))

;; We need a default fieldset for agents.
(define (make-default-agent-fields id breed)
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

;; There are some default agentsets.
;; 'turtles' is one. 'patches' is another. For simplicity, it might
;; have to be that all agentsets 
(define turtles (agentset 'turtle 'turtles (make-hash)))
(add-agentset turtles)

(define patches (agentset 'patch  'patches (make-hash)))
(add-agentset patches)

;; Adding agents to an agentset should be easy.
(define/contract (add-to-agentset! as ag)
  (-> agentset? agent? agent?)
  (hash-set! (agentset-agents as) (agent-id ag) ag)
  ag)

;; Those are static agentsets. Some agentsets are actually more
;; dynamic. For example, 'turtles-here' is an agentset, but
;; it queries the world and returns the turtles on the current
;; patch. These will have to be implemented differently.
;; Or, they're just functions.

;; PROCEDURE
;; other : agentset -> agentset
;; PURPOSE
;; Returns an agentset with everything save for the current agent.
;; Must be executed in a context where the current agent is defined.
(define/contract (other as)
  (-> agentset? agentset?)
  (define current-agent-id (agent-id (current-agent)))
  (define h (make-hash))
  (for ([(id agnt) (agentset-agents as)])
    ;; Only keep things that are not the current agent.
    (unless (= current-agent-id id)
      (hash-set! h id agnt)))
  ;; Return a new set of agents.
  (agentset (agentset-breed as)
            (agentset-plural as)
            h))

;; The 'ask' macro is easier now. It isn't 'ask-turtles' and 'ask-fishes,' but instead
;; just 'ask' followed by an agentset.
(define-syntax (ask stx)
  (syntax-case stx ()
    [(_ as bodies ...)
     #`(begin
         (for ([(id agent) (agentset-agents as)])
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

;; PROCEDURE
;; create : agentset number -> agentset
;; PURPOSE
;; Creates new agents in the agentset.
;; Returns the extended agentset.
(define (create as num)
  (define h (agentset-agents as))
  (define breed (agentset-breed as))
  (define start-id (hash-count h))
  (for ([id (range num)])
    (define offset-id (+ id start-id))
    ;; FIXME
    ;; I need a default set of values in the agent's fields.
    (define new-agent (agent offset-id breed (make-default-agent-fields offset-id breed)))
    (add-to-agentset! as new-agent)
    )
  as)

;; Now, it would be nice to be able to have a "with" form
;; that is used as part of the agentset filtering.
(define-syntax (with stx)
  (syntax-case stx ()
    [(with as bool-exp)
     #`(let ()
         (define subset (make-hash))
         (for ([(id agent) (agentset-agents as)])
           (current-agent agent)
           (when bool-exp
             (hash-set! subset
                        (agent-id (current-agent))
                        (current-agent))))
         (agentset (agentset-breed as)
                   (agentset-plural as)
                   subset))
     ]))

(define-syntax (have stx)
  (syntax-case stx ()
    [(_ var)
     #`(hash-ref (agent-fields (current-agent)) (quote var) false)]))


(define-syntax (get stx)
  (syntax-case stx ()
    [(_ var)
     #`(hash-ref (agent-fields (current-agent)) (quote var) false)]))

(ask (with turtles (> (have id) 3))
       (printf "id ~a~n" (agent-id (current-agent))))



(create turtles 15)
(ask (with turtles (< (agent-id (current-agent)) 3))
     (printf "~a~n" (agent-id (current-agent))))

(ask (with turtles (and (> (have id) 3)
                        (> (have direction) 0)
                        (< (have direction) 90)))
       (printf "id ~a ~a~n" (get id) (get direction)))

(define-syntax (gots? stx)
  (syntax-case stx ()
    [(_ as bool-exp)
     #`(let ()
         (define counter 0)
         (for ([(id agent) (agentset-agents as)])
           (current-agent agent)
           (when bool-exp
             (set! counter (add1 counter))))
         ;; If the bool-exp was ever true, I incremented
         ;; the counter. So, I should return true.
         (> counter 0))
     ]))

(when (gots? turtles (> (have id) 10))
  (printf "Turtles have ids greater than 10!~n"))
