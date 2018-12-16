#lang racket
(require syntax/parse
         racket/contract)
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
    (define new-agent (agent offset-id breed (make-hash)))
    (add-to-agentset! as new-agent)
    )
  as)


