#lang racket
(provide (all-defined-out))

;; For this to work, I need individual agents.
;; They can all be the same. They'll keep their fields
;; in a hash. I debated calling that field "has", so it would be
;; (agent-has ...) as the accessor...
(struct agent (id breed fields) #:transparent)
;; An agentset should be a struct, so I know when I'm working with it.
(struct agentset (breed plural base-fields agents)
  #:transparent #:mutable)


;; Adding agents to an agentset should be easy.
(define/contract (add-to-agentset! λ:as ag)
  (-> procedure? agent? agent?)
  (define agents (hash-copy (agentset-agents (λ:as))))
  (hash-set! agents (agent-id ag) ag)
  (set-agentset-agents! (λ:as) agents)
  ag)

(define/contract (remove-from-agentset! λ:as ag:id)
  (-> procedure? number? number?)
  (define agents (hash-copy (agentset-agents (λ:as))))
  (hash-remove! agents ag:id)
  (set-agentset-agents! (λ:as) agents)
  ag:id)

;; A critical part of NetLogo is keeping track of all of the agents.
;; There are "default" agent sets, as well as new agent sets that the
;; user can introduce (eg. breed [fish fishes]). I need a way
;; to track all of these agent sets.
;;
;; agentsets are keyed by their plural.
(define agentsets (make-hash))

(define current-agent (make-parameter false))
(define current-patch (make-parameter false))

;; Globals
;; I'll store globals in a hash.
(define globals (make-hash))

(require (for-syntax syntax/parse))

(define-syntax (get stx)
  (syntax-parse stx
    [(_get (~literal global) k)
     #`(hash-ref globals (quote k))]

    [(_get (~literal patch) k)
     #`(cond
         [(hash-ref (agent-fields (current-patch)) 'dirty?)
          (define patch (hash-ref
                         (agentset-agents (hash-ref agentsets 'dirty-patches))
                         (agent-id (current-patch))))
          (hash-ref patch (quote k))]
         [else
          (hash-ref (agent-fields (current-patch)) (quote k))])]
    
    [(_get k)
     #`(cond
         ;; This should work for patches if patches are agents.
         [(and (current-agent)
               (hash-has-key? (agent-fields (current-agent)) (quote k)))
          (hash-ref (agent-fields (current-agent)) (quote k))]
         [else (error 'get "No key found for ~a" (quote k))])]
    [(_get a k)
     #`(cond
         ;; This should work for patches if patches are agents.
         [(hash-has-key? (agent-fields a) (quote k))
          (hash-ref (agent-fields a) (quote k))]
         [else (error 'get "No key found for ~a" (quote k))])]
    ))

(define-syntax (set stx)
  (syntax-parse stx
    [(_set (~literal global) k:id expr:expr)
     #`(hash-set! globals (quote k) expr)]

    [(_set (~literal global) k:expr expr:expr)
     #`(hash-set! globals k expr)]

    [(_set (~literal patch) k:id expr:expr)
     #`(begin
         (hash-set! (agent-fields (current-patch)) 'dirty? true)
         (hash-set! (agent-fields (current-patch)) (quote k) expr)
         (cond
           [(get (current-patch) dirty?)
            (define new-agent (agent (agent-id (current-patch))
                                     'patch
                                     (agent-fields (current-patch))))
            (add-to-agentset! (λ () (hash-ref agentsets 'dirty-patches)) new-agent)]
           [else
            ;; FIXME
            ;; If we were dirty, and are now clean, then the state of the dirty patch
            ;; should be moved over to the clean set. This way, if (say) some kind of
            ;; tracking variable was being used, it would carry over. The dirtyness really
            ;; only has to do with whether we should redraw the patch, not whether
            ;; it should have its brain wiped.
            (remove-from-agentset! (λ () (hash-ref agentsets 'dirty-patches))
                                   (agent-id (current-patch)))])
         )]
    [(_set k expr)
     #`(hash-set! (agent-fields (current-agent)) (quote k) expr)]
    [(_set a k expr)
     #`(hash-set! (agent-fields a) (quote k) expr)]    
    ))


;; Shortcut
;(define g get-global)
;(define global get-global)

;; Default globals
(define defaults '([world-cols     80]
                   [world-rows     80]
                   ))
(for ([p defaults])
  (set global (first p) (second p)))

;; Calculated globals
(define (set-frame-dimensions)
  (set global frame-width  (quotient 600 (get world-cols)))
  (set global frame-height (quotient 600 (get world-rows))))


;; These can be changed if the user chooses another edge-handling approach.
(set global edge-x (λ (val)
                     (define max (get global world-cols))
                     (cond
                       [(>= val max) (modulo (exact-floor val) max)]
                       [(< val 0) (modulo (+ max val) max)]
                       [else val])))
     (set global edge-y (λ (val)
                          (define max (get global world-rows))
                          (cond
                            [(>= val max) (modulo (exact-floor val) max)]
                            [(< val 0) (modulo (+ max val) max)]
                            [else val])))

     (define (combine l1 l2)
       (cond
         [(empty? l1) l2]
         [(member (first l1) l2)
          (combine (rest l1) l2)]
         [else
          (cons (first l1) (combine (rest l1) l2))]))