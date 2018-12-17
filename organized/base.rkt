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

(define current-agent (make-parameter false))
(define current-patch (make-parameter false))

;; Globals
;; I'll store globals in a hash.
(define globals (make-hash))

;; And, provide an ADT.
;(define (set-global! k v) (hash-set! globals k v))
(define-syntax (set-global! stx)
  (syntax-case stx ()
    [(_ k v)
     #`(hash-set! globals (quote k) v)]))

(define-syntax (get-global stx)
  (syntax-case stx ()
    [(_ k)
     #`(hash-ref globals (quote k) false)]
    [(_ k missing)
     #`(hash-ref globals (quote k) missing)]))

(define-syntax (get stx)
  (syntax-case stx ()
    [(_ k)
     #`(cond
         ;; This should work for patches if patches are agents.
         [(and (current-agent)
               (hash-has-key? (agent-fields (current-agent)) (quote k)))
          (hash-ref (agent-fields (current-agent)) (quote k))]
         ;; This is a fallback. It lets a turtle get the pcolor, and
         ;; it will return a value based on the [x, y] of the turtle.
         [(and (current-patch)
               (hash-has-key? (agent-fields (current-patch)) (quote k)))
          (hash-ref (agent-fields (current-patch)) (quote k))]
         [(hash-has-key? globals (quote k))
          (hash-ref globals (quote k))]
         [else (error 'get "No key found for ~a" (quote k))])]
    [(_ a k)
     #`(cond
         ;; This should work for patches if patches are agents.
         [(hash-has-key? (agent-fields a) (quote k))
          (hash-ref (agent-fields a) (quote k))]
         ;; This is a fallback. It lets a turtle get the pcolor, and
         ;; it will return a value based on the [x, y] of the turtle.
         [(hash-has-key? (agent-fields a) (quote k))
          (hash-ref (agent-fields a) (quote k))]
         [(hash-has-key? globals (quote k))
          (hash-ref globals (quote k))]
         [else (error 'get "No key found for ~a" (quote k))])]
    ))

(require (for-syntax syntax/parse))

(define-syntax (set stx)
  (syntax-parse stx
    [(_set (~literal global) k:id expr:expr)
     #`(hash-set! globals (quote k) expr)]
    [(_set (~literal global) k:expr expr:expr)
     #`(hash-set! globals k expr)]
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
  (set global frame-width  (quotient 600 (get-global world-cols)))
  (set global frame-height (quotient 600 (get-global world-rows))))


;; These can be changed if the user chooses another edge-handling approach.
(set global edge-x (λ (val)
                       (define max (get-global world-cols))
                       (cond
                         [(> val max) (modulo val max)]
                         [(< val 0) (modulo (+ max val) max)]
                         [else val])))
(set global edge-y (λ (val)
                       (define max (get-global world-rows))
                       (cond
                         [(> val max) (modulo val max)]
                         [(< val 0) (modulo (+ max val) max)]
                         [else val])))

(define (combine l1 l2)
  (cond
    [(empty? l1) l2]
    [(member (first l1) l2)
     (combine (rest l1) l2)]
    [else
     (cons (first l1) (combine (rest l1) l2))]))