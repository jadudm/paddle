;;; Copyright 2018 Matt Jadud <matt@jadud.com>. All rights reserved.
;;; This work is licensed under the terms of the MIT license.

#lang racket
(require (only-in racket/draw color%))

(provide (all-defined-out))
(require "types.rkt"
         "patches.rkt")

;(require (for-syntax "patches.rkt"))

;; For getting values from the interface.
(define interface-values (make-hash))
(define interface-dirty? false)
(define (set-interface-dirty! v)
  (set! interface-dirty? v))


(define next-agent-id 0)
(define (set-next-agent-id! v)
  (set! next-agent-id v))
(define (get-next-agent-id)
  next-agent-id)

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
    [(_get (~datum global) k)
     #`(hash-ref globals (quote k))]

    [(_get (~datum patch) k)
     #`(get-patch-field (current-patch) (quote k))]
    
    [(_get k)
     #`(cond
         ;; This should work for patches if patches are agents.
         [(and (agent? (current-agent))
               (hash-has-key? (agent-fields (current-agent)) (quote k)))
          (hash-ref (agent-fields (current-agent)) (quote k))]
         [else (error 'get "No key found for ~a" (quote k))])]
    [(_get a k)
     #`(cond
         ;; This should work for patches if patches are agents.
         [(and (agent? a)
               (hash-has-key? (agent-fields a) (quote k)))
          (hash-ref (agent-fields a) (quote k))]
         [else (error 'get "No key found for ~a" (quote k))])]
    ))

(define-syntax (set stx)
  (syntax-parse stx
    [(_set (~datum global) k:id expr:expr)
     #`(hash-set! globals (quote k) expr)]

    [(_set (~datum global) k:expr expr:expr)
     #`(hash-set! globals k expr)]

    [(_set (~datum patch) k:id expr:expr)
     #`(set-patch-field! (current-patch) (quote k) expr)]
    
    [(_set k expr)
     #`(when (agent? (current-agent))
         (hash-set! (agent-fields (current-agent)) (quote k) expr))]
    
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


;; FIXME: There are 14 core colors.
;; It will take some effort to build the NetLogo color table.
(define/contract (rgb r g b)
  (-> byte? byte? byte? rgb-color?)
  (rgb-color r g b))

;; Everything is in a coordinate system with OpenGL where the number of
;; columns and rows is scaled to the height and width of the viewport.
;; Therefore, [79.9, 79.9] will map to [79,79], which is less than 80x80.
;; Can we ever get a value outside the range? I don't know. This has to do with
;; whether we map by wrapping or not.


(define (wrap val max)
  ;;(printf "val ~a max ~a~n" val max)
  (define v (exact-floor val))
  (cond
    [(>= val max) (modulo v max)]
    [(< val 0) (modulo (+ max v) max)]
    [else val])
  )

;; This needs to be memoized for performance.
;; Actually, it is probably not performance critical.
(define ->patch
  (case-lambda
    [(x y)
     ;;(printf "x ~a y ~a~n" x y)
     ;(define edge-y ((get global edge-y) (exact-floor y)))
     ;(define edge-x ((get global edge-x) (exact-floor x)))
     (define xp (wrap (exact-floor x) (get global world-cols)))
     (define yp (wrap (exact-floor y) (get global world-rows)))
     
     (+ (* yp (get global world-rows)) xp)]
    [(agent)
     (->patch (hash-ref (agent-fields agent) 'xcor)
              (hash-ref (agent-fields agent) 'ycor))]
    ))


(define clear-patch!
  (case-lambda
    [()
     (cond
       [(number? (current-patch)) (clear-patch! (current-patch))]
       [else
        (clear-patch! (->patch (current-agent)))])]
    [(pid)
     (clean-patch! pid)]))
     


;; values

(define (get-patch-coordinate pid)
  (coordinate (quotient  pid (get global world-cols))
              (remainder pid (get global world-rows))))
