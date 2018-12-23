#lang racket

(provide (all-defined-out))
(require "state.rkt"
         "types.rkt"
         "util.rkt"
         )

(require (for-syntax syntax/parse))

(define-syntax (get stx)
  (syntax-parse stx
    [(_get (~datum global) k)
     #`(get-global (quote k))]

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
     #`(set-global! (quote k) expr)]

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


;; ---------

(define dirty-patch!
  (case-lambda
    [(x y) (dirty-patch! (* (exact-floor x) (exact-floor y)))]
    [(pid) (hash-set! dirty-bits pid true)]))

(define clean-patch!
  (case-lambda
    [(x y) (clean-patch! (* (exact-floor x) (exact-floor y)))]
    [(pid) (when (hash-has-key? dirty-bits pid)
             (set-patch-field-no-dirty! pid 'pcolor (rgb-color 0 0 0))
             (hash-remove! dirty-bits pid))]))

(define patch-dirty?
  (case-lambda
    [(x y) (patch-dirty? (* (exact-floor x) (exact-floor y)))]
    [(pid) (hash-ref dirty-bits pid)]))

(define (get-dirty-patch-ids)
  (hash-keys dirty-bits))

(define set-patch-field-no-dirty!
  (case-lambda
    [(x y k v) (set-patch-field! (* (exact-floor x) (exact-floor y)) k v)]
    [(pid k v)
     ;; (dirty-patch! pid)
     (hash-set! (vector-ref (the-world) pid) k v)]))

(define set-patch-field!
  (case-lambda
    [(x y k v) (set-patch-field! (* (exact-floor x) (exact-floor y)) k v)]
    [(pid k v)
     ;; Setting something on a patch makes it dirty.
     (dirty-patch! pid)
     ;;(printf "set pid ~a k ~a v ~a~n" pid k v)
     (hash-set! (vector-ref (the-world) pid) k v)]))

(define get-patch-field
  (case-lambda
    [(x y k) (get-patch-field (* x y) k)]
    [(pid k)
     (define result (hash-ref (vector-ref (the-world) pid) k 'NoPatchFieldsFound))
     ;;(printf "get pid ~a k ~a v ~a~n" pid k result)
     result]
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
    

;; This needs to be memoized for performance.
;; Actually, it is probably not performance critical.
(define ->patch
  (case-lambda
    [(x y)
     (define xp (wrap (exact-floor x) (get global world-cols)))
     (define yp (wrap (exact-floor y) (get global world-rows)))
     
     (+ (* yp (get global world-rows)) xp)]
    [(agent)
     (->patch (hash-ref (agent-fields agent) 'xcor)
              (hash-ref (agent-fields agent) 'ycor))]
    ))