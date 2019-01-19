#lang racket

(provide (all-defined-out))
(require "state.rkt"
         "types.rkt"
         "util.rkt"
         )

(require (for-syntax syntax/parse racket/syntax "agents.rkt" "util.rkt"))

(define-syntax (get stx)
  (syntax-parse stx
    [(_get (~datum global) k)
     #`(get-global (quote k))]

    [(_get (~datum patch) k)
     #`(get-patch-field (current-patch) (quote k))]

    [(_get (~datum patch) p k)
     #`(get-patch-field p (quote k))]

    [(_get (~datum patch-here) k:id)
     (with-syntax ([apid (format-id stx "agent-pid")])
       #`(let ()
           (get-patch-field (vector-ref (current-agent) apid) (quote k))))]
    
    [(_get k)
     (with-syntax ([field (format-id stx "agent-~a" #'k)])
       #`(vector-ref (current-agent) k))]
     
    [(_get a k)
     (with-syntax ([field (format-id stx "agent-~a" #'k)])
       #`(vector-ref a k))]
    ))

(define-syntax (set stx)
  (syntax-parse stx
    [(_set (~datum global) k:id expr:expr)
     #`(set-global! (quote k) expr)]

    [(_set (~datum global) k:expr expr:expr)
     #`(hash-set! globals k expr)]

    [(_set (~datum patch) k:id expr:expr)
     ;; FIXME
     ;; Should patches be quoted, or literal?
     #`(set-patch-field! (current-patch) (quote k) expr)]

    [(_set (~datum patch-here) k:id expr:expr)
     (with-syntax ([apid (format-id stx "agent-pid")])
       #`(set-patch-field! (vector-ref (current-agent) apid) (quote k) expr))]
    
    [(_set k expr)
     #`(begin
         #;(printf "Setting agent ~a field ~a to ~a~n"
                   (current-agent)
                   (quote k)
                   expr)
         (when (string-contains? (symbol->string (quote k)) "-direction")
           #;(printf "\tPatching direction.~n")
           (define-values (vx vy) (degrees->components expr))
           (vector-set! (current-agent) agent-vx vx)
           (vector-set! (current-agent) agent-vy vy)
           #;(printf "\tvx: ~a~n\tvy: ~a~n" vx vy)
           )
         (vector-set! (current-agent) k expr))]
     
    [(_set a k expr)
     #`(vector-set! a k expr)]
    ))


;; This needs to be memoized for performance.
;; Actually, it is probably not performance critical.
(define ->patch ->pid)
;; ---------

(define dirty-patch!
  (case-lambda
    [(x y) (dirty-patch! (* (exact-floor x) (exact-floor y)))]
    [(pid) (hash-set! dirty-bits pid true)]))

(define clean-patch!
  (case-lambda
    [(x y) (clean-patch! (* (exact-floor x) (exact-floor y)))]
    [(pid) (when (hash-has-key? dirty-bits pid)
             (set-patch-field-no-dirty! pid 'color (color 0 0 0))
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
     (hash-set! (vector-ref (get-global 'the-world) pid) k v)]))

(define set-patch-field!
  (case-lambda
    [(x y k v) (set-patch-field! (* (exact-floor x) (exact-floor y)) k v)]
    [(pid k v)
     ;; Setting something on a patch makes it dirty.
     (dirty-patch! pid)
     ;;(printf "set pid ~a k ~a v ~a~n" pid k v)
     (hash-set! (vector-ref (get-global 'the-world) pid) k v)]))

(define get-patch-field
  (case-lambda
    [(x y k) (get-patch-field (* x y) k)]
    [(pid k)
     (define result (hash-ref (vector-ref (get-global 'the-world) pid) k 'NoPatchFieldsFound))
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
