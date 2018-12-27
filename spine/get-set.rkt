#lang racket

(provide (all-defined-out))
(require "state.rkt"
         "types.rkt"
         "util.rkt"
         )

(require (for-syntax syntax/parse racket/syntax "agents.rkt"))

(define-syntax (get stx)
  (syntax-parse stx
    [(_get (~datum global) k)
     #`(get-global (quote k))]

    [(_get (~datum patch) k)
     #`(get-patch-field (current-patch) (quote k))]
    
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
     #`(set-patch-field! (current-patch) (quote k) expr)]
    
    [(_set k expr)
     (with-syntax ([field (format-id stx "agent-~a" #'k)])
       #`(begin
           #;(printf "Setting agent ~a field ~a to ~a~n"
                   (vector-ref (current-agent) 2)
                   k
                   expr)
           ;; (sleep 1)
           (vector-set! (current-agent)
                        k
                        expr)))]
     
    [(_set a k expr)
     (with-syntax ([field (format-id stx "agent-~a" #'k)])
       #`(vector-set! a
                      k
                      expr))]
    ))


;; This needs to be memoized for performance.
;; Actually, it is probably not performance critical.
(define ->patch ->pid)
