#lang racket

(provide (all-defined-out))

(struct color (red green blue) #:transparent)

;; Needed in multiple places, for syntax.
(define agent-base-fields   '(breed plural id pid x y vx vy direction shape color))
(begin-for-syntax
  (define agent-base-fields '(breed plural id pid x y vx vy direction shape color)))

(require (for-syntax syntax/parse racket/syntax racket/list))
(define-syntax (create-accessors stx)
  (syntax-parse stx
    [(_ca)
     (with-syntax ([(id ...)
                    (for/list ([o agent-base-fields])
                      (format-id stx "agent-~a" o))]
                    [(nums ...) (range (length agent-base-fields))]
                   )
       #`(begin (define id nums) ...
                (provide id) ...))]))

(create-accessors)