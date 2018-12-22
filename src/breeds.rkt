#lang racket

(require "structs.rkt")
(require (for-syntax syntax/parse
                     racket/syntax))
(require syntax/parse
         racket/syntax)

(provide (all-defined-out))

(define-syntax (create-breed stx)
  (syntax-parse stx
    [(_ s p)
     (with-syntax ([base  (format-id stx "~a" #'s)]
                   [_make- (format-id stx "_make-~a" #'s)]
                   [make- (format-id stx "make-~a" #'s)]
                   [pred? (format-id stx "~a?" #'s)]
                   [set   (format-id stx "~a-set!" #'s)]
                   [get   (format-id stx "~a-get" #'s)]
                   [_stx   stx])
       
       #`(begin
           (define-values (base _make- pred? _get _set) 
             (make-struct-type (quote s) struct:agent
                               2 0
                               false '()
                               false))
           (define special-fields '(singular plural))
           (define (make- a b c d e f)
             (_make- a b c d e f (quote s) (quote p)))
           (define-syntax (set stx2)
             (syntax-parse stx2
               [(_ f v)
                #`(set (current-agent) f v)]
               [(_ ag f v)
                 #`(let ()
                    (define ndx (index-of agent-fields (quote f)))
                    (cond
                      [(and (number? ndx) (<= ndx 6))
                       ;; (printf "~a ~a~n" ndx agent-fields)
                       (agent-set! ag ndx v)]
                      [else
                       (_set ag (index-of special-fields (quote f)))]))
                 ]))
           (define-syntax (get stx2)
             (syntax-parse stx2
               [(_ f)
                #`(get (current-agent) f)]
               
               [(_ ag f)
                #`(let ()
                    (define ndx (index-of agent-fields (quote f)))
                    (cond
                      [(and (number? ndx) (<= ndx 6))
                       ;; (printf "~a ~a~n" ndx agent-fields)
                       (agent-get ag ndx)]
                      [else
                       (_get ag (index-of special-fields (quote f)))]))]
               )))
       )]))

(module+ test
  (require rackunit)
  (create-breed turtle turtles)
  (define t1 (make-turtle 1 2 3 4 (color 0 0 0) 5))
  (check-equal? 1 (turtle-get t1 id))
  (check-equal? 'turtle (turtle-get t1 singular))
  )