#lang racket

(provide create-breed)
(require syntax/parse
         (for-syntax syntax/parse
                     racket/syntax))
(require "agents.rkt"
         "agentsets.rkt"
         "state.rkt")

(define-syntax (create-breed stx)
  (syntax-parse stx
    
    [(_ s:id p:id (~optional (~seq #:have user-fields ...)
                             #:defaults ([(user-fields 1)  '()])))

     (with-syntax ([base   (format-id stx "struct:~a" #'s)]
                   [_make- (format-id stx "_make-~a" #'s)]
                   [make-  (format-id stx "make-~a" #'s)]
                   [pred?  (format-id stx "~a?" #'s)]
                   [set    (format-id stx "~a-set!" #'s)]
                   [get    (format-id stx "~a-get" #'s)]
                   ;; [current (format-id stx "current-~a" #'s)]
                   [_stx   stx])
       
       #`(begin
           (define special-fields (quote (user-fields ...)))
           (define all-fields (append agent-fields agent-control-fields special-fields))
           ;; (printf "~a sf: ~a (length ~a)~n" (quote s) special-fields (length special-fields))
           ;; This forms the basis of most (ask ...) forms.
           (define p (make-parameter false))
           (p (quote p))
           (hash-set! agentsets (quote p) (agentset (quote s) (quote p) (make-hash) all-fields special-fields))

           (define make- (λ values
                           (define v (make-vector (length all-fields)))
                           ;; Set the first 6 fields.
                           (for ([ndx (length agent-fields)])
                             (vector-set! v ndx (list-ref values ndx)))
                           (vector-set! v 6 (quote s))
                           (vector-set! v 7 (quote p))
                           (vector-set! v 8 default-draw-function)
                           (vector-set! v 9 all-fields)
                           
                           (for ([ndx (length (quote (user-fields ...)))])
                             (vector-set! v (+ ndx (length (append agent-fields agent-control-fields)))
                                          (list-ref values (+ (length agent-fields) ndx))))
                           v))
           
           (define-syntax-rule (get a k)
             (let ()
               (define ndx (index-of all-fields (quote k)))
               ;; (printf "get: ~a~n" ndx)
               (vector-ref a ndx)))
           (define-syntax-rule (set a k v)
             (vector-set! a (index-of (quote k) all-fields) v))
           )

       
       )]
    ))

(module+ test
  (require rackunit)
  (require "colors.rkt")
  (create-breed turtle turtles)
  (define t1 (make-turtle 1 2 3 4 (color 0 0 0) 5))
  (printf "t1: ~a~n" t1)
  (check-equal? (turtle-get t1 id) 1)
  (check-equal? (turtle-get t1 singular) 'turtle)
  
  (create-breed fish fishes #:have eyes)
  (define f1 (make-fish 1 2 3 4 (color 0 0 0) 5 42))
  (check-equal? (fish-get f1 eyes) 42)

  (define current-agent (make-parameter false))
  (create-breed people peoples #:have legs arms)
  (define p1 (make-people 1 2 3 4 (color 0 0 0) 5 6 7))
  (check-equal? (people-get p1 legs) 6)
  (check-equal? (people-get p1 singular) 'people)
  )