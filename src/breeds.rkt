#lang racket

(require "structs.rkt")

(require syntax/parse
         (for-syntax syntax/parse
                     racket/syntax))

(provide create-breed)

(define-syntax (create-breed stx)
  (syntax-parse stx
    
    [(_ s:id p:id (~optional (~seq #:have user-fields ...)
                             #:defaults ([(user-fields 1)  '()])))
     (with-syntax ([base  (format-id stx "~a" #'s)]
                   [_make- (format-id stx "_make-~a" #'s)]
                   [make- (format-id stx "make-~a" #'s)]
                   [pred? (format-id stx "~a?" #'s)]
                   [set   (format-id stx "~a-set!" #'s)]
                   [get   (format-id stx "~a-get" #'s)]
                   [_stx   stx])
       
       #`(begin
           (define special-fields (append (quote (user-fields ...)) '(singular plural) ))
           ;; (printf "~a sf: ~a (length ~a)~n" (quote s) special-fields (length special-fields))
           
           (define-values (base _make- pred? _get _set)
             (make-struct-type (quote s) struct:agent
                               (length special-fields) 0
                               false '()
                               false))
           
           (define make-
             (lambda args
               (define num-values (- (+ 6 (length special-fields)) 2))
               ;; (printf "args: ~a nv: ~a args ~a sf ~a~n" (length args) num-values args special-fields)
               (cond
                 [(= (length args) num-values)
                  (define apply-args
                    (append args
                            (list (quote s) (quote p))
                            ))
                  ;; (printf "apply-args: ~a (length ~a)~n" apply-args (length apply-args))
                  (apply _make- apply-args)]
                 [else
                  (error (quote make-) "expects ~a values." num-values)]
                 )))
           
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
                #`(begin
                    (printf "ca: ~a~n" (current-agent))
                    (get (current-agent) f))]
               
               [(_ ag f)
                #`(let ()
                    (define ndx (index-of agent-fields (quote f)))
                    (cond
                      [(and (number? ndx) (<= ndx 6))
                       ;; (printf "~a ~a~n" ndx agent-fields)
                       (agent-get ag ndx)]
                      [else
                       ;; (printf "sf: ~a f: ~a~n" special-fields (quote f))
                       (_get ag (index-of special-fields (quote f)))]))]
               )))
       )]
    ))

(module+ test
  (require rackunit)
  (create-breed turtle turtles)
  (define t1 (make-turtle 1 2 3 4 (color 0 0 0) 5))
  (check-equal? (turtle-get t1 id) 1)
  (check-equal? (turtle-get t1 singular) 'turtle)
  
  (create-breed fish fishes #:have eyes)
  (define f1 (make-fish 1 2 3 4 (color 0 0 0) 5 42))
  (check-equal? (fish-get f1 eyes) 42)

  (define current-agent (make-parameter false))
  (create-breed people peoples #:have legs arms)
  (define p1 (make-people 1 2 3 4 (color 0 0 0) 5 6 7))
  (check-equal? (people-get p1 legs) 6)
  )