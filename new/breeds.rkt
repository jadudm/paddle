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
           (define special-fields (append (quote (user-fields ...)) '(singular plural)))
           ;; (printf "~a sf: ~a (length ~a)~n" (quote s) special-fields (length special-fields))

           ;; https://docs.racket-lang.org/reference/creatingmorestructs.html
           (define-values (base _make- pred? _get _set)
             (make-struct-type (quote s) struct:agent
                               (length special-fields) 0
                               false
                               (list (cons prop:field-names (append agent-fields special-fields)))
                               false))
           
           ;; This forms the basis of most (ask ...) forms.
           (define p (make-parameter false))
           (p (agentset (quote s) (quote p) (make-hash) _make- pred? _get _set special-fields))
           (hash-set! agentsets (quote p) p)
           
           (define make-
             (lambda args
               (define num-values (+ (length agent-fields) (length special-fields) 2))
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

           ;; https://docs.racket-lang.org/guide/pattern-macros.html#%28tech._identifier._macro%29
           ;; Unsure if this should be a syntax. It would expose the singular as an identifier
           ;; macro that expands to the current agent. 
           #|
           (define-syntax s
             (lambda (stx)
               (syntax-case stx ()
                 [s (identifier? (syntax s)) (syntax (current-agent))])))
            |#
           ;; Or... use a with-syntax above, and do...
           ;;(define current (make-parameter false))

           
           (define-syntax (set stx2)
             (syntax-parse stx2
               [(_ f v)
                #`(set (current-agent) f v)]
               [(_ ag f v)
                #`(let ()
                    (define ndx (index-of (append agent-fields
                                                  special-fields)
                                          (quote f)))
                    (define setter (if (< ndx (length agent-fields)) agent-set! _set))
                    (define new-ndx (if (< ndx (length agent-fields)) ndx (- ndx (length agent-fields))))
                    ;; (printf "Setting ~a ~a ~a~n" (quote f) new-ndx v) 
                    (setter ag new-ndx v))]
                ))
           
           (define-syntax (get stx2)
             (syntax-parse stx2
               [(_ f)
                #`(begin
                    ;; (printf "ca: ~a~n" (current-agent))
                    (get (current-agent) f))]
               
               [(_ ag f)
                #`(let ()
                    (define ndx (index-of (append agent-fields
                                                  special-fields)
                                          (quote f)))
                    (define getter (if (< ndx (length agent-fields)) agent-get _get))
                    (define new-ndx (if (< ndx (length agent-fields)) ndx (- ndx (length agent-fields))))
                    ;; (printf "Getting ~a ~a ~a~n" (quote f) new-ndx (getter ag new-ndx))
                    (getter ag new-ndx))]
               )))
       )]
    ))

(module+ test
  (require rackunit)
  (require "colors.rkt")
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
  (check-equal? (people-get p1 singular) 'people)
  )