#lang racket
(require racket/gui)
(require (for-syntax syntax/parse racket/syntax))

(provide (all-defined-out))

(struct agent (id singular plural fields)
  #:transparent)

(begin-for-syntax

  ;; Creates a current-agent parameter that is used in the
  ;; of the form
  ;;    current-<breed>
  ;; that is used in the 
  ;;    ask-<breed>
  ;; form
  (define (expand-current-agent stx singular)
    (with-syntax ([current-agent (format-id stx "current-~a" singular)])
      (syntax/loc stx
        (define current-agent (make-parameter false)))))
  
  (define (expand-breed-pred stx singular plural)
    (with-syntax ([breed-pred? (format-id stx "~a?" singular)]
                  [breed (format-id stx "~a" singular)])
      (syntax/loc stx
        (define (breed-pred? o)
          (and (agent? o)
               (equal? (agent-singular o) 'breed))))))

  (define (expand-breed-vec stx plural)
    (with-syntax ([breed-vec (format-id stx "~a-vec" plural)])
      (syntax/loc stx
        (define breed-vec (make-vector 0)))))

  (define (expand-breed-fields stx singular)
    (with-syntax ([breed-fields (format-id stx "~a-vec" singular)])
      (syntax/loc stx
        (define breed-fields empty))))

  (define (expand-ask-breed stx singular plural)
    (with-syntax ([ask-breed (format-id stx "ask-~a" plural)]
                  [current-agent (format-id stx "current-~a" singular)]
                  [breed-vec (format-id stx "~a-vec" plural)]
                  [(_ fields ...) stx])
      (syntax/loc stx
        (define-syntax (ask-breed stx-inner)
          (syntax-parse stx-inner
            [(_ bodies (... ...))
             (syntax/loc stx-inner
               (for ([critter breed-vec])
                 (parameterize ([current-agent critter])
                   bodies (... ...))
                 ))]
            )))))

  (define (expand-create-breed syntax plural)
    (with-syntax ([create-breed (format-id stx "create-~a" plural)]
                  [breed-vec    (format-id stx "~a-vec" plural)])
      (syntax/loc stx
        (
                 
  )
  
(define-syntax (define-breed stx)
  (syntax-parse stx
    [(_ singular plural)
     #`(begin
         #,(expand-current-agent   stx #'singular          )
         #,(expand-breed-vec       stx            #'plural )
         #,(expand-breed-fields    stx #'singular          )
         #,(expand-breed-pred      stx #'singular #'plural )
         #,(expand-ask-breed       stx #'singular #'plural )
         )]))

