#lang racket

(provide (all-defined-out))

(require (for-syntax racket/syntax syntax/parse))
(require "agentsets.rkt"
         "util.rkt"
         "agents.rkt"
         "state.rkt"
         )

(require (for-syntax racket/list "types.rkt"))

(define-syntax (create-breed stx)
  (syntax-parse stx
    [(_cb singular plural)
     ;; Lets define some vector indicies!
     (with-syntax ([(breed-ids ...)
                    (for/list ([id agent-base-fields])
                      (format-id stx "~a-~a" #'singular id))]
                   [(agent-ids ...)
                    (for/list ([id agent-base-fields])
                      (format-id stx "agent-~a" id))]
                   [(nums ...) (range (length agent-base-fields))]
                   )
                      
     #`(begin
         (define plural (quote plural))
         ;; (define singular (quote singular))
         ;; FIXME: Start small for testing.
         (set-agentset! (quote plural) (make-vector DEFAULT-AGENTSET-SIZE false))
         (init-meta (quote plural))
         ;; (printf "asm: ~a~n" agentsets-meta)
         (set-agentset-meta! (quote plural)
                             (combine-to-symbol (quote singular) '-next-index) 0)
         ;; (printf "asm: ~a~n" agentsets-meta)
         ;; Here, I really want the symbols 'singular and 'plural.
         ;; Hence the quasi-unquote-syntax.
         (set-agentset-meta! (quote plural) #,(quote 'plural) (quote plural))
         (set-agentset-meta! (quote plural) #,(quote 'singular) (quote singular))
         (define agent-ids nums) ...
         (define breed-ids nums) ...
         ))
     ]))

(module+ test
  (require rackunit)
  (create-breed turtle turtles)
  (check-equal? agent-x 4)
  (check-equal? turtle-x 4)
  (check-equal? agent-y 5)
  (check-equal? turtle-y 5)
  (check-equal? agent-direction 6)
  )