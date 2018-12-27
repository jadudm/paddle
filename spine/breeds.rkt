#lang racket

(provide (all-defined-out))

(require (for-syntax racket/syntax syntax/parse))
(require "agentsets.rkt"
         "util.rkt"
         "agents.rkt")

(define-syntax (create-breed stx)
  (syntax-parse stx
    [(_cb singular plural)
     #`(begin
         (define plural (quote plural))
         ;; FIXME: Start small for testing.
         (set-agentset! (quote plural) (make-vector 20 false))
         (init-meta (quote plural))
         ;; (printf "asm: ~a~n" agentsets-meta)
         (set-agentset-meta! (quote plural)
                             (combine-to-symbol (quote singular) '-next-index) 0)
         ;; (printf "asm: ~a~n" agentsets-meta)
         ;; Here, I really want the symbols 'singular and 'plural.
         ;; Hence the quasi-unquote-syntax.
         (set-agentset-meta! (quote plural) #,(quote 'plural) (quote plural))
         (set-agentset-meta! (quote plural) #,(quote 'singular) (quote singular))
         )
     ]))

(module+ test
  (create-breed turtle turtles)
  (create turtles 5)
  (show-agents turtles))