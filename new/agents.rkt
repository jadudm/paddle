#lang racket

(require racket/fixnum)

(require "colors.rkt")

(provide
 struct:agent
 make-agent
 agent?
 agent-get
 agent-set!
 agent-x agent-y
 (contract-out
  [agent-fields  (listof symbol?)]
  ))

(define agent-fields '(id pid x y color direction))

;; The base structure for all agents.
;; (struct agent (id pid x y color direction) #:transparent)
(define-values (struct:agent make-agent agent? agent-get agent-set!) 
  (make-struct-type 'agent false
                    6 0
                    false '() false))

(define (agent-x a)
  (agent-get a 2))
(define (agent-y a)
  (agent-get a 3))