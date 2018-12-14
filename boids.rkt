#lang racket
(require "world.rkt"
         "forms.rkt")

(world-rows 40)
(world-cols 40)

(define-breed fish fishes have fins age)
(create-fishes 5)

;; Both current-agent and current-fish are in-scope for the
;; iteration. 
(ask-fishes
 (printf "Hello from ~a~n" (agent-id (current-fish)))
 (printf "~a~n" (agent-fields (current-fish)))
 )
