#lang racket
(require ;;"world.rkt"
         "forms.rkt")


(define-breed fish fishes)
(create-fishes 5)

;; Both current-agent and current-fish are in-scope for the
;; iteration. 
(ask-fishes
 (printf "Hello from ~a~n" (agent-id (current-fish)))
 
 (set-x! (random 100))
 (set-y! (random 100))
 (fish-set-y! (random 100))
 (printf "~a~n" (agent-fields (current-fish)))
 )
