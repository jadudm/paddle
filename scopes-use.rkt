#lang racket
(require "world.rkt"
         "scopes-base.rkt")

(define-breed fish fishes)
(ask-fishes
 (printf "Hello.~n"))