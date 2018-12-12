#lang racket
(require "world.rkt"
         "forms.rkt")

(world-rows 40)
(world-cols 40)

(define-breed fish fishes)
(create-fishes 5)

(ask-fishes
 (printf "Hello.~n"))

