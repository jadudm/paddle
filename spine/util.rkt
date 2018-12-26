#lang racket

(provide (contract-out
          [->string (-> any/c string?)]
          [combine-to-symbol (-> any/c ... symbol?)]
          ))

(define (->string o) (format "~a" o))

(define (combine-to-symbol . args)
  (string->symbol (apply string-append (map ->string args))))