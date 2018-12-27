#lang racket

(provide (contract-out
          [->string (-> any/c string?)]
          [combine-to-symbol (-> any/c ... symbol?)]
          [wrap (-> number? number? number?)]
          ))

(define (->string o) (format "~a" o))

(define (combine-to-symbol . args)
  (string->symbol (apply string-append (map ->string args))))

(define (wrap val max)
  ;;(printf "val ~a max ~a~n" val max)
  (define v (exact-floor val))
  (cond
    [(>= val max) (modulo v max)]
    [(< val 0) (modulo (+ max v) max)]
    [else val])
  )