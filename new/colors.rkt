#lang racket

(require (only-in racket/draw the-color-database color%))

(provide (contract-out
          [get-color         (-> (or/c number? string?) color?)]
          [color-names       (listof string?)]
          [num-colors        number?]
          [get-random-color  (-> color?)]
          [struct color      ((red byte?)
                              (green byte?)
                              (blue byte?))]
          ))

;; FIXME
;; Should I only have rgb as a type?
(struct color (red green blue) #:transparent)

;; Straight from the Racket color database.
;; https://docs.racket-lang.org/draw/color-database___.html
(define color-names
  (send the-color-database get-names))
(define num-colors
  (length color-names))

;; PURPOSE
;; Will return an (rgb ...) value for either a color name
;; or a color index.
(define get-color
  (match-lambda*
    [(list (? (λ (n)
                (and (number? n)
                     (< n (length color-names)))) n))
     (get-color (list-ref color-names n))]

    [(list (? string? s))
     (define c   (send the-color-database find-color s))
     (cond
       [c
        (color (send c red) (send c green) (send c blue))]
       [else
        (error 'get-color "'~a' is not a color." s)]
    )]))

(define (get-random-color)
  (get-color (random num-colors)))

(module+ test
  (require rackunit)

  (check-equal?         (get-color "red")         (color 255 0 0))
  (check-equal?         (get-color 42)            (color 233 150 122))
  (check-equal?         (get-color "darksalmon")  (color 233 150 122))
  (check-pred   color?  (get-random-color))
  )

