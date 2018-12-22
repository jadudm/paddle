#lang racket

(provide (all-defined-out))
(require "types.rkt")

(define (combine l1 l2)
  (cond
    [(empty? l1) l2]
    [(member (first l1) l2)
     (combine (rest l1) l2)]
    [else
     (cons (first l1) (combine (rest l1) l2))]))


;; FIXME: There are 14 core colors.
;; It will take some effort to build the NetLogo color table.
(define/contract (rgb r g b)
  (-> byte? byte? byte? rgb-color?)
  (rgb-color r g b))

;; Everything is in a coordinate system with OpenGL where the number of
;; columns and rows is scaled to the height and width of the viewport.
;; Therefore, [79.9, 79.9] will map to [79,79], which is less than 80x80.
;; Can we ever get a value outside the range? I don't know. This has to do with
;; whether we map by wrapping or not.

(define (wrap val max)
  ;;(printf "val ~a max ~a~n" val max)
  (define v (exact-floor val))
  (cond
    [(>= val max) (modulo v max)]
    [(< val 0) (modulo (+ max v) max)]
    [else val])
  )

