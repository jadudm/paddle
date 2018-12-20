#lang racket

;; A turtle is a structure.
(struct turtle (id breed fields) #:transparent)

;; I keep track of them globally.
(define turtles (make-parameter (make-hash)))

(define (make-default-turtle breed id)
  (turtle id breed (make-hash)))

;; What if there's only turtles.
(define create-turtles
  (case-lambda
    [(breed n)
     (when (hash-ref (turtles) breed true)
       (hash-set! (turtles) breed (make-vector n false)))
     (for ([id (range n)])
       (vector-set! (hash-ref (turtles) breed)
                    id (make-default-turtle breed id)))]
    [(n)
     (create-turtles 'turtles n)]))

(require (for-syntax syntax/parse))
(define funs (make-hash))

(begin-for-syntax
  (define (format-sym fmt . args)
    (string->symbol (apply format (cons fmt args)))))
(define-syntax (create-breed stx)
  (syntax-parse stx
    [(_ singular:id plural:id)
     (with-syntax ([ask-breed
                    (datum->syntax stx (format-sym "ask-~a" (syntax-e #'plural)))]
                   [x (datum->syntax stx 'hi)])
       #`(begin
           (define ask-breed 3)
           (define x 3)
           ))]))

(create-breed turtle turtles)

;; EXAMPLE
(create-turtles 3)
