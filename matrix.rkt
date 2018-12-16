#lang racket

(provide (all-defined-out))

(struct matrix (array rows cols)
  #:methods gen:custom-write
  [(define (write-proc m port mode)
     (begin
       (for ([y (range (matrix-rows m))])
         (fprintf port "[ ")
         (for ([x (range (matrix-cols m))])
           (fprintf port "~a " (vector-ref (vector-ref (matrix-array m) x) y)))
         (fprintf port "]~n"))))
   ])

(define (make-matrix rows cols #:init [init (λ () (make-hash))])
  (define rowv (make-vector cols))
  (for ([xndx (range cols)])
    (vector-set! rowv xndx (make-vector rows false)))
  (for ([xndx (range cols)])
    (for ([yndx (range rows)])
      (vector-set! (vector-ref rowv xndx)
                   yndx
                   (make-hash #;`((bin-x ,xndx)
                                (bin-y ,yndx))))
      'pass))
        
  (matrix rowv rows cols))

(define (matrix-get-x/y m x y)
  (vector-ref (vector-ref (matrix-array m) x) y)
  )

(define (matrix-set-x/y! m x y v)
  (vector-set! (vector-ref (matrix-array m) x) y v)
  v)



