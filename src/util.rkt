#lang racket

(provide (contract-out
          [->string (-> any/c string?)]
          [combine-to-symbol (-> any/c ... symbol?)]
          [wrap (-> number? number? number?)]
          [theta (-> number? number? number?)]
          [degrees->components (-> number? (values number? number?))]
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

(define pi-conv (/ pi 180))
(define (degrees->components deg)
  (define dir (* deg pi-conv))
  (define vx (cos dir))
  (define vy (sin dir))
  (values vx vy))

(define (theta vx vy)
  (if (zero? vy)
      0
      (let ([t (/ (atan (/ (abs vx) (abs vy))) pi-conv)])
        (cond
          [(and (> 0 vy)
                (> 0 vx))
           (- (- 360 t) 90)]
          [(> 0 vx) (+ 90 t)]
          [(> 0 vy) (+ t 270)]
          [else (- 90 t)]))))