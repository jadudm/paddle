#lang racket
(provide parse-rle half-x half-y x-offset y-offset)
(require paddle)

(define (half-x) (/ (get-global 'world-columns) 2))
(define (half-y) (/ (get-global 'world-rows) 2))
(define x-offset (make-parameter half-x))
(define y-offset (make-parameter half-y))

(x-offset 0)
(y-offset 0)

;; http://www.conwaylife.com/wiki/RLE
(define (parse-rle los)
  (define (parse-rle* los yo)
    (cond
      [(empty? los) empty]
      [(regexp-match "^#" (first los))
       (parse-rle* (rest los) yo)]
      [(regexp-match #rx"x = ([0-9]+), y = ([0-9]+),.*?" (first los))
       =>
       (λ (m)
         (x-offset (string->number (second m)))
         (y-offset (string->number (third m)))
         (parse-rle* (rest los) yo))]
      [else
       ;; We have a string of positions.
       ;; Gather them, then parse them.
       ;; Pass an empty stack.
       ;; This eliminates issues of the stack having to persist through calls
       ;; to parse-rle (eg. multiple-line patterns).
       (define the-pattern (apply string-append
                                (map string-trim (cons (first los) (rest los)))))
       ;; (printf "rest: ~a~n" the-pattern)
       (parse-string (string->list the-pattern)  0 yo empty)]
      ))
  (parse-rle* los (y-offset)))

(define (in-range? n s e)
  (and (>= n s) (<= n e)))
(define (push v s)
  (cons v s))

(define (pop-number s)
  (define tot 0)
  (for ([n (map (λ (c) (string->number (format "~a" c))) s)]
        [exp (range 0 (length s))])
    (set! tot (+ tot (* n (expt 10 exp)))))
  ;; Implied when the stack is empty is a length of 1.
  (cond
    [(zero? tot) 1]
    [else tot]))

;; The stack needs to be passed from one call of this
;; function to the next, because of newlines in the
;; pattern.
(define (parse-string loc x y stack)
  (cond
    [(empty? loc) empty]
    ;; If I find a number, push it on the stack.
    [(in-range? (char->integer (first loc))
                (char->integer #\0) (char->integer #\9))
     (parse-string (rest loc) x y (push (first loc) stack))]

    ;; If I find a black directive, pop the number from the stack,
    ;; and increment x accordingly.
    [(equal? (first loc) #\b)
     (define num (pop-number stack))
     (parse-string (rest loc) (+ x num) y empty)]
    ;; If I find a filled-in square, add those to the squares we found.
    ;; Pop and clear the stack.
    [(equal? (first loc) #\o)
     (define tot-points (pop-number stack))
     (define points
       (for/list ([n (range 1 (add1 tot-points))])
         (list (+ x n) y)))
     (append points (parse-string (rest loc) (+ x tot-points) y empty))]
    ;; If I find a new row marker, pop the stack, and advance y.
    [(equal? (first loc) #\$)
     (define tot-lines (pop-number stack))
     (parse-string (rest loc) 0 (- y tot-lines) empty)]
    ;; When we're here, we're done.
    [(equal? (first loc) #\!) empty]
    ;; We should not get here.
    [else (error 'parse-string "Should not get here: ~a" (first loc))]
    ))

(define gosper
  "#N Gosper glider gun
#O Bill Gosper
#C A true period 30 glider gun.
#C The first known gun and the first known finite pattern with unbounded growth.
#C www.conwaylife.com/wiki/index.php?title=Gosper_glider_gun
x = 36, y = 9, rule = B3/S23
24bo11b$
22bobo11b$
12b2o6b2o12b2o$
11bo3bo4b2o12b2o$
2o8bo5bo3b2o14b$
2o8bo3bob2o4bobo11b$
10bo5bo7bo11b$
11bo3bo20b$12b2o!")