;;; Copyright 2018 Matt Jadud <matt@jadud.com>
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
;;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#lang racket

(require paddle)

(make-world 10 10 400 400)
;;(tick-pause (/ 1 60))

(define get-neighbors
  (case-lambda
    [(x y)
     (define n empty)
     (for ([x-offset (list -1 0 1)])
       (for ([y-offset (list -1 0 1)])
         (unless (and (zero? x-offset) (zero? y-offset))
           (define new-x (+ x x-offset))
           (define new-y (+ y y-offset))
           (set! n (cons (list (wrap new-x (get global world-cols))
                               (wrap new-y (get global world-rows)))
                         n)))))
       (map ->patch (map first n) (map second n))]
    [(pid)
     (parameterize ([current-patch pid])
       (get-neighbors (get patch pxcor)
                      (get patch pycor)))]
    ))

(define yellow (rgb 255 255 0))

(define (setup)
  (ask patches
       (set patch alive? false)
       (set patch neighbors (get-neighbors (current-patch)))
       (clear-patch! (current-patch))
       
       (when (zero? (random 6))
         (set patch pcolor yellow)
         (set patch alive? true))
       ))

(define (neighbors-alive neighbors)
  (define found-alive 0)
  (for ([nid neighbors])
    (parameterize ([current-patch nid])
      (when (get patch alive?)
        (set! found-alive (add1 found-alive)))))
  found-alive)

(define special 0)
(define state 'draw)

(define (go)
  ;;(printf "SPECIAL ~a~n" special)
  ;;(printf "----------~n")
  
  (parameterize ([current-patch special])
    (cond
      [(equal? state 'draw)
       
       (define n (get-neighbors (get patch pid)))
       ;;(printf "px ~a py: ~a~n" (get patch pxcor) (get patch pycor))
       
       (ask-patches (using n)
                    (set patch pcolor (rgb 255 255 0))
                    ;;(printf "\tpx ~a py: ~a~n" (get patch pxcor) (get patch pycor))
                    )
       (set patch pcolor (rgb 255 0 0))
       ]
      [(equal? state 'clear)
       (ask patches (clear-patch!))
       ]))
  
  (cond
    [(equal? state 'draw)
     (set! state 'clear)]
    [(equal? state 'clear)
     (set! special (modulo (add1 special) (* (get global world-cols)
                                             (get global world-rows))))
     (set! state 'draw)])
  )

(define (go2)
  (ask patches
       (define na (neighbors-alive (get patch neighbors)))
       (when (and (> (ticker) 30) (< (get patch pid) 10))
         (printf "p ~a na ~a~n" (get patch pid) na))
       ;;(printf "pid: ~a na: ~a~n" (current-patch) na)
       (cond
         ;; Any dead cell with exactly three live neighbors
         ;; becomes a live cell, as if by reproduction.
         [(and (not (get patch alive?))
               (= na 3))
          (set patch alive? true)
          (set patch pcolor yellow)]
         ;; Any live cell with fewer than two live
         ;; neighbors dies, as if by underpopulation.
         [(and (get patch alive?) (< na 2))
          (set patch alive? false)
          (clear-patch! (current-patch))]
         ;; Any live cell with two or three live
         ;; neighbors lives on to the next generation.
         [(and (get patch alive?) (member na '(2 3)))
          'ImGonnaDieAnotherDay]
         ;; Any live cell with more than three live
         ;; neighbors dies, as if by overpopulation.
         [(and (get patch alive?) (> na 3))
          (set patch alive? false)
          (clear-patch! (current-patch))])
       ))

(run-world setup go)


