#lang racket

(require "base.rkt"
         "agentsets.rkt"
         "world.rkt"
         "interface.rkt"
         "patches.rkt"
         "types.rkt"
         )

(make-world 200 200 500 500)
(tick-pause (/ 1 30))

(define get-neighbors
  (match-lambda*
    [(list x y)
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
    [(list pid)
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
       
       (when (zero? (random 19))
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


(define (go)
  (ask patches
       (define na (neighbors-alive (get patch neighbors)))
       #;(when (and (> (ticker) 30) (< (get patch pid) 10))
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


