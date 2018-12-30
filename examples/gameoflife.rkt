;;; Copyright 2018 Matt Jadud <matt@jadud.com>. All rights reserved.
;;; This work is licensed under the terms of the MIT license.

;; USAGE
;; To use this microworld, run the code. Then, in the interactions pane,
;;
;; (run-pattern 'puffer-1 100 600)
;;
;; This runs a Game of Life pattern that will be downloaded from the
;; Conway's Game of Life wiki (http://www.conwaylife.com/). The numbers
;; are the world dimensions (100x100, in this case) and the size of the
;; frame on the screen (600x600).
;;
;; There are a limited number of patterns defined here; you can add more
;; to the list at the bottom of this file.
;;
;; If you want to run a random world, simply run
;;
;; (run-random 100 600)

#lang racket

(require paddle)
(require net/url)

;; ================ SETUP ========================
(define (setup)
  (ask patches
    (set patch alive? false)
    (set patch neighbors (get-neighbors (current-patch)))
    (clear-patch! (current-patch))
       
    (when (zero? (random 13))                 
      (set patch color yellow)
      (set patch alive? true))
    ))

;; ================= GO ==========================
(define (go)

  (ask patches
    (define na (neighbors-alive (get patch neighbors)))
    (set patch na na))

  (ask patches
    (define na (get patch na))
    (cond
      [(get patch alive?)
       (cond
         [(or (zero? na) (= na 1))
          (set patch alive? false)
          (clear-patch! (current-patch))]
         [(or (= na 2) (= na 3))
          'DieAnotherDay]
         [(> na 3)
          (set patch alive? false)
          (clear-patch! (current-patch))]
         )]
      [(not (get patch alive?))
       (when (= na 3)
         (set patch alive? true)
         (set patch color yellow))])
    )
  )

;; I want my cells to be yellow, and I don't want to have
;; type the RGB values over and over.
(define yellow (color 255 255 0))

;; Given a patch, I want the neighbors.
;; This should become part of the standard library.
(define get-neighbors
  (case-lambda
    [(x y)
     (define n empty)
     (for ([x-offset (list -1 0 1)])
       (for ([y-offset (list -1 0 1)])
         (unless (and (zero? x-offset) (zero? y-offset))
           (define new-x (+ x x-offset))
           (define new-y (+ y y-offset))
           (set! n (cons (list (wrap new-x (get-global 'world-columns))
                               (wrap new-y (get-global 'world-rows)))
                         n)))))
     (map ->patch (map first n) (map second n))]
    [(pid)
     (parameterize ([current-patch pid])
       (get-neighbors (get patch patch-x)
                      (get patch patch-y)))]
    ))


;; This is an important part of the GoL algorithm.
;; It tells me how many of my immediate neighbors are
;; alive. 
(define (neighbors-alive neighbors)
  (define found-alive 0)
  (for ([nid neighbors])
    (parameterize ([current-patch nid])
      (when (get patch alive?)
        (set! found-alive (add1 found-alive)))))
  found-alive)

;; ------------------------------------------
;; FUN
;;
;; This is me playing. I wrote an RLE parser so I can
;; read in patterns from the web.
          
(define (start-at points x y)
  (map (λ (p)
         (list (+ x (first p))
               (+ y (second p)))) points))

(define (setup-points ps)
  (thunk
   (ask patches
     (set patch neighbors (get-neighbors (current-patch))))
  
   (ask patches
     (set patch alive? false)
     (clear-patch!))

   (define s (first ps))
   (define start-x (second ps))
   (define start-y (third ps))
   (define set-points
     (for/list ([p (start-at s start-x start-y)])
       ;; (printf "p: [~a,~a]~n" (first p) (second p))
       (current-patch (->patch (first p) (second p)))
       (set patch alive? true)
       (set patch color yellow)
       p))
   
   set-points
   ))

(define (get-pattern tag)
  (port->lines (get-pure-port
                (string->url
                 (second (assoc tag gol-patterns))))))
    
;; Run the world.
;; (parse-rle (regexp-split "\n" elementary-knightship))
(require "rle.rkt" "rle-patterns.rkt")
#;(run-world (setup-points (list (parse-rle (get-pattern 'gosper))
                               (- (half-x) (exact-floor (/ (x-offset) 2)))
                               (+ (half-y) 0 #;(exact-floor (/ (y-offset) 2))))
                         )
           go)

(define (run-pattern tag world frame)
  (define parsed (parse-rle (get-pattern tag)))
  ;; (printf "parsed: ~a~n" parsed)
  (make-world world frame)
  (create-patches)
  (run-world (setup-points (list parsed
                               (exact-floor (- (half-x) (/ (x-offset) 2)))
                               (exact-floor (+ (half-y) (/ (y-offset) 2))))
                         )
           go))

(define gol-patterns
  '([gosper             "http://www.conwaylife.com/patterns/gosperglidergun.rle"]
    [frothing-puffer    "http://www.conwaylife.com/patterns/frothingpuffer.rle"]
    [glider-train       "http://www.conwaylife.com/patterns/glidertrain.rle"]
    [coe-ship           "http://www.conwaylife.com/patterns/coeship.rle"]
    [puffer-1           "http://www.conwaylife.com/patterns/puffer1.rle"]
    ))

(define (run-random world frame)
  (make-world world frame)
  (create-patches)
  (run-world setup go))