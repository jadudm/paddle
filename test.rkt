#lang racket
(require racket/gui
         pict)


(define width 600)
(define height 400)


;;(define mouse-x (make-parameter (/ width 2)))
;;(define mouse-y (make-parameter (/ height 2)))

(define world-state (make-hash))
(hash-set! world-state 'mouse-x (/ width 200))
(hash-set! world-state 'mouse-y (/ height 200))

(define prev-mouse-x
  (case-lambda
    [() (hash-ref world-state 'prev-mouse-x (/ width 200))]
    [(posn) (hash-set! world-state 'prev-mouse-x posn)]))

(define prev-mouse-y
  (case-lambda
    [() (hash-ref world-state 'prev-mouse-y (/ height 200))]
    [(posn) (hash-set! world-state 'prev-mouse-y posn)]))

(define mouse-x
  (case-lambda
    [() (hash-ref world-state 'mouse-x)]
    [(posn) (prev-mouse-x (hash-ref world-state 'mouse-x))
            (hash-set! world-state 'mouse-x posn)]))

(define mouse-y
  (case-lambda
    [() (hash-ref world-state 'mouse-y)]
    [(posn) (prev-mouse-y (hash-ref world-state 'mouse-y))
            (hash-set! world-state 'mouse-y posn)]))


(define world-dc (make-parameter false))
(define world-threads (make-parameter (list)))

(define setup-fun (make-parameter false))
(define draw-fun  (make-parameter false))

(define (add-to-world-threads th)
  (world-threads (cons th (world-threads))))

(define microframe%
  (class frame%
    (define/augment (on-close)
      (printf "Shutting down.~n")
      (for-each kill-thread (world-threads)))
    (super-new)))

(define microcanvas%
  (class canvas%   
    (define/override (on-event e)
      (let ([type (send e get-event-type)])
        (when (equal? type 'left-down)
          (printf "~a~n" e)
          ))
      (super on-event e))

    (define in-frame (make-parameter false))
    
    (define/override (on-subwindow-event receiver e)
      (case (send e get-event-type)
          [(motion)
           ;; (printf "x ~a y ~a~n" (mouse-x) (mouse-y))
           (mouse-x (send e get-x))
           (mouse-y (send e get-y))]
          [(enter)
           (in-frame true)]
          [(leave)
           (in-frame false)])
      (super on-subwindow-event receiver e))
    
    (super-new)))

(define (make-draw-thread ch)
  (define (f) (new microframe%
                   [label "Testing"]
                   [width width]
                   [height height]))
  
  (define (c f) (new microcanvas%
                     [parent f]
                     [paint-callback
                      (lambda (canvas dc)
                        ;;(printf "on-paint 
                        ((draw-fun))
                        )]
                     ))
  
  (define frame (f))
  (define canvas (c frame))
  (world-dc (send canvas get-dc))
  
  (send frame show true)
  ((setup-fun))

  (add-to-world-threads
   (thread
    (λ ()
      (let loop ([msg (channel-get ch)])
        (send canvas on-paint)
        (loop (channel-get ch))))))
  )
  
(define (make-clock-thread ch)
  (add-to-world-threads
   (thread
    (λ ()
      (let loop ()
        (channel-put ch 'tick)
        ;; (printf "tick~n")
        (sleep (/ 1 60))
        (loop))))))

(define (run-world setup draw)
  (define tick-ch (make-channel))
  (setup-fun setup)
  (draw-fun  draw)
  (make-draw-thread tick-ch)
  (make-clock-thread tick-ch)
  )

;; DRAWING FUNCTIONS

(define (draw-ellipse pos-x pos-y e-width e-height)
  (send (world-dc) draw-ellipse pos-x pos-y e-width e-height))

(define (clear)
  (send (world-dc) clear))

(define (draw-fish pos-x pos-y fish-width fish-height #:open-mouth? [om? false])
  (define pict (standard-fish fish-width fish-height #:open-mouth om?))
  (draw-pict pict (world-dc) pos-x pos-y)) 

;; WHAT THE USER DOES

(define dots%
  (class object%
    (field [offset 0])
    (define/public (draw)
      (set! offset (add1 offset))
      (draw-ellipse (modulo (+ (mouse-x) offset) width) (mouse-y) 10 10)
      (draw-ellipse (modulo (+ (mouse-x) offset) width) (+ (mouse-y) 10) 10 10)
      (draw-ellipse (modulo (+ (mouse-x) offset 10) width) (mouse-y) 10 10)
      (draw-ellipse (modulo (+ (mouse-x) offset 10) width) (+ (mouse-y) 10) 10 10)
      )
    
    (super-new)
    ))

(define • (new dots%))

(define (setup)
  'pass)

(define (draw)
  (clear)
  (send • draw)
  (if (> (mouse-x) (/ width 2))
    (draw-fish (mouse-x) (mouse-y) (* 200 (/ (mouse-x) width)) 50 #:open-mouth? true)
    (draw-fish (mouse-x) (mouse-y) 80 50 #:open-mouth? false))
  )

(run-world setup draw)
