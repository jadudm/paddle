#lang racket

(require "base.rkt"
         "agentsets.rkt"
         "world.rkt")

;; I'll make it a small world.
;; The window is 600x600, so the number of
;; columns essentially determines the resolution and
;; size of the agents in the world.
(make-world 100 100 400 400)
;; At full speed, it runs pretty fast. This determines
;; the pause length between ticks of the world.
(tick-pause (/ 1 60))
;; I'll have a breed of agents called fish.
(create-breed turtle turtles)
;; And I want this many fish.
(create turtles 100)

;; Patches
(create-patches)

;; To set them up, I want to do the following.
;; In this case, I do nothing.
(define (setup)
  (ask turtles
   (set xcor (random (get world-cols)))
   (set ycor (random (get world-rows)))
   (set color (rgb 255 0 0))
   )
  (ask (with turtles (zero? (get id)))
       (set color (rgb 255 255 0)))
  )
  
(define (go)
  (ask turtles
   (move .5)
   (wiggle))

  (ask (other turtles)
       (set patch pcolor (rgb 0 0 0))
       (set patch dirty? false))
  
  (ask (with turtles (zero? (get id)))
       ;;(printf "xcor ~a ycor ~a~n" (get xcor) (get ycor))
       (flush-output)
       (set patch pcolor (rgb (+ 64 (random 128))
                              (+ 64 (random 128))
                              (+ 64 (random 128))))
       )
  )


  #;(ask patches
         (set dirty? true)
         (set pcolor (rgb (exact-floor (* 255 (/ (get pxcor) (get world-cols))))
                          (exact-floor (* 255 (/ (get pycor) (get world-rows))))
                          0
                          )))
#;(set color (rgb (modulo (+ 32 (random 32) (ticker)) 256)
                       255
                       (modulo (ticker) 256)))

#;(ask patches
       (set pcolor (rgb (+ 64 (random 64))
                        (+ 64 (random 64))
                        (+ 64 (random 64)))))

;; One of the things fish do is wiggle. I flip a
;; coin, and depending on the result, I wiggle a bit
;; to the left or to the right.
(define (wiggle)
  (if (> (random 100) 50)
      (right (random 10))
      (left (random 10))))

;; Shimmering means we randomly choose a new color.
(define (shimmer)
  (set color (rgb (+ 128 (random 64))
                  (+ 128 (random 64))
                  (+ 128 (random 64)))))


;; Finally, we run the world.
(run-world setup go)