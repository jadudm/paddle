#lang racket

(require "base.rkt"
         "agentsets.rkt"
         "world.rkt"
         "interface.rkt"
         )

;; I'll make it a small world.
;; The window is 600x600, so the number of
;; columns essentially determines the resolution and
;; size of the agents in the world.
(make-world 80 80 600 600)
;; At full speed, it runs pretty fast. This determines
;; the pause length between ticks of the world.
;; (tick-pause (/ 1 60))
;; I'll have a breed of agents called fish.
(create-breed turtle turtles)
;; And I want this many fish.
(create turtles 30)
;;(give turtles r-component)

;; Patches
(create-patches)

;; To set them up, I want to do the following.
;; In this case, I do nothing.
(define (setup)
  (widgets (slider turtles 'r 1 255)
           (slider turtles 'g 1 255)
           (slider turtles 'b 1 255))
  
  
  (ask turtles
   (set xcor (random (get global world-cols)))
   (set ycor (random (get global world-rows)))
   (set color (rgb 255 0 128))
   )
  
  (ask (with turtles (member (get id) (range 5)))
       (set color (rgb 255 255 0)))
  )

(define erasing? false)
(define using-widgets? true)
(define (go)
  
  (when (zero? (modulo (ticker) 120))
    (set! erasing? (not erasing?)))
  
  (ask (with turtles (and erasing? (not (member (get id) (range 5)))))
       (set patch pcolor (rgb 0 0 0))
       (set patch dirty? false))

  (if using-widgets?
      (ask (with turtles (member (get id) (range 5)))
           (set patch pcolor (rgb (random (get r)) 
                                  (random (get g))
                                  (random (get b))
                                  )))

      (ask (with turtles (member (get id) (range 5)))
           (set patch pcolor (rgb (+ 64 (random 128))
                                  (+ 64 (random 128))
                                  (+ 64 (random 128))
                                  ))))

  (ask turtles
   (move 1)
   (wiggle))
  )

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