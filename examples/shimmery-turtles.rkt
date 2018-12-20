;;; Copyright 2018 Matt Jadud <matt@jadud.com>
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
;;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#lang racket

(require paddle)

;; I must create the world first.
(make-world 100 100 700 700)

;; The world does not always need patches.
;; This world does.
;; (create-patches)

;; My creatures are called turtles.
(create-breed turtle turtles)

;; I want many turtles.
(create turtles 2000)

;; I could slow things down.
;;(tick-pause (/ 1 60))

;; I want to set up my turtles.
(define (setup)

  ;; First, I want some sliders.
  (widgets (slider turtles 'r 1 255)
           (slider turtles 'g 1 255)
           (slider turtles 'b 1 255))
  
  ;; Then, I want to ask my turtles to scatter themselves
  ;; randomly around the universe. They are mauve.
  (ask turtles
   (set xcor (random (get global world-cols)))
   (set ycor (random (get global world-rows)))
   (set color (rgb 255 0 0))
   )

  ;; Only a special few of my turtles are yellow.
  (ask (with turtles (zero? (get id)))
       (set color (rgb 255 255 0))
       )

  )


;; I want my turtles to go!
(define (go)

  ;; First, I ask all my turtles to wiggle and then move.
  (ask turtles
   (wiggle)
   (move 1)
   )
  
  ;; Then, I ask all the not-special turtles to clear the patch
  ;; they are on.
  (ask (with turtles (not (zero? (get id))))
       (clear-patch!))

  ;; Finally,  I ask the special turtles to use the slider values
  ;; to set their patches to a pretty color.
  (ask (with turtles (zero? (get id)))
       (set patch pcolor (rgb (random (get r)) 
                              (random (get g))
                              (random (get b))
                              ))
       
       (ask (other (sniff turtles 6))
            ;;(printf "found ~a~n" (get (current-agent) id))
            (shimmer (current-agent))
            (die)
            ))
  )

;; One of the things fish do is wiggle. I flip a
;; coin, and depending on the result, I wiggle a bit
;; to the left or to the right.
(define (wiggle)
  (if (> (random 100) 50)
      (right (random 10))
      (left (random 10))))

;; Shimmering means we randomly choose a new color.
;; None of my turtles currently shimmer.
(define (shimmer a)
  (set a color (rgb (+ 128 (random 64))
                  (+ 128 (random 64))
                  (+ 128 (random 64)))))


;; Finally, I run the world.
(run-world setup go)