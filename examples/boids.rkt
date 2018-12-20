;;; Copyright 2018 Matt Jadud <matt@jadud.com>
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
;;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#lang racket

(require paddle)

;; I must create the world first.
(make-world 200 200 800 800)

;; My creatures are called turtles.
(create-breed boid boids)

;; I want many turtles.
(create boids 200)

;; Boids have a vision limit
(give boids vision)

;; I could slow things down.
;;(tick-pause (/ 1 60))

(define minimum-separation 1)

(define (current-vision)
  (* (/ (get eyesight) 100) 4))

(define (setup-interface)
   (widgets
   (slider boids 'bursty 1 100)
   (slider boids 'wiggly 1 100)
   (slider boids 'eyesight 1 100)
   ))

;; I want to set up my turtles.
(define (setup)
  
  ;; Then, I want to ask my turtles to scatter themselves
  ;; randomly around the universe. They are mauve.
  (ask boids
   (set xcor (random (get global world-cols)))
   (set ycor (random (get global world-rows)))
 
   (set direction (random 360))

   ;; This is fun... let's vary how far boids
   ;; can see! Some flock better than others...
   (set vision 4)
   
   (set color (rgb 255 255 0))
   )
  )


;; I want my boids to go!
(define (go)
  (ask boids
       (flock)
       (wiggle (* (/ (get wiggly) 100) 10))
       (move (+ 0.5 (/ (get bursty) 100)))
       )
  )

;; distance-from : agent -> agent -> value
;; Takes an agent, returns a function
;; that consumes an agent, and returns the distance
;; between the two.
(define distance-from-me
  (lambda (them)
    (define me (current-agent))
    (define me-x (get me xcor))
    (define me-y (get me ycor))
    (define them-x (get them xcor))
    (define them-y (get them ycor))
    (+ (abs (- them-x me-x))
       (abs (- them-y me-y)))))

;; min-of
;; takes a comparator of one arg, and
;; returns the agent that satisfies the pred.
(define (min-of as compute)
  (define them (agentset-agents (as)))
  (define result
    (sort
     (for/list ([(id agent) them])
      (list id (compute agent)))
     <
     #:key second))
  (hash-ref them
            (first (first result))))
            

;; This is a thing boids do.
(define (flock)
  (define flockmates (other (sniff boids (current-vision))))
  (when (any? flockmates)
    (define nearest-neighbor
      (min-of flockmates distance-from-me))
    (cond
      [(< (distance-from-me nearest-neighbor)
          minimum-separation)
       (separate-from nearest-neighbor (random 10))]
      [else
       (align flockmates (random 10))
       ;;(cohere)
       ])))

;; Could operate on an agent or a set.
;; Might want to normalize these all on sets,
;; even if it contains a single agent.
(define (separate-from neighbor amount)
  (define my-direction    (get direction))
  (define their-direction (get neighbor direction))
  (cond
    [(< my-direction their-direction)
     (set direction (- my-direction amount))]
    [(>= my-direction their-direction)
     (set direction (+ my-direction amount))]))

(define (average-direction as)
  (define x-sum 0)
  (define y-sum 0)
  (for ([(id agent) (agentset-agents (as))])
    (define dx (cos (get agent direction)))
    (define dy (sin (get agent direction)))
    (set! x-sum (+ dx x-sum))
    (set! y-sum (+ dy y-sum)))
  (cond
    [(and (zero? x-sum) (zero? y-sum))
     (get direction)]
    [else (atan x-sum y-sum)]))
          
(define (align flockmates amount)
  (define ad (average-direction flockmates))
  (define my-direction (get direction))
  (cond
    ;; turn towards
    [(< my-direction ad)
     (set direction (+ my-direction amount))]
    [(>= my-direction ad)
     (set direction (- my-direction amount))]))


        
    
    
;; One of the things fish do is wiggle. I flip a
;; coin, and depending on the result, I wiggle a bit
;; to the left or to the right.
(define (wiggle amount)
  (if (> (random 100) 50)
      (right amount)
      (left amount)))

;; Shimmering means we randomly choose a new color.
;; None of my turtles currently shimmer.
(define (shimmer a)
  (set a color (rgb (+ 128 (random 64))
                  (+ 128 (random 64))
                  (+ 128 (random 64)))))


;; Finally, I run the world.
(run-world setup go #:interface setup-interface)