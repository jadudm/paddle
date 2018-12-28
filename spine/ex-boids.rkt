;;; Copyright 2018 Matt Jadud <matt@jadud.com>. All rights reserved.
;;; This work is licensed under the terms of the MIT license.

#lang racket

(require paddle)

;; I must create the world first.
(make-world 300 800)

;; My creatures are called boids.
;; Boids have a vision limit.
(create-breed boid boids #:have eyesight)

;; I want many turtles.
(create boids 800)

(define minimum-separation 2)

(define (current-vision)
  (* (/ (get boid-eyesight) 100) 4))

#;(define (setup-interface)
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
    (set boid-x (random (get global world-columns)))
    (set boid-y (random (get global world-rows)))
    (set boid-direction (random 360))

   ;; This is fun... let's vary how far boids
   ;; can see! Some flock better than others...
   (set boid-eyesight 3)
   
   (set boid-color (color 255 255 0))
   )
  )


;; I want my boids to go!
(define (go)
  (ask boids
       (flock)
       (wiggle 10 #;(* (/ (get wiggly) 100) 10))
       (move 0.5 #;(+ 0.5 (/ (get bursty) 100)))
       )
  )

;; distance-from : agent -> agent -> value
;; Takes an agent, returns a function
;; that consumes an agent, and returns the distance
;; between the two.
(define distance-from-me
  (lambda (them)
    (define me (current-agent))
    ;; (printf "ca: ~a~n" me)
    ;; (printf "th: ~a~n" them)
    (define me-x (get me boid-x))
    (define me-y (get me boid-y))
    (define them-x (get them boid-x))
    (define them-y (get them boid-y))
    (+ (abs (- them-x me-x))
       (abs (- them-y me-y)))))

;; min-of
;; takes a comparator of one arg, and
;; returns the agent that satisfies the pred.
(define (min-of as compute)
  ;; This sorts the result by the computation.
  (define result
    (sort
     (for/list ([agent as])
       (parameterize ([current-agent agent])
         (list (compute agent) agent)))
     <
     #:key first))
  ;; I have a list of lists.
  ;; I want the first agent. This would be the first list
  ;; and the second value in that list.
  (second (first result)))

(define (any? as)
  (> (length (get-agents as)) 0))

;; This is a thing boids do.
(define (flock)
  (define flockmates (sniff boids (get boid-eyesight)))
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
  (define my-direction    (get boid-direction))
  (define their-direction (get neighbor boid-direction))
  (cond
    [(< my-direction their-direction)
     (set boid-direction (- my-direction amount))]
    [(>= my-direction their-direction)
     (set boid-direction (+ my-direction amount))]))

(define (average-direction as)
  (define x-sum 0)
  (define y-sum 0)
  (for ([agent as])
    (define dx (cos (get agent boid-direction)))
    (define dy (sin (get agent boid-direction)))
    (set! x-sum (+ dx x-sum))
    (set! y-sum (+ dy y-sum)))
  (cond
    [(and (zero? x-sum) (zero? y-sum))
     (get boid-direction)]
    [else (atan x-sum y-sum)]))
          
(define (align flockmates amount)
  (define ad (average-direction flockmates))
  (define my-direction (get boid-direction))
  (cond
    ;; turn towards
    [(< my-direction ad)
     (set boid-direction (+ my-direction amount))]
    [(>= my-direction ad)
     (set boid-direction (- my-direction amount))]))


        
    
    
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
  (set a boid-color (color (+ 128 (random 64))
                      (+ 128 (random 64))
                      (+ 128 (random 64)))))


;; Finally, I run the world.
(run-world setup go) ;;#:interface setup-interface)