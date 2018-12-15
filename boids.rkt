#lang racket
(require "base.rkt"
         "world.rkt"
         "forms.rkt")

;; I'll make it a small world.
;; The window is 600x600, so the number of
;; columns essentially determines the resolution and
;; size of the agents in the world.
(world-cols 120)
(world-rows 120)

;; At full speed, it runs pretty fast. This determines
;; the pause length between ticks of the world.
(tick-pause (/ 1 60))

;; I'll have a breed of agents called fish.
(define-breed fish fishes)
;; And I want this many fish.
(create-fishes 300)

;; To set them up, I want to do the following.
;; In this case, I do nothing.
(define (setup)
  (ask-fishes
   (set-x! (random (world-cols)))
   (set-y! (random (world-rows)))
   (set-color! (rgb 255 0 0))
   (when (= (agent-id (current-fish)) 0)
     (set-color! (rgb 255 255 0)))
   ))
  

;; Perhaps this should be called 'act'?
;; Perhaps not. Every time it is time to do stuff,
;; the 'go' function is run. I will ask all the fishes
;; to move forward one, to wiggle a bit, and to shimmer.
;; The 'go' function is called over-and-over.

(define (uniq-append h1 h2)
  (for ([(k v) h1])
    (hash-set! h2 k v))
  h2)

(define spreaders (make-hash))
(hash-set! spreaders 0 (vector-ref fishes-vec 0))

(define (go)
  (ask-fishes
   (move 1)
   (wiggle)
   )

  ; (printf "l ~a~n" (hash-count spreaders))
  (for ([(id a) spreaders])
    (parameterize ([current-agent a])
      (define nearby (hash-sniff 1))
      (set! spreaders (uniq-append spreaders nearby))
      (for ([(fid friend) nearby])
        (set-color! friend (rgb 0 255 255)))
      )))

;; One of the things fish do is wiggle. I flip a
;; coin, and depending on the result, I wiggle a bit
;; to the left or to the right.
(define (wiggle)
  (if (> (random 100) 50)
      (right (random 10))
      (left (random 10))))

;; Shimmering means we randomly choose a new color.
(define (shimmer)
  (set-color! (rgb (+ 128 (random 64))
                   (+ 128 (random 64))
                   (+ 128 (random 64)))))


;; Finally, we run the world.
(run-world setup go)