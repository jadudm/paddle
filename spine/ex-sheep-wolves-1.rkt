#lang racket

(require paddle)

;; Sheep will have kids at SHIP-KID-ENERGY energy.
;; They start with SHEEP-START-E energy, and gain SHEEP-GAIN
;; every tick.

;; Wolves start with WOLF-START, and lose WOLF-LOSS
;; every world tick. They gain SHEEP-ENERGY every time
;; they eat a sheep. 

(define RxC 150)
(make-world RxC 800)
(set-max-agents! 10000)
(create-breed sheep sheeps #:have energy)
(create-breed wolf wolves #:have energy)
(create sheeps 300)
(create wolves 50)

(define SHEEP-START-E 10)
(define SHEEP-GAIN 1)
(define SHEEP-KID-ENERGY 30)

(define WOLF-START-E 10)
(define WOLF-LOSS 0.7)
(define WOLF-KID-ENERGY 35)
(define WOLF-EAT-RADIUS 4)

(create-log wolves-count)
(create-log sheeps-count)
(create-plot wolves-count sheeps-count)

(define (setup)
  (ask sheeps
    (set sheep-color (color 255 255 255))
    (set sheep-x (random RxC))
    (set sheep-y (random RxC))
    (set sheep-energy SHEEP-START-E)
    )

  (ask wolves
    (set wolf-color (color 0 127 127))
    (set wolf-x (random RxC))
    (set wolf-y (random RxC))
    (set wolf-energy WOLF-START-E)
    )
  
  )

(define (wiggle mn mx)
  (define amount (+ mn (random (- mx mn))))
  (if (even? amount)
      (right amount)
      (left  amount)))

(define (go)
  (sleep .1)
  ;; Sheep wiggle, move, and gain energy.
  (ask sheeps
    (wiggle 5 15)
    (move 0.9)
    ;; FIXME
    ;; There should be an increment! or increase-by form.
    (set sheep-energy (add1 (get sheep-energy)))
    )

  (ask sheeps
    (when (>= (get sheep-energy) 10)
      (set sheep-color (color 255 255 255))))
  
  (ask sheeps
    (when (> (get sheep-energy) SHEEP-KID-ENERGY)
      (set sheep-energy 5)
      (define parent (current-agent))
      (define kids (hatch sheeps (add1 (random 3))))
      ;; (printf "~a had ~a kids~n" (get sheep-id) (length kids))
      (ask kids
        (set sheep-direction (random 360))
        (set sheep-x (get parent sheep-x))
        (set sheep-y (get parent sheep-y))
        (set sheep-energy SHEEP-START-E)
        (set sheep-color (color 127 127 0))
        )))
  
  ;; Wolves wiggle and move
  ;; They wiggle more and are faster.
  (ask wolves
    (wiggle 8 18)
    (move 1.1)
    (set wolf-energy (sub1 (get wolf-energy))))

  ;; Wolves sniff for sheep.
  (ask wolves
    ;; Any sheep found are snaks for the wolves.
    (define snaks (sniff sheeps WOLF-EAT-RADIUS))
    (define this-wolf (current-agent))
    (ask snaks
      ;; This is harder than it needs to be.
      (set this-wolf wolf-energy (+ (get this-wolf wolf-energy)
                                    (get sheep-energy)))
      (die)
      ))

  (ask wolves
    (when (<= (get wolf-energy) 0)
      (die)))

  (ask wolves
    (when (>= (get wolf-energy) WOLF-KID-ENERGY)
      (set wolf-energy 8)
      (define parent (current-agent))
      (define kids (hatch wolves (add1 (random 2))))
      ;; (printf "~a had ~a kids~n" (get sheep-id) (length kids))
      (ask kids
        (set wolf-direction (random 360))
        (set wolf-x (get parent wolf-x))
        (set wolf-y (get parent wolf-y))
        (set wolf-energy 8)
        (set wolf-color (color 255 0 0))
        )
      ;;(sleep 5)
      ))

  ;; Log data for the plot
  (log wolves-count wolves)
  (log sheeps-count sheeps)
  )

(run-world setup go)
