#lang racket
;;(require (except-in paddle move))
;;(require (only-in "../src/netlogo.rkt" move))
(require paddle)
(require math/statistics)

(define RxC 200)
(make-world RxC 800)
(create-patches)

(set-edge-mode! bounce)

(create-breed particle particles #:have velocity new-direction)
(create particles 100)
(define particle-radius 1)
(define default-velocity (* 3/4 particle-radius))
(define yellow (color 255 255 0))

(define (choice ls)
  (list-ref ls (random (length ls))))

(define (between? low num high)
  (and (> num low) (< num high)))

(define (setup)
  (ask particles
    (set particle-velocity default-velocity)
    (set particle-x (add1 (random (* 5/8 RxC) RxC)))
    (set particle-y (random (* 5/8 RxC) RxC))
    (set particle-direction (random 360))
    (set particle-shape (shape:disk particle-radius 20))
    )

  (define wall-units 24)
  (ask patches
    (when (and (= (get patch patch-x) (/ RxC 2))
               (or (between? 7/12 (/ (get patch patch-y) RxC) 1)
                   (between? 3/12 (/ (get patch patch-y) RxC) 6/12)
                   (between? 0 (/ (get patch patch-y) RxC) 2/12)
                   )
               )
      (set patch color yellow))
    (when (and (= (get patch patch-y) (exact-floor (/ RxC 3)))
               (or (between? 9/12 (/ (get patch patch-x) RxC) 1)
                   (between? 3/12 (/ (get patch patch-x) RxC) 8/12)
                   (between? 0 (/ (get patch patch-x) RxC) 2/12)))
      (set patch color yellow))
    )
  )

(define (go)
  (sleep 0.01)
  (ask particles
    (collide))
  (ask particles
     (set particle-direction (get particle-new-direction)))
  (ask particles
    (bounce-wall))
  (ask particles
    (move (get particle-velocity)))
  )

(define (patch-east)
  (modulo (add1 (vector-ref (current-agent) agent-pid)) (* RxC RxC)))
(define (patch-west)
  (modulo (sub1 (vector-ref (current-agent) agent-pid)) (* RxC RxC)))
(define (patch-south)
  (modulo (- (vector-ref (current-agent) agent-pid) RxC) (* RxC RxC)))
(define (patch-north)
  (modulo (+ (vector-ref (current-agent) agent-pid) RxC) (* RxC RxC)))


(define (bounce-wall)
  (cond
    [(or (equal? (get patch (patch-west) color) yellow)
         (equal? (get patch (patch-east) color) yellow))
     (set particle-vx (- (get particle-vx)))
     (set particle-direction
          (theta (get particle-vx) (get particle-vy)))
     (set particle-velocity (+ (get particle-velocity) default-velocity))]
    [(or (equal? (get patch (patch-north) color) yellow)
         (equal? (get patch (patch-south) color) yellow))
     (set particle-vy (- (get particle-vy)))
     (set particle-direction
          (theta (get particle-vx) (get particle-vy)))
     (set particle-velocity (* (get particle-velocity) 3))
     ]))
  

(define (have-neighbors? ls)
  (not (empty? ls)))

(define (sign-of them us)
  (cond
    [(and (positive? them) (positive? us)) +]
    [(and (negative? them) (negative? us)) -]
    [(and (negative? them) (positive? us)
          (positive? (+ them us))) -]
    [(and (negative? them) (positive? us)
          (negative? (+ them us))) +]
    [(and (positive? them) (negative? us)
          (positive? (+ them us))) -]
    [(and (positive? them) (negative? us)
          (negative? (+ them us))) +]
    [else +]
    ))

(define (collide)
  (define neighbors (sniff particles (* 2 particle-radius)))
  (cond
    [(have-neighbors? neighbors) 
     (define nvxs (map (λ (a) (get a particle-vx)) neighbors))
     (define nvys (map (λ (a) (get a particle-vy)) neighbors))
     (define myvx (get particle-vx))
     (define myvy (get particle-vy))
     (define mvx (mean (cons myvx nvxs)))
     (define mvy (mean (cons myvy nvys)))
     (define signy (sign-of mvy myvy))
     (define signx (sign-of mvx myvx))
         
     (set particle-new-direction (theta (- mvx) (- mvy)))
     (set particle-velocity (+ (* 1.25 default-velocity (length neighbors))
                               default-velocity))]
    
    [else
     (set particle-new-direction (get particle-direction))
     (set particle-velocity default-velocity)]))


;; distance-from: agent agent -> number
;; Calculates the pythagorean distance between
;; me and another agent.
(define (distance/pythagorean me neigh)
  (sqrt (+ (expt (- (get neigh agent-x) (get me agent-x)) 2)
           (expt (- (get neigh agent-y) (get me agent-y)) 2))))

;; Or, the manhattan distance.
(define (distance/manhattan me neigh)
  (+ (abs (- (get neigh agent-x) (get me agent-x)))
     (abs (- (get neigh agent-y) (get me agent-y)))))

(define distance-from distance/pythagorean)

(define (find-nearest me neighbors)
  (define n-with-dist
    (map (λ (n)
           (list (distance-from me n) n))
         neighbors))
  ;; (printf "fn: ~a~n" n-with-dist)
  (second (first (sort 
                  n-with-dist
                  <
                  #:key first)))
  )


  
(define (collide2)
  (define neighbors (sniff particles (* 2 particle-radius)))
  (cond
    [(> (length neighbors) 0)
     (define neighbor
       (find-nearest (current-agent) neighbors))
     (set particle-new-direction (get neighbor particle-direction))
     (set neighbor particle-new-direction (get particle-direction))
     (set particle-velocity (+ particle-radius
                               (distance-from (current-agent) neighbor)))]
    [else
     (set particle-new-direction (get particle-direction))
     (set particle-velocity default-velocity)]
    ))
     

(run-world setup go)