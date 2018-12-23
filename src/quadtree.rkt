#lang racket
(require "structs.rkt")
(require racket/hash)

(struct empty-quad ())
(struct quad (agents
              x y
              width height
              child-nw child-ne child-sw child-se)
  #:transparent)

;; Max agents per quad
(define THRESHOLD 8)

;; Using cols/rows as width/height, to be
;; in keeping with OpenGL implementation.
(define (make-quadtree width height agents)
  ;;(printf "m-q: ~a ~a ~a~n" width height agents)
  (split (quad agents
               0 0 width height
               (empty-quad) (empty-quad) (empty-quad) (empty-quad))))

(define (make-child-quadtree x y width height agents)
  ;;(printf "m-c-q: ~a ~a ~a ~a~n" x y width height)
  (quad agents
               x y width height
               (empty-quad) (empty-quad) (empty-quad) (empty-quad)))
  
(define (count-agents qt)
  (hash-count (quad-agents qt)))

(define (split qt)
  (define sub-width  (/ (quad-width  qt) 2))
  (define sub-height (/ (quad-height qt) 2))
  (define x (quad-x qt))
  (define y (quad-y qt))
  (cond
    [(< (count-agents qt) THRESHOLD) qt]
    [else
     ;; Insert the agents from this node into a new
     ;; node that has kids.
     (define child-nw
       (make-child-quadtree
        x (+ y sub-height)
        sub-width sub-height
        (insert x (+ y sub-height)
                sub-width sub-height
                (quad-agents qt))))
     
     (define child-ne
       (make-child-quadtree
        (+ x sub-width) (+ y sub-height)
        sub-width sub-height
       (insert (+ x sub-width) (+ y sub-height)
               sub-width sub-height
               (quad-agents qt))))
     
     (define child-se
       (make-child-quadtree
        (+ x sub-width) y
        sub-width sub-height
        (insert (+ x sub-width) y
                sub-width sub-height
                (quad-agents qt))))
     
     (define child-sw
       (make-child-quadtree
        x y
        sub-width
        sub-height
        (insert x y
                sub-width sub-height
                (quad-agents qt))))

     (quad (quad-agents qt)
           (quad-x qt) (quad-y qt)
           (quad-width qt) (quad-height qt)
           (split child-nw)
           (split child-ne)
           (split child-sw)
           (split child-se))]
    ))

(define (insert x y w h agents)
  (define in-bounds (make-hash))
  (for ([(id a) agents])
    (when (and (>= (agent-x a) x)
               (< (agent-x a) (+ x w))
               (>= (agent-y a) y)
               (< (agent-y a) (+ y h)))
      (hash-set! in-bounds id a)))
  in-bounds)

(define DIM 400)

(define (random-agents n)
  (define h (make-hash))
  (for ([i n])
    (hash-set!
     h i
     (make-agent i i
                 (random DIM) (random DIM)
                 (color 0 0 0)
                 (random 360))))
  h)

(define (quadtree-depth qt)
  (cond
    [(empty-quad? qt) 0]
    [else
     (define depths (map (λ (qt)
                           (quadtree-depth qt))
                         (list (quad-child-nw qt)
                               (quad-child-ne qt)
                               (quad-child-se qt)
                               (quad-child-sw qt))))
     (add1 (apply max depths))]))

(define (lower-bound n)
  (if (< n 0) 0 n))
(define (upper-bound n u)
  (if (> n u) u n))

(define (in-radius ah x y r)
  (define h (make-hash))
  (for ([(id agent) ah])
    #;(printf "- [~a, ~a] -> [~a, ~a] d ~a < r ~a~n"
          x y (agent-x agent) (agent-y agent) (distance x y agent) r)
    (when (<= (distance x y agent) r)
      (hash-set! h id agent)))
  h)

(define (neighbors qt dim x y radius)
  (define nw-x (lower-bound (- x radius)))
  (define nw-y (upper-bound (+ y radius) dim))
  (define se-x (upper-bound (+ x radius) dim))
  (define se-y (lower-bound (- y radius)))

  (define (search qt dim)
    (define h (make-hash))
    (cond
      ;; If we've hit an empty quad, return the agents at this level.
      [(empty-quad? (quad-child-nw qt)) (in-radius (quad-agents qt) x y radius)]
      [(empty-quad? (quad-child-ne qt)) (in-radius (quad-agents qt) x y radius)]
      [(empty-quad? (quad-child-se qt)) (in-radius (quad-agents qt) x y radius)]
      [(empty-quad? (quad-child-sw qt)) (in-radius (quad-agents qt) x y radius)]
      [else
       ;; If the NW corner of my bounds are greater than the NW corner of the quadrant, and
       ;; less than the width of the quadtrant...
       [when (and (<= nw-x (+ (quad-x qt) (/ (quad-width qt) 2)))
                  (>  nw-y (- (quad-height qt) (/ (quad-height qt) 2))))
         (hash-union! h (search (quad-child-nw qt) (/ dim 2)))]
       
       ;; What about the NE? We need to be in the left half and upper right.
       [when (and (>= se-x (- (quad-width qt) (/ (quad-width qt) 2)))
                  (>  nw-y (- (quad-height qt) (/ (quad-height qt) 2))))
         (hash-union! h (search (quad-child-ne qt) (/ dim 2)))]
       
       ;; SE
       [when (and (>= se-x (- (quad-width qt) (/ (quad-width qt) 2)))
                  (<  se-y (- (quad-height qt) (/ (quad-height qt) 2))))
         (hash-union! h (search (quad-child-se qt) (/ dim 2)))]
       
       ;; SW
       [when (and (<= nw-x (- (quad-width qt) (/ (quad-width qt) 2)))
                  (<  se-y (- (quad-height qt) (/ (quad-height qt) 2))))
         (hash-union! h (search (quad-child-sw qt) (/ dim 2)))]
       h]))
  (search qt dim))
  

(define (count-in-quad qt)
  (cond
    [(empty-quad? (quad-child-nw qt)) 0]
    [(empty-quad? (quad-child-ne qt)) 0]
    [(empty-quad? (quad-child-se qt)) 0]
    [(empty-quad? (quad-child-sw qt)) 0]
    [else
     (apply + (map (λ (q) (+ (hash-count (quad-agents q)) (count-in-quad q)))
                   (list (quad-child-nw qt)
                         (quad-child-ne qt)
                         (quad-child-se qt)
                         (quad-child-sw qt))))]))

(define distance
  (case-lambda
    [(x1 y1 x2 y2)
     (exact-floor (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))]
    [(x1 y1 a)
     (exact-floor (sqrt (+ (expt (- x1 (agent-x a)) 2) (expt (- y1 (agent-y a)) 2))))]
    ))


(define ra (random-agents 40000))
(define t (time (make-quadtree DIM DIM ra)))
(quadtree-depth t)
(hash-count (quad-agents t))

(define px (make-parameter 0))
(define py (make-parameter 0))
(define radius (make-parameter (* DIM 2)))

;; (neighbors qt dim x y radius)
(hash-count (neighbors t DIM (px) (py) (radius)))

;; With a radius that covers all agents, I... find less than all agents. I lost 500 of 20,000.
;; FIXED. I was dropping some on insert, because of exclusion on > vs. >=