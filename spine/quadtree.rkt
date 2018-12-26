#lang racket

;; This abstraction is unnecessary, because
;; agents all carry their x/y data with them.
(define (make-point x y data)
  (vector x y data))
(define point-x 0)
(define point-y 1)
(define point-data 2)

;; Should rectangles just be vectors?
;; If I'm going for compactness and speed, why not?
(define (make-rect x y w h)
  (vector x y w h))

; Racket should be able to inline this.
(define rect-x 0)
(define rect-y 1)
(define rect-w 2)
(define rect-h 3)

(define (contains? point rect)
  (and (>= (vector-ref point point-x)
           (- (vector-ref rect rect-x)
              (vector-ref rect rect-w)))
       (<= (vector-ref point point-x)
           (+ (vector-ref rect rect-x)
              (vector-ref rect rect-w)))
       (>= (vector-ref point point-y)
           (- (vector-ref rect rect-y)
              (vector-ref rect rect-h)))
       (<= (vector-ref point point-y)
           (+ (vector-ref rect rect-y)
              (vector-ref rect rect-h)))))

(define (intersects? r1 r2)
  (not (or (> (- (vector-ref r1 rect-x)
                 (vector-ref r1 rect-w))
              (+ (vector-ref r2 rect-x)
                 (vector-ref r2 rect-w)))
           (< (+ (vector-ref r1 rect-x)
                 (vector-ref r1 rect-w))
              (- (vector-ref r2 rect-x)
                 (vector-ref r2 rect-w)))
           (> (- (vector-ref r1 rect-y)
                 (vector-ref r1 rect-h))
              (+ (vector-ref r2 rect-y)
                 (vector-ref r2 rect-h)))
           (< (+ (vector-ref r1 rect-y)
                 (vector-ref r1 rect-h))
              (- (vector-ref r2 rect-y)
                 (vector-ref r2 rect-h))))))

(define quadtree%
  (class object%
    (define points empty)
    ;; Subtrees
    (define ne false)
    (define nw false)
    (define se false)
    (define sw false)
    
    (define divided? false)
    (init-field boundary capacity)

    (define/public (subdivide)
      (define x (vector-ref boundary rect-x))
      (define y (vector-ref boundary rect-y))
      (define w (/ (vector-ref boundary rect-w) 2))
      (define h (/ (vector-ref boundary rect-h) 2))

      (define ner (make-rect (+ x w) (- y h) w h))
      (set! ne (new quadtree%
                    (boundary ner)
                    (capacity capacity)))
      (define nwr (make-rect (- x w) (- y h) w h))
      (set! nw (new quadtree%
                    (boundary nwr)
                    (capacity capacity)))
      (define ser (make-rect (+ x w) (+ y h) w h))
      (set! se (new quadtree%
                    (boundary ser)
                    (capacity capacity)))
      (define swr (make-rect (- x w) (+ y h) w h))
      (set! sw (new quadtree%
                    (boundary swr)
                    (capacity capacity)))
      (set! divided? true))

    (define/public (insert point)
      (cond
        [(not (contains? point boundary))
         false]
        [(< (length points) capacity)
         (set! points (cons point points))
         true]
        [else
         (when (not divided?)
           (subdivide))
         (or (send ne insert point)
             (send nw insert point)
             (send se insert point)
             (send sw insert point))]))

    (define/public (query range)
      (cond
        [(not (intersects? range boundary)) empty]
        [else
         
         (define found-p
           (filter (λ (p)
                     (contains? p range))
                   points))
         (define sub-p
           (cond
             [divided?
              (append
               (send ne query range)
               (send nw query range)
               (send se query range)
               (send sw query range))
              ]
             [else empty]))
         
         (append found-p sub-p)]))
    
    (super-new)))


(module+ test
  (define qt (new quadtree%
                  (boundary (make-rect 0 0 100 100))
                  (capacity 4)))
  (for ([diag 100])
    (define x diag)
    (define y diag)
    (send qt insert (make-point x y '_))
    )

  (send qt query
        (make-rect 0 0 5 5)
        )
  '------
  (send qt query
        (make-rect 5 5 5 5)
        )
  '------
  (send qt query
        (make-rect 10 10 1 1)
        )

  
  )
