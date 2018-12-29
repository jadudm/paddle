#lang racket

(provide quadtree% make-rect make-point)
(require "types.rkt")

;; This abstraction is unnecessary, because
;; agents all carry their x/y data with them.
(define (make-point x y data)
  (vector x y data))
(define point-x 0)
(define point-y 1)
(define point-data 2)
(define (get-point-data v)
  (vector-ref v point-data))

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

    (define/public (remove! point)
      (cond
        [(not (contains? point boundary))
         false]
        [else
         (set! points
               (filter  (λ (p)
                          (define agent-here (vector-ref (vector-ref p point-data) agent-id))
                          (define rem-agent  (vector-ref (vector-ref point point-data) agent-id))
                          (not (= agent-here rem-agent)))
                        points))
         
         (when divided?
           (send ne remove! point)
           (send nw remove! point)
           (send se remove! point)
           (send sw remove! point))]
        ))
        
    
    (define/public (query range)
      (cond
        [(not (intersects? range boundary)) empty]
        [else
         
         (define found-p
           (map get-point-data
                (filter (λ (p)
                          (contains? p range))
                        points)))
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
  (require rackunit)
  
  (define qt (new quadtree%
                  (boundary (make-rect 0 0 10 10))
                  (capacity 4)))
  (for ([diag 10])
    (define x diag)
    (define y diag)

    ;; Need to insert fake agents...
    (send qt insert (make-point x y (vector 0 0 diag diag)))
    )

  (define (set-equal? v1)
    (λ (v2)
      (andmap (λ (p)
                (ormap (λ (v)
                         (equal? p v))
                       v2))
              v1)))
  
  (check-pred (set-equal? '(#(0 0 3 3)
                            #(0 0 2 2)
                            #(0 0 1 1)
                            #(0 0 0 0)
                            #(0 0 5 5)
                            #(0 0 4 4)
                            ))
              (send qt query
                    (make-rect 0 0 5 5)
                    ))
  (check-pred (set-equal? '(#(0 0 3 3)
                            #(0 0 2 2)
                            #(0 0 1 1)
                            #(0 0 0 0)
                            #(0 0 7 7)
                            #(0 0 6 6)
                            #(0 0 5 5)
                            #(0 0 4 4)
                            #(0 0 9 9)
                            #(0 0 8 8)))
              (send qt query
                    (make-rect 5 5 5 5)))
              
  (check-pred (set-equal? '(#(0 0 9 9)))
              (send qt query
                    (make-rect 10 10 1 1)
                    ))

  ;; fake an agent... breed plural id pid x y ...
  (send qt remove! (make-point 9 9 (vector 'x 'x 9 'x)))
  (check-pred (set-equal? '(#(0 0 8 8)))
              (send qt query
                    (make-rect 10 10 2 2)
                    ))

  
  )
