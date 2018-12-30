#lang racket

(require paddle memoize)

(define RxC 100)
(make-world RxC 500)
(create-breed heatbug heatbugs
              #:have ideal-temp output-heat unhappiness)

;; Create as many bugs as we have patches.
(create heatbugs 100)
(create-patches)

;; 0 - 200
(define min-ideal-temp 10)
(define max-ideal-temp 40)

;; 0 - 100
(define min-output-heat 5)
(define max-output-heat 25)

;; FIXME
;; NetLogo has a notion of "sprout" that lets patches...
;; grow 'n' turtles at their location. I don't have this
;; machinery in place. However, it apparently would simplify
;; assigning unique coordinates to the heatbugs.
(define/memo (coordinates)
  (apply append
         (for/list ([x (range 0 RxC)])
           (for/list ([y (range 0 RxC)])
             (list x y)))))
(define patch-coordinates (coordinates))
(define (pick-coordinate)
  (define patch (one-of patch-coordinates))
  (set! patch-coordinates (remove patch patch-coordinates))
  patch)

(define (setup)
  (ask patches
    (set patch temp 0))
    
  (ask heatbugs
    (define me (current-agent))
    (define patch-coord (pick-coordinate))
    (set heatbug-x (first patch-coord))
    (set heatbug-y (second patch-coord))
    (set heatbug-pid (->pid (get heatbug-x) (get heatbug-y)))
    
    (set heatbug-ideal-temp
         (random min-ideal-temp max-ideal-temp))
    (set heatbug-output-heat
         (random min-output-heat max-output-heat))

    (set heatbug-unhappiness (- ideal-temp (get patch temp)))
    
    (color-by-ideal-temp)
    (face me (one-of (neighbors heatbugs)))
    ))

(define (color-by-ideal-temp)
  (define range-adj (/ (- max-ideal-temp min-ideal-temp) 2))
  (define scale
    (exact-floor
     (* (/ (get heatbug-ideal-temp) max-ideal-temp)
        127)))
                      
  (set heatbug-color (color 100 (+ 127 scale) 100)))

(define (neighbors as)
  (sniff (exact-floor (/ RxC 3))))

(define (one-of ls)
  (list-ref ls (random (length ls))))

(define (face me neighbor)
  (radians->degrees
   (atan (/ (- (get heatbug-x neighbor) (get me heatbug-x))
            (- (get heatbug-y neighbor) (get me heatbug-y))))))

(run-world setup (thunk 'pass))