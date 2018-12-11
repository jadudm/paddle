#lang racket
(require racket/gui)

(provide (all-defined-out))

(define world-width  (make-parameter 200))
(define world-height (make-parameter 200))

(define globals (make-hash))

(define breeds (make-hash))
;; (define agents (make-hash))
(define agent-hashes (make-hash))
(define current-agent (make-parameter false))

(struct location (x y) #:transparent)
(struct Nothing ())

;; https://stackoverflow.com/questions/2187657/calculate-second-point-knowing-the-starting-point-and-distance
;; https://math.stackexchange.com/questions/604324/find-a-point-n-distance-away-from-a-specified-point-in-a-given-direction
(define pi-conv (/ pi 180))
(define (qopx d)
  (cond
    [(and (>= 0) (<= 180)) +]
    [else -]))

(define (qopy d)
  (cond
    [(and (>= -90) (<= 90)) +]
    [else -]))


(define agent%
  (class object%
    (init-field id
                breed
                ;; Innate methods. Users cannot
                ;; override these.
                [innate-methods (make-hash)]
                [innate-fields (make-hash)]
                [owned-methods (make-hash)]
                ;; User-added methods
                [owned-fields (make-hash)]
                )
    
    (define/public (invoke key . args)
      ;; (printf "i ~a ~a~n" key args)
      (cond
        [(hash-has-key? innate-methods key)
         ;; (printf "innate method: ~a~n" key)
         (apply (hash-ref innate-methods key) args)]
        [(hash-has-key? owned-methods key)
         ;; (printf "owned method: ~a~n" key)
         (apply (hash-ref owned-methods key) args)]
        [else
         (printf "Method does not exist: ~a~n" key)]
        ))

    (define (init-fields)
      (define h (make-hash))
      (define random-color 
        (let ([colors (send the-color-database get-names)])
          (list-ref colors (random (length colors)))))
      (hash-set! h 'direction (random 360))
      (hash-set! h 'breed breed)
      (hash-set! h 'id id)
      (hash-set! h 'shape 'wedge)
      (hash-set! h 'x (/ (world-width) 2))
      (hash-set! h 'y (/ (world-height) 2))
      (hash-set! h 'color random-color)
      h
      )
    
    (define (init-methods)
      (define h (make-hash))
      (hash-set! h 'get-breed (λ () (hash-ref innate-fields 'breed)))
      (hash-set! h 'get-id (λ () (hash-ref innate-fields 'id)))
      (hash-set! h 'set-x (λ (x) (hash-set! innate-fields 'x
                                            (cond
                                              [(> x (world-width))
                                               (- x (world-width))]
                                              [(< x 0)
                                               (+ x (world-width))]
                                              [else x]))))
                                                   
      (hash-set! h 'set-y (λ (y) (hash-set! innate-fields 'y
                                            (cond
                                              [(> y (world-height))
                                               (- y (world-height))]
                                              [(< y 0)
                                               (+ y (world-height))]
                                              [else y]))))
      
      (hash-set! h 'set-xy (case-lambda 
                             [(x y)
                              ((hash-ref h 'set-x) x)
                              ((hash-ref h 'set-y) y)
                              ]
                             [(s)
                              (when (location? s)
                                (hash-set! innate-fields 'x (location-x s))
                                (hash-set! innate-fields 'y (location-y s)))]
                             ))
      
      (hash-set! h 'get-xy (λ () (location (hash-ref innate-fields 'x)
                                           (hash-ref innate-fields 'y))))
      (hash-set! h 'get-x (λ () (hash-ref innate-fields 'x)))
      (hash-set! h 'get-y (λ () (hash-ref innate-fields 'y)))
      (hash-set! h 'knows
                 (λ () (append
                        (for/list ([(k v) innate-fields]) k)
                        (for/list ([(k v) owned-fields]) k))))
                          
                            
      #|
(define direction (* (hash-ref innate-fields 'direction)
                                        (/ pi 180)))
                   (define dx (* magnitude (sin direction)))
                   (define dy (* magnitude (cos direction)))
                   ((hash-ref h 'set-x)
                    (+  dx))
                   ((hash-ref h 'set-y)
                    (+ (hash-ref innate-fields 'y) dy))

|#
      (hash-set! h 'move
                 (λ (magnitude)
                   (define direction (hash-ref innate-fields 'direction))
                   (define-values (new-x new-y)
                     (offset (hash-ref innate-fields 'x)
                             (hash-ref innate-fields 'y)
                             direction magnitude))
                   ((hash-ref h 'set-x) new-x)
                   ((hash-ref h 'set-y) new-y)
                   ))
      (hash-set! h 'right (λ (deg)
                            (hash-set!
                             innate-fields 'direction
                             (modulo (+ (hash-ref innate-fields 'direction)
                                        deg) 360))))
      (hash-set! h 'left (λ (deg)
                            (hash-set!
                             innate-fields 'direction
                             (modulo (- (hash-ref innate-fields 'direction)
                                        deg) 360))))

      (define black-pen
        (let ([p (new pen%)])
          (send p set-color (make-object color% "black"))
          p))
      
      (define black-brush (new brush% [style 'solid] [color "black"]))
      (hash-set! h 'draw (λ ()
                           (define dc (hash-ref globals 'world-dc))
                           (define p (new pen%))
                           (define b (new brush%
                                          [style 'solid]
                                          [color (hash-ref innate-fields 'color)]))
                           
                           (send dc set-brush b)
                           (send p set-color
                                 (make-object color%
                                   (hash-ref innate-fields 'color)))
                           (send dc set-pen p)
                           (define body-x (hash-ref innate-fields 'x))
                           (define body-y (hash-ref innate-fields 'y))
                           (define dir (hash-ref innate-fields 'direction))
                           (send dc draw-ellipse body-x body-y 8 8)

                           (define-values (head-x head-y)
                             (offset body-x body-y dir 4))

                           (send dc set-pen black-pen)
                           (send dc set-brush black-brush)
                           (send dc draw-ellipse head-x head-y 2 2)
                           ))

      (hash-set! h 'set-color (λ (c)
                                (hash-set! innate-fields 'color c)))
      h
      )

    ;; Called by breeds-own...
    (define/public (add-own-field sym)
      ;; (printf "Adding field: ~a~n" sym)
      (hash-set! owned-fields sym Nothing)
      (hash-set! owned-methods (string->symbol (format "get-~a" sym))
                 (λ ()
                   ;; (printf "getting ~a = ~a ~n" sym (hash-ref owned-fields sym))
                   (hash-ref owned-fields sym)))
      (hash-set! owned-methods (string->symbol (format "set-~a" sym))
                 (λ (v)
                   ;; (printf "setting ~a ~a~n" sym v)
                   (hash-set! owned-fields sym v)))
      )

    (set! innate-methods (init-methods))
    (set! innate-fields  (init-fields))
    (super-new)
    ))