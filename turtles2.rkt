#lang racket

;; How to synchronize many threads over multiple phases.
;; In CSP/occam, I'd use a barrier.

(define world-width 200)
(define world-height 200)

(define breeds (make-hash))
(define agents (make-hash))

(struct location (x y) #:transparent)
(struct Nothing ())

(define agent%
  (class object%
    (init-field id
                breed
                [cmd-ch (make-channel)]
                [rsp-ch (make-channel)]
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
      (hash-set! h 'direction (random 360))
      (hash-set! h 'breed breed)
      (hash-set! h 'id id)
      (hash-set! h 'shape 'wedge)
      h
      )
    
    (define (init-methods)
      (define h (make-hash))
      (hash-set! h 'get-breed (λ () (hash-ref innate-fields 'breed)))
      (hash-set! h 'get-id (λ () (hash-ref innate-fields 'id)))
      (hash-set! h 'set-x (λ (x) (hash-set! innate-fields 'x
                                            (cond
                                              [(> x (hash-ref owned-fields
                                                               'world-width))
                                               (- x (hash-ref owned-fields
                                                              'world-width))]
                                              [(< x 0)
                                               (+ x (hash-ref owned-fields
                                                              'world-width))]
                                              [else x]))))
                                                   
      (hash-set! h 'set-y (λ (y) (hash-set! innate-fields 'y
                                            (cond
                                              [(> y (hash-ref owned-fields
                                                              'world-height))
                                               (- y (hash-ref owned-fields
                                                              'world-height))]
                                              [(< y 0)
                                               (+ y (hash-ref owned-fields
                                                              'world-width))]
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
      (hash-set! h 'move
                 (λ (magnitude)
                   ;; https://stackoverflow.com/questions/2187657/calculate-second-point-knowing-the-starting-point-and-distance
                   ;; https://math.stackexchange.com/questions/604324/find-a-point-n-distance-away-from-a-specified-point-in-a-given-direction
                   (define direction (* (hash-ref innate-fields 'direction)
                                        (/ pi 180)))
                   (define dx (* magnitude (sin direction)))
                   (define dy (* magnitude (cos direction)))
                   ((hash-ref h 'set-x)
                    (+ (hash-ref innate-fields 'x) dx))
                   ((hash-ref h 'set-y)
                    (+ (hash-ref innate-fields 'y) dy))
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

      (hash-set! h 'draw (λ ()
                           (define p (new pen%))
                           (define b (new brush%
                                          [style 'solid]
                                          [color (hash-ref innate-fields 'color)]))
                           (send (hash-ref owned-fields 'dc)
                                 set-brush b)
                           (send p set-color
                                 (make-object color%
                                   (hash-ref innate-fields 'color)))
                           (send (hash-ref owned-fields 'dc)
                                      set-pen p)
                           (send (hash-ref owned-fields 'dc)
                                 draw-ellipse
                                 (hash-ref innate-fields 'x)
                                 (hash-ref innate-fields 'y)
                                 8 8)))

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

(require (for-syntax racket/syntax)
         (for-syntax syntax/stx)
         syntax/stx)

(define-syntax (define-breed stx)
  (syntax-case stx ()
    [(_ singular plural)
     (with-syntax ([breed-pred? (format-id #'singular "~a?" #'singular)]
                   [ask-breed (format-id #'plural "ask-~a" #'plural)]
                   [ask-one   (format-id #'singular "ask-~a" #'singular)]
                   [create-breeds (format-id #'plural "create-~a" #'plural)]
                   [breeds-own (format-id #'plural "~a-own" #'plural)]
                   [current-breed (format-id #'singular "current-~a" #'singular)]
                   )
       #`(begin
           (define current-breed (make-parameter false))
           (define (breed-pred? o)
             (and (object? o)
                  (is-a? o agent%)
                  (equal? (send o get-breed) (quote breed))))

           (define-syntax (ask-one stx)
             (syntax-case stx ()
               [(_ n (f** a** (... ...)) (... ...))
                #'(begin
                    (let ([f* (quote (f** (... ...)))]
                          [a* (list (list (λ () a**) (... ...)) (... ...))])
                      (first (reverse
                              (for/list ([f f*]
                                         [a a*])
                                (let ([agent (list-ref (hash-ref agents (quote plural))
                                                       n)])
                                  (apply dynamic-send
                                         (append (list agent 'invoke f)
                                                 (map (λ (p) (p)) a))))
                                )))))
                ]))
          
           (define-syntax (ask-breed stx)
             (syntax-case stx ()
               [(_ (f** a** (... ...)) (... ...))
                #'(begin
                    (let ([f* (quote (f** (... ...)))]
                          [a* (list (list (λ () a**) (... ...)) (... ...))])
                      (for ([f f*]
                            [a a*])
                        (for ([agent (hash-ref agents (quote plural))])
                          (apply dynamic-send
                                 (append (list agent 'invoke f) (map (λ (p) (p)) a))))
                        )))]))
               

           (define (create-breeds n)
             (hash-set! agents
                        (quote plural)
                        (map (λ (n) (new agent% [id n] [breed (quote singular)]))
                             (range n))))

           (define-syntax (breeds-own stx)
             (syntax-case stx ()
               [(_ fields (... ...))
                #'(for ([agent (hash-ref agents (quote plural))])
                    (for ([field (quote (list fields (... ...)))])
                      (dynamic-send agent 'add-own-field field)))]
               ))

           (define-syntax (draw-breed stx)
             (syntax-case stx ()
               [(_ dc)
                #'(for ([agent (hash-ref agents (quote plural))])
                    (dynamic-send agent 'draw))]))
                          
             
                         
           ))]))

;;; --------------------------------------------
(define-breed turtle turtles)
(create-turtles 30)
;; (ask-turtles (set-xy 3 5) (set-y 100))

(define-breed patch patches)


#|
(define-breed mouse mice)
(create-mice 8)
;; We can't add anything to the mice until they are created.
;; It's... I don't know if that is good or bad.
(mice-own age hairs)

(ask-mice
 (set-xy (random 10)
         (random 10))
 (set-y (random 100))
 (set-age (random 5))
 (set-hairs (random 1000000 1000000000))
 )

"Results of setting mice"
(for/list ([a (hash-ref agents 'mice)])
    (send a invoke 'get-xy))

"Randomizing 1000 - 10000"
(ask-mice (set-xy (random 1000 10000)
                  (random 1000 10000)))
(for/list ([a (hash-ref agents 'mice)])
    (send a invoke 'get-xy))
|#

(define (move-turtles)
  (if (zero? (modulo (random 11) 2))
      (ask-turtles (move 1)
                   (right (random 15)))
      (ask-turtles (move 1)
                   (left (random 15))
                   )))

(define (show-turtles)
  (for ([a (hash-ref agents 'turtles)])
    (printf "~a x ~a~n" (send a invoke 'get-id) (send a invoke 'get-x))))

(define (go)
  (move-turtles)
  )

(define (setup)
  'pass)

(define ticker 0)
(define (tick)
  (set! ticker (add1 ticker))
  (sleep (/ 1 60)))

(require racket/gui)
(define (run-world)
    (define dc false)
    (define frame-thread
      (thread (λ ()
                ;; (define glconf (new gl-config%))
                
                (define f (new frame%
                               [label "paddle"]
                               [width world-width]
                               [height world-height]
                               ))
                
                (define c (new canvas%
                               [parent f]
                               ;;[gl-config glconf]
                               [style '(no-autoclear)]
                               ))
                (set! dc (send c get-dc))
                (send dc clear)
                
                (send f show true))))
  
    (define world-thread
      (thread (λ ()
                
                (sleep 1)
                
                (for ([(k v) agents])
                  (for ([a v])
                    (send a add-own-field 'dc)

                    (send a add-own-field 'world-width)
                    (send a add-own-field 'world-height)
                    (send a invoke 'set-world-width world-width)
                    (send a invoke 'set-world-height world-height)
                    
                    (send a invoke 'set-dc dc)
                    (send a invoke 'set-x (/ world-width 2))
                    (send a invoke 'set-y (/ world-height 2))

                    (send a invoke 'set-color
                          (let ([colors (send the-color-database get-names)])
                            (list-ref colors (random (length colors)))))
                    ))

                (setup)
              
                (let loop ()
                  (go)
                  
                  (for ([(k v) agents])
                    (for ([a v])
                      (send a invoke 'draw)))
                  (tick)
                  (send dc clear)
                  (loop)
                  ))))

    (λ () (map kill-thread (list frame-thread world-thread))))

(define shutdown    
  (run-world))

#|
(require 2htdp/universe
         2htdp/image)

(define startstate
  (for ([(k v) agents])
    (for ([a v])
      (send a invoke 'set-x (/ 800 2))
      (send a invoke 'set-y (/ 800 2)))))

(define (make-new-state old)
  (go))


 (require lang/posn)
(define (draw-world s)
  (place-images
   (flatten
    (for/list ([(k v) agents])
     (for/list ([a v])
       (ellipse 8 8 "solid" "green"))))
   (flatten
    (for/list ([(k v) agents])
      (for/list ([a v])
        (make-posn (send a invoke 'get-x)
              (send a invoke 'get-y)))))
    (empty-scene 800 800)))
  
(define (main)
  (big-bang startstate
    [on-tick make-new-state (/ 1 1000)]
    [to-draw draw-world 800 800]
    ;; [on-key reset]
    ))
(main)
|#