#lang racket
(require "agent.rkt")

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
                   [breed-hash (format-id #'plural "~a-hash" #'plural)]
                   )
       #`(begin
           (define breed-hash (make-hash))
          
           (define (breed-pred? o)
             (and (object? o)
                  (is-a? o agent%)
                  (equal? (send o get-breed) (quote breed))))
          
           (define-syntax (ask-breed stx)
             (syntax-case stx ()
               [(_ f* (... ...))
                #'(begin
                    (for ([(id critter) breed-hash])
                        (parameterize ([current-agent critter])
                          f* (... ...))
                        ))]))

           (define (create-breeds n)
             (let ([critters (make-hash)])
               (for ([id (range n)])
                 (hash-set! critters
                            id
                            (new agent% [id n] [breed (quote singular)]))
                 )
               (set! breed-hash critters)
               (hash-set! agent-hashes (quote plural) breed-hash)
               ))
               

           (define-syntax (breeds-own stx)
             (syntax-case stx ()
               [(_ fields (... ...))
                #'(for ([(tag critters) breed-hash])
                    (for ([critter critters])
                      (for ([field (quote (list fields (... ...)))])
                        (dynamic-send critter 'add-own-field field))))
               ]))
           
           ))]))

;; Quick way to access agent functions using the
;; current agent parameter.

(define-syntax (define-agent stx)
  (syntax-case stx ()
    [(_ fun (args ...))
     #`(define (fun args ...)
         (apply dynamic-send
                (append (list (current-agent) 'invoke (quote fun))
                        (list args ...))))
     ]))


;; Need one of these for every agent function.
(define-agent get-id ())
(define-agent get-x  ())
(define-agent move (d))
(define-agent right (deg))
(define-agent left (deg))
(define-agent set-color (c))

(define patch-size (make-parameter 8))
(define-breed patch patches)
(create-patches (* (/ (world-width) (patch-size))
                   (/ (world-width) (patch-size))))

;;; --------------------------------------------
(define-breed turtle turtles)
(create-turtles 10)
;; (ask-turtles (set-xy 3 5) (set-y 100))


(define (move-turtles)
  (ask-turtles
   (cond
     [(zero? (modulo (random 11) 2))
      (move 1)
      (right (random 15))]
     [else
      (move 1)
      (left (random 15))])
   ))

(define (go)
  (move-turtles)
  )

(define (setup)
  (ask-turtles
   (set-color "firebrick")))


(define ticker (make-parameter 0))
(define (tick)
  (ticker (add1 (ticker)))
  (sleep (/ 1 60)))

(require racket/gui)
(define (run-world)
    (define frame-thread
      (thread (λ ()
                ;; (define glconf (new gl-config%))
                
                (define f (new frame%
                               [label "paddle"]
                               [width (world-width)]
                               [height (world-height)]
                               ))
                
                (define c (new canvas%
                               [parent f]
                               ;;[gl-config glconf]
                               [style '(no-autoclear)]
                               ))
                (hash-set! globals 'world-dc (send c get-dc)) 
                (send (hash-ref globals 'world-dc) clear)
                
                (send f show true))))
  
    (define world-thread
      (thread (λ ()
                
                (sleep 1)

                (for ([(plural agent-h) agent-hashes])
                  ;; (printf "breed ~a~n" plural)
                  (for ([(id critter) agent-h])
                    ;; (printf "Adding things to ~a ~a~n" plural id)
                    ;; (send critter add-own-field 'dc)
                    ;;(send critter invoke 'set-dc dc)
                    
                    ;;(send critter invoke 'set-x (/ (world-width) 2))
                    ;;(send critter invoke 'set-y (/ (world-height) 2))
                    'pass
                    ))

                (setup)
              
                (let loop ()
                  (go)
                  
                  (for ([(plural agent-h) agent-hashes])
                    (for ([(id critter) agent-h])
                      (send critter invoke 'draw)))
                  (tick)
                  (send (hash-ref globals 'world-dc) clear)
                  (loop)
                  ))))

    (λ () (map kill-thread (list frame-thread world-thread))))

(define shutdown    
  (run-world))
