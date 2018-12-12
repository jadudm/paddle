#lang racket
(require racket/gui)

(provide (for-syntax (all-defined-out))
         (all-defined-out))

;; Global variables... perhaps replace params?
(define globals (make-hash))
;; This tracks all of the agent vectors
(define agent-vectors (make-hash))

;; For tracking who we are working with right now
(define current-agent (make-parameter false))

(struct Nothing ())

;; https://stackoverflow.com/questions/2187657/calculate-second-point-knowing-the-starting-point-and-distance
;; https://math.stackexchange.com/questions/604324/find-a-point-n-distance-away-from-a-specified-point-in-a-given-direction
(define pi-conv (/ pi 180))

;; Can an agent just be a vector of values?
;; In Racket, structs should be roughly as fast,
;; and I would get some typing lift. So, we'll use
;; structs instead of vectors.
#|
#:methods gen:custom-write
  [(define (write-proc a port mode)
     (fprintf port "Agent[ ~a ] Breed[ ~a ]"
              (agent-id a) (agent-singular a)))]
|#
(struct agent (id singular plural fields)
  #:transparent)

(define (select-random-color)
  (define random-color 
    (let ([colors (send the-color-database get-names)])
      (list-ref colors (random (length colors)))))
  (send the-color-database find-color random-color))

(define (color-name->color-obj name)
  (send the-color-database find-color name))

(define (rgb r g b)
  (make-object color% r g b))
(define (rgba r g b a)
  (make-object color% r g b a))

;; Construct an agent constructor.
(define (make-base-agent singular plural breed-overrides)
  (λ (id)
    (define fields (make-hash))

    (hash-set! fields 'color (select-random-color))
    
    (hash-set! fields 'direction (random 360))
    (hash-set! fields 'breed singular)
    (hash-set! fields 'id id)
    (hash-set! fields 'shape 'wedge)
    ; (hash-set! fields 'x (/ (world-width) 2))
    ; (hash-set! fields 'y (/ (world-height) 2))
    
    
    ;; Apply the overrides
    (for ([overrides (list breed-overrides)])
      (for ([(k v) overrides])
        (hash-set! fields k v)))
    
    ;; Return the agent.
    (agent id singular plural fields)))

(define make-turtle
  (make-base-agent 'turtle 'turtles (make-hash)))


;; AGENT METHODS
;; These become functions.
(struct NoValueFound ())

(require racket/syntax
         syntax/stx)

(require (for-syntax syntax/parse racket/syntax racket/sequence))


(begin-for-syntax
  (define (format-tag-ht        ctx loc tag)      (format-id ctx "~a-ht"      tag      #:source loc))
  (define (format-tag-get-name  ctx loc tag name) (format-id ctx "~a-get-~a"  tag name #:source loc))
  (define (format-tag-set-name! ctx loc tag name) (format-id ctx "~a-set-~a!" tag name #:source loc)))

(define-syntax (introduce stx)
  (syntax-parse stx
    [(_introduce tag:id x:id ...)
     (define tag-sym (syntax-e #'tag))
     (with-syntax ([tag-ht           (format-tag-ht stx #'tag tag-sym)]
                   [(tag-get-x ...)  (for/list ([name (in-syntax #'(x ...))])
                                       (format-tag-get-name stx name tag-sym (syntax-e name)))]
                   [(tag-set-x! ...) (for/list ([name (in-syntax #'(x ...))])
                                       (format-tag-set-name! stx name tag-sym (syntax-e name)))])
       (syntax/loc stx
         (begin
           ;; (define tag-ht (make-hasheq))
           (define (tag-get-x)    (hash-ref  (agent-fields (current-agent)) 'x false)) ...
           (define (tag-set-x! v) (hash-set! (agent-fields (current-agent)) 'x v))  ...)))]))

(begin-for-syntax

  (define (expand-breed-pred stx singular plural)
    (with-syntax ([breed-pred? (format-id stx "~a?" #'singular)])
      (syntax/loc stx
        (define (breed-pred? o)
          (and (agent? o)
               (equal? (agent-singular o) (quote singular)))))))

  (define (expand-breed-vec stx plural)
    (with-syntax ([breed-vec (format-id stx "~a-vec" #'plural)])
      (syntax/loc stx
        (define breed-vec (make-vector 0)))))

  (define (expand-breed-fields stx singular)
    (with-syntax ([breed-fields (format-id stx "~a-vec" #'singular)])
      (syntax/loc stx
        (define breed-fields empty))))

  (define (ask-breed stx plural)
    (with-syntax ([ask-breed (format-id stx "ask-~a" #'plural)]
                  [(_ fields ...) stx])
      (syntax/loc stx
        (for ([critter breed-vec])
          (parameterize ([current-agent critter])
            fields ...)
          ))))

  )
  
(define-syntax (define-breed stx)
  (syntax-parse stx
    [(_ singular plural)
     (syntax/loc stx
       (begin
         (expand-breed-vec       stx            #'plural )
         (expand-breed-fields    stx #'singular          )
         (expand-breed-pred      stx #'singular #'plural )
         (expand-ask-breed       stx            #'plural )
         ))]))


#;(with-syntax ([breed-pred? (format-id #'singular "~a?" #'singular)]
                   [ask-breed (format-id #'plural "ask-~a" #'plural)]
                   [ask-one   (format-id #'singular "ask-~a" #'singular)]
                   [create-breeds (format-id #'plural "create-~a" #'plural)]
                   [breeds-own (format-id #'plural "~a-own" #'plural)]
                   [breed-fields (format-id #'singular "~a-fields" #'singular)]
                   [breed-set!   (format-id #'singular "~a-set!" #'singular)]
                   [breed-get    (format-id #'singular "~a-get" #'singular)]
                   [breed-vec (format-id #'plural "~a-vec" #'plural)]
                   [no-breeds (format-id #'plural "no-~a" #'plural)]
                   )
       #`(begin
           (define breed-vec (make-vector 0))
           (define breed-fields empty)
           
           (define (breed-pred? o)
             (and (agent? o)
                  (equal? (agent-singular o) (quote singular))))
           
           (define-syntax (ask-breed stx)
             (syntax-case stx ()
               [(_ f* (... ...))
                #'(begin
                    (for ([critter breed-vec])
                      (parameterize ([current-agent critter])
                        f* (... ...))
                      ))]))

           (define (create-breeds n)
             (set! breed-vec (make-vector n false))
             (for ([id (range n)])
               (define maker
                 (make-base-agent
                  (quote singular) (quote plural) (make-hash)))
               (vector-set! breed-vec id (maker id)))
             (hash-set! agent-vectors (quote plural) breed-vec)
             )

           #;(define-syntax (breeds-own stx)
               (syntax-parse stx
                 [(_ fields:id (... ...))
                  #`(introduce singular fields (... ...))]))

           (define-syntax (breeds-own stx)
             (syntax-parse stx
               [(_ ids:id (... ...))
                (with-syntax ([(getter (... ...))
                               (datum->syntax #'singular
                                              (map (λ (id)
                                                     (string->symbol (format "~a-get-~a"
                                                                             (syntax->datum #'singular)
                                                                             id)))
                                                   (syntax->datum #'(ids (... ...)))))]
                              [(setter (... ...))
                               (datum->syntax #'singular
                                              (map (λ (id)
                                                     (string->symbol (format "~a-set-~a!"
                                                                             (syntax->datum #'singular)
                                                                             id)))
                                                   (syntax->datum #'(ids (... ...)))))])
                  #`(begin
                      (define (getter)
                        (hash-ref (agent-fields (current-agent)) 'ids)) 
                      (... ...)
                      (define (setter v)
                        (hash-set! (agent-fields (current-agent)) 'ids v))
                      (... ...)
                      ))]))
                          
           
           (define no-breeds empty)
           ))
     

#|

                #`(begin
                      #,@(for/list ([field (quote (list fields (... ...)))])
                           #`(begin
                               (define #,(format-id (datum->syntax #f field)
                                                    "get-~a"
                                                    (datum->syntax #f field))
                                 (case-lambda
                                   [()
                                    (hash-ref (agent-fields (current-agent))
                                              (quote field) (NoValueFound))]
                                   [(a)
                                    (hash-ref (agent-fields a)
                                              (quote field) (NoValueFound))]))
                               (define #,(format-id field "set-~a!" field)
                                 (case-lambda
                                   [(v)
                                    (hash-set! (agent-fields (current-agent))
                                               (quote field) v)]
                                   [(a v)
                                    (hash-set! (agent-fields a)
                                               (quote field) v)]))))
                  )
|#


;;;;;;
;; Should I assume we're always parameterizing
;; over the (current-agent) parameter? If so,
;; that would mean the user never provides the
;; agent argument in these functions.
(define-syntax (define-agent-set/get stx)
  (syntax-case stx ()
    [(_ field)
     (with-syntax ([get (format-id #'field "get-~a" #'field)]
                   [set (format-id #'field "set-~a!" #'field)])
       #`(begin
           (define get
             (case-lambda
               [()
                (hash-ref (agent-fields (current-agent))
                          (quote field) (NoValueFound))]
               [(a)
                (hash-ref (agent-fields a)
                          (quote field) (NoValueFound))]))
           (define set
             (case-lambda
               [(v)
                (hash-set! (agent-fields (current-agent))
                           (quote field) v)]
               [(a v)
                (hash-set! (agent-fields a)
                           (quote field) v)]))
           ))]))

(define-agent-set/get id)
(define-agent-set/get breed)
(define-agent-set/get x)
(define-agent-set/get y)
(define-agent-set/get direction)
(define-agent-set/get color)
(define-agent-set/get world-rows)
(define-agent-set/get world-cols)

(define (offset x y direction magnitude)
  (define dir (* (+ direction 90) pi-conv))
  (define dy (* magnitude (sin dir)))
  (define dx (* magnitude (cos dir)))
  (values (+ x dx) (+ y dy)))

(define (wrap v edge)
  (cond
    [(> v edge) 0]
    [(<= v 0) edge]
    [else v]))

(define (move magnitude)
  (define direction (get-direction (current-agent)))
  (define-values (new-x new-y)
    (offset (get-x (current-agent))
            (get-y (current-agent))
            direction magnitude))
  (set-x! (current-agent)
          (wrap new-x (get-world-rows (current-agent))))
  (set-y! (current-agent)
          (wrap new-y (get-world-cols (current-agent))))
  )

(define (right d)
  (set-direction! (current-agent) (+ (get-direction (current-agent)) d)))

(define (left d)
  (set-direction! (current-agent) (- (get-direction (current-agent)) d)))
