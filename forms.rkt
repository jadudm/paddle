#lang racket
(require (only-in racket/draw
                  the-color-database
                  color%))

(require "base.rkt")

(provide (for-syntax (all-defined-out))
         (all-defined-out))

;; Global variables... perhaps replace params?
(define globals (make-hash))
;; This tracks all of the agent vectors
(define agent-vectors (make-hash))

(struct Nothing ())

;; https://stackoverflow.com/questions/2187657/calculate-second-point-knowing-the-starting-point-and-distance
;; https://math.stackexchange.com/questions/604324/find-a-point-n-distance-away-from-a-specified-point-in-a-given-direction
(define pi-conv (/ pi 180))

(struct agent (id singular plural fields)
  #:transparent)
(define current-agent (make-parameter false))
(define agents-have   (make-parameter (make-hash)))

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
    
    ;; Apply the overrides
    (for ([overrides (list breed-overrides)])
      (for ([(k v) overrides])
        (hash-set! fields k v)))
    
    ;; Return the agent.
    (agent id singular plural fields)))

;; AGENT METHODS
;; These become functions.
(struct NoValueFound ())


(require racket/syntax
         syntax/stx)

(require (for-syntax syntax/stx syntax/parse racket/syntax racket/sequence))

(begin-for-syntax
 
  
  ;; Creates a current-agent parameter that is used in the
  ;; of the form
  ;;    current-<breed>
  ;; that is used in the 
  ;;    ask-<breed>
  ;; form
  (define (expand-current-agent stx singular)
    (with-syntax ([current-agent (format-id stx "current-~a" singular)])
      (syntax/loc stx
        (define current-agent (make-parameter false)))))
  
  (define (expand-breed-pred stx singular plural)
    (with-syntax ([breed-pred? (format-id stx "~a?" singular)]
                  [breed (format-id stx "~a" singular)])
      (syntax/loc stx
        (define (breed-pred? o)
          (and (agent? o)
               (equal? (agent-singular o) 'breed))))))

  (define (expand-breed-vec stx singular plural)
    (with-syntax ([breed-vec (format-id stx "~a-vec" plural)]
                  [breed-funs (format-id stx "~a-funs" singular)])
      (syntax/loc stx
        (begin
          (define breed-vec  (make-vector 0))
          (define breed-funs (make-hash))))))

  (define (expand-breed-fields stx singular)
    (with-syntax ([breed-fields (format-id stx "~a-vec" singular)])
      (syntax/loc stx
        (define breed-fields empty))))
  
  (define (expand-create-breed stx singular plural)
    (with-syntax ([create-breed (format-id stx "create-~a" plural)]
                  [breed-vec    (format-id stx "~a-vec" plural)])
      (syntax/loc stx
        (begin
          (define (create-breed n)
            (set! breed-vec (make-vector n false))
            (for ([id (range n)])
              (define maker
                (make-base-agent
                 (quote singular) (quote plural) (make-hash)))
              (vector-set! breed-vec id (maker id)))
            (hash-set! agent-vectors (quote plural) breed-vec))
          ))))

  (define (expand-breeds-own stx singular plural fields)
    (with-syntax ([breeds-own (format-id stx "~a-own" plural)]
                  [s singular]
                  [(tag-get-x ...)  (for/list ([name (in-syntax fields)])
                                      (datum->syntax stx
                                                     (string->symbol
                                                      (format "~a-get-~a"
                                                              (syntax->datum singular)
                                                              (syntax-e name)))))]
                  [(tag-set-x! ...) (for/list ([name (in-syntax fields)])
                                      (datum->syntax stx
                                                     (string->symbol
                                                      (format "~a-set-~a!"
                                                              (syntax->datum singular)
                                                              (syntax-e name)))))]
                  [(f* ...) (datum->syntax stx fields)]
                  )
      #`(begin
          (define tag-get-x
            (case-lambda
              [() (hash-ref
                   (agent-fields (current-agent))
                   (quote f*) NoValueFound)]
              [(a) (hash-ref
                    (agent-fields a)
                    (quote f*) NoValueFound)]))
          ...
          (define tag-set-x!
            (case-lambda
              [(v) (hash-set!
                    (agent-fields (current-agent))
                    (quote f*) v)]
              [(a v) (hash-set!
                      (agent-fields a)
                      (quote f*) v)]))
          ...
          #|(hash-set! (agents-have) (quote tag-get-x)
                      (λ () (hash-ref (agent-fields (current-agent))  false)))
           ...
           (hash-set! (agents-have) (quote tag-set-x!)
                      (λ (v) (hash-set! (agent-fields (current-agent)) (quote f*) v)))
           ...
|#
  
          )))

    
  (define (expand-ask-breed stx singular plural)
    (with-syntax ([ask-breed            (format-id stx "ask-~a" plural)]
                  [current-agent-breed  (format-id stx "current-~a" singular)]
                  [breed-vec            (format-id stx "~a-vec" plural)]
                  [(_ fields ...)       stx])
      (syntax/loc stx
        (define-syntax (ask-breed stx-inner)
          (syntax-parse stx-inner
            [(_ bodies (... ...))
             (syntax/loc stx-inner
               (for ([critter breed-vec])
                 (parameterize ([current-agent-breed critter]
                                [current-agent critter])
                   bodies (... ...))
                 ))]
            )))))
  
   

#;(define (core-acc/get stx core-fields)
  (with-syntax ([(set-x! ...)
                 (for/list ([name core-fields])
                   (datum->syntax stx
                                  (string->symbol
                                   (format "set-~a!"
                                           name))))]
                [(get-x ...)
                 (for/list ([name core-fields])
                   (datum->syntax stx
                                  (string->symbol
                                   (format "get-~a"
                                           name))))])
    #`(begin
        (define get-x
          (case-lambda
            [() (hash-ref
                 (agent-fields (current-agent))
                 (quote f*) NoValueFound)]
            [(a) (hash-ref
                  (agent-fields a)
                  (quote f*) NoValueFound)]))
        ...
        (define set-x!
          (case-lambda
            [(v) (hash-set!
                  (agent-fields (current-agent))
                  (quote f*) v)]
            [(a v) (hash-set!
                    (agent-fields a)
                    (quote f*) v)]))
        ...
        )))
  )


(begin-for-syntax
  (define core-fields '(x y color direction)))

(define-syntax (define-breed stx)
  (with-syntax ([(core ...)
                 (datum->syntax stx core-fields)])
  (syntax-parse stx
    [(_ singular plural (~literal have) fields:id ...)
     #`(begin
           #,(expand-current-agent   stx #'singular          )
           #,(expand-breed-vec       stx #'singular #'plural )
           #,(expand-breed-fields    stx #'singular          )
           #,(expand-breed-pred      stx #'singular #'plural )
           #,(expand-create-breed    stx #'singular #'plural )
           #,(expand-ask-breed       stx #'singular #'plural )
           #,(expand-breeds-own      stx #'singular #'plural
                                     (syntax->list #'(fields ...)))
           #,(expand-breeds-own      stx #'singular #'plural
                                     (syntax->list #'(core ...)))
           )]
    [(_ singular plural)
     #`(begin
         #,(expand-current-agent   stx #'singular          )
         #,(expand-breed-vec       stx #'singular #'plural )
         #,(expand-breed-fields    stx #'singular          )
         #,(expand-breed-pred      stx #'singular #'plural )
         #,(expand-create-breed    stx #'singular #'plural )
         #,(expand-ask-breed       stx #'singular #'plural )
         #,(expand-breeds-own      stx #'singular #'plural
                                     (syntax->list #'(core ...)))
         )
     ])))

;; These are needed by everybody, and can't wait for agent
;; breeds to be introduced.

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
          (wrap new-x (world-rows)))
  (set-y! (current-agent)
          (wrap new-y (world-cols)))
  )

(define (right d)
  (set-direction! (current-agent) (+ (get-direction (current-agent)) d)))

(define (left d)
  (set-direction! (current-agent) (- (get-direction (current-agent)) d)))


(define (sniff radius)
  
  (define this-agent (current-agent))
  (define this-x (get-x))
  (define this-y (get-y))

  (define north (+ this-y radius))
  (define south (- this-y radius))
  (define east  (+ this-x radius))
  (define west  (- this-x radius))
  
  (define nearby empty)
  
  (for ([(plural agent-vec) agent-vectors])
    (unless (equal? plural 'patches)
      (for ([critter agent-vec])
        (parameterize ([current-agent critter])
          (when (and (not (= (agent-id (current-agent))
                             (agent-id this-agent)))
                     (< (get-x) east)
                     (> (get-x) west)
                     (> (get-y) south)
                     (< (get-y) north))
            (set! nearby (cons (current-agent) nearby)))))))

  nearby
  )

(define (hash-sniff radius)
  
  (define this-agent (current-agent))
  (define this-x (get-x))
  (define this-y (get-y))

  (define north (+ this-y radius))
  (define south (- this-y radius))
  (define east  (+ this-x radius))
  (define west  (- this-x radius))
  
  (define nearby (make-hash))

  ;; This is an expensive way to do this.
  ;; It gets slow with 150+ agents.
  (for ([(plural agent-vec) agent-vectors])
    (unless (equal? plural 'patches)
      (for ([critter agent-vec])
        (parameterize ([current-agent critter])
          (when (and (not (= (agent-id (current-agent))
                             (agent-id this-agent)))
                     (< (get-x) east)
                     (> (get-x) west)
                     (> (get-y) south)
                     (< (get-y) north))
            (hash-set! nearby
                       (agent-id (current-agent))
                       (current-agent)
                       ))))))

  nearby
  )
