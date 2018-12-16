#lang racket
(require (only-in racket/draw
                  the-color-database
                  color%))

(require "base.rkt"
         "matrix.rkt")

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

(define (format-symbol fmt . args)
  (string->symbol
   (apply format
          (cons fmt args))))

;; Construct an agent constructor.
(define (make-base-agent singular plural breed-overrides)
  (λ (id)
    (define fields (make-hash))

    (hash-set! fields 'color (select-random-color))
    
    (hash-set! fields 'direction (random 360))
    (hash-set! fields 'breed singular)
    ;; So we can have multiple breeds in the matrix...
    ;; FIXME: Or, should cells track different breeds differently?
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
                  [breed-vec    (format-id stx "~a-vec" plural)]
                  [sing singular]
                  [plur plural])
      
      (syntax/loc stx
        (begin
          (define (create-breed n)
            (set! breed-vec (make-vector n false))
            (for ([id (range n)])
              (define maker
                (make-base-agent
                 (quote sing) (quote plur) (make-hash)))
              (vector-set! breed-vec id (maker id)))
            (hash-set! agent-vectors (quote plur) breed-vec))
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
  )


;; FIXME Do I need these?
#;(begin-for-syntax
    (define core-fields '(x y color direction)))

(define-syntax (define-breed stx)
  (with-syntax (#;[(core ...) (datum->syntax stx core-fields)])
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
           #;(expand-breeds-own      stx #'singular #'plural
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
           #;(expand-breeds-own      stx #'singular #'plural
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

(struct bin (col row) #:transparent)
(define-agent-set/get id)
(define-agent-set/get breed)
;(define-agent-set/get x)
;(define-agent-set/get y)
(define-agent-set/get bin-x)
(define-agent-set/get bin-y)
(define-agent-set/get direction)
(define-agent-set/get color)
(define-agent-set/get current-bin)


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


;; Manipulating the world

(define the-world (make-matrix (world-cols) (world-rows)))

(define (make-world x y)
  (world-cols x)
  (world-rows y)
  (set! the-world (make-matrix (world-cols) (world-rows)))
  )

(define sniff
  (let ([sets (make-hash)])
    (case-lambda
      [() sets]
      [(r)
       (sniff (current-agent) r)]
      [(a radius)
       (define x (get-bin-x a))
       (define y (get-bin-y a))
       (define memo-key (format "~a-~a-~a" (get-bin-x a) (get-bin-y a) radius))
       ;;(printf "memo-key ~a~n" memo-key)
       (cond
        [(not (hash-ref sets memo-key false))
         (define pairs (make-hash))
         (for ([r radius])
           (define start-x (- x r))
           (define start-y (- y r))
           (define end-x (+ x r))
           (define end-y (+ y r))
           (for ([x (range start-x end-x)])
             (for ([y (range start-y end-y)])
               (define actual-x
                 (cond
                   [(< x 0)
                    (+ (world-cols) x)]
                   [(> x (world-cols))
                    (- x (world-cols))]
                   [else x]))
               (define actual-y
                 (cond
                   [(< y 0)
                    (+ (world-rows) y)]
                   [(> y (world-rows))
                    (- y (world-rows))]
                   [else y]))
               (hash-set! pairs actual-x actual-y)
               )))
         (hash-set! sets memo-key pairs)
         pairs]
        [else
         ;;(printf "memoized ~a~n" memo-key)
         (hash-ref sets memo-key 'WHAT)]
        )])))
                                      
                            
        
        
  
(define (set-cell-value! x y k v)
  (hash-set! (matrix-get-x/y the-world x y)
             k v))

(define (get-cell-value x y k #:else [e false])
  (hash-ref (matrix-get-x/y the-world x y) k e))

(define (cell-remove-agent x y breed id)
  (define h (hash-ref (matrix-get-x/y the-world x y) 'agents (make-hash)))
  (when (and (hash-has-key? h breed)
             (hash-has-key? (hash-ref h breed) id))
    (hash-remove! (hash-ref h breed) id)))
    

(define (cell-add-agent! x y breed id)
  (define h (hash-ref (matrix-get-x/y the-world x y) 'agents (make-hash)))
  (cond
    [(hash-has-key? h breed)
     (hash-set! (hash-ref h breed) id true)]
    [else
     (define h2 (make-hash))
     (hash-set! h2 id true)
     (hash-set! h breed h2)
     (set-cell-value! x y 'agents h)
     ]))


;; Agent interactions with world.

(define get-x
  (case-lambda
    [() (get-x (current-agent))]
    [(a)
     ;;(printf "id ~a keys ~a~n" (agent-id a) (for/list ([(k v) (agent-fields a)]) k))
     (hash-ref (agent-fields a) 'x)]))

(define get-y
  (case-lambda
    [() (get-y (current-agent))]
    [(a)
     (hash-ref (agent-fields a) 'y)]))

(define (->bin-x v)
  (modulo (inexact->exact (floor v)) (world-cols)))
(define (->bin-y v)
  (modulo (inexact->exact (floor v)) (world-rows)))

(define set-x!
  (case-lambda
    [(v) (set-x! (current-agent) v)]
    [(a v)
     ;; (printf "set-x! id ~a v ~a~n" (agent-id a) v)
     ;; What is the agent's current bin-x and bin-y?
     (define current-bin-x (get-bin-x a))
     (define current-bin-y (get-bin-y a))

     ;; If we're moving bins, update the world.
     (define new-bin-x (->bin-x v))
     
     (when (NoValueFound? current-bin-x)
       (set! current-bin-x new-bin-x)
       (set-bin-x! new-bin-x))
     
     (when (not (= new-bin-x
                   current-bin-x))
       (cell-remove-agent current-bin-x current-bin-y
                          (get-breed a)
                          (agent-id a))
       (cell-add-agent! new-bin-x current-bin-y
                        (get-breed a)
                        (agent-id a)))
     ;; Update our bins
     (set-bin-x! new-bin-x)

     (hash-set! (agent-fields a) 'x v)
     ]))
               
(define set-y!
  (case-lambda
    [(v) (set-y! (current-agent) v)]
    [(a v)
     ;; What is the agent's current bin-x and bin-y?
     (define current-bin-x (get-bin-x a))
     (define current-bin-y (get-bin-y a))
     ;; If we're moving bins, update the world.
     (define new-bin-y (->bin-y v))
     
     (when (NoValueFound? current-bin-y)
       (set! current-bin-y new-bin-y)
       (set-bin-y! new-bin-y))
     
     (when (not (= new-bin-y
                   current-bin-y))
       (cell-remove-agent current-bin-x current-bin-y
                          (get-breed a)
                          (agent-id a))
       (cell-add-agent! current-bin-x new-bin-y
                        (get-breed a)
                        (agent-id a)))
     ;; Update our bins
     (set-bin-y! new-bin-y)

     (hash-set! (agent-fields a) 'y v)
     ]))
