#lang racket
(require syntax/parse
         racket/contract
         (only-in racket/draw color%)
         sgl/gl)
(require "base.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;
;; BREEDS
;; Breeds suggest new agentsets.
(define-syntax (create-breed stx)
  (syntax-case stx ()
    [(_ breed plural)
     #`(begin
         (add-agentset (agentset (quote breed)
                                 (quote plural)
                                 (hash-keys (make-default-agent-fields
                                             'unused-id
                                             (quote breed)))
                                 (make-hash)))
         (define plural
           (λ () (hash-ref agentsets (quote plural))))
         )
     ]))

;; And, everything will depend on knowing the current agent.
;; This has moved to a separate module, because it is required
;; for syntax and runtime. The module boundary solves that phase issue.

;; And, I need a way to manipulate agentsets. Better an ADT...
(define/contract (add-agentset as)
  (-> agentset? agentset?)
  (hash-set! agentsets (agentset-plural as) as)
  as)

;; Adding fields to a things...
(define (append-agentset-base-fields! as fields)
  ;; And build a new agentset.
  (define new-agentset
    (agentset (agentset-breed (as))
              (agentset-plural (as))
              (combine (agentset-base-fields (as)) fields)
              (agentset-agents (as))))
  (hash-set! agentsets (agentset-plural (as)) new-agentset))

;; This has to happen regardless of when
(define-syntax (give stx)
  (syntax-case stx ()
    [(_ as fields ...)
     ;; FIXME
     ;; This sets the global params for the agentset. However,
     ;; it should also update all agents.
     #`(begin
         ;; First, extend the fields the agentset carries.
         (append-agentset-base-fields! as (quote (fields ...)))

         ;; Then, extend all of the existing agents.
         ;; (printf "Dealing with ~a~n" (as))
         (for ([(id agent) (agentset-agents (as))])
           ;; (printf "Looking at agent ~a~n" id)
           ;; Unless they already have a value there, we'll
           ;; assign a value of zero, so every agent gets the key.
           (for ([k (quote (fields ...))])
             ;; (printf "Adding field: ~a~n" k)
             (unless (hash-has-key? (agent-fields agent) k)
               ;; (printf "~a added to ~a:~a~n" k (agentset-breed (as)) id)
               (hash-set! (agent-fields agent) k 'default)))))
     ]))

;; FIXME: There are 14 core colors.
;; It will take some effort to build the NetLogo color table.
(define/contract (rgb r g b)
  (-> byte? byte? byte? (is-a?/c color%))
  (make-object color% r g b))

(define (default-draw-function)
  (let ()
    (define turtle-x (get xcor))
    (define turtle-y (get ycor))
    ;;(printf "a ~a x ~a y ~a~n" (agent-id (current-agent)) turtle-x turtle-y)
    (glPushMatrix)
    (glTranslatef turtle-x turtle-y 0)
    (glRotatef (get direction) 0 0 1)
    (glTranslatef (- turtle-x) (- turtle-y) 0)
          
    (glBegin GL_TRIANGLES)
    ;; These return bytes.
    (define color-obj (get color))
    (glColor3ub (send color-obj red)
                (send color-obj green)
                (send color-obj blue))
    ;; FIXME This does not center the agent in a square.          
    (glVertex3f turtle-x (+ (/ 1 2) turtle-y) 0)
    (glVertex3f (- turtle-x (/ 1 2)) (- turtle-y (/ 1 2)) 0)
    (glVertex3f (+ turtle-x (/ 1 2)) (- turtle-y (/ 1 2)) 0)
    (glEnd)

    (glBegin GL_QUADS)
    (glColor3ub 255 255 255)
    (glVertex3f (- turtle-x .1) (- turtle-y .1) 0)
    (glVertex3f (- turtle-x .1) (+ turtle-y .1) 0)
    (glVertex3f (+ turtle-x .1) (+ turtle-y .1) 0)
    (glVertex3f (+ turtle-x .1) (- turtle-y .1) 0)
    (glEnd)
    
          
    (glPopMatrix)
    ))

;; We need a default fieldset for agents.
(define/contract (make-default-agent-fields id breed)
  (-> (or/c symbol? number?) symbol? hash?) 
  (define h (make-hash))
  (hash-set! h 'id id)
  (hash-set! h 'breed breed)
  (hash-set! h 'xcor 0)
  (hash-set! h 'ycor 0)
  (hash-set! h 'direction (random 360))
  (hash-set! h 'color (rgb (+ 64 (random 128))
                           (+ 64 (random 128))
                           (+ 64 (random 128))))
  (hash-set! h 'draw default-draw-function)
  h)


;; Those are static agentsets. Some agentsets are actually more
;; dynamic. For example, 'turtles-here' is an agentset, but
;; it queries the world and returns the turtles on the current
;; patch. These will have to be implemented differently.
;; Or, they're just functions.

;; CONTRACT
;; other : agentset -> agentset
;; PURPOSE
;; Returns an agentset with everything save for the current agent.
;; Must be executed in a context where the current agent is defined.
(define/contract (other as)
  (-> procedure? procedure?)
  (define current-agent-id (agent-id (current-agent)))
  (define h (make-hash))
  (for ([(id agnt) (agentset-agents (as))])
    ;; Only keep things that are not the current agent.
    (unless (= current-agent-id id)
      (hash-set! h id agnt)))
  ;; Return a new set of agents.
  (λ () (agentset (agentset-breed (as))
                  (agentset-plural (as))
                  (agentset-base-fields (as))
                  h)))


;; Everything is in a coordinate system with OpenGL where the number of
;; columns and rows is scaled to the height and width of the viewport.
;; Therefore, [79.9, 79.9] will map to [79,79], which is less than 80x80.
;; Can we ever get a value outside the range? I don't know. This has to do with
;; whether we map by wrapping or not.
(define (->patch a x y)
  (define edge-y ((get global edge-y) (exact-floor y)))
  (define edge-x ((get global edge-x) (exact-floor x)))
  (define world-rows (get global world-rows))
  
  ;;(printf "\te-y ~a e-x ~a w-r ~a~n" edge-y edge-x world-rows)
  
  (exact-floor (+ (* edge-y world-rows) edge-x))
  )


(define (set-current-patch a)
  ;; Floor the agent's position, multiply, and use
  ;; that as a pretend patch-id to work backwards.
  (define xcor (get a xcor))
  (define ycor (get a ycor))
  (define patch-id (->patch a xcor ycor))
  
  ;; Patches are world-cols x world-rows. And, their ids are
  ;; (define pxcor (remainder patch-id (get world-cols)))
  ;; (define pycor (quotient  patch-id (get world-rows))))
  (define hash:patches (agentset-agents (hash-ref agentsets 'patches)))
  (cond
    [(hash-has-key? hash:patches patch-id)
     (current-patch (hash-ref hash:patches patch-id))]
    [else
     (error 'set-current-patch
            "xcor ~a ycor ~a e-x ~a e-y ~a ->patch ~a patch-id ~a~n"
            xcor ycor
            ((get global edge-x) (exact-floor xcor))
            ((get global edge-y) (exact-floor ycor))
            (->patch a xcor ycor)
            patch-id)])
  )
  

;; The 'ask' macro is easier now. It isn't 'ask-turtles' and 'ask-fishes,' but instead
;; just 'ask' followed by an agentset.
(define-syntax (ask stx)
  (syntax-case stx ()
    [(_ as bodies ...)
     #`(begin
         (cond
           [(equal? (agentset-breed (as)) 'patch)
            (printf "Asking (= patch ~a).~n" (agentset-breed (as)))
            (for ([(id agent) (agentset-agents (as))])
              (current-agent agent)
              (current-patch agent)
              bodies ...)]
           [else
            (printf "Asking ~a.~n" (agentset-breed (as)))
            (for ([(id agent) (agentset-agents (as))])
              (current-agent agent)
              (set-current-patch agent)
              bodies ...)]
         ))]))

(define-syntax (ask2 stx)
  (syntax-case stx ()
    [(_ as bodies ...)
     #`(begin
         (for ([(id agent) (agentset-agents (as))])
              (current-agent agent)
              (set-current-patch agent)
              bodies ...))]))

;; However, I can't test anything yet, because I can't create any agentsets.
;; I need to be able to do that. I could 'create-turtles', and then I need
;; 'create-fishes', and so on. So, I should have a way of specifying breeds
;; just like a specify agentsets. Perhaps the agentset is good enough,
;; because it carries the info I need?
;;
;; This also looks like it is a magic value, when really the agentset
;; is just the parameter.

;; CONTRACT
;; create : agentset number -> agentset
;; PURPOSE
;; Creates new agents in the agentset.
;; Returns the extended agentset.
(define/contract (create λ:as num)
  (-> procedure? number? procedure?)
  (define h (agentset-agents (λ:as)))
  (define breed (agentset-breed (λ:as)))
  (define start-id (hash-count h))
 
  (for ([id (range num)])
    (define offset-id (+ id start-id))
    (when (zero? (modulo id 1000))
      (printf "\tc: ~a~n" id))
    
    ;; FIXME
    ;; I need a default set of values in the agent's fields.
    (define default-fields (make-hash))
    (unless (equal? breed 'patch)
      (set! default-fields (make-default-agent-fields offset-id breed)))
    
    ;; Add in the fields that have been added.
    (for ([f (agentset-base-fields (λ:as))])
      (unless (hash-has-key? default-fields f)
        ;; FIXME: Always give zero as value?
        (hash-set! default-fields f 0)))
    (define new-agent (agent offset-id breed default-fields))
    (add-to-agentset! λ:as new-agent)
    )
  λ:as)

;; Now, it would be nice to be able to have a "with" form
;; that is used as part of the agentset filtering.
(define-syntax (with stx)
  (syntax-case stx ()
    [(with as bool-exp)
     #`(let ()
         (define subset (make-hash))
         (for ([(id agent) (agentset-agents (as))])
           (current-agent agent)
           (when bool-exp
             (hash-set! subset
                        (agent-id (current-agent))
                        (current-agent))))
         ;; Agentsets need to come back as a thunk.
         (λ () (agentset (agentset-breed (as))
                         (agentset-plural (as))
                         (agentset-base-fields (as))
                         subset)))
     ]))

(define-syntax (have stx)
  (syntax-case stx ()
    [(_ var)
     #`(hash-ref (agent-fields (current-agent)) (quote var) false)]
    [(_ a var)
     #`(hash-ref (agent-fields a) (quote var) false)]
    ))


(define-syntax (agent-get stx)
  (syntax-case stx ()
    [(_ var)
     #`(hash-ref (agent-fields (current-agent)) (quote var) false)]
    [(_ a var)
     #`(hash-ref (agent-fields a) (quote var) false)]))

(define-syntax (agent-set! stx)
  (syntax-case stx ()
    [(_ var expr)
     #`(hash-set! (agent-fields (current-agent)) (quote var) expr)]
    [(_ a var expr)
     #`(hash-set! (agent-fields a) (quote var) expr)]))

(define-syntax (any? stx)
  (syntax-case stx ()
    [(_ as bool-exp)
     #`(let ()
         (define counter 0)
         (for ([(id agent) (agentset-agents (as))])
           (current-agent agent)
           (when bool-exp
             (set! counter (add1 counter))))
         ;; If the bool-exp was ever true, I incremented
         ;; the counter. So, I should return true.
         (> counter 0))
     ]))


;; What if I want to know if everyone has a property?
(define-syntax (all? stx)
  (syntax-case stx ()
    [(_ as bool-exp)
     #`(let ()
         (define flag true)
         (for ([(id agent) (agentset-agents (as))])
           (current-agent agent)
           (set! flag (and flag bool-exp)))
         flag)
     ]))

;; How about the ability to pull one agent at random?

;; CONTRACT
;; one-of : agentset -> agentset
;; PURPOSE
;; Selects one agent at random from the agentset, returning
;; an agentset with just one agent. This has an expensive
;; operation to turn the keys into a list... but, I don't know
;; a better way to guarantee that I'll get a valid id.
(define/contract (one-of as)
  (-> agentset? agentset?)
  (define h (agentset-agents (as)))
  (define keys (hash-keys h))
  (agentset (agentset-breed (as))
            (agentset-plural (as))
            (hash-ref h (list-ref keys (random (length keys))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Movement
(define pi-conv (/ pi 180))

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
  (define direction (get (current-agent) direction))
  (define-values (new-x new-y)
    (offset (get (current-agent) xcor)
            (get (current-agent) ycor)
            direction magnitude))
  (set (current-agent)
       xcor
       (wrap new-x (get global world-rows)))
  (set (current-agent)
       ycor
       (wrap new-y (get global world-cols)))
  )

(define (right d)
  (set (current-agent) direction (+ (get (current-agent) direction) d)))

(define (left d)
  (set (current-agent) direction (- (get (current-agent) direction) d)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Patches are not that different.

;; The world has to do with patches.
;; It requires the agentsets module, because patches are an agentset.

;;(define patches (agentset 'patch  'patches empty (make-hash)))
;;(add-agentset patches)
(create-breed patch patches)
(create-breed dirty-patch dirty-patches)

;; If patches are indexed reasonably in the agentset, then it should be
;; possible to index into them quickly.
;;
;; If they are sequential, I can mathematically map to the correct patch.
;; This will all need to be done at run-time. The user needs to be able
;; to change the number or rows/cols, and as a result, all of this needs to be
;; dynamic.
(define (create-patches)
  (create patches (* (get global world-cols) (get global world-rows)))

  (give patches dirty? draw pcolor pxcor pycor)
  
  (define counter 0)
  (ask patches
       (set patch* dirty? false)
       (printf "setting patch ~a~n" (get patch id))
       (printf "\tpatch ~a~n" (get (current-agent) id))
       
       (set patch* id (get patch id))
       (set patch* draw draw-patch)
       (set patch* pxcor (quotient (get patch id) (get global world-cols)))
       (set patch* pycor (remainder  (get patch id) (get global world-rows)))
       
       (set! counter (add1 counter))
       (when (zero? (modulo counter 1000))
         (printf "\tcreate-patches: ~a~n" counter))
       )
  )

(define (draw-patch)
  (define side 1)
  (define col (get patch pxcor))
  (define row (get patch pycor))
  (let ()
    (glBegin GL_QUADS)
    (define r (send (get patch pcolor) red))
    (define g (send (get patch pcolor) green))
    (define b (send (get patch pcolor) blue))
    (when (zero? (get (current-patch) id))
      (set! r 255)
      (set! g 255)
      (set! b 255)
      )
    (glColor3ub r g b)
    
    (glVertex3f (+ 0 (* side row)) (+ 0 (* col side)) 0)
    (glVertex3f (+ side (* side row)) (+ 0 (* col side)) 0)
    (glVertex3f (+ side (* side row)) (+ side (* col side)) 0)
    (glVertex3f (+ 0 (* side row)) (+ side (* col side)) 0)
    (glEnd)
    ))


