#lang racket
(require syntax/parse
         racket/contract
         sgl/gl)

(require "base.rkt"
         "backing.rkt"
         "patches.rkt"
         "types.rkt"
         )

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
                                             (quote breed)
                                             (quote plural)))
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
  ;; We have to add this to every agent.
  (define agents (for/hash ([(id agent)
                             (agentset-agents (as))])
                   ;; Hopefully my set macro works...
                   ;; FIXME handle boolean? defaults.
                   (set agent field 0)
                   (values id agent)))
  ;; And build a new agentset.
  (define new-agentset
    (agentset (agentset-breed (as))
              (agentset-plural (as))
              (combine (agentset-base-fields (as)) fields)
              agents))
  (hash-set! agentsets (agentset-plural (as)) new-agentset))

;; This has to happen regardless of when
(define (give* as . fields)
  (append-agentset-base-fields! as fields)
  ;;(printf "Dealing with ~a~n" (as))
  (for ([(id agent) (agentset-agents (as))])
    ;;(printf "Looking at agent ~a~n" id)
    ;; Unless they already have a value there, we'll
    ;; assign a value of zero, so every agent gets the key.
    (for ([k fields])
      ;;(printf "Adding field: ~a~n" k)
      (unless (hash-has-key? (agent-fields agent) k)
        ;;(printf "~a added to ~a:~a~n" k (agentset-breed (as)) id)
        (hash-set! (agent-fields agent) k 0)))))

(define-syntax (give stx)
  (syntax-case stx ()
    [(_ as fields ...)
     #`(give* as (quote (fields ...)))
     ]))

(define (default-draw-function)
  (let ()
    (define turtle-x (get xcor))
    (define turtle-y (get ycor))
    ;;(printf "a ~a x ~a y ~a~n" (agent-id (current-agent)) turtle-x turtle-y)
    ;; (glClear GL_DEPTH_BUFFER_BIT)
    (glPushMatrix)
    
    (glTranslatef turtle-x turtle-y 0)
    (glRotatef (get direction) 0 0 1)
    (glTranslatef (- turtle-x) (- turtle-y) 0)
          
    (glBegin GL_TRIANGLES)
    ;; These return bytes.
    (define color-obj (get color))
    (glColor3ub (rgb-color-r color-obj)
                (rgb-color-g color-obj)
                (rgb-color-b color-obj))
    
    ;; FIXME This does not center the agent in a square.          
    (glVertex3f turtle-x (+ (/ 1 2) turtle-y) 0.1)
    (glVertex3f (- turtle-x (/ 1 2)) (- turtle-y (/ 1 2)) 0.1)
    (glVertex3f (+ turtle-x (/ 1 2)) (- turtle-y (/ 1 2)) 0.1)
    (glEnd)

    (glBegin GL_QUADS)
    (glColor3ub 255 255 255)
    (glVertex3f (- turtle-x .1) (- turtle-y .1) 0.2)
    (glVertex3f (- turtle-x .1) (+ turtle-y .1) 0.2)
    (glVertex3f (+ turtle-x .1) (+ turtle-y .1) 0.2)
    (glVertex3f (+ turtle-x .1) (- turtle-y .1) 0.2)
    (glColor3ub 0 0 0)
    (glEnd)
    (glPopMatrix)
    ))

;; We need a default fieldset for agents.
(define/contract (make-default-agent-fields id breed plural)
  (-> (or/c symbol? number?) symbol? symbol? hash?) 
  (define h (make-hash))
  (hash-set! h 'id id)
  (hash-set! h 'breed breed)
  (hash-set! h 'plural plural)
  (hash-set! h 'xcor 0)
  (hash-set! h 'ycor 0)
  
  (hash-set! h 'direction (random 360))
  (hash-set! h 'color (rgb (+ 64 (random 128))
                           (+ 64 (random 128))
                           (+ 64 (random 128))))
  (hash-set! h 'draw default-draw-function)
  ;; What patch are we over?
  (hash-set! h 'patch-id 0)
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
  

(define (generate-radius x y r)
  (define points empty)
  (for ([x-range (range (- x r) (+ x r))])
    (for ([y-range (range (- y r) (+ y r))])
      (set! points (cons (list x-range y-range) points))))
  points)

;; Memoizing sniff...

(define (hash3 a b c)
  (* (* (* (* a 37) b) 37) c))
  
(define sniff-memo
  (let ([memo (make-hash)])
    (match-lambda*
      [(list (quote preload) x y (var radius))
       ;;(printf "sniff-memo preload~n")
       (define key (hash3 x y radius))
       (define points (generate-radius x y radius))
       (define patch-ids (map ->patch
                              (map first points)
                              (map second points)))
       (hash-set! memo key patch-ids)
       ]
      [(list (var radius))
       ;;(printf "sniff radius~n")
       (sniff (hash-ref agentsets
                        (hash-ref (agent-fields (current-agent)) 'plural)) radius)]
      [(list (var as) (var radius))
       ;;(printf "sniff as radius~n")
       (define key (hash3 (exact-floor (get (current-agent) xcor))
                          (exact-floor (get (current-agent) ycor))
                          radius))

       (define patch-ids 0)
       
       (cond
         [(hash-has-key? memo key)
          (set! patch-ids (hash-ref memo key))]
         [else
          (define points (generate-radius
                          (get (current-agent) xcor)
                          (get (current-agent) ycor)
                          radius))
          (set! patch-ids (map ->patch
                                 (map first points)
                                 (map second points)))
          (hash-set! memo key patch-ids)])
          
       (define found (make-hash))
       (for ([pid patch-ids])
         ;; (printf "checking patch ~a~n" pid)
         
         (define h (get-backing-from-patch-id pid))
         
         (for ([(found-id _boolean_not_agent_) h])
           ;; (printf "\tfound agent ~a~n" found-id)
           (hash-set! found found-id true)))
       
       (define result (λ ()  (agentset (agentset-breed (as))
                                       (agentset-plural (as))
                                       (agentset-base-fields (as))
                                       (for/hash ([(id _bool_) found])
                                         (values id
                                                 (hash-ref (agentset-agents (as)) id)))
                                       )))
       result]
      )))
                                          
(define sniff sniff-memo)

(require (for-syntax syntax/parse))

(define-syntax (ask-patches stx)
  (syntax-parse stx
    ;; What about a patchset? It should be a list of pids.
    [(_ap ((~datum using) ps) bodies:expr ...)
     #`(for ([pid ps])
         (parameterize ([current-patch pid])
           bodies ...))]
    
    [(_ap bodies:expr ...)
     #`(for ([i (range (vector-length the-world))])
         (parameterize ([current-patch i])
           bodies ...))]

    ))

;; The 'ask' macro is easier now. It isn't 'ask-turtles' and 'ask-fishes,' but instead
;; just 'ask' followed by an agentset.
(define-syntax (ask stx)
  (syntax-parse stx
    [(_ask (~datum patches) bodies:expr ...)
     ;; Need to visit all of the patches.
     ;; This will be expensive.
     #`(for ([i (range (vector-length the-world))])
         (parameterize ([current-patch i])
           bodies ...))
     ]
    [(_ask as bodies:expr ...)
     #`(for ([(id agent) (agentset-agents (as))])
           ;; (printf "ask agent ~a ~n" agent)
           (current-agent agent)
           (current-patch (->patch (hash-ref (agent-fields agent) 'xcor)
                                   (hash-ref (agent-fields agent) 'ycor)))
           ;; FIXME I don't think this is needed.
           ;; (set-current-patch agent)
           bodies ...)
     ] ))

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
  (define plural (agentset-plural (λ:as)))
  ;; This should guarantee globally unique agent ids.
  (define start-id (get-next-agent-id))
  (set-next-agent-id! (+ start-id num))
  
  (for ([id (range num)])
    (define offset-id (+ id start-id))
    ;; FIXME
    ;; I need a default set of values in the agent's fields.
    (define default-fields (make-default-agent-fields offset-id breed plural))
    ;; Add in the fields that have been added.
    (for ([f (agentset-base-fields (λ:as))])
      (unless (hash-has-key? default-fields f)
        ;; FIXME: Always give zero as value?
        (hash-set! default-fields f 0)))
    (define new-agent (agent offset-id breed default-fields))
    (add-to-agentset! λ:as new-agent)
    ;; NEED TO DO THIS
    ;; (update-location! new-agent 0 0)
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

(define-syntax (any? stx)
  (syntax-case stx ()
    [(_ as)
     #`(let ()
         (> (hash-count (agentset-agents (as))) 0))]
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
     ]
    ))


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

(define (wrap val max)
  (cond
    [(>= val max) (modulo val max)]
    [(< val 0) (modulo (+ max val) max)]
    [else val])
  )

(define (move magnitude)
  (define direction (get (current-agent) direction))
  (define-values (new-x new-y)
    (offset (get (current-agent) xcor)
            (get (current-agent) ycor)
            direction magnitude))
  (set (current-agent)
       xcor
       (wrap new-x (get global world-cols)))
  (set (current-agent)
       ycor
       (wrap new-y (get global world-rows)))

  (set (current-agent)
       prev-patch-id
       (get patch-id))
  
  (set (current-agent)
       patch-id
       (->patch (get xcor) (get ycor)))

  ;; For tracking agent locations.
  (update-backing! (current-agent))

  )

(define (right d)
  (set (current-agent) direction (+ (get (current-agent) direction) d)))

(define (left d)
  (set (current-agent) direction (- (get (current-agent) direction) d)))

(define die
  (case-lambda
    [() (die (current-agent))]
    [(agent)
     (define as (hash-ref agentsets (hash-ref (agent-fields (current-agent))
                                                   'plural)))
     (define aid (agent-id (current-agent)))
     (remove-from-agentset! (λ () as) aid)
     (remove-from-backing! agent)
     ]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Patches are not that different.

;; The world has to do with patches.
;; It requires the agentsets module, because patches are an agentset.

;;(define patches (agentset 'patch  'patches empty (make-hash)))
;;(add-agentset patches)
;; (create-breed patch patches)
;; (create-breed dirty-patch dirty-patches)




(define-syntax (increment! stx)
  (syntax-parse stx
    [(_ var:id)
     #`(set! var (add1 var))]
    [(_ var:id ammount:number)
     #`(set! var (+ var ammount))]
    ))