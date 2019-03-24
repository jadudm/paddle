#lang racket


(require sgl/gl)
(require "agentsets.rkt"
         "util.rkt"
         "state.rkt"
         "types.rkt"
         )

(provide (rename-out [create/hash create])
         (rename-out [hatch/hash   hatch])
         show-agents
         (rename-out [triangle shape:triangle])
         (rename-out [triangle shape:turtle])
         (rename-out [circle shape:circle])
         (rename-out [disk shape:disk])
         ;;agent-direction
         ;;agent-vy
         ;;agent-vx
         )

(define (make-default-agent sing plur ndx)
  (define x (/ (get-global 'world-columns) 2))
  (define y (/ (get-global 'world-rows) 2))
  (define vx (/ (random -100 100) 100.0))
  (define vy (/ (random -100 100) 100.0))
  (define dir (theta vx vy))
  (apply vector
         (append (list sing plur
                       ndx
                       ;; Our initial patch is the middle.
                       (->pid x y)
                       x y
                       vx vy dir
                       ;; This is a function of one arg.
                       ;; It consumes the current agent vector.
                       triangle
                       (color (random 256)
                              (random 256)
                              (random 256)))
                 (map (λ (f) 0)
                      (drop (get-agentset-meta plur 'fields)
                            (length agent-base-fields)))) 
         ))


;; This creates new agent vectors and inserts them into
;; the correct breed's agentset.
(define (create/spine plural-sym num #:return-new-set [rns false])
  (define singular (get-agentset-meta plural-sym 'singular))
  (define sym (combine-to-symbol singular '-next-index))
  (define starting-ndx (get-agentset-meta plural-sym sym))

  (unless (>= (+ num starting-ndx) MAX-AGENTSET-SIZE)
    ;; Try compacting if necessary
    ;; I have now implemented memory management...
    ;; This is bad.
    (when (>= (+ num starting-ndx) (* (get-max plural-sym) 0.7))
      (compact-agentset plural-sym)
      (set! starting-ndx (get-agentset-meta plural-sym sym)))

    (when (>= (+ num starting-ndx) (* (get-max plural-sym) 0.9))
      (set-max! plural-sym (* 2 (get-max plural-sym)))
      (extend-agentset/vector plural-sym))

    (define new-set (make-vector 0))
    (when rns
      (set! new-set (make-vector num)))
  
    (for ([ndx (range starting-ndx (+ num starting-ndx))])
      ;; (printf "Creating agent: ~a~n" ndx)
      (let ([agent (make-default-agent singular plural-sym ndx)]
            [asvec (get-agentset plural-sym)])
        (vector-set! asvec ndx agent)
        (when rns
          (vector-set! new-set (- ndx starting-ndx) agent))
        )
      (set-agentset-meta! plural-sym (combine-to-symbol singular '-next-index) (+ num starting-ndx))
      (set-agentset-meta! plural-sym
                          'default-drawing-function
                          draw-agent)
      )

    (cond
      [rns (vector->list new-set)]
      [else (void)])
    ))

(define (create/hash plural-sym num #:return-new-set [rns false])
  (define singular (get-agentset-meta plural-sym 'singular))
  (define sym (combine-to-symbol singular '-next-index))
  (define starting-ndx (get-agentset-meta plural-sym sym))

  ;; Need an ADT...
  (define new-set (make-empty-agentset))
   
  (for ([ndx (range starting-ndx (+ num starting-ndx))])
    ;; (printf "Creating agent: ~a~n" ndx)
    (let ([agent (make-default-agent singular plural-sym ndx)]
          [asvec (get-agentset plural-sym)])
      (insert-into-agentset! asvec ndx agent)
      (when rns
        (insert-into-agentset! new-set (- ndx starting-ndx) agent))
      )
     
    (set-agentset-meta! plural-sym (combine-to-symbol singular '-next-index) (+ num starting-ndx))
    (set-agentset-meta! plural-sym
                        'default-drawing-function
                        draw-agent)
    )
   
  (cond
    [rns (hash-values new-set)]
    [else (void)])
  )

(define (hatch/spine plural-sym num)
  (create/spine plural-sym num #:return-new-set true))

(define (hatch/hash plural-sym num)
  (create/hash plural-sym num #:return-new-set true))
  

;; Once agents start dying, this will not work.
(define (show-agents plural)
  (define agentset (hash-ref agentsets plural))
  (define max-id (get-max-id plural))
  (for ([ndx (range max-id)])
    ;; FIXME: What happens when agents die? Set them to false?
    (define agent (get-agent agentset ndx))
    (when (vector? agent)
      (printf "~a: ~a~n" ndx agent))))

(define (triangle a)
  (define turtle-x (vector-ref a agent-x))
  (define turtle-y (vector-ref a agent-y))
  (define vx (vector-ref a agent-vx))
  (define vy (vector-ref a agent-vy))
  (define direction (theta vx vy))
  
  (glPushMatrix)
  (glTranslatef turtle-x turtle-y 0)
  (glRotatef (modulo (- (exact-floor direction) 90) 360) 0 0 1)
  (glTranslatef (- turtle-x) (- turtle-y) 0)
          
  (glBegin GL_TRIANGLES)
  ;; These return bytes.
  (define c (vector-ref a agent-color))
  (glColor3ub (color-red c)
              (color-green c)
              (color-blue c))
    
  ;; FIXME This does not center the agent in a square.          
  (glVertex3f turtle-x (+ (/ 1 2) turtle-y) 0.1)
  (glVertex3f (- turtle-x (/ 1 2)) (- turtle-y (/ 1 2)) 0.1)
  (glVertex3f (+ turtle-x (/ 1 2)) (- turtle-y (/ 1 2)) 0.1)
  (glEnd)

  #;(glBegin GL_LINES)
  #;(glVertex3f turtle-x turtle-y 0.1)
  #;(glVertex3f turtle-x (+ 1 turtle-y) 0.1)
  #;(glEnd)
  (glPopMatrix))

;; https://stackoverflow.com/questions/22444450/drawing-circle-with-opengl
(define (round radius segments type)
    (λ (a)
    (define turtle-x (vector-ref a agent-x))
    (define turtle-y (vector-ref a agent-y))
    (define vx (vector-ref a agent-vx))
    (define vy (vector-ref a agent-vy))
    (define c (vector-ref a agent-color))
    (glColor3ub (color-red c)
                (color-green c)
                (color-blue c))
    (glBegin type)
    (for ([i segments])
      (define theta (/ (* 2 pi i) segments))
      (define x (* radius (cos theta)))
      (define y (* radius (sin theta)))
      (glVertex2f (+ turtle-x x) (+ turtle-y y)))
    (glEnd)))

(define (circle radius segments)
  (round radius segments GL_LINE_LOOP))

(define (disk radius segments)
  (round radius segments GL_TRIANGLE_FAN))  

;; FIXME
(define (draw-agent a)
  (let ()
    ((vector-ref a agent-shape) a)
    ))
