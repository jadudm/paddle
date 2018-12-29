#lang racket


(require sgl/gl)
(require "agentsets.rkt"
         "util.rkt"
         "state.rkt"
         "types.rkt"
         )

(provide create
         hatch
         show-agents
         agent-direction
         )

(define (make-default-agent sing plur ndx)
  (define x (/ (get-global 'world-columns) 2))
  (define y (/ (get-global 'world-rows) 2))
  (apply vector
         (append (list sing plur
                       ndx
                       ;; Our initial patch is the middle.
                       (->pid x y)
                       x y
                       (random 360)
                       (color (random 256)
                              (random 256)
                              (random 256)))
                 (map (λ (f) 0)
                      (drop (get-agentset-meta plur 'fields)
                            (length agent-base-fields)))) 
         ))


;; This creates new agent vectors and inserts them into
;; the correct breed's agentset.
(define (create plural-sym num #:return-new-set [rns false])
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
      (extend-agentset plural-sym))

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

(define (hatch plural-sym num)
  (create plural-sym num #:return-new-set true))
  

;; Once agents start dying, this will not work.
(define (show-agents plural)
  (define agentset (hash-ref agentsets plural))
  (define max-id (get-max-id plural))
  (for ([ndx (range max-id)])
    ;; FIXME: What happens when agents die? Set them to false?
    (define agent (vector-ref agentset ndx))
    (when (vector? agent)
      (printf "~a: ~a~n" ndx agent))))


;; FIXME
(define (draw-agent a)
  (let ()
    ;; (printf "agent: ~a~n" a)
    (define turtle-x (vector-ref a agent-x))
    (define turtle-y (vector-ref a agent-y))
    ;;(printf "a ~a x ~a y ~a~n" (agent-id (current-agent)) turtle-x turtle-y)
    ;; (glClear GL_DEPTH_BUFFER_BIT)
    (glPushMatrix)
    
    (glTranslatef turtle-x turtle-y 0)
    (glRotatef (vector-ref a agent-direction) 0 0 1)
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

    #;(begin
        (glBegin GL_QUADS)
        (glColor3ub 255 255 255)
        (glVertex3f (- turtle-x .1) (- turtle-y .1) 0.2)
        (glVertex3f (- turtle-x .1) (+ turtle-y .1) 0.2)
        (glVertex3f (+ turtle-x .1) (+ turtle-y .1) 0.2)
        (glVertex3f (+ turtle-x .1) (- turtle-y .1) 0.2)
        (glColor3ub 0 0 0)
        (glEnd))
    
    (glPopMatrix)
    ))
