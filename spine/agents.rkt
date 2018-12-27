#lang racket

(require (except-in racket/gui set)
         sgl/gl)

(require "agentsets.rkt"
         "util.rkt"
         "state.rkt"
         "types.rkt"
         )

(provide create
         show-agents
         agent-id
         agent-x
         agent-y
         agent-direction
         )

(define base-fields '(breed plural id pid x y direction color))
(define agent-id 2)
(define agent-x 4)
(define agent-y 5)
(define agent-direction 6)
(define agent-color 7)

(define (make-default-agent sing plur ndx)
  (define x (/ (get-global 'world-columns) 2))
  (define y (/ (get-global 'world-rows) 2))
  (vector sing plur
          ndx
          ;; Our initial patch is the middle.
          (->pid x y)
          x y
          (random 360)
          (color (random 256)
                 (random 256)
                 (random 256))
          ))


;; This creates new agent vectors and inserts them into
;; the correct breed's agentset.
(define (create plural-sym num)
  (define singular (get-agentset-meta plural-sym 'singular))
  (define sym (combine-to-symbol singular '-next-index))
  (define starting-ndx (get-agentset-meta plural-sym sym))
  (for ([ndx (range starting-ndx (+ num starting-ndx))])
    ;; (printf "Creating agent: ~a~n" ndx)
    (let ([agent (make-default-agent singular plural-sym ndx)]
          [asvec (get-agentset plural-sym)])
      (vector-set! asvec ndx agent)
      )
    (set-agentset-meta! plural-sym (combine-to-symbol singular '-next-index) (+ num starting-ndx))
    (set-agentset-meta! plural-sym
                        'default-drawing-function
                        draw-agent)
    ))


;; Once agents start dying, this will not work.
(define (show-agents plural)
  (define agentset (hash-ref agentsets plural))
  (define max-id (get-max-id plural))
  (for ([ndx (range max-id)])
    ;; FIXME: What happens when agents die? Set them to false?
    (define agent (vector-ref agentset ndx))
    (when (vector? agent)
      (printf "~a: ~a~n" ndx agent))))


   
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
