#lang racket

(provide struct:agent
         make-agent
         agent?
         agent-get
         agent-set!
         agent-id agent-pid
         agent-x agent-y
         agent-color agent-direction
         prop:draw draw-ref draw?
         prop:field-names field-names-ref
         (contract-out
          [agent-fields  (listof symbol?)]
          ))

(require sgl/gl)
(require "colors.rkt")

(define agent-fields '(id pid x y color direction))


(define (default-draw-function agent)
  (let ()
    ;; (printf "Drawing ~a~n" (agent-id agent))
    
    (define turtle-x (agent-x agent))
    (define turtle-y (agent-y agent))
    ;;(printf "a ~a x ~a y ~a~n" (agent-id (current-agent)) turtle-x turtle-y)
    ;; (glClear GL_DEPTH_BUFFER_BIT)
    (glPushMatrix)
    
    (glTranslatef turtle-x turtle-y 0)
    (glRotatef (agent-direction agent) 0 0 1)
    (glTranslatef (- turtle-x) (- turtle-y) 0)
          
    (glBegin GL_TRIANGLES)
    ;; These return bytes.
    (define color-obj (agent-color agent))
    (glColor3ub (color-red color-obj)
                (color-green color-obj)
                (color-blue color-obj))
    
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

;; https://docs.racket-lang.org/reference/structprops.html
(define-values (prop:draw draw? draw-ref) (make-struct-type-property 'draw))
(define-values (prop:field-names field-names field-names-ref) (make-struct-type-property 'field-names))

;; The base structure for all agents.
;; (struct agent (id pid x y color direction) #:transparent)
(define-values (struct:agent make-agent agent? agent-get agent-set!)
  ;; name super-type init-field-count auto-field-count
  ;; auto-v properties inspector
  (make-struct-type 'agent false
                    (length agent-fields) 0
                    false
                    (list (cons prop:draw default-draw-function)
                          (cons prop:field-names agent-fields)
                          )
                    false))


(define (agent-id a)          (agent-get a 0))
(define (agent-pid a)         (agent-get a 1))
(define (agent-x a)           (agent-get a 2))
(define (agent-y a)           (agent-get a 3))
(define (agent-color a)       (agent-get a 4))
(define (agent-direction a)   (agent-get a 5))

