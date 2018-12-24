#lang racket

(provide 
         (contract-out
          [make-agent            (-> number? number? number? number?
                                     color? number?
                                     symbol? symbol?
                                     (-> vector? any)
                                     (listof symbol?)
                                     vector?)]
          [agent-fields          (listof symbol?)]
          [agent-control-fields  (listof symbol?)]
          [default-draw-function (-> vector? any)]
          [agent-x               (-> vector? number?)]
          [agent-y               (-> vector? number?)]
          [agent-id              (-> vector? number?)]
          [agent-color           (-> vector? color?)]
          [agent-plural          (-> vector? symbol?)]
          [get-agent-fields      (-> vector? (listof symbol?))]
          ))

(require sgl/gl)
(require "colors.rkt")

(define agent-fields '(id pid x y color direction))
(define agent-control-fields '(singular plural draw fields))

(define (make-agent id pid x y color direction singular plural default-draw-function special-fields)
  (vector
   ;0  1  2 3
   id pid x y
   ;; 4     5
   color direction
   ;; 6     7
   singular plural
   ;; 8
   default-draw-function
   ;; 9
   (append agent-fields agent-control-fields special-fields)))

(define (agent-id a)          (vector-ref a 0))
(define (agent-pid a)         (vector-ref a 1))
(define (agent-x a)           (vector-ref a 2))
(define (agent-y a)           (vector-ref a 3))
(define (agent-color a)       (vector-ref a 4))
(define (agent-direction a)   (vector-ref a 5))
(define (agent-plural a)      (vector-ref a 7))
(define (get-agent-fields a)  (vector-ref a 9))

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

#|
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
|#
