#lang racket
(require racket/gui
         sgl/gl)

(require "base.rkt"
         "forms.rkt")
(provide (all-defined-out))


(define ticker (make-parameter 0))
(define tick-pause (make-parameter (/ 1 60)))
(define (tick)
  (ticker (add1 (ticker)))
  (sleep (tick-pause)))

(define (draw-agents)
  (printf "(draw-agents)~n")
  (for ([(plural agent-vec) agent-vectors])
    (unless (equal? plural 'patches)
      (for ([critter agent-vec])
        (parameterize ([current-agent critter])
          (define turtle-x (get-x))
          (define turtle-y (get-y))
          ;;(printf "a ~a x ~a y ~a~n" (agent-id (current-agent)) turtle-x turtle-y)


          (glPushMatrix)
          (glTranslatef (get-x) (get-y) 0)
          (glRotatef (get-direction) 0 0 1)
          (glTranslatef (- (get-x)) (- (get-y)) 0)
          
          (glBegin GL_TRIANGLES)
          ;; These return bytes.
          (define color-obj (get-color))
          (glColor3ub (send color-obj red)
                      (send color-obj green)
                      (send color-obj blue))
          ;; This does not center the agent in a square.
          
          (glVertex3f turtle-x (+ (/ 1 2) turtle-y) 0)
          (glVertex3f (- turtle-x (/ 1 2)) (- turtle-y (/ 1 2)) 0)
          (glVertex3f (+ turtle-x (/ 1 2)) (- turtle-y (/ 1 2)) 0)
          (glEnd)

          
          (glPopMatrix)
          )))))

(define (setup-gl-draw)
  (glClearColor 0.0 0.0 0.0 0.0)
  (glClear GL_COLOR_BUFFER_BIT)
 
  (glShadeModel GL_SMOOTH)
 
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  ;; glOrtho(0.0, 50.0, 0.0, 50.0, -1.0, 1.0);
  (glOrtho 0.0 (world-rows) 0.0 (world-cols) -1.0 1.0)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity))

(define (draw-grid)
  (define side 1)
  (for ([row (world-rows)])
    (for ([col (world-cols)])
      
      
      (glBegin GL_QUADS)
      (cond
        [(and (even? col) (even? row))
         (glColor3ub #x80 #x80 #x80)]
        [(even? col) (glColor3ub #xFF #xFF #xFF)]
        [(even? row) (glColor3ub #x66 #x66 #x66 )]
        [else (glColor3ub #x00 #x00 #x00)])
      (glVertex3f (+ 0 (* side row)) (+ 0 (* col side)) 0)
      (glVertex3f (+ side (* side row)) (+ 0 (* col side)) 0)
      (glVertex3f (+ side (* side row)) (+ side (* col side)) 0)
      (glVertex3f (+ 0 (* side row)) (+ side (* col side)) 0)
      
      (glEnd)

      
      )))


(define (draw-opengl)
  (setup-gl-draw)
  (draw-agents)
  ;; (draw-grid)
  )


(define (resize w h)
  (glViewport 0 0 w h))

(define paddle-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    (define/override (on-paint)
      (with-gl-context (λ ()
                         (draw-opengl)
                         (swap-gl-buffers)
                         )))
    (define/override (on-size width height)
      (with-gl-context (λ() (resize width height) (on-paint))))
    (super-instantiate () (style '(gl)))))

(define paddle-frame%
  (class frame%
    (define/augment (on-close)
      ((shutdown)))
    (super-new)))

(define shutdown (make-parameter false))

(define (default-setup)
    (for ([(plural agent-vec) agent-vectors])
    (unless (equal? plural 'patches)
      (for ([critter agent-vec])
        (parameterize ([current-agent critter])
          (set-x! 0)
          (set-y! 0)
          (set-direction! (random 360))
          (set-color! (rgb (+ 64 (random 64))
                           (+ 64 (random 64))
                           (+ 64 (random 64))))
          )))))

(define (run-world setup go)
  ;; I need to setup turtles first.
  ;; I provide a default, in case the user
  ;; does nothing.
  (default-setup)
  (setup)

  (printf "Building frame.~n")
  (define win (new paddle-frame%
                   [label "paddle gl"]
                   [min-width 600]
                   [min-height 600]))

  (printf "Building canvas.~n")
  (define gl  (new paddle-canvas%
                   [parent win]))
 
  (send win show #t)
  
  (define draw-thread
    (thread (λ ()
                
              (sleep 1)
              (let loop ()
                (go)
                (send gl on-paint)
                (tick)
                (loop)
                ))))

  (shutdown (λ () (map kill-thread (list draw-thread)))))
