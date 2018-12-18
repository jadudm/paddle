#lang racket
(require (except-in racket/gui set)
         sgl/gl)

(require "base.rkt"
         "agentsets.rkt")

(provide (all-defined-out))

(define ticker (make-parameter 0))
(define tick-pause (make-parameter (/ 1 60)))
(define (tick)
  (ticker (add1 (ticker)))
  (sleep (tick-pause)))

(define (make-world cols rows width height)
  (set global world-cols cols)
  (set global world-rows rows)
  (set global frame-width width)
  (set global frame-height height))

(define (draw-agents)
  ;;(printf "(draw-agents)~n")
  (for ([(plural as) agentsets])
    ;; Draw the patches first.
    (when (equal? plural 'patches)
      (for ([(id critter) (agentset-agents as)])
        (parameterize ([current-agent critter])
          (when (get dirty?)
            ((get draw))
            (set dirty? false)
            ))))
      

    ;; Draw the agents second
    (unless (equal? plural 'patches)
      (for ([(id critter) (agentset-agents as)])
        (parameterize ([current-agent critter])
          ((get draw))
          )))))

(define (setup-gl-draw)
  (glClearColor 0.0 0.0 0.0 0.0)
  (glClear GL_COLOR_BUFFER_BIT)
 
  (glShadeModel GL_SMOOTH)
 
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  ;; glOrtho(0.0, 50.0, 0.0, 50.0, -1.0, 1.0);
  (glOrtho 0.0 (get world-rows) 0.0 (get world-cols) -1.0 1.0)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity))

(define (draw-grid)
  (define side 1)
  (for ([row (get world-rows)])
    (for ([col (get world-cols)])
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
      (printf "The sky is falling!~n")
      ((stop)))
    (super-new)))

(define stop (make-parameter false))

(define (run-world setup go)
  (setup)

  (printf "Building frame.~n")
  (define win (new paddle-frame%
                   [label "paddle gl"]
                   [min-width  (get frame-width)]
                   [min-height (get frame-height)]))

  (printf "Building canvas.~n")
  (define gl  (new paddle-canvas%
                   [parent win]))
 
  (send win show #t)
  
  (define draw-thread
    (thread (λ ()
                
              (sleep 1)
              (collect-garbage)
              (let loop ()
                (go)
                (send gl on-paint)
                (tick)
                (loop)
                
                ))))

  (stop (λ () (map kill-thread (list draw-thread)))))