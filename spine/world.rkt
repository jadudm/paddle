#lang racket

(provide (all-defined-out))
(require (except-in racket/gui set)
         sgl/gl)
(require "state.rkt"
         "agentsets.rkt"
         "agents.rkt"
         "get-set.rkt"
         )

(define (make-world cols pixels)
  (set-global! 'world-columns cols)
  (set-global! 'world-rows cols)
  (set-global! 'frame-width pixels)
  (set-global! 'frame-height pixels))


(define threads-to-kill '())
(define (add-thread-to-kill! t)
  (set! threads-to-kill (cons t threads-to-kill)))
(define stop (make-parameter false))


(require "quadtree.rkt")
(define (build-quadtree)
  (define qt (new quadtree%
                  (boundary (make-rect 0 0
                                       (get-global 'world-columns)
                                       (get-global 'world-rows)))
                  (capacity 4)))
  
  (for ([(plural as) agentsets])
    (for ([a as])
      (when a
        ;; Don't insert ourselves.
        (send qt insert (make-point (vector-ref a agent-x)
                                    (vector-ref a agent-y)
                                    a)))))
  (current-quadtree qt)
  )
  

(define (run-world setup go
                   #:interface [interface false])
  
  (printf "Building frame.~n")
  (define-values (screen-x screen-y)
    (get-display-size))
  (define win (new paddle-frame%
                   [label "paddle gl"]
                   [min-width  (get-global 'frame-width)]
                   [min-height (get-global 'frame-height)]
                   [x (- screen-x (get-global 'frame-width) 50)]
                   [y 50]
                   ))
  
  (printf "Building canvas.~n")
  (define gl  (new paddle-canvas%
                   [parent win]))

  (printf "Running setup.~n")            
  (setup)
  (collect-garbage 'major)
  (printf "Done with setup.~n")
  
  (define (draw-thread)
    (thread (λ ()
              (send win show #t)
              (send gl on-paint)
              (tick)
              
              (let loop ()

                ;; (printf "About to run (go)~n")
                (build-quadtree)
                (go)
                (send gl on-paint)
                (tick)
                

                ;; Rinse and repeat
                (loop)
                ))))

  ;; This actually spawns the draw thread...
  
  (add-thread-to-kill! (draw-thread))
  (stop (λ ()
          (map kill-thread threads-to-kill)
          )))


(define (setup-gl-draw)
  (glClearColor 0.0 0.0 0.0 0.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

  ;;(glClearDepth 1.0)
  (glDepthFunc GL_LEQUAL)
  (glEnable GL_DEPTH_TEST)
  
  (glShadeModel GL_SMOOTH)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  ;; glOrtho(0.0, 50.0, 0.0, 50.0, -1.0, 1.0);
  (glOrtho 0.0 (get-global 'world-rows) 0.0 (get-global 'world-columns) -1.0 1.0)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity))


(define (draw-agents)  
  (for ([(plural as) agentsets])
    (when (not (member plural '(patches dirty-patches)))
      (for ([critter (get-agentset plural)])
        (when critter
          (parameterize ([current-agent critter])
            ((get-agentset-meta plural 'default-drawing-function) critter)
            ))))
    ))

(define (world-draw)
  (setup-gl-draw)
  ;; (draw-patches)
  (draw-agents)
  )

(define paddle-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    (define/override (on-paint)
      (with-gl-context (λ ()
                         (world-draw)
                         (swap-gl-buffers)
                         )))
    (define/override (on-size width height)
      (with-gl-context (λ() (glViewport 0 0 width height) (on-paint))))
    (super-instantiate () (style '(gl)))))

(define paddle-frame%
  (class frame%
    (define/augment (on-close)
      (printf "The sky is falling!~n")
      (send this show false)
      ((stop)))
    (super-new)))