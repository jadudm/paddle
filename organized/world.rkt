#lang racket
(require (except-in racket/gui set)
         sgl/gl)

(require "base.rkt"
         "backing.rkt"
         )

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
  (set global frame-height height)
  ;; For tracking agent locations.
  (setup-backing-world! rows cols))

(define (draw-agents)
  ;;(printf "(draw-agents)~n")
  (for ([(plural as) agentsets])
    
    ;; Draw the patches first.
    (when (equal? plural 'dirty-patches)
      ;; (printf "Drawing patches.~n")
      (for ([(id patch) (agentset-agents as)])
        (parameterize ([current-agent patch]
                       [current-patch patch])
          ;; FIXME The alternative is to go through all patches
          ;; and decide if they're dirty. Instead, I've instrumented
          ;; (set patch ...) so that it tracks dirtyness, and moves dirty patches
          ;; into the dirty-patch breed.
          #;(printf "dirty id ~a pid ~a px ~a py ~a~n"
                    (get id) (get patch-id)
                    (get pxcor) (get pycor))
          ((get draw))
          )))

    ;;(glClear GL_DEPTH_BUFFER_BIT)
    ;; Draw the agents second
    (when (not (member plural '(patches dirty-patches)))
      ;; (printf "Drawing agents.~n")
      (for ([(id critter) (agentset-agents as)])
        (parameterize ([current-agent critter])
          ((get draw))
          )))

    
    
    ))

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
  (glOrtho 0.0 (get global world-rows) 0.0 (get global world-cols) -1.0 1.0)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity))

(define (draw-grid)
  (define side 1)
  (for ([row (get global world-rows)])
    (for ([col (get global world-cols)])
       'pass
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
      (send this show false)
      ((stop)))
    (super-new)))

(define stop (make-parameter false))

(define threads-to-kill '())
(define (add-thread-to-kill! t)
  (set! threads-to-kill (cons t threads-to-kill)))

(define (run-world setup go)
  (printf "Running setup.~n")
  (setup)
  (printf "Done with setup.~n")
  
  (printf "Building frame.~n")
  (define-values (screen-x screen-y)
    (get-display-size))
  
  (define win (new paddle-frame%
                   [label "paddle gl"]
                   [min-width  (get global frame-width)]
                   [min-height (get global frame-height)]
                   [x (- screen-x (get global frame-width) 50)]
                   [y 50]
                   ))

  (printf "Building canvas.~n")
  (define gl  (new paddle-canvas%
                   [parent win]))
 
  (send win show #t)
  
  (collect-garbage 'major)
  
  (define draw-thread
    (λ ()
      (thread (λ ()
              ;; (sleep 1)

              (let loop ()
                ;; Update turtles every third tick, if the interface is dirty.
                (when interface-dirty?
                  ;;(printf "ivd~n")
                  (set-interface-dirty! false)
                  (for ([(var ivs) interface-values])
                    ;; For each agent in the agentset given
                    (for ([(id agent) (agentset-agents ((iv-agentset ivs)))])
                      ;; Set the variables in those agents.
                      (hash-set! (agent-fields agent)
                                 (iv-agent-variable ivs)
                                 (iv-value ivs)))))
                
                (go)
                (send gl on-paint)
                (tick)
                

                ;; Rinse and repeat
                (loop)
                )))))

  ;; This actually spawns the draw thread...
  (sleep 0.5)
  (add-thread-to-kill! (draw-thread))
  (stop (λ ()
          (map kill-thread threads-to-kill)
          )))
