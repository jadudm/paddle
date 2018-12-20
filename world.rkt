#lang racket
(require (except-in racket/gui set)
         sgl/gl)

(require "base.rkt"
         "backing.rkt"
         "patches.rkt"
         "types.rkt"
         "agentsets.rkt"
         )

(define flag-ch (make-channel))

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
  (setup-backing-world! cols rows)
  (create-patches cols rows)
  (printf "Done making world.~n")
  #;(printf "Precompute sniffs to distance 6.~n")
  #;(for ([dist (range 1 6)])
    (printf "\tdist: ~a~n" dist)
    (for ([x (range cols)])
      (for ([y (range rows)])
        (sniff 'preload x y dist))))
  )

(define (draw-patches)
  (define side 1)
  ;;(printf "Trying to draw patches.~n")
  ;; I need to go through and draw the patches that are dirty.
  ;; These are in a vector.
  (for ([pid (get-dirty-patch-ids)])
    (current-patch pid)
    (define coord (get-patch-coordinate pid))
    (define col (coordinate-x coord))
    (define row (coordinate-y coord))
    (begin
      (glBegin GL_QUADS)
      (define r (rgb-color-r (get patch pcolor)))
      (define g (rgb-color-g (get patch pcolor)))
      (define b (rgb-color-b (get patch pcolor)))
      ;;(printf "draw px ~a py ~a~n" col row)
      ;;(sleep 0)
        
      (glColor3ub r g b)
      ;; (glTranslatef col row 0)
      (glVertex3f (+ 0 (* side row)) (+ 0 (* col side)) -0.1)
      (glVertex3f (+ side (* side row)) (+ 0 (* col side)) -0.1)
      (glVertex3f (+ side (* side row)) (+ side (* col side)) -0.1)
      (glVertex3f (+ 0 (* side row)) (+ side (* col side)) -0.1)
      ;; (glTranslatef (- col) (- row) 0)
      (glEnd)
      )
    ))
(define (draw-agents)
  
  (for ([(plural as) agentsets])
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

(define (draw-opengl)
  (setup-gl-draw)
  (draw-patches)
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

  (thread (λ ()
            (printf "Running setup.~n")
            (setup)
            (collect-garbage 'major)
            (printf "Done with setup.~n")
            
            (channel-put flag-ch true)))
  
  (define draw-thread
    (thread (λ ()
              ;; (sleep 1)
              (channel-get flag-ch)
              
              (send win show #t)
              (send gl on-paint)
              (tick)
              
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

                ;; (printf "About to run (go)~n")
                (go)
                (send gl on-paint)
                (tick)
                

                ;; Rinse and repeat
                (loop)
                ))))

  ;; This actually spawns the draw thread...
  
  (add-thread-to-kill! draw-thread)
  (stop (λ ()
          (map kill-thread threads-to-kill)
          )))
