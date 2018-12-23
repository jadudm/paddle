#lang racket
(require (except-in racket/gui set)
         sgl/gl)

(require "backing.rkt"
         "patches.rkt"
         "types.rkt"
         "get-set.rkt"
         "patches.rkt"
         "interface.rkt"
         "state.rkt"
         )

(provide (all-defined-out))

(define flag-ch (make-channel))

(define make-world
  (case-lambda
    [(cols rows width height)
     (set global world-cols cols)
     (set global world-rows rows)
     (set global frame-width width)
     (set global frame-height height)
     ;; For tracking agent locations.
     (setup-backing-world! cols rows)
     (create-patches cols rows)
     (printf "Done making world.~n")]
    [(world)
     (make-world world world 600 600)]
    [(world frame)
     (make-world world world frame frame)]))
     
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
      (define pcolor (get patch pcolor))
      (define r (rgb-color-r pcolor))
      (define g (rgb-color-g pcolor))
      (define b (rgb-color-b pcolor))
        
      (glColor3ub r g b)
      (glVertex3f (+ 0 (* side row)) (+ 0 (* col side)) -0.1)
      (glVertex3f (+ side (* side row)) (+ 0 (* col side)) -0.1)
      (glVertex3f (+ side (* side row)) (+ side (* col side)) -0.1)
      (glVertex3f (+ 0 (* side row)) (+ side (* col side)) -0.1)
      (glEnd)
      )
    ))

(define (draw-agents)  
  (for ([(plural as) agentsets])
    (when (not (member plural '(patches dirty-patches)))
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

(define (world-draw)
  (setup-gl-draw)
  (draw-patches)
  (draw-agents)
  ;; (draw-grid)
  )

(draw-opengl world-draw)





(define threads-to-kill '())
(define (add-thread-to-kill! t)
  (set! threads-to-kill (cons t threads-to-kill)))

(define (run-world setup go
                   #:interface [interface false])
  
  (printf "Building frame.~n")
  

  (thread (λ ()
            (printf "Running setup.~n")

            ;; Setup the interface first, if it exists.
            (when interface
              (interface))
            
            (setup)
            (collect-garbage 'major)
            (printf "Done with setup.~n")
            
            (channel-put flag-ch true)))
  
  (define draw-thread
    (thread (λ ()
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
