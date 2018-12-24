#lang racket

(provide make-world run-world stop)
(require (except-in racket/gui set)
         sgl/gl)

(require "state.rkt"
         "agents.rkt"
         "agentsets.rkt"
         )

(define paddle-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    (define/override (on-paint)
      (with-gl-context (λ ()
                         ;; (printf "on-paint~n")
                         (setup-gl-draw)
                         (draw-agents)
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
      (stop))
    (super-new)))


(define (setup-gl-draw)
  ;; (printf "setup-gl-draw~n")
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
    (define dfndx (index-of (agentset-fields as) 'draw))
    (when (not (member plural '(patches dirty-patches)))
      (for ([(id critter) (agentset-agents as)])
        (parameterize ([current-agent critter])
          (define fun (vector-ref critter dfndx))
          ;; (printf "drawing ~a ~a~n" (agent-breed (current-agent)) (agent-id    (current-agent)))
          ;; (printf "draw-ref: ~a~n" (draw-ref (current-agent)))
          (fun critter)
          )))
    ))

(define make-world
  (case-lambda
    [(world-dim frame-dim)
     (set-global! 'world-columns world-dim)
     (set-global! 'world-rows    world-dim)
     (set-global! 'frame-width   frame-dim)
     (set-global! 'frame-height  frame-dim)
     (printf "Done making world.~n")]
    [(world)
     (make-world world world 600 600)]
    ))

(define the-frame  (make-parameter false))
(define the-canvas (make-parameter false))

(define (make-frame/canvas)
  (define-values (screen-x screen-y)
    (get-display-size))
  (the-frame (new paddle-frame%
                  [label "paddle gl"]
                  [min-width  (get-global 'frame-width)]
                  [min-height (get-global 'frame-height)]
                  [x (- screen-x (get-global 'frame-width) 50)]
                  [y 50]
                  ))
  
  (printf "Building canvas.~n")
  (the-canvas  (new paddle-canvas%
                    [parent (the-frame)]))
  )

(define (draw-thread go)
  (thread (thunk
           (let loop ([f (the-frame)])
             (when (not f)
               (sleep 0.1)
               (loop (the-frame))))
           
           (printf "Showing the frame.~n")
           (send (the-frame) show true)
           (send (the-canvas) on-paint)
           (tick)
           
           (let loop ()
             ;; Update turtles every third tick, if the interface is dirty.
             #|
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
             |#

             (go)
             (send (the-canvas) on-paint)
             (tick)
             (loop)
             ))))

(define stop false)

(define (run-world setup go
                   #:interface [interface false])
  (make-frame/canvas)
  (when interface (interface))
  (setup)
  
  (define t:draw  (draw-thread go))
  (set! stop (thunk (map kill-thread (list t:draw))))
  )

  
    