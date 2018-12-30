#lang racket

(provide (contract-out
          [make-world                    (case->
                                          (-> number? number? any)
                                          (-> number? number? number? number? any))]
          [run-world                     (-> procedure? procedure? any)]
          [draw-world                    (-> any)]
          ))

;; Libraries in the Racket distribution
(require (except-in racket/gui set)
         sgl/gl)

;; Libraries from Paddle.
(require "agents.rkt"
         "agentsets.rkt"
         "gui.rkt"
         "quadtree.rkt"
         "state.rkt"
         "get-set.rkt"
         "types.rkt")

;; ------------------------------------------------------------
;; EXTERNAL

;; Sets the global parameters for the world.
;; These parameters are used by the drawing functions
;; and patch creation. 
(define make-world
  (case-lambda
    [(cols pixels)
     ;; (printf "Making world c ~a p ~a~n" cols pixels)
     (set-global! 'world-columns cols)
     (set-global! 'world-rows cols)
     (set-global! 'frame-width pixels)
     (set-global! 'frame-height pixels)
     (set-global! 'ticker 0)
     ]
    [(cols rows width height)
     (set-global! 'world-columns cols)
     (set-global! 'world-rows rows)
     (set-global! 'frame-width width)
     (set-global! 'frame-height height)
     (set-global! 'ticker 0)]
    ))

;; RUN-WORLD
;; This runs everything.
(define (run-world user-setup
                   user-go
                   #:interface [interface false])

    (define-values (frame canvas)
      (world-setup user-setup))
  
  ;; This actually spawns the draw thread...
  (add-thread-to-kill! (draw-thread frame canvas user-go))
  (stop (λ ()
          (map kill-thread threads-to-kill)
          ;; Clean up globals
          (clear-global-structures)
          ;; Collect garbage.
          (collect-garbage 'major)
          )))

;; ----------------------------------------------------------------
;; INTERNAL

(define (make-frame)
  (define-values (screen-x screen-y)
    (get-display-size))
  (new paddle-frame%
       ;; Child field
       ;; stop is a parameter...
       ;; Parent fields
       [label "paddle gl"]
       [min-width  (get-global 'frame-width)]
       [min-height (get-global 'frame-height)]
       [x (- screen-x (get-global 'frame-width) 50)]
       [y 50]
       ))

(define (make-canvas win)
  (new paddle-canvas%
       ;; Child fields
       ;; [stop stop]
       ;; Parent fields
       [world-draw world-draw]
       [parent win]))

(define draw-world (make-parameter false))

(define (draw-thread win gl go)
    (thread (λ ()              
              (let loop ()
                (build-quadtree)
                (go)
                (send gl on-paint)
                (world-tick)
                ;; Rinse and repeat
                (loop)
                ))))

(define (world-setup user-setup)
  ;; Initialize the global variables table with
  ;; some reasonable defaults... if there aren't any
  ;; values there, that is.
  (when (or (not (get-global 'world-columns))
            (not (get-global 'frame-width)))
    (global-defaults))  
  (define win (make-frame))
  (define gl  (make-canvas win))
  (draw-world (thunk (send gl on-paint)))
  (user-setup)
  (collect-garbage 'major)
  (send win show #t)
  (send gl on-paint)
  (world-tick)
  (values win gl))

;; ------------------------------------------------------
;; GL DRAWING FUNCTIONS

(define (world-draw)
  (setup-gl-draw)
  (draw-patches)
  (draw-agents)
  )

(define (setup-gl-draw)
  (define rows (get-global 'world-rows))
  (define cols (get-global 'world-columns))
  ;; (printf "gl: ~ax~a~n" rows cols)
  
  (glClearColor 0.0 0.0 0.0 0.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

  ;;(glClearDepth 1.0)
  (glDepthFunc GL_LEQUAL)
  (glEnable GL_DEPTH_TEST)
  
  (glShadeModel GL_SMOOTH)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  ;; glOrtho(0.0, 50.0, 0.0, 50.0, -1.0, 1.0);
  (glOrtho 0.0 rows 0.0 cols -1.0 1.0)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity))

(define (draw-agents)  
  (for ([(plural as) agentsets])
    (when (not (member plural '(patches dirty-patches)))
      ;; FIXME
      ;; This is a leaky abstraction from the agentsets.
      (for ([critter (agentset->list (get-agentset plural))])
        (when critter
          (parameterize ([current-agent critter])
            ((get-agentset-meta plural 'default-drawing-function) critter)
            ))))
    ))

(define (get-patch-coordinate pid)
  (values (quotient  pid (get-global 'world-columns))
          (remainder pid (get-global 'world-rows))))

(define (draw-patches)
  (define side 1)
  ;;(printf "Trying to draw patches.~n")
  ;; I need to go through and draw the patches that are dirty.
  ;; These are in a vector.
  (for ([pid (get-dirty-patch-ids)])
    (current-patch pid)
    (define-values (coordinate-x coordinate-y)
      (get-patch-coordinate pid))
    (define col coordinate-x)
    (define row coordinate-y)
    (begin
      (glBegin GL_QUADS)
      (define pcolor (get patch color))
      (define r (color-red pcolor))
      (define g (color-green pcolor))
      (define b (color-blue pcolor))
        
      (glColor3ub r g b)
      (glVertex3f (+ 0 (* side row)) (+ 0 (* col side)) -0.1)
      (glVertex3f (+ side (* side row)) (+ 0 (* col side)) -0.1)
      (glVertex3f (+ side (* side row)) (+ side (* col side)) -0.1)
      (glVertex3f (+ 0 (* side row)) (+ side (* col side)) -0.1)
      (glEnd)
      )
    ))