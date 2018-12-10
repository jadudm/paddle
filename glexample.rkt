 
#lang racket/gui
(require sgl/gl)
 
(define (resize w h)
  (glViewport 0 0 w h))

(define rows 40)
(define cols 40)

(define step-x 0.1)
(define step-y 0.1)

(define num-turtles 100)
(define turtle-xs (make-vector num-turtles 18))
(define turtle-ys (make-vector num-turtles 18))

(define (draw-opengl)
  (glClearColor 0.0 0.0 0.0 0.0)
  (glClear GL_COLOR_BUFFER_BIT)
 
  (glShadeModel GL_SMOOTH)
 
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  ;; glOrtho(0.0, 50.0, 0.0, 50.0, -1.0, 1.0);
  (glOrtho 0.0 rows 0.0 cols -1.0 1.0)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)

  ;; glVertex3f(-1.0f, -1.0f, 0.0f); // The bottom left corner  
  ;; glVertex3f(-1.0f, 1.0f, 0.0f); // The top left corner  
  ;; glVertex3f(1.0f, 1.0f, 0.0f); // The top right corner  
  ;; glVertex3f(1.0f, -1.0f, 0.0f); // The bottom right corner

  (define side 1)
  (for ([row rows])
    (for ([col cols])
      (glBegin GL_QUADS)
      (cond
        [(and (even? col) (even? row)) (glColor3f 0.2 0.2 0.2)]
        [(even? col) (glColor3f 1 1 1)]
        [(even? row) (glColor3f 0.4 0.4 0.4)]
        [else (glColor3f (/ (random 10) 10)
                         (/ (random 10) 10)
                         (/ (random 10) 10))])
      (glVertex3f (+ 0 (* side row)) (+ 0 (* col side)) 0)
      (glVertex3f (+ side (* side row)) (+ 0 (* col side)) 0)
      (glVertex3f (+ side (* side row)) (+ side (* col side)) 0)
      (glVertex3f (+ 0 (* side row)) (+ side (* col side)) 0)      
      (glEnd)))
  
  
  
  ;; (glVertex3f x y z)
  (for ([n num-turtles])
    (define turtle-x (vector-ref turtle-xs n))
    (define turtle-y (vector-ref turtle-ys n))
    (glBegin GL_TRIANGLES)
    (glColor3f (/ (random 10) 10) (/ (random 10) 10) (/ (random 10) 10))
    (glVertex3f turtle-x (+ (/ 1 2) turtle-y) 0)
    (glVertex3f (- turtle-x (/ 1 2)) (- turtle-y (/ 1 2)) 0)
    (glVertex3f (+ turtle-x (/ 1 2)) (- turtle-y (/ 1 2)) 0)
    (glEnd)
    ))
 
 
(define my-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    (define/override (on-paint)
      (with-gl-context (λ ()
                         (draw-opengl)
                         (swap-gl-buffers))))
    (define/override (on-size width height)
      (with-gl-context (λ() (resize width height) (on-paint))))
    (super-instantiate () (style '(gl)))))

(define win (new frame% [label "Racket Rosetta Code OpenGL example"]
                 [min-width 600] [min-height 600]))
(define gl  (new my-canvas% [parent win]))
 
(send win show #t)


(define (tick-thread ch)
  (thread (λ () (let loop ()
                  (channel-put ch 'ping)
                  ;; (sleep (/ 1 60))
                  (loop)))))

(define (pick-item ls)
  (list-ref ls (random (length ls))))

(define (update-thread ch)
  (thread (λ ()
            (let loop ()
              (channel-get ch)

              (for ([n num-turtles])
                ;; (printf "x ~a y ~a~n" turtle-x turtle-y)
                (define turtle-x (vector-ref turtle-xs n))
                (define turtle-y (vector-ref turtle-ys n))
                
                (set! turtle-x ((pick-item (list + -))
                                turtle-x
                                (* step-x (random 3))))
                (set! turtle-y ((pick-item (list + -))
                                turtle-y
                                (* step-y (random 3))))

                (when (> turtle-x rows)
                  (set! turtle-x 0))
                (when (> turtle-y cols )
                  (set! turtle-y 0))
                (vector-set! turtle-xs n turtle-x)
                (vector-set! turtle-ys n turtle-y))
                
              (send gl on-paint)
              (loop)
              ))))

(define (go)
  (define tick-ch (make-channel))
  (define tt (tick-thread tick-ch))
  (define ut (update-thread tick-ch))
  (λ ()
    (map kill-thread (list tt ut))))

(define (run secs)
  (define start (current-seconds))
  (define killer (go))
  (let loop ()
    (cond
      [(> (- (current-seconds) start) secs)
       (killer)]
      [else (loop)])))

(run 30)