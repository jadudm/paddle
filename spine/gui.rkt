#lang racket

(provide paddle-canvas% paddle-frame%)

(require racket/gui
         sgl/gl)

(require "state.rkt")

(define paddle-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    (init-field world-draw)
    
    (define/override (on-paint)
      (with-gl-context (λ ()
                         (world-draw)
                         (swap-gl-buffers)
                         )))
    (define/override (on-size width height)
      (with-gl-context (λ() (glViewport 0 0 width height) (on-paint))))
    (super-instantiate () (style '(gl)))))

;; ------------------------------------------------------
;; GUI-BASE
;; FIXME
;; This should probably be in its own module, so that I can
;; use these frames in other parts of the system. For example,
;; the interface builder needs these frames as well.
(define paddle-frame%
  (class frame%
    ;; (init-field stop)
    
    (define/augment (on-close)
      (printf "The sky is falling!~n")
      ;; Hide the window
      (send this show false)
      ;; Kill the execution threads
      ((stop))
      )
    (super-new)))