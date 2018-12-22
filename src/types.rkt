#lang racket

(provide (all-defined-out))
(require sgl/gl (except-in racket/gui set))
(require "state.rkt")

;; A critical part of NetLogo is keeping track of all of the agents.
;; There are "default" agent sets, as well as new agent sets that the
;; user can introduce (eg. breed [fish fishes]). I need a way
;; to track all of these agent sets.
;;
;; agentsets are keyed by their plural.
(define agentsets (make-hash))

(define current-agent (make-parameter false))
(define current-patch (make-parameter false))


;; For this to work, I need individual agents.
;; They can all be the same. They'll keep their fields
;; in a hash. I debated calling that field "has", so it would be
;; (agent-has ...) as the accessor...
(struct agent (id breed fields) #:transparent)

;; An agentset should be a struct, so I know when I'm working with it.
(struct agentset (breed plural base-fields agents)
  #:transparent #:mutable)

(struct coordinate (x y) #:transparent)

(struct rgb-color (r g b) #:transparent)

(struct iv (agent-variable value agentset) #:transparent)
(struct slider (agentset var low high)
  #:transparent)


(define paddle-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    (define/override (on-paint)
      (with-gl-context (λ ()
                         ((draw-opengl))
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