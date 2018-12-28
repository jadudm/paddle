#lang racket

(provide (all-defined-out))

;; MAX AGENTS
;; I'm going to cap the number of agents that can be created
;; at any given time. This is both a conservative number as
;; well as a performance-inspired number.
(define DEFAULT-AGENTSET-SIZE 5000)
(define (set-max-agents! n)
  (set! DEFAULT-AGENTSET-SIZE n))

;; STATE PARAMETERS
;; There are several macros that pass state through these parameters.
;; For example, (ask ...) sets the (current-agent) as a matter of course.
;; This makes it easier to then implement (get ...) and (set ...), as they
;; operate on the current agent.
(define current-agent    (make-parameter false))
(define current-patch    (make-parameter false))
(define current-agentset (make-parameter false))
(define current-quadtree (make-parameter false))

;; GLOBALS
;; I am confident there is a better way. These parameters have
;; defaults (below), and are for things like the world dimensions.
;; I have tried not to abuse the globals hash too much.
(define globals (make-hash))
(define-values (get-global set-global!)
  (values
   (λ (k) (hash-ref globals k false))
   (λ (k v) (hash-set! globals k v))))

;; DEFAULT GLOBALS
;; There's a few values that are good to have in place at world
;; startup. These seem to be the set of necessary globals.
(set-global! 'world-columns 20)
(set-global! 'world-rows    20)
(set-global! 'frame-width  400)
(set-global! 'frame-height 400)
(set-global! 'ticker 0)

;; MAPPING COORDINATES TO PATCH-SPACE
;; The patches are a linear vector. Given an [x,y] coordinate,
;; this function maps you through to the vector index.
(define (->pid x y)
  (+ (* (get-global 'world-rows) y) x))
;; DIRTY PATCHES
;; I only draw patches that are dirty. Tracking dirty state happens
;; in a hash-table, for lookup speed.
(define dirty-bits (make-hash))

;; WORLD TICKER
;; I've moved this from a parameter to a global. I found that
;; the parameter was not updating correctly in all places.
(define ticker (thunk (get-global 'ticker)))
(define tick-pause (make-parameter (/ 1 60)))
(define (world-tick)
  (set-global! 'ticker (add1 (get-global 'ticker))))

;; DB LOGGING CONNECTIONS
;; When you create a log, it sticks a connection object
;; into this hash table. When you log something, the connection
;; is used for the insert. 
(define log-conns (make-hash))

;; GUI THREADS
;; The GUIs all live on threads, and those should be killed
;; when one of the GUI elements is killed.
(define threads-to-kill '())
(define (add-thread-to-kill! t)
  (set! threads-to-kill (cons t threads-to-kill)))
(define stop (make-parameter false))