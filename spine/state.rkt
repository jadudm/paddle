#lang racket

(provide (all-defined-out))
(require "quadtree.rkt"
         "types.rkt")

;; MAX AGENTS
;; I'm going to cap the number of agents that can be created
;; at any given time. This is both a conservative number as
;; well as a performance-inspired number.
(define DEFAULT-AGENTSET-SIZE 1000)
(define MAX-AGENTSET-SIZE 8000)

;; AGENTSETS
;; plural-symbol -> vector
;; eg.
;; 'turtles -> (vector (vector ...) ...)
(define agentsets (make-hash))
;; Metadata is two deep. Every breed has its own metadata.
;; plural -> (hash (key value) ...)
(define agentsets-meta (make-hash))

;; For setting and getting the agentset vectors.
(define-values (get-agentset set-agentset!)
  (values
   (λ (k)   (hash-ref  agentsets k false))
   (λ (k v) (hash-set! agentsets k v))))

;; For setting and getting metadata about a given breed.
(define-values (get-agentset-meta set-agentset-meta!)
  (values
   (λ (plural k)   (hash-ref  (hash-ref agentsets-meta plural) k false))
   (λ (plural k v) (hash-set! (hash-ref agentsets-meta plural) k v))))

;; Initialize the metadata. Speciically, create an empty
;; hash table for a new breed. Used in the create-breed macro.
(define (init-meta plural)
  (hash-set! agentsets-meta plural (make-hash)))

(define-values (get-max set-max!)
  (values
   (λ (plural)   (get-agentset-meta  plural 'max-agents))
   (λ (plural n) (set-agentset-meta! plural 'max-agents n))))
   

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
(define (global-defaults)
  (printf "Setting global defaults.~n")
  (set-global! 'world-columns 20)
  (set-global! 'world-rows    20)
  (set-global! 'frame-width  400)
  (set-global! 'frame-height 400)
  (set-global! 'ticker 0))

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

;; CLEANING UP
;; Is there a way to get the garbage collector to free everything when I
;; stop a world? I'll try nuking the globals. Hopefully, the GC can then
;; free up everything they're referencing. Currently, I have a lot of things
;; hanging around in memory, even after killing the execution threads.
(define (clear-global-structures)
  (printf "Cleaning up.~n")
  ;; Wipe the globals table
  (set! globals (make-hash))
  (global-defaults)
  ;; Clear the parameters
  (current-agent false)
  (current-patch false)
  (current-agentset false)
  (current-quadtree false)
  ;; Clear dirty bits for the patches
  (set! dirty-bits (make-hash))
  ;; Wipe all the agentsets
  (set! agentsets (make-hash))
  (set! agentsets-meta (make-hash))
  )


;; QUADTREE
;; This gets touched from multiple places.
;; Used to build a quadtree of all of the agents on every world draw.
;; FIXME
;; https://bitbucket.org/jadudm/paddle/issues/8/should-quadtree-building-be-always-or
(define (build-quadtree)
  (define qt (new quadtree%
                  (boundary (make-rect 0 0
                                       (get-global 'world-columns)
                                       (get-global 'world-rows)))
                  (capacity 4)))
  ;; FIXME
  ;; https://bitbucket.org/jadudm/paddle/issues/6/quadtree-has-not-been-tested-with-multiple
  (for ([(plural as) agentsets])
    ;; FIXME
    ;; Leaky abstraction from agentsets.
    (for ([(id a) as])
      (when a
        ;; Don't insert ourselves.
        (send qt insert (make-point (vector-ref a agent-x)
                                    (vector-ref a agent-y)
                                    a)))))
  (current-quadtree qt)
  )
  
