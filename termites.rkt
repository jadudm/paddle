#lang racket

(require "base.rkt"
         "agentsets.rkt"
         "world.rkt"
         "interface.rkt"
         )

;; I must create the world first.
(make-world 10 10 300 300)
(create-patches)

(create-breed termite termites)
(create termites 2)
(give termites woodchip? no-pickup)


(define (setup)
  ;; Scatter the termites
  (ask termites
       (set xcor (random (get global world-cols)))
       (set ycor (random (get global world-rows)))
       (set direction (random 360))
       )
  
  (ask patches
       (when (> (random 100) 95)
         #;(printf "p ~a px ~a py ~a x ~a y ~a~n"
                   (get id) (get pxcor) (get pycor) (get xcor) (get ycor))
         (set patch pcolor (rgb 255 255 0))
         ))
  )

(define (coinflip)
  (> (random 100) 50))

(define (wiggle)
  (if (coinflip)
      (right (random 8))
      (left (random 8))))

(define (go)
  (wiggle)
  (move 0.5)

  
  (cond
    ;; When I have a woodchip, and I still can't pick
    ;; things up, decrement the time I can't pick things up.
    [(> (get no-pickup) 0)
     (set no-pickup (sub1 (get no-pickup)))]

    ;; When I'm allowed to pick things up,
    ;; and I don't have a woodchip, and
    ;; the patch is dirty...
    [(and (zero? (get no-pickup))
          (not (get woodchip?))
          (dirty? (patch-here)))
    ;; Say I have a woodchip
    (set woodchip? true)
    ;; Clear the patch
    (set patch pcolor (rgb 0 0 0))
    (set patch dirty? false)]

    ;; When I have a woodchip, and I find another woodchip,
    ;; drop the one I have...

    ;; ah, shit. sniff does not work with patches, I'll bet.
    [(and (any? (sniff patches (get patch dirty?)))

(run-world setup go)

