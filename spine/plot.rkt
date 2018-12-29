#lang racket

(provide (all-defined-out))
(require "state.rkt"
         racket/gui
         plot
         db)

(define plot-details (make-hash))

(define save-frame%
  (class frame%
    (define/override (on-size w h)
      (printf "Resized~n")
      (hash-set! plot-details
                 'bitmap
                 (new bitmap-dc% [bitmap (make-object bitmap% w h)])))
    (super-new)))

(define save-canvas%
  (class canvas%
    (define/override (on-event mouse-evt)
      (when (symbol=? (send mouse-evt get-event-type) 'right-down)
        (send (send (hash-ref plot-details 'bitmap) get-bitmap)
              save-file
              (format "~a-~a.png"
                      (hash-ref plot-details 'filename)
                      (ticker))
              'png))
      )
    (super-new)))
      
(define (intersperse ls o)
  (cond
    [(empty? ls) empty]
    [(empty? (rest ls)) (list (first ls))]
    [else
     (cons (first ls)
           (cons o (intersperse (rest ls) o)))]))

;; http://tools.medialab.sciences-po.fr/iwanthue/
(define distinct-colors
  (map (λ (ls)
         (make-object color% (first ls) (second ls) (third ls)))
       '([221 148 51] [100 99 211] [138 188 57] [176 95 211] 
                      [85 199 95] [204 77 172] [66 147 45] [225 74 134] 
                      [88 171 106] [206 62 70] [92 205 169] [211 89 45] 
                      [85 136 229] [192 180 55] [138 82 156] [109 135 42] 
                      [149 153 222] [175 145 62] [83 104 167] [164 182 107] 
                      [167 62 102] [52 118 67] [215 136 192] [59 158 139] 
                      [209 113 111] [72 177 218] [153 92 44] [139 74 95] 
                      [107 108 44] [223 155 110])))

(define-syntax-rule (create-plot db-tags ...)
  (let ([W 400]
        [H 200]
        )
    (hash-set! plot-details
               'filename
               (apply string-append
                      (intersperse
                       (map (λ (o)
                              (format "~a" o))
                            (quote (db-tags ...))) "_")))
    (define (plot-thread)
      (thread (λ ()
                (define f (new save-frame%
                               [label "plot"]
                               [width W]
                               [height H]))
                (send f show true)
                (define c (new save-canvas% [parent f]))
                (define dc (send c get-dc))
                (hash-set! plot-details
                           'bitmap
                           (new bitmap-dc%
                                [bitmap (make-object bitmap% W H)]))
                (define conns (for/list ([tag (quote (db-tags ...))])
                                (hash-ref log-conns tag)))
                (define val-max (make-parameter -10000))
                (define val-min (make-parameter  10000))
                (define data-exists? false)
                
                (let loop ()
                  (sleep 1)
                  
                  (define the-lines
                    (for/list ([conn conns]
                               [color-ndx  (range (length conns))])
                      
                      (define rows (query-rows conn "select * from counts"))
                      
                      (define the-data
                        (cond
                          [(> (length rows) 0)
                           ;; (printf "Plotting ~a rows~n" (length rows))
                           (define mx (query-value conn "select max(value) from counts"))
                           (when (< (val-max) mx)
                             (val-max mx))
                           
                           (define mn (query-value conn "select min(value) from counts"))
                           (when (> (val-min) mn)
                             (val-min mn))
                           
                           (define data
                             (for/vector ([row rows])
                               (vector (vector-ref row 1) (vector-ref row 2))))
                           (set! data-exists? true)
                           data]
                          [else '()]))
                      
                      (lines the-data 
                             #:color (list-ref distinct-colors
                                               (modulo color-ndx (length distinct-colors))))
                      ))
                  ;; (printf "LINES: ~a~n" the-lines)
                  (when data-exists?
                    (plot/dc the-lines
                             (hash-ref plot-details 'bitmap)
                             0 0
                             (send f get-width)
                             (send f get-height)
                             #:x-label "Ticks"
                             #:y-label "Turtles"
                             #:y-max (* (val-max) 1.1)
                             #:y-min (* (val-min) 1.1)
                             )
                    (send dc draw-bitmap (send (hash-ref plot-details 'bitmap) get-bitmap) 0 0)
                    #;(plot/dc the-lines
                             dc 0 0
                             (send f get-width)
                             (send f get-height)
                             #:x-label "Ticks"
                             #:y-label "Turtles"
                             #:y-max (* (val-max) 1.1)
                             #:y-min (* (val-min) 1.1)
                             )
                    )
                  'pass
                  (loop)
                  ))))
    (add-thread-to-kill! (plot-thread))
    ))