;;; Copyright 2018 Matt Jadud <matt@jadud.com>
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
;;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#lang racket/gui
(require "types.rkt"
         "base.rkt"
         "agentsets.rkt"
         "world.rkt")
(provide (all-defined-out))

(define (widgets . pieces)
  (define-values (screen-x screen-y)
    (get-display-size))

  (define frame-width 200)
  (define f (new paddle-frame%
                 [label "pokepoke"]
                 [width frame-width]
                 [height 400]
                 [x (- screen-x
                       (get global frame-width)
                       frame-width
                       50 50)]
                 [y 50]
                 ))
  (define vp (new vertical-pane%
                  [parent f]))
  
  (for ([p pieces])
    (match p
      [(struct slider (as var low high))
       ;; When I build the gui, give all the agents the new variable.
       (give* as var)
       (for ([(id agent) (agentset-agents (as))])
         (hash-set! (agent-fields agent)
                    var
                    low))
             
       ;; Instantiate
       (define s (new slider%
                      [label (format "~a" var)]
                      [min-value low]
                      [max-value high]
                      [parent vp]
                      [callback (λ (widget evt)
                                  (when (member (send evt get-event-type) '(slider))
                                    ;;(printf "Setting interface dirty bit.~n")
                                    (set-interface-dirty! true)
                                    ;; send back the var, value, and agentset
                                    (hash-set! interface-values
                                               var
                                               (iv var (send s get-value) as))))]
                      ))
       s]))
  
  (send f show true)

  )