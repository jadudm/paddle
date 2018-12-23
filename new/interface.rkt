#lang racket/gui
(require "agentsets.rkt"
         "state.rkt"
         )
(provide (all-defined-out))


(struct iv (agent-variable value agentset) #:transparent)
(struct slider (agentset var low high)
  #:transparent)

;; For getting values from the interface.
(define interface-values (make-hash))
(define interface-dirty? false)
(define (set-interface-dirty! v)
  (set! interface-dirty? v))

(define (widgets . pieces)
  (define-values (screen-x screen-y)
    (get-display-size))

  (define frame-width 200)
  (define f (new paddle-frame%
                 [label "pokepoke"]
                 [width frame-width]
                 [height 400]
                 [x (- screen-x
                       (get-global 'frame-width)
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