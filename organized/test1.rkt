#lang racket

(require "base.rkt"
         "agentsets.rkt")

;; TESTING

;; There are some default agentsets.
;; 'turtles' is one. 'patches' is another. For simplicity, it might
;; have to be that all agentsets 
;; (define turtles (agentset 'turtle 'turtles empty (make-hash)))
"Creating turtles"
(create-breed turtle turtles)
;; (add-agentset turtles)
"Giving them attributes"
(give turtles boogers)
"Creating 5 turtles."
(create turtles 5)

"Let us see what we have wrought"
(turtles)

"Asking turtles to do something"
(ask (with turtles (< (agent-id (current-agent)) 3))
     (printf "~a~n" (agent-id (current-agent))))

(ask (with turtles (and (> (have id) 3)
                        (> (have direction) 0)
                        (< (have direction) 90)))
       (printf "id ~a ~a~n" (agent-get id) (agent-get direction)))


(ask (with turtles (> (have id) 3))
       (printf "id ~a~n" (agent-id (current-agent))))


;; Now I can ask if an agentset has some property.
;; I would like it if I didn't have to wrap agent variables up
;; in a syntax, but otherwise I have to introduce users to symbols...
;; perhaps I should. That would, though, require that I interpret
;; the boolean expressions, and expand symbols out to hash-refs.
(when (any? turtles (> (have id) 10))
  (printf "At least one turtle has an id greater than 10!~n"))

(when (all? turtles (> (have id) -1))
  (printf "ALL turtles have ids greater than -1!~n"))
(when (all? turtles (> (have id) 5))
  (printf "ALL turtles have ids greater than 5!~n"))