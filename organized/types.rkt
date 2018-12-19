#lang racket

(provide (all-defined-out))


;; For this to work, I need individual agents.
;; They can all be the same. They'll keep their fields
;; in a hash. I debated calling that field "has", so it would be
;; (agent-has ...) as the accessor...
(struct agent (id breed fields) #:transparent)
;; An agentset should be a struct, so I know when I'm working with it.
(struct agentset (breed plural base-fields agents)
  #:transparent #:mutable)


(struct rgb-color (r g b) #:transparent)

(struct iv (agent-variable value agentset) #:transparent)
(struct slider (agentset var low high)
  #:transparent)