#lang racket

;; 'ask' is a query language. It operates over agentsets.
;;
;; (ask <agentset> bodies ...)
;;
;; There are query operators, like
;;
;; (here <agentset>)
;;
;; which look for agents that are on the current patch. Or,
;;
;; (sniff <radius>)
;;
;; which return agents of the same breed within the given radius
;; from the current agent.
;;
;; So, all forms that live in <agentset> have the contract
;;
;; fun : agentset -> agentset

