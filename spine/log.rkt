#lang racket
(provide create-log log get-log)

(require db sql
         "netlogo.rkt"
         "agents.rkt"
         "state.rkt")


(define log-conns (make-hash))
(define-syntax-rule (create-log log-tag)
  (begin
    (let ([conn (sqlite3-connect #:database 'memory)])
      (hash-set! log-conns (quote log-tag) conn)
      (query-exec
       conn
       "create table counts (ndx integer primary key, tick integer, value integer)"))))

(define (get-log tag)
  (hash-ref log-conns tag))

(define-syntax-rule (log tag as)
  (let ([count 0])
    (for ([agent (get-agents as)])
      (parameterize ([current-agent agent])
        (when agent
          (set! count (add1 count)))))
    (query-exec
     (hash-ref log-conns (quote tag))
     (format "insert into counts (tick, value) values (~a, ~a)"
             (ticker)
             count))))

(module+ test
  (require "netlogo.rkt"
         "agents.rkt"
         "get-set.rkt"
         "breeds.rkt"
         "state.rkt")
  
  (create-breed turtle turtles)
  (create turtles 200)
  
  (create-log turtles)

  (for ([tick (range 100)])
    (ticker tick)
    (log turtles
         (where turtles (< (get turtle-id) 20))))
  )