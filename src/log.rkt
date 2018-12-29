#lang racket
(provide create-log log get-log log-conns)

(require db sql
         "netlogo.rkt"
         "agents.rkt"
         "state.rkt")

(define-syntax-rule (create-log log-tag)
  (begin
    (define fname (format "~a.sqlite" (quote log-tag)))
    ;; Make sure it is an SQLite DB file.
    (when (and (file-exists? fname)
               (regexp-match "sqlite$" fname))
      (delete-file fname))
    ;; Create an empty file.
    (close-output-port (open-output-file fname))
    
    (let ([conn (sqlite3-connect #:database (format "~a.sqlite"
                                                    (quote log-tag)
                                                    ))])
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