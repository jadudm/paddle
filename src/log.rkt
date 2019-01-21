#lang racket
(provide create-log log get-log log-conns)

(require db sql
         "netlogo.rkt"
         "agents.rkt"
         "state.rkt")

(define-syntax-rule (create-log log-tag . fields)
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
      (hash-set! log-conns (string->symbol (format "~a-fields" (quote log-tag))) (quote fields))
      (define field-string (apply string-append
                                  (add-between (map (λ (s) (format "~a number" s)) (quote fields)) ", ")))
      (define qstring
        (format "create table ~a (ndx integer primary key, tick integer ~a"
                (quote log-tag)
                (if (> (string-length field-string) 0)
                    (format ", ~a)" field-string)
                    ")")
                ))
      ;;(printf "qstring: ~a~n" qstring)
      (query-exec conn qstring)
      )))

(define (get-log tag)
  (hash-ref log-conns tag))

(define (count-agents as)
  (define count 0)
  (for ([agent (get-agents as)])
    (when agent
      (set! count (add1 count))))
  count)

(define-syntax (log stx)
  (syntax-case stx ()
    [(log tag as values ...)
     #`(let ()
         (define fields (hash-ref log-conns (string->symbol (format "~a-fields" (quote tag)))))
         (define field-string
           (apply string-append
                  (add-between (map (λ (s) (format "~a" s)) fields) ", ")))
         ;;(printf "fs: ~a~n" field-string)
         
         ;(for-each (λ (v) (printf "v: ~a~n" v)) (list values ...))

         (for ([agent as])
           (parameterize ([current-agent agent])
             (define value-string
               (apply string-append
                      (add-between (map (λ (o) (format "~a" o)) (list values ...)) ", ")))
             ;;(printf "vs: ~a~n" value-string)
             
             (define insert-string
               (format "insert into ~a (tick ~a) values (~a ~a)"
                       (quote tag)
                       (if (> (string-length field-string) 0)
                           (string-append ", " field-string) "")
                       (ticker)
                       (if (> (string-length value-string) 0)
                           (string-append ", " value-string) "")
                       ))
             ;;(printf "is: ~a~n" insert-string)
             
             (query-exec
              (hash-ref log-conns (quote tag))
              insert-string)))
         )]))

(module+ test
  (require "netlogo.rkt"
           "agents.rkt"
           "get-set.rkt"
           "breeds.rkt"
           "state.rkt"
           "world.rkt")

  (make-world 100 100)
  (create-breed turtle turtles)
  (create turtles 200)
  
  (create-log turtles count)

  (for ([tick (range 100)])
    (world-tick)
    (define set (where turtles (< (get turtle-id) 20)))
    (log turtles set
         (count-agents set)
         ))
  )