#lang racket

(require db)
(require "state.rkt")

(provide create-table
         insert
         )

(struct table (conn fname clean-name name columns sanitizers))


(define (create-table name
                      #:if-exists [exists 'overwrite]
                      #:columns [columns empty]
                      #:sanitizers [sanitizers empty])

  ;; Clean the DB name
  (define cname (clean-name name))
  
  ;; Create an empty DB file.
  (define fname (create-empty-file cname exists))

  (define the-table
    (table (sqlite3-connect
            #:database fname)
           fname
           cname
           name
           columns
           sanitizers))
  ;; Create the table in the SQLite file
  (setup-sqlite the-table)

  the-table)

(define (->str o)
  (format "~a" o))

(define (clean-name str)
  (for ([sym '("/" "\\" "-" ":" ";" "\\." "," "\\+")])
    (set! str (regexp-replace* sym str "")))
  str)

(define (create-empty-file name exists)
  (define fname (format "~a-~a.sqlite" name (current-seconds)))
  ;; Make sure it is an SQLite DB file.
  (when (and (equal? exists 'overwrite)
             (file-exists? fname)
             (regexp-match "sqlite$" fname))
    (delete-file fname))
  ;; Create an empty file.
  (close-output-port (open-output-file fname))
  fname)

(define (setup-sqlite the-table)
  (define field-string (apply string-append
                              (add-between
                               (map (λ (s) (format "~a number" (clean-name (->str s))))
                                    (table-columns the-table)) ", ")))
  (define qstring
    (format "create table ~a (ndx integer primary key, tick integer ~a"
            (table-clean-name the-table)
            (if (> (string-length field-string) 0)
                (format ", ~a)" field-string)
                ")")
            ))
  ;;(printf "qstring: ~a~n" qstring)
  (query-exec (table-conn the-table) qstring))



;; FIXME
;; Should insert work on single rows, or sould
;; we pass a set that gets logged?
;; FIXME
;; Have multiple interfaces? What about one that
;; logs all the data from an agent/agentset?
(define (insert-into-table t vals)
  (cond
    ;; If we have the same number of columns...
    ;; FIXME
    ;; This is where sanitizers would come in.
    [(= (length vals) (length (table-columns t)))

     ;; Set up the fields portion of the string
     (define field-string
       (apply string-append
              (add-between (map (λ (s) (format "~a" s)) (table-columns t)) ", ")))

     ;; Set up the values
     (define value-string
       (apply string-append
              (add-between (map (λ (o) (format "~a" o)) vals) ", ")))
     ;;(printf "vs: ~a~n" value-string)
             
     (define insert-string
       (format "insert into ~a (tick ~a) values (~a ~a)"
               (table-clean-name t)
               (if (> (string-length field-string) 0)
                   (string-append ", " field-string) "")
               (ticker)
               (if (> (string-length value-string) 0)
                   (string-append ", " value-string) "")
               ))
     ;;(printf "is: ~a~n" insert-string)
             
     (query-exec (table-conn t) insert-string)
     ]
    ))

(define insert
  (match-lambda*
    ;; table values
    [(list (? table? t) vals ...)
     (insert-into-table t vals)
     ]
    ;; table cols values
    [(list (? table? t) (list (? string? s) ...) vals ...)
     '...]
    ))
    
         
