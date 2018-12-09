#lang racket

;; How to synchronize many threads over multiple phases.
;; In CSP/occam, I'd use a barrier.

(define breeds (make-hash))
(define agent-threads (make-hash))

(define agent%
  (class object%
    (init-field breed
                id
                [cmd-ch (make-channel)]
                [rsp-ch (make-channel)])
    
    (define/public (go)
      (let loop ()
        (case (channel-get cmd-ch)
          ['move (to-move rsp-ch)]
          ['sniff (to-sniff rsp-ch)])
        (loop)))
    
    (define (to-move resp)
      (printf "m~n")
      (flush-output)
      (channel-put rsp-ch 'done))
    
    (define (to-sniff resp)
      (printf "s~n")
      (flush-output)
      (channel-put resp 'done))

    (define/public (get-breed)
      breed)

    (super-new)
    ))

(require (for-syntax racket/syntax))

(define-syntax (make-breed stx)
  (syntax-case stx ()
    [(_ breed)
     (with-syntax ([breed-pred? (format-id #'breed "~a?" #'breed)])
       #`(begin
           (define (breed-pred? o)
             (and (object? o)
                  (is-a? o agent%)
                  (equal? (send o get-breed) (quote breed))))
           ))]))

                         
                         



      

;; Blocking channels would do it.
(define (make-turtles n)
  (define cmd-channels  (map (λ (n) (make-channel)) (range n)))
  (define resp-channels (map (λ (n) (make-channel)) (range n)))
  (define ids (range n))

  
  #;(define turtle-threads
    (map (λ (id cmd resp)
           (thread (λ () (send (new turtle%
                                    [id id]
                                    [breed 'turtle])
                               go))))
         (map (λ (n) (string->symbol (format "t~a" n))) ids)
         cmd-channels
         resp-channels
         ))

  (for-each (λ (ch) (channel-put ch 'sniff)) cmd-channels)
  (map (λ (ch)
         (channel-get ch)) resp-channels)
  (printf "-----~n")
  (for-each (λ (ch) (channel-put ch 'move)) cmd-channels)
  (map (λ (ch)
         (channel-get ch)) resp-channels)


  )
