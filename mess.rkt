
#|

                #`(begin
                      #,@(for/list ([field (quote (list fields (... ...)))])
                           #`(begin
                               (define #,(format-id (datum->syntax #f field)
                                                    "get-~a"
                                                    (datum->syntax #f field))
                                 (case-lambda
                                   [()
                                    (hash-ref (agent-fields (current-agent))
                                              (quote field) (NoValueFound))]
                                   [(a)
                                    (hash-ref (agent-fields a)
                                              (quote field) (NoValueFound))]))
                               (define #,(format-id field "set-~a!" field)
                                 (case-lambda
                                   [(v)
                                    (hash-set! (agent-fields (current-agent))
                                               (quote field) v)]
                                   [(a v)
                                    (hash-set! (agent-fields a)
                                               (quote field) v)]))))
                  )
|#


;;;;;;
;; Should I assume we're always parameterizing
;; over the (current-agent) parameter? If so,
;; that would mean the user never provides the
;; agent argument in these functions.
(define-syntax (define-agent-set/get stx)
  (syntax-case stx ()
    [(_ field)
     (with-syntax ([get (format-id #'field "get-~a" #'field)]
                   [set (format-id #'field "set-~a!" #'field)])
       #`(begin
           (define get
             (case-lambda
               [()
                (hash-ref (agent-fields (current-agent))
                          (quote field) (NoValueFound))]
               [(a)
                (hash-ref (agent-fields a)
                          (quote field) (NoValueFound))]))
           (define set
             (case-lambda
               [(v)
                (hash-set! (agent-fields (current-agent))
                           (quote field) v)]
               [(a v)
                (hash-set! (agent-fields a)
                           (quote field) v)]))
           ))]))

(define-agent-set/get id)
(define-agent-set/get breed)
(define-agent-set/get x)
(define-agent-set/get y)
(define-agent-set/get direction)
(define-agent-set/get color)
(define-agent-set/get world-rows)
(define-agent-set/get world-cols)

(define (offset x y direction magnitude)
  (define dir (* (+ direction 90) pi-conv))
  (define dy (* magnitude (sin dir)))
  (define dx (* magnitude (cos dir)))
  (values (+ x dx) (+ y dy)))

(define (wrap v edge)
  (cond
    [(> v edge) 0]
    [(<= v 0) edge]
    [else v]))

(define (move magnitude)
  (define direction (get-direction (current-agent)))
  (define-values (new-x new-y)
    (offset (get-x (current-agent))
            (get-y (current-agent))
            direction magnitude))
  (set-x! (current-agent)
          (wrap new-x (get-world-rows (current-agent))))
  (set-y! (current-agent)
          (wrap new-y (get-world-cols (current-agent))))
  )

(define (right d)
  (set-direction! (current-agent) (+ (get-direction (current-agent)) d)))

(define (left d)
  (set-direction! (current-agent) (- (get-direction (current-agent)) d)))
