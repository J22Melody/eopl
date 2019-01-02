(define empty-stack
  (lambda ()
    '()
  )
)

(define empty-stack?
  (lambda (stk)
    (null? stk)
  )
)

(define push
  (lambda (stk x)
    (cons x stk)
  )
)

(define pop
  (lambda (stk)
    (cdr stk)
  )
)

(define top
  (lambda (stk)
    (car stk)
  )
)

(define s (push (push (empty-stack) 'a) 'b))
s
(empty-stack? s)
(top s)
(pop s)
