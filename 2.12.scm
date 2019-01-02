(define empty-stack
  (lambda ()
    (lambda (cmd)
      (cond
        ((eqv? cmd 'empty-stack?) #t)
        ((eqv? cmd 'pop) #f)
        ((eqv? cmd 'top) #f)
      )
    )
  )
)

(define push
  (lambda (stk x)
    (lambda (cmd)
      (cond
        ((eqv? cmd 'empty-stack?) #f)
        ((eqv? cmd 'top) x)
        ((eqv? cmd 'pop) stk)
      )
    )
  )
)

(define pop
  (lambda (stk)
    (stk 'pop)
  )
)

(define empty-stack?
  (lambda (stk)
    (stk 'empty-stack?)
  )
)

(define top
  (lambda (stk)
    (stk 'top)
  )
)

(define s (push (push (empty-stack) 'a) 'b))
s
(empty-stack? s)
(top s)
(top (pop s))
(top (pop (pop s)))
