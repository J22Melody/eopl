(define var-exp
  (lambda (var)
    (list 'var-exp var)
  )
)

(define lambda-exp
  (lambda (bound-var body)
    (list 'lambda-exp bound-var body)
  )
)

(define app-exp
  (lambda (rator rand)
    (list 'app-exp rator rand)
  )
)

(define var-exp?
  (lambda (exp)
    (eqv? 'var-exp (car exp))
  )
)

(define lambda-exp?
  (lambda (exp)
    (eqv? 'lambda-exp (car exp))
  )
)

(define app-exp?
  (lambda (exp)
    (eqv? 'app-exp (car exp))
  )
)

