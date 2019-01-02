(define empty-env
  (lambda () 
    '()
  )
)

(define extend-env
  (lambda (var val env)
    (list (list var val) env)
  )
)

(define extend-env*
  (lambda (lovar loval env)
    (if (null? lovar)
      env
      (extend-env
        (car lovar)
        (car loval)
        (extend-env* (cdr lovar) (cdr loval) env)
      )
    )
  )
)

(define apply-env
  (lambda (env search-var)
    (if (null? env)
      #f
      (if (eqv? search-var (caar env))
        (cadar env)
        (apply-env (cadr env) search-var)
      )
    )
  )
)

(define e
  (extend-env*
    '(a b c d)
    '(1 2 3 4)
    (empty-env)
  )
)

(apply-env e 'd)
(apply-env e 'y)