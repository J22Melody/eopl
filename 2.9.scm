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

(define empty-env?
  (lambda (env) 
    (null? env)
  )
)

(define has-binding?
  (lambda (env search-var)
    (if (empty-env? env)
      #f
      (if (eqv? search-var (caar env))
        #t
        (has-binding? (cadr env) search-var)
      )
    )
  )
)

(define e
  (extend-env 'd 6
    (extend-env 'y 8
      (extend-env 'x 7
        (extend-env 'y 14
          (empty-env)
        )
      )
    )
  )
)

(has-binding? e 'd)
(has-binding? e 'y)
(has-binding? e 'a)