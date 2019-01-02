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

(empty-env? e)
(empty-env? (empty-env))