(define empty-env
  (lambda ()
    (list
      (lambda (search-var)
        #f
      )
      (lambda ()
        #t
      )
    )
  )
)

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (list
      (lambda (search-var)
        (if (eqv? search-var saved-var) 
          saved-val
          (apply-env saved-env search-var)
        )
      )
      (lambda ()
        #f
      )
    )
  )
)

(define apply-env
  (lambda (env search-var)
    ((car env) search-var)
  )
)

(define empty-env?
  (lambda (env)
    ((cadr env))
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

(apply-env e 'd)
(apply-env e 'y)
(empty-env? e)
(empty-env? (empty-env))