(define-datatype env env?
  (empty-env)
  (non-empty-env
    (var symbol?)
    (val symbol?)
    (saved-env env?)
  )
)

(define has-binding?
  (lambda (target-env search-var) 
    (cases env target-env
      (empty-env () #f)
      (non-empty-env (var val saved-env)
        (if (eqv? search-var var)
          #t
          (has-binding? saved-env search-var)
        )
      )
    )
  )
)