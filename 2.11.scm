(define empty-env
  (lambda () 
    '()
  )
)

(define extend-env*
  (lambda (lovar loval env)
    (if (null? lovar)
      env
      (cons
        (list lovar loval)
        env
      )
    )
  )
)

(define e
  (extend-env*
    '(x y)
    '(9 8)
    (extend-env*
      '(a b c d)
      '(1 2 3 4)
      (empty-env)
    )
  )
)

e