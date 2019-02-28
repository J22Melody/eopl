(var-exp (var)
  (let ((val1 (apply-env env var)))
    (cases expval val1
      (ref-val (ref) (deref ref))
      (else () val1)
    )
  )
)

(letmutable-exp (var exp1 body)
  (let ((val1 (value-of exp1 env)))
    (value-of body
      (extend-env var (newref val1) env)
    )
  )
)

(let-exp (var exp1 body)
  (let ((val1 (value-of exp1 env)))
    (value-of body
      (extend-env var val1 env)
    )
  )
)