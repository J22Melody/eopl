(define batch-extend-env
  (lambda (vars vals env)
    (if (null? vars)
      env
      (batch-extend-env 
        (cdr vars) 
        (cdr vals) 
        (extend-env (car vars) (car vals) env)
      )
    )
  )
)

(define-datatype proc proc?
  (procedure
    (vars (list-of identifier?))
    (body expression?)
  )
)

(define apply-procedure
  (lambda (proc1 vals env)
    (cases proc proc1
      (procedure (vars body)
        (value-of body (batch-extend-env vars vals env))
      )
    )
  )
)

(proc-exp (vars body)
  (proc-val (procedure vars body))
)

(call-exp (rator rands)
  (let 
    ((proc (expval->proc (value-of rator env)))
    (args (map (lambda (rand) (value-of rand env)) rands))) 
    (apply-procedure proc args env)
  )
)