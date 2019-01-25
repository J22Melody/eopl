;;; based on 3.21.scm

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
    (saved-env environment?)
  )
)

(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
      (procedure (vars body saved-env)
        (value-of body (batch-extend-env vars vals saved-env))
      )
    )
  )
)

(define free-variables
  (lambda (body vars)
    (cases expression body
      (const-exp (num) 
        '()
      )
      (var-exp (var) 
        (if (memq var vars)
          '()
          (list var)
        )
      )
      (diff-exp (exp1 exp2)
        (append
          (free-variables exp1 vars)
          (free-variables exp2 vars)
        )
      )
      (zero?-exp (exp1)
        (free-variables exp1 vars)
      )
      (if-exp (exp1 exp2 exp3)
        (append
          (free-variables exp1 vars)
          (free-variables exp2 vars)
          (free-variables exp3 vars)
        )
      )
      (let-exp (var exp1 body)
        (append
          (free-variables exp1 vars)
          (free-variables body (cons var vars))
        )
      )
      (proc-exp (vars1 body)
        (free-variables body (append vars1 vars))
      )
      (call-exp (rator rands)
        (append
          (free-variables rator vars)
          (flatmap (lambda (rand) (free-variables rand vars)) rands)
        )
      )
    )
  )
)

(define filter-env
  (lambda (env lst)
    (filter 
      (lambda (binding) (memq (car binding) lst) 
      env
    )
  )
)

(proc-exp (vars body)
  (proc-val (procedure vars body 
    (filter-env env (free-variables body vars)))
  )
)

(call-exp (rator rands)
  (let 
    ((proc (expval->proc (value-of rator env)))
    (args (map (lambda (rand) (value-of rand env)) rands))) 
    (apply-procedure proc args)
  )
)