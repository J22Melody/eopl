(letproc-exp (var body proc-var proc-body)
  (let 
    ((val (proc-val (procedure proc-var proc-body env))))
    (value-of 
      body
      (extend-env var val env)
    )
  )
)