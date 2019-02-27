(let-exp (vars exps body)
  (let ((refs 
    (map (lambda (exp) 
      (newref (value-of exp env))) 
    exps))
  )
    (value-of body
      (batch-extend-env vars refs env)
    )
  )
)

(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
      (procedure (vars body saved-env)
        (value-of body
          (batch-extend-env vars 
            (map (lambda (val1) (newref val1)) vals) 
          saved-env)
        )
      )
    )
  )
)
