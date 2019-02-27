(define extend-env-rec
  (lambda (p-names b-vars p-bodies saved-env)
    (if (null? p-names)
      saved-env
      (let ((vec (make-vector 1)))
        (let ((new-env (extend-env (car p-names) vec saved-env)))
          (vector-set! vec 0
            (newref 
              (proc-val 
                (procedure (car b-vars) (car p-bodies) new-env)
              )
            )
          )
          (extend-env-rec (cdr p-names) (cdr b-vars) (cdr p-bodies) new-env)
        )
      )
    )
  )
)
