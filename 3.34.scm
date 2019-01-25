(define-datatype environment environment? (empty-env)
  (extend-env
    (var identifier?)
    (val expval?)
    (env environment?)
  )
)

(define extend-env-rec
  (lambda (p-name b-var body saved-env)
    (let ((vec (make-vector 1)))
      (let ((new-env (extend-env p-name vec saved-env)))
        (vector-set! vec 0 (proc-val (procedure b-var body new-env)))
        new-env
      )
    )
  )
)

(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env ()
        (report-no-binding-found search-var)
      ) 
      (extend-env (saved-var saved-val saved-env)
        (if (eqv? saved-var search-var)
          (if (vector? saved-val)
            (vector-ref vec 0) 
            saved-val
          )
          (apply-env saved-env search-var)
        )
      )
    )
  )
)
