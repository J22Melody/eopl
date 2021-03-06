(define-datatype environment environment? (empty-env)
  (extend-env
    (var identifier?)
    (val expval?)
    (env environment?)
  )
  (extend-env-rec
    (p-names (list-of identifier?))
    (b-vars (list-of identifier?))
    (bodies (list-of expression?))
    (env environment?)
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
          saved-val
          (apply-env saved-env search-var)
        )
      )
      (extend-env-rec (p-names b-vars p-bodies saved-env) 
        (if (memq search-var p-names)
          (let ((idx (list-index (p-names search-var))))
            (proc-val (procedure (list-ref b-vars idx) (list-ref p-bodies idx) env)) 
          )
          (apply-env saved-env search-var)
        )
      )
    )
  )
)