;;; origin

(define-datatype environment environment? (empty-env)
  (extend-env
    (var identifier?)
    (val expval?)
    (env environment?)
  )
  (extend-env-rec
    (p-name identifier?)
    (b-var identifier?)
    (body expression?)
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
      (extend-env-rec (p-name b-var p-body saved-env) 
        (if (eqv? search-var p-name)
          (proc-val (procedure b-var p-body env)) 
          (apply-env saved-env search-var)
        )
      )
    )
  )
)

;;; exercise

(define-datatype environment environment? (empty-env)
  (extend-env
    (var identifier?)
    (val expval?)
    (env environment?)
  )
  (extend-env-rec
    (p-name identifier?)
    (b-vars (list-of identifier?))
    (body expression?)
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
      (extend-env-rec (p-name b-vars p-body saved-env) 
        (if (eqv? search-var p-name)
          (proc-val (procedure b-vars p-body env)) 
          (apply-env saved-env search-var)
        )
      )
    )
  )
)