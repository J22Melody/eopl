(define-datatype lc-exp lc-exp? 
  (var-exp (var symbol?))
  (lambda-exp
    (bound-vars (list-of symbol?))
    (body lc-exp?)
  )
  (app-exp
    (rator lc-exp?)
    (rands (list-of lc-exp?))
  )
)

(define parse-expression
  (lambda (datum)
    (cond
      ((symbol? datum) 
        (if (eqv? 'lambda datum)
          'lambda-is-not-a-valid-symbol
          (var-exp datum)
        )
      ) 
      ((pair? datum)
        (if (eqv? (car datum) 'lambda)
          (cond
            ((not (eqv? (length datum) 3)) 'lambda-exp-requires-3-args)
            ((not (list? (cadr datum))) 'bound-vars-should-be-a-list)
            (else (lambda-exp
              (cadr datum)
              (parse-expression (caddr datum))
            ))
          )
          (cond
            ((not (eqv? (length datum) 3)) 'app-exp-requires-3-args)
            ((not (list? (cadr datum))) 'rands-should-be-a-list)
            (else (app-exp
              (parse-expression (car datum))
              (map parse-expression (cadr datum))
            )
          )
        )
      )
      (else 'unknown-expression-structure)
    )
  )
)