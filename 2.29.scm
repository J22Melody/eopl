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
      ((symbol? datum) (var-exp datum)) 
      ((pair? datum)
        (if (eqv? (car datum) 'lambda) 
          (lambda-exp
            (cadr datum)
            (parse-expression (caddr datum))
          ) 
          (app-exp
            (parse-expression (car datum))
            (map parse-expression (cdr datum))
          )
        )
      )
      (else #f)
    )
  )
)