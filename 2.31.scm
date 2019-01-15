(define-datatype prefix-exp prefix-exp? 
  (const-exp (num integer?))
  (diff-exp
    (operand1 prefix-exp?)
    (operand2 prefix-exp?)
  )
)

(define parse-expression-rec
  (lambda (lst)
    (if (number? (car lst))
      (cons 
        (const-exp (car lst))
	      (cdr lst)
      )
      (if (eqv? (car lst) '-)
        (if (null? (cdr lst))
          'error-need-operand
          (let* 
            (
              (next (parse-expression-rec (cdr lst)))
              (op1 (car next))
              (next (parse-expression-rec (cdr next)))
              (op2 (car next))
              (rest (cdr next))
            )
            (cons 
              (diff-exp op1 op2)
              rest
            )
          )
        )
      )
    )
  )
)

(define parse-expression
  (lambda (lst)
    (car (parse-expression-rec lst))
  )
)