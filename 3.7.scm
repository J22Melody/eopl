(define-datatype program program? 
  (a-program 
    (exp1 expression?)
  )
)

(define-datatype expression expression? 
  (const-exp 
    (num number?)
  )
  (minus-exp
    (exp1 expression?)
  )
  (addition-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (diff-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (multiplication-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (quotient-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (zero?-exp
    (exp1 expression?)
  )
  (if-exp
    (exp1 expression?)
    (exp2 expression?)
    (exp3 expression?)
  )
  (var-exp
    (var identifier?)
  )
  (let-exp
    (var identifier?)
    (exp1 expression?)
    (body expression?)
  )
)

(define init-env
  (lambda ()
    (extend-env
      ’i (num-val 1)
      (extend-env
        ’v (num-val 5)
        (extend-env
          ’x (num-val 10)
          (empty-env)
        )
      )
    )
  )
)

(define-datatype expval expval? 
  (num-val
    (num number?)
  )
  (bool-val
    (bool boolean?)
  )
)

(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (report-expval-extractor-error ’num val))
    )
  )
)

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (report-expval-extractor-error ’bool val))
    )
  )
)

(define run
  (lambda (string)
    (value-of-program (scan&parse string))
  )
)

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (init-env))
      )
    )
  )
)

(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) 
        (num-val num)
      )
      (var-exp (var) 
        (apply-env env var)
      )
      (minus-exp (exp1)
        (let 
          ((val1 (value-of exp1 env)))
          (let
            ((num1 (expval->num val1)))
            (num-val (- 0 num1))
          )
        )
      )
      (add-exp (exp1 exp2)
        (let
          ((val1 (value-of exp1 env))
          (val2 (value-of exp2 env))) 
          (let
            ((num1 (expval->num val1))
            (num2 (expval->num val2)))
            (num-val (+ num1 num2))
          )
        )
      )
      (diff-exp (exp1 exp2)
        (let
          ((val1 (value-of exp1 env))
          (val2 (value-of exp2 env))) 
          (let
            ((num1 (expval->num val1))
            (num2 (expval->num val2)))
            (num-val (- num1 num2))
          )
        )
      )
      (multiplication-exp (exp1 exp2)
        (let
          ((val1 (value-of exp1 env))
          (val2 (value-of exp2 env))) 
          (let
            ((num1 (expval->num val1))
            (num2 (expval->num val2)))
            (num-val (* num1 num2))
          )
        )
      )
      (quotient-exp (exp1 exp2)
        (let
          ((val1 (value-of exp1 env))
          (val2 (value-of exp2 env))) 
          (let
            ((num1 (expval->num val1))
            (num2 (expval->num val2)))
            (num-val (/ num1 num2))
          )
        )
      )
      (zero?-exp (exp1)
        (let 
          ((val1 (value-of exp1 env)))
          (let 
            ((num1 (expval->num val1)))
            (if (zero? num1)
              (bool-val #t)
              (bool-val #f)
            )
          )
        )
      )
      (if-exp (exp1 exp2 exp3)
        (let 
          ((val1 (value-of exp1 env)))
          (if (expval->bool val1)
            (value-of exp2 env)
            (value-of exp3 env)
          )
        )
      )
      (let-exp (var exp1 body)
        (let 
          ((val1 (value-of exp1 env)))
          (value-of 
            body
            (extend-env var val1 env)
          )
        )
      )
    )
  )
)
