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
  (equal?-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (greater?-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (less?-exp
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
  (cond-exp
    (conds (list-of expression?))
    (acts (list-of expression?))
  )
  (var-exp
    (var identifier?)
  )
  (let-exp
    (vars (list-of identifier?))
    (exps (list-of expression?))
    (body expression?)
  )
  (let*-exp
    (vars (list-of identifier?))
    (exps (list-of expression?))
    (body expression?)
  )
  (unpack-exp
    (vars (list-of identifier?))
    (exp1 expression?)
    (body expression?)
  )
  (cons-exp
    (exp1 expression?)
    (exp2 expression?)
  )
  (car-exp
    (exp1 expression?)
  )
  (cdr-exp
    (exp1 expression?)
  )
  (null?-exp
    (exp1 expression?)
  )
  (emptylist-exp)
  (list-exp
    (exps (list-of expression?))
  )
)

(define init-env
  (lambda ()
    (extend-env
      'i (num-val 1)
      (extend-env
        'v (num-val 5)
        (extend-env
          'x (num-val 10)
          (empty-env)
        )
      )
    )
  )
)

(define batch-extend-env
  (lambda (vars vals env)
    (if (null? vars)
      env
      (batch-extend-env 
        (cdr vars) 
        (cdr vals) 
        (extend-env (car vars) (car vals) env)
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
  (pair-val
    (head expval?)
    (tail expval?)
  )
  (emptylist-val)
)

(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (report-expval-extractor-error 'num val))
    )
  )
)

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (report-expval-extractor-error 'bool val))
    )
  )
)

(define expval->head
  (lambda (val)
    (cases expval val
      (pair-val (head tail) head)
      (else (report-expval-extractor-error 'pair val))
    )
  )
)

(define expval->tail
  (lambda (val)
    (cases expval val
      (pair-val (head tail) tail)
      (else (report-expval-extractor-error 'pair val))
    )
  )
)

(define expval->null?
  (lambda (val)
    (cases expval val
      (emptylist-val () (bool-val #t))
      (else (bool-val #f))
    )
  )
)

(define expval->list
  (lambda (val)
    (cases expval val
      (emptylist-val () '())
      (pair-val (head tail) (cons head (expval->list tail)))
      (else (report-expval-extractor-error 'pair val))
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
      (equal?-exp (exp1 exp2)
        (let
          ((val1 (value-of exp1 env))
          (val2 (value-of exp2 env))) 
          (let
            ((num1 (expval->num val1))
            (num2 (expval->num val2)))
            (bool-val (= num1 num2))
          )
        )
      )
      (greater?-exp (exp1 exp2)
        (let
          ((val1 (value-of exp1 env))
          (val2 (value-of exp2 env))) 
          (let
            ((num1 (expval->num val1))
            (num2 (expval->num val2)))
            (bool-val (> num1 num2))
          )
        )
      )
      (less?-exp (exp1 exp2)
        (let
          ((val1 (value-of exp1 env))
          (val2 (value-of exp2 env))) 
          (let
            ((num1 (expval->num val1))
            (num2 (expval->num val2)))
            (bool-val (< num1 num2))
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
      (cond-exp (conds acts)
        (if (null? conds)
          (report-error 'no-condition-matches)
          (if (expval->bool (value-of (car conds) env))
            (value-of (car acts) env)
            (value-of (cond-exp (cdr conds) (cdr acts)))
          )
        )
      )
      (let-exp (vars exps body)
        (value-of 
          body 
          (batch-extend-env
            vars 
            (map
              (lambda (exp) 
                (value-of exp env)
              ) 
              exps
            ) 
            env
          )
        )
      )
      (let*-exp (vars exps body)
        (if (null? vars)
          (value-of body env)
          (value-of 
            (let-exp (cdr vars) (cdr exps)
              (value-of
                body
                (extend-env (car vars) (value-of (car exps) env) env)
              )
            )
          )
        )
      )
      (unpack-exp (vars exp1 body)
        (value-of 
          body 
          (batch-extend-env
            vars 
            (expval->list (value-of exp1 env)) 
            env
          )
        )
      )
      (cons-exp (exp1 exp2)
        (let 
          ((val1 (value-of exp1 env))
          (val2 (value-of exp2 env)))
          (pair-val val1 val2)
        )
      )
      (car-exp (exp1)
        (let 
          ((val1 (value-of exp1 env)))
          (expval->head val1)
        )
      )
      (cdr-exp (exp1)
        (let 
          ((val1 (value-of exp1 env)))
          (expval->tail val1)
        )
      )
      (null?-exp (exp1)
        (let 
          ((val1 (value-of exp1 env)))
          (expval->null? val1)
        )
      )
      (emptylist-exp ()
        (emptylist-val)
      )
      (list-exp (exps)
        (if (null? exps)
          (emptylist-val)
          (pair-val
            (value-of (car exps) env)
            (value-of (list-exp (cdr exps)))
          )
        )
      )
    )
  )
)
