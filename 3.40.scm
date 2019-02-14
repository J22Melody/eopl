;;; translator

(define empty-senv
  (lambda ()
    '()
  )
)

(define extend-senv
  (lambda (var senv)
    (cons var senv)
  )
)

(define apply-senv
  (lambda (senv var)
    (cond
      ((null? senv) (report-unbound-var var))
      ((eqv? var (car senv)) 0)
      (else (+ 1 (apply-senv (cdr senv) var)))
    )
  )
)

(define translation-of
  (lambda (exp senv senv2)
    (cases expression exp
      (const-exp (num) 
        (const-exp num)
      ) 
      (diff-exp (exp1 exp2)
        (diff-exp
          (translation-of exp1 senv senv2)
          (translation-of exp2 senv senv2)
        )
      )
      (zero?-exp (exp1)
        (zero?-exp (translation-of exp1 senv senv2))
      )
      (if-exp (exp1 exp2 exp3)
        (if-exp
          (translation-of exp1 senv senv2)
          (translation-of exp2 senv senv2)
          (translation-of exp3 senv senv2)
        )
      )
      (var-exp (var)
        (nameless-var-exp (apply-senv senv var))
      )
      (let-exp (var exp1 body)
        (nameless-let-exp 
          (translation-of exp1 senv senv2) 
          (translation-of body (extend-senv var senv) senv2)
        )
      )
      (letrec-exp (proc-name bound-var proc-body letrec-body)
        (nameless-letrec-exp
          (translation-of proc-body (extend-senv bound-var senv) (extend-senv proc-name senv2))
          (translation-of letrec-body senv (extend-senv proc-name senv2))
        )
      )
      (proc-exp (var body)
        (nameless-proc-exp
          (translation-of body (extend-senv var senv) senv2)
        )
      )
      (call-exp (rator rand)
        (call-exp
          (translation-of rator senv senv2) 
          (translation-of rand senv senv2)
        )
      )
      (else
        (report-invalid-source-expression exp)
      )
    )
  )
)

(define translation-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (a-program
          (translation-of exp1 (empty-senv) (empty-senv))
        )
      )
    )
  )
)

;;; interpreter

(define nameless-environment?
  (lambda (x)
    ((list-of expval?) x)
  )
)

(define empty-nameless-env
  (lambda ()
    '()
  )
)

(define extend-nameless-env
  (lambda (val nameless-env)
    (cons val nameless-env)
  )
)

(define apply-nameless-env
  (lambda (nameless-env n) 
    (list-ref nameless-env n)
  )
)

(define apply-nameless-env-letrec
  (lambda (nameless-env n)
    (proc-val
      (procedure (list-ref nameless-env n) nameless-env)
    )
  )
)

(define-datatype proc proc?
  (procedure
    (body expression?)
    (saved-nameless-env nameless-environment?)
    (saved-nameless-env2 nameless-environment?)
  )
)

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (body saved-nameless-env saved-nameless-env2)
        (value-of body (extend-nameless-env val saved-nameless-env) saved-nameless-env2)
      )
    )
  )
)

(define value-of
  (lambda (exp nameless-env nameless-env2)
    (cases expression exp
      (const-exp (num) 
        ;;; ...as before...
      ) 
      (diff-exp (exp1 exp2) 
        ;;; ...as before...
      )
      (zero?-exp (exp1) 
        ;;; ...as before...
      ) 
      (if-exp (exp1 exp2 exp3) 
        ;;; ...as before...
      )
      (nameless-var-exp (n) 
        (apply-nameless-env nameless-env n)
      )
      (nameless-letrec-var-exp (n) 
        (apply-nameless-env-letrec nameless-env2 n)
      )
      (nameless-let-exp (exp1 body)
        (let ((val (value-of exp1 nameless-env)))
          (value-of body (extend-nameless-env val nameless-env) nameless-env2)
        )
      )
      (nameless-letrec-exp (proc-body letrec-body)
        (value-of letrec-body nameless-env (extend-nameless-env proc-body nameless-env2))
      )
      (nameless-proc-exp (body)
        (proc-val
          (procedure body nameless-env nameless-env2)
        )
      )
      (call-exp (rator rand) 
        (let ((proc (expval->proc (value-of rator nameless-env))) (arg (value-of rand nameless-env))) 
          (apply-procedure proc arg)
        )
      )
      (else
        (report-invalid-translated-expression exp)
      )
    )
  )
)

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (empty-nameless-env) (empty-nameless-env))
      )
    )
  )
)

(define run
  (lambda (string)
    (value-of-program
      (translation-of-program
        (scan&parse string)
      )
    )
  )
)