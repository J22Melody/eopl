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
  (lambda (exp senv)
    (cases expression exp
      (const-exp (num) 
        (const-exp num)
      ) 
      (diff-exp (exp1 exp2)
        (diff-exp
          (translation-of exp1 senv)
          (translation-of exp2 senv)
        )
      )
      (zero?-exp (exp1)
        (zero?-exp (translation-of exp1 senv))
      )
      (if-exp (exp1 exp2 exp3)
        (if-exp
          (translation-of exp1 senv)
          (translation-of exp2 senv)
          (translation-of exp3 senv)
        )
      )
      (cond-exp (conds acts)
        (cond-exp
          (map (lambda (exp1) (translation-of exp1 senv)) conds)
          (map (lambda (exp1) (translation-of exp1 senv)) acts)
        )
      )
      (var-exp (var)
        (nameless-var-exp (apply-senv senv var))
      )
      (let-exp (var exp1 body)
        (nameless-let-exp 
          (translation-of exp1 senv) 
          (translation-of body (extend-senv var senv))
        )
      )
      (unpack-exp (vars exp1 body)
        (nameless-unpack-exp
          (translation-of exp1 senv)
          (translation-of body (batch-extend-senv vars senv))
        )
      )
      (proc-exp (var body)
        (nameless-proc-exp
          (translation-of body (extend-senv var senv))
        )
      )
      (call-exp (rator rand)
        (call-exp
          (translation-of rator senv) 
          (translation-of rand senv)
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
          (translation-of exp1 (empty-senv))
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

(define-datatype proc proc?
  (procedure
    (body expression?)
    (saved-nameless-env nameless-environment?)
  )
)

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (body saved-nameless-env)
        (value-of body (extend-nameless-env val saved-nameless-env))
      )
    )
  )
)

(define value-of
  (lambda (exp nameless-env)
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
      (cond-exp (conds acts)
        (if (null? conds)
          (report-error 'no-condition-matches)
          (if (expval->bool (value-of (car conds) nameless-env))
            (value-of (car acts) nameless-env)
            (value-of (cond-exp (cdr conds) (cdr acts)) nameless-env)
          )
        )
      )
      (call-exp (rator rand) 
        ;;; ...as before...
      )
      (nameless-var-exp (n) 
        (apply-nameless-env nameless-env n)
      )
      (nameless-let-exp (exp1 body)
        (let ((val (value-of exp1 nameless-env)))
          (value-of body (extend-nameless-env val nameless-env))
        )
      )
      (nameless-unpack-exp (exp1 body)
        (let ((vals (value-of exp1 nameless-env)))
          (value-of body
            (batch-extend-nameless-env
              (expval->list vals) 
              nameless-env
            )
          )
        )
      )
      (nameless-proc-exp (body)
        (proc-val
          (procedure body nameless-env)
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
        (value-of exp1 (empty-nameless-env))
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