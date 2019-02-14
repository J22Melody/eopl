;;; translator

(define empty-senv
  (lambda ()
    '()
  )
)

(define extend-senv
  (lambda (vars senv)
    (cons vars senv)
  )
)

(define apply-senv
  (lambda (senv var)
    (cond
      ((null? senv) (report-unbound-var var))
      ((memq var (car senv)) (cons 0 (list-index var (car senv))))
      (else 
        (cons 
          (+ 1 (car (apply-senv (cdr senv) var))) 
          (cdr (apply-senv (cdr senv) var))
        )
      )
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
      (var-exp (var)
        (nameless-var-exp (apply-senv senv var))
      )
      (let-exp (vars exp1 body)
        (nameless-let-exp 
          (translation-of exp1 senv) 
          (translation-of body (extend-senv vars senv))
        )
      )
      (proc-exp (vars body)
        (nameless-proc-exp
          (translation-of body (extend-senv vars senv))
        )
      )
      (call-exp (rator rands)
        (call-exp
          (translation-of rator senv) 
          (map (lambda (rand) (translation-of rand senv)) rands)
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
    ((list-of (list-of expval?)) x)
  )
)

(define empty-nameless-env
  (lambda ()
    '()
  )
)

(define extend-nameless-env
  (lambda (vals nameless-env)
    (cons vals nameless-env)
  )
)

(define apply-nameless-env
  (lambda (nameless-env n) 
    (list-ref (list-ref nameless-env (car n)) (cdr n))
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
      (call-exp (rator rands) 
        ;;; ...as before...
      )
      (nameless-var-exp (n) 
        (apply-nameless-env nameless-env n)
      )
      (nameless-let-exp (exp1 body)
        (let ((vals (map (lambda (exp) (value-of exp nameless-env)) exp1)))
          (value-of body (extend-nameless-env vals nameless-env))
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