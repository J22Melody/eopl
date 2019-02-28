(define run
  (lambda (string)
    (run-program (scan&parse string))
  )
)

(define run-program
  (lambda (pgm)
    (cases program pgm
      (a-program (stmt)
        (run-stmt stmt (init-env))
      )
    )
  )
)

(define run-stmt
  (lambda (stmt env)
    (cases statement stmt
      (assign-stmt (var exp1)
        (setref!
          (apply-env env var)
          (value-of exp1 env)
        )
      )
      (print-stmt (exp1)
        (display (expval->any (value-of exp1 env)))
      )
      (read-stmt (var)
        (setref!
          (apply-env env var)
          (num-val (string->number (read-line)))
        )
      )
      (block-stmt (stmts)
        (map 
          (lambda (stmt1) (run-stmt stmt1 env)) 
          stmts
        )
      )
      (if-stmt (exp1 stmt1 stmt2)
        (let 
          ((val1 (value-of exp1 env)))
          (if (expval->bool val1)
            (run-stmt stmt1 env)
            (run-stmt stmt2 env)
          )
        )
      )
      (while-stmt (exp1 stmt1)
        (letrec 
          ((inner
            (lambda ()
              (if (expval->bool (value-of exp1 env))
                (begin
                  (run-stmt stmt1 env)
                  (inner)
                )
                0
              )
            )
          ))
          (inner)
        )
      )
      (declare-stmt (vars stmt1)
        (let ((refs
          (map 
            (lambda (var) (newref (num-val 0))) 
            vars
          )
        ))
          (run-stmt stmt1 (batch-extend-env vars refs env))
        )
      )
    )
  )
)