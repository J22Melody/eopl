(define subst
  (lambda (new old slist)
    (if (null? slist)
      '() 
      (cons 
        (let ((sexp (car slist)))
          (if (symbol? sexp)
            (if (eqv? sexp old) new sexp) 
            (subst new old sexp)
          )
        )
        (subst new old (cdr slist))
      )
    )
  )
)

(subst 'a 'b '((b c) (b () d)))