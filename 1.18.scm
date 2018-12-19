(define swapper 
  (lambda (s1 s2 slist)
    (if (null? slist)
      '() 
      (cons 
        (let ((sexp (car slist)))
          (if (symbol? sexp)
            (cond 
              ((eqv? sexp s1) s2)
              ((eqv? sexp s2) s1) 
              (else sexp)
            )
            (swapper s1 s2 sexp)
          )
        )
        (swapper s1 s2 (cdr slist))
      )
    )
  )
)

(swapper 'a 'd '(a b c d))
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '((x) y (z (x))))