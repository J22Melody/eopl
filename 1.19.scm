(define list-set 
  (lambda (lst n x)
    (if (eqv? n 0)
      (cons
        x
        (cdr lst)
      )
      (cons
        (car lst)
        (list-set (cdr lst) (- n 1) x)
      )
    )
  )
)

(list-set '(a b c d) 2 '(1 2))
(list-ref (list-set '(a b c d) 3 '(1 5 10)) 3)