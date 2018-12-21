(define g
  (lambda (head tail)
    (g-rec head tail (car head)) 
  )
)

(define g-rec
  (lambda (head tail cnt)
    (if (null? tail)
      (list (list cnt (cadr head)))
      (cons
        (list cnt (cadr head))
        (g-rec (car tail) (cdr tail) (+ 1 cnt))
      )
    )
  )
)

(define number-elements
  (lambda (lst)
    (if (null? lst) 
      '()
      (g 
        (list 0 (car lst)) 
        (number-elements (cdr lst))
      )
    )
  )
)

(number-elements '(a b c d e))

