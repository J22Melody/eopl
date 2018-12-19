(define up
  (lambda (lst)
    (if (null? lst)
      '()
      (if (list? (car lst))
        (append
          (car lst)
          (up (cdr lst))
        )
        (cons
          (car lst)
          (up (cdr lst))
        )
      )
        
    )
  )
)

(up '((1 2) (3 4)))
(up '((x (y)) z))