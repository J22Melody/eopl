(define invert
  (lambda (lst)
    (if (null? lst)
      '()
      (cons
        (let ((list2 (car lst)))
          (cons
            (cadr list2)
            (car list2)
          )
        )
        (invert (cdr lst))
      )
    )
  )
)

(invert '((a 1) (a 2) (1 b) (2 b)))