(define down
  (lambda (lst)
    (if (null? lst)
      '()
      (cons
        (list (car lst))
        (down (cdr lst))
      )
    )
  )
)

(down '(1 2 3))
(down '((a) (fine) (idea)))
(down '(a (more (complicated)) object))