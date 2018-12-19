(define remove-first
  (lambda (s los)
    (if (null? los)
      '()
      (if (eqv? (car los) s)
        (cdr los)
        (cons (car los) (remove-first s (cdr los)))
      )
    )
  )
)

(remove-first 'b '(a b c a b c))

(define remove-all
  (lambda (s los)
    (if (null? los)
      '()
      (if (eqv? (car los) s)
        (remove-all s (cdr los))
        (cons (car los) (remove-all s (cdr los)))
      )
    )
  )
)

(remove-all 'b '(a b c a b c)) 