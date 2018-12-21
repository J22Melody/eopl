;;; Binary-search-tree ::= () | (Int Binary-search-tree Binary-search-tree)

(define path
  (lambda (n bs-tree)
    (cond 
      ((null? bs-tree) #f)
      ((eqv? n (car bs-tree)) '())
      (else (let ((left-result (path n (cadr bs-tree))))
        (if (eqv? #f left-result)
          (let ((right-result (path n (caddr bs-tree))))
            (if (eqv? #f right-result)
              #f
              (cons 'right right-result)
            )
          )
          (cons 'left left-result)
        )
      ))
    )
  )
)

(path 17 
  '(14 
    (7 
      ()
      (12 () ())
    ) 
    (26 
      (20 
        (17 () ()) 
        ()
      ) 
      (31 () ())
    )
  )
)