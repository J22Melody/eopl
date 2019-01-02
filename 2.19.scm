(define number->bintree
  (lambda (n)
    (list n '() '())
  )
)

(define current-element
  (lambda (tree)
    (car tree)
  )
)

(define insert-to-left
  (lambda (n tree)
    (list (car tree) (list n (cadr tree) '()) (caddr tree))
  )
)

(define insert-to-right
  (lambda (n tree)
    (list (car tree) (cadr tree) (list n '() (caddr tree)))
  )
)

(define at-leaf?
  (lambda (tree)
    (null? tree)
  )
)

(define move-to-left
  (lambda (tree)
    (if (at-leaf? tree)
      #f
      (cadr tree)
    )
  )
)

(define move-to-right
  (lambda (tree)
    (if (at-leaf? tree)
      #f
      (caddr tree)
    )
  )
)

(number->bintree 13)
(define t1 
  (insert-to-right 14
    (insert-to-left 12
      (number->bintree 13)
    )
  )
)
(move-to-left t1)
(current-element (move-to-left t1))
(at-leaf? (move-to-right (move-to-left t1)))
(insert-to-left 15 t1)
