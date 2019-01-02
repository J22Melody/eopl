(define number->bintree
  (lambda (n)
    (list (list n 'leaf 'leaf) 'root)
  )
)

(define current-element
  (lambda (tree)
    (caar tree)
  )
)

(define insert-to-left
  (lambda (n tree)
    (list (list (current-element tree) (list n (cadar tree) 'leaf) (caddar tree)) (cadr tree))
  )
)

(define insert-to-right
  (lambda (n tree)
    (list (list (current-element tree) (cadar tree) (list n 'leaf (caddar tree))) (cadr tree))
  )
)

(define at-leaf?
  (lambda (tree)
    (eqv? (car tree) 'leaf)
  )
)

(define at-root?
  (lambda (tree)
    (eqv? (cadr tree) 'root)
  )
)

(define move-to-left
  (lambda (tree)
    (if (at-leaf? tree)
      #f
      (list (cadar tree) (list (current-element tree) 'left-ref (caddar tree) (cadr tree)))
    )
  )
)

(define move-to-right
  (lambda (tree)
    (if (at-leaf? tree)
      #f
      (list (caddar tree) (list (current-element tree) 'right-ref (cadar tree) (cadr tree)))
    )
  )
)

(define move-up
  (lambda (tree)
    (if (at-root? tree)
      'root
      (let ((current (car tree)) (parent (cadr tree)))
        (cond
          ((eqv? (cadr parent) 'left-ref)
            (list (list (car parent) current (caddr parent)) (cadddr parent))
          )
          ((eqv? (cadr parent) 'right-ref)
            (list (list (car parent) (caddr parent) current) (cadddr parent))
          )
          (else #f)
        )
      )
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
t1
(move-to-left t1)
(current-element (move-to-left t1))
(move-to-right (move-to-left t1))
(move-up (move-to-right (move-to-left t1)))
(move-up (move-up (move-to-right (move-to-left t1))))
(move-up (move-up (move-up (move-to-right (move-to-left t1)))))

