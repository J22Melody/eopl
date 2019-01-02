(define one
  (lambda ()
    'one
  )
)

(define diff
  (lambda (x y)
    (list 'diff x y)
  )
)

(define diff-tree->integer
  (lambda (l)
    (if (eqv? l (one))
      1
      (-
        (diff-tree->integer (cadr l))
        (diff-tree->integer (caddr l))
      )
    )
  )
)

(define zero 
  (lambda ()
    (diff (one) (one))
  )
)

(define is-zero? 
  (lambda (n) 
    (eqv? 0 (diff-tree->integer n))
  )
)

(define successor 
  (lambda (n) 
    (diff n (diff (zero) (one)))
  )
) 

(define predecessor
  (lambda (n) 
    (diff n (one))
  )
)

(define diff-tree-plus
  (lambda (x y) 
    (diff x (diff (zero) y))
  )
)

(diff-tree->integer (successor (successor (successor (zero)))))
(diff-tree->integer (predecessor (predecessor (predecessor (successor (zero))))))
(diff-tree->integer (diff-tree-plus (successor (successor (successor (zero)))) (successor (successor (successor (zero))))))