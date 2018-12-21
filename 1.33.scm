;;; Bintree::=Int |(Symbol Bintree Bintree)

(define leaf
  (lambda (symbol)
    (list 'leaf symbol)
  )
)

(define interior-node
  (lambda (symbol left right)
    (list 'node symbol left right)
  )
)

(define leaf?
  (lambda (l)
    (eqv? 'leaf (car l))
  )
)

(define content-of
  (lambda (l)
    (cadr l)
  )
)

(define lson
  (lambda (l)
    (caddr l)
  )
)

(define rson
  (lambda (l)
    (cadddr l)
  )
)

(define mark-leaves-with-red-depth
  (lambda (tree)
    (mark-leaves-with-red-depth-rec tree 0)
  )
)

(define mark-leaves-with-red-depth-rec
  (lambda (tree depth)
    (if (leaf? tree)
      (leaf depth)
      (interior-node
        (content-of tree)
        (mark-leaves-with-red-depth-rec 
          (lson tree) 
          (if (eqv? (content-of tree) 'red) (+ 1 depth) depth)
        )
        (mark-leaves-with-red-depth-rec 
          (rson tree) 
          (if (eqv? (content-of tree) 'red) (+ 1 depth) depth)
        )
      )
    )
  )
)

(mark-leaves-with-red-depth
  (interior-node 'red
    (interior-node 'bar
      (leaf 26)
      (leaf 12)
    )
    (interior-node 'red
      (leaf 11)
      (interior-node 'quux
        (leaf 117)
        (leaf 14)
      )
    )
  )
)
