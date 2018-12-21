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

(define double-tree
  (lambda (tree)
    (if (leaf? tree)
      (leaf (* 2 (content-of tree)))
      (interior-node
        (content-of tree)
        (double-tree (lson tree))
        (double-tree (rson tree))
      )
    )
  )
)

;;; test

(define my-leaf 
  (leaf 5)
)

(content-of my-leaf)

(define my-tree
  (interior-node 1 my-leaf (interior-node 2 (leaf 3) (leaf 4)))
)

(content-of my-tree)
(content-of (lson my-tree))
(content-of (rson my-tree))
(content-of (lson (rson my-tree)))

(define my-tree-doubled
  (double-tree my-tree)
)

(content-of my-tree-doubled)
(content-of (lson my-tree-doubled))
(content-of (rson my-tree-doubled))
(content-of (lson (rson my-tree-doubled)))

