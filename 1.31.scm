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

;;; test

(define my-leaf 
  (leaf 'e)
)

(content-of my-leaf)

(define my-tree
  (interior-node 'a my-leaf (interior-node 'b (leaf 'c) (leaf 'd)))
)

(content-of my-tree)
(content-of (lson my-tree))
(content-of (rson my-tree))
(content-of (lson (rson my-tree)))