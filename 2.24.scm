(define-datatype bintree bintree? 
  (leaf-node (num integer?))
  (interior-node
    (key symbol?)
    (left bintree?)
    (right bintree?)
  )
)

(define bintree-to-list
  (lambda (btree) 
    (cases bintree btree
      (leaf-node (n) (list 'leaf-node n))
      (interior-node (x ltree rtree)
        (list 'interior-node x (bintree-to-list ltree) (bintree-to-list rtree))
      )
    )
  )
)