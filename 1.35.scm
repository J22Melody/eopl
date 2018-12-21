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

(define number-leaves
  (lambda (tree)
    (number-leaves-rec tree 0)
  )
)

(define number-leaves-rec
  (lambda (tree n)
    (if (leaf? tree)
      (leaf n)
      (let 
        ((left 
          (number-leaves-rec 
            (lson tree) 
            n
          )
        ))
        (interior-node
          (content-of tree)
          left
          (number-leaves-rec 
            (rson tree) 
            (+ (content-of (rightest-leaf left)) 1)
          )
        )
      )
    )
  )
)

(define rightest-leaf
  (lambda (tree)
    (if (leaf? tree)
      tree
      (rightest-leaf (rson tree))
    )
  )
)

(number-leaves
  (interior-node 'foo
    (interior-node 'bar
      (leaf 26)
      (leaf 12)
    )
    (interior-node 'baz
      (leaf 11)
      (interior-node 'quux
        (leaf 117)
        (leaf 14)
      )
    )
  )
)