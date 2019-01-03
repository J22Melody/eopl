(define-datatype bintree bintree? 
  (leaf-node (num integer?))
  (interior-node
    (key symbol?)
    (left bintree?)
    (right bintree?)
  )
)

(define max-interior
  (lambda (btree) 
    (cases bintree btree
      (interior-node (x ltree rtree)
        (cases bintree ltree
          (leaf-node (ltree-n)
            (cases bintree rtree
              (leaf-node (rtree-n)
                x
              )
              (interior-node (rtree-x rtree-ltree rtree-rtree)
                (if (> ltree-n 0)
                  x
                  (max-interior rtree)
                )
              )
            )
          )
          (interior-node (ltree-x ltree-ltree ltree-rtree)
            (cases bintree rtree
              (leaf-node (rtree-n)
                (if (> rtree-n 0)
                  x
                  (max-interior ltree)
                )
              )
              (interior-node (rtree-x rtree-ltree rtree-rtree)
                (let* 
                  (
                    (l-max (max-interior ltree))
                    (r-max (max-interior rtree))
                    (sum (+ l-max r-max))
                    (btree-max (max l-max r-max sum))
                  )
                  (cond
                    ((eqv? bintree-max l-max) ltree-x)
                    ((eqv? bintree-max r-max) rtree-x)
                    ((eqv? bintree-max sum) x)
                  )
                )
              )
            )
          )
        )
      )
      (else #f)
    )
  )
)