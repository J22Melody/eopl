(define-datatype red-blue-tree red-blue-tree? 
  (leaf-node (num integer?))
  (red-node 
    (left red-blue-tree?)
    (right red-blue-tree?)
  )
  (blue-node
    (node-list (list-of red-blue-tree?))
  )
)

(define mark-leaves-with-red-depth
  (lambda (tree) 
    (mark-leaves-with-red-depth-rec tree 0)
  )
)

(define mark-leaves-with-red-depth-rec
  (lambda (tree count) 
    (cases red-blue-tree tree
      (leaf-node (n) (leaf-node count))
      (red-node (left right)
        (red-node 
          (mark-leaves-with-red-depth-rec left (+ count 1)) 
          (mark-leaves-with-red-depth-rec right (+ count 1))
        )
      )
      (blue-node (node-list)
        (blue-node
          (map 
            (lambda (node)
              (mark-leaves-with-red-depth-rec node count)
            )
            node-list
          )
        )
      )
    )
  )
)