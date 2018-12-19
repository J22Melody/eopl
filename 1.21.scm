(define product
  (lambda (sos1 sos2)
    (if (null? sos2)
      '()
      (append
        (product-symbol sos1 (car sos2))
        (product sos1 (cdr sos2))
      )
    )
  )
)

(define product-symbol
  (lambda (sos x)
    (map 
      (lambda (y) (list y x))
      sos
    )
  )
)

(product '(a b c) '(x y))