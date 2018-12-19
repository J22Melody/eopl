(define filter-in
  (lambda (pred lst)
    (if (null? lst)
      '()
      (let 
        (
          (first (car lst)) 
          (rest (filter-in pred (cdr lst)))
        )
        (if (pred first)
          (cons first rest)
          rest
        )
      )
    )
  )
)

(filter-in number? '(a 2 (1 3) b 7))
(filter-in symbol? '(a (b c) 17 foo))