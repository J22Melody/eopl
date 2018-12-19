(define list-index
  (lambda (pred lst)
    (if (null? lst)
      #f
      (if (pred (car lst))
        0
        (if (eqv? (list-index pred (cdr lst)) #f)
          #f
          (+ 1 (list-index pred (cdr lst)))
        )
      )
    )
  )
)

(list-index number? '(a 2 (1 3) b 7))
(list-index symbol? '(a (b c) 17 foo))
(list-index symbol? '(1 2 (a b) 3))