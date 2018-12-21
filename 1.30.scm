(define merge/predicate
  (lambda (pred loi1 loi2)
    (cond 
      ((and (null? loi1) (not (null? loi2))) loi2)
      ((and (null? loi2) (not (null? loi1))) loi1)
      (else 
        (let ((first1 (car loi1)) (first2 (car loi2)))
          (if (pred first1 first2)
            (cons first1 (merge/predicate pred (cdr loi1) loi2))
            (cons first2 (merge/predicate pred loi1 (cdr loi2)))
          )
        )
      )
    )
  )
)

(define sort/predicate
  (lambda (pred loi)
    (cond
      ((null? loi) '())
      ((eqv? (length loi) 1) loi)
      ((eqv? (length loi) 2)
        (if (pred (car loi) (cadr loi))
          loi
          (cons (cadr loi) (list (car loi)))
        )
      )
      (else
        (merge/predicate
          pred
          (sort/predicate pred (list (car loi) (cadr loi)))
          (sort/predicate pred (cddr loi))
        )
      )
    )
  )
)

(sort/predicate < '(8 2 5 2 3))
(sort/predicate > '(8 2 5 2 3))
