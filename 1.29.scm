(define merge
  (lambda (loi1 loi2)
    (cond 
      ((and (null? loi1) (not (null? loi2))) loi2)
      ((and (null? loi2) (not (null? loi1))) loi1)
      (else 
        (let ((first1 (car loi1)) (first2 (car loi2)))
          (if (< first1 first2)
            (cons first1 (merge (cdr loi1) loi2))
            (cons first2 (merge loi1 (cdr loi2)))
          )
        )
      )
    )
  )
)

(define sort
  (lambda (loi)
    (cond
      ((null? loi) '())
      ((eqv? (length loi) 1) loi)
      ((eqv? (length loi) 2)
        (if (<= (car loi) (cadr loi))
          loi
          (cons (cadr loi) (list (car loi)))
        )
      )
      (else
        (merge
          (sort (list (car loi) (cadr loi)))
          (sort (cddr loi))
        )
      )
    )
  )
)

(sort '(8 2 5 2 3))
