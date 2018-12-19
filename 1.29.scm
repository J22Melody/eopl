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
  (lambda (soi)
    (cond
      ((null? soi) '())
      ((eqv? (length soi) 1) soi)
      ((eqv? (length soi) 2)
        (if (<= (car soi) (cadr soi))
          soi
          (cons (cadr soi) (list (car soi)))
        )
      )
      (else
        (merge
          (sort (list (car soi) (cadr soi)))
          (sort (cddr soi))
        )
      )
    )
  )
)

(sort '(8 2 5 2 3))
