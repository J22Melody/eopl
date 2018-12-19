(define merge
  (lambda (loi1 loi2)
    (if (null? loi2)
      loi1
      (merge
        (merge-integer loi1 (car loi2))
        (cdr loi2)
      )
    )
  )
)

(define merge-integer
  (lambda (loi n)
    (if (null? loi)
      (list n)
      (let ((first (car loi)))
        (if (< n first)
          (cons n loi)
          (cons first (merge-integer (cdr loi) n))
        )
      )
    )
  )
)

(merge '(1 4) '(1 2 8))
(merge '(35 62 81 90 91) '(3 83 85 90))