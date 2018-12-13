(define nth-element
  (lambda (lst n)
    (if (null? lst)
      (report-list-too-short n) 
      (if (zero? n)
        (car lst)
        (nth-element (cdr lst) (- n 1))
      )
    )
  )
)

(define report-list-too-short
  (lambda (n) (error "List too short by ~s elements.~%" (+ n 1)))
)

(nth-element '(a b c d e) 100)
