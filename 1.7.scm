(load-option 'format)

(define nth-element
  (lambda (lst n)
    (nth-element-rec lst n (lambda ()
      (format #f "List ~S too short by ~S elements." lst n)
    ))
  )
)

(define nth-element-rec
  (lambda (lst n report-list-too-short)
    (if (null? lst)
      (report-list-too-short) 
      (if (zero? n)
        (car lst)
        (nth-element-rec (cdr lst) (- n 1) report-list-too-short)
      )
    )
  )
)    

(nth-element '(a b c d e) 4)




