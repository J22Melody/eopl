(define number->sequence
  (lambda (n)
    (list n '() '())
  )
)

(define current-element
  (lambda (seq)
    (car seq)
  )
)

(define insert-to-left
  (lambda (n seq)
    (list (car seq) (cons n (cadr seq)) (caddr seq))
  )
)

(define insert-to-right
  (lambda (n seq)
    (list (car seq) (cadr seq) (cons n (caddr seq)))
  )
)

(define at-left-end?
  (lambda (seq)
    (null? (cadr seq))
  )
)

(define at-right-end?
  (lambda (seq)
    (null? (caddr seq))
  )
)

(define move-to-left
  (lambda (seq)
    (if (at-left-end? seq)
      #f
      (list (caadr seq) (cdadr seq) (cons (car seq) (caddr seq)))
    )
  )
)

(define move-to-right
  (lambda (seq)
    (if (at-left-end? seq)
      #f
      (list (caaddr seq) (cons (car seq) (cadr seq)) (cdaddr seq))
    )
  )
)

(number->sequence 7)
(current-element '(6 (5 4 3 2 1) (7 8 9)))
(move-to-left '(6 (5 4 3 2 1) (7 8 9)))
(move-to-right '(6 (5 4 3 2 1) (7 8 9)))
(insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9)))
(insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9)))
