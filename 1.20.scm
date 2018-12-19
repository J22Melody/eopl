(define count-occurrences-rec
  (lambda (s slist cnt)
    (if (null? slist)
      cnt 
      (+ 
        (let ((sexp (car slist)))
          (if (symbol? sexp)
            (if (eqv? sexp s)
              (+ cnt 1)
              cnt
            )
            (count-occurrences-rec s sexp 0)
          )
        )
        (count-occurrences-rec s (cdr slist) 0)
      )
    )
  )
)

(define count-occurrences
  (lambda (s slist)
    (count-occurrences-rec s slist 0)
  )
)

(count-occurrences 'x '((f x) y (((x z) x))))
(count-occurrences 'x '((f x) y (((x z) () x))))
(count-occurrences 'w '((f x) y (((x z) x))))