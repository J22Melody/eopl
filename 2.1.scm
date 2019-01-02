(define BASE 16)

(define zero 
  (lambda () 
    '()
  )
)

(define is-zero? 
  (lambda (n) 
    (null? n)
  )
)

(define successor 
  (lambda (n) 
    (if (is-zero? n)
      '(1)
      (if (eqv? (+ (car n) 1) BASE)
        (cons
          0
          (successor (cdr n))
        )
        (cons
          (+ (car n) 1)
          (cdr n)
        )
      )
    )
  )
) 

(define predecessor 
  (lambda (n) 
    (if (and (eqv? 1 (car n)) (eqv? (length n) 1))
      (zero)
      (if (eqv? (car n) 0)
        (cons
          (- BASE 1)
          (predecessor (cdr n))
        )
        (cons
          (- (car n) 1)
          (cdr n)
        )
      )
    )
  )
)

(define plus
  (lambda (x y)
    (if (is-zero? x)
      y
      (successor (plus (predecessor x) y))
    )
  )
)

(define multiply
  (lambda (x y)
    (if (is-zero? x)
      (zero)
      (plus (multiply (predecessor x) y) y)
    )
  )
)

(define factorial
  (lambda (n)
    (cond 
      ((is-zero? n) (zero))
      ((and (eqv? 1 (car n)) (eqv? (length n) 1) n))
      (else (multiply n (factorial (predecessor n))))
    )
  )
)

(define from-n
  (lambda (n)
    (cond 
      ((zero? n) (zero))
      (else (successor (from-n (- n 1))))
    )
  )
)

(successor (successor (successor (zero))))
(predecessor (predecessor (predecessor (successor (successor (successor (zero)))))))
(plus (successor (successor (successor (zero)))) (successor (successor (successor (zero)))))
(multiply (successor (successor (successor (zero)))) (successor (successor (successor (zero)))))
(factorial (successor (successor (successor (successor (zero))))))
(factorial (from-n 10))

;;; Aborting!: maximum recursion depth exceeded
;;; The maximum recursion depth needs to be not less than factorial(10)