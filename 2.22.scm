(define-datatype stack stack?
  (empty-stack)
  (non-empty-stack
    (x symbol?)
    (saved-stack stack?)
  )
)

(define pop
  (lambda (target-stack) 
    (cases stack target-stack
      (empty-stack () #f)
      (non-empty-stack (x saved-stack)
        saved-stack
      )
    )
  )
)

(define top
  (lambda (target-stack) 
    (cases stack target-stack
      (empty-stack () #f)
      (non-empty-stack (x saved-stack)
        x
      )
    )
  )
)

(define empty-stack?
  (lambda (target-stack) 
    (cases stack target-stack
      (empty-stack () #t)
      (else #f)
    )
  )
)