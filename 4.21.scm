(setdynamic-exp (var exp1 body)
  (let* (
    (ref1 (apply-env env var))
    (saved-val (deref ref1))
    (val1 (value-of exp1 env))
    (ret 0)
  )
    (begin
      (setref! ref1 val1)
      (set! ret (value-of body env))
      (setref! ref1 saved-val)
      ret
    )
  )
)
