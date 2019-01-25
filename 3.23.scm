;;; Result: 12
;;; PROC language below

let makemult = 
  proc (maker) 
    proc (x)
      proc (y)
        if zero?(x)
          then 0
        else 
          -(((maker maker) -(x,1)), -(0,y))
in let times = 
  proc (x) 
    proc (y) 
      (((makemult makemult) x) y)
in let makefactorial = 
  proc (maker)
    proc (num)
      if zero?(num)
        then 1
      else 
        (times num ((maker maker -(num 1))))
in let factorial = 
  proc (num)
    ((makefactorial makefactorial) num)
    