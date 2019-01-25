;;; PROC language below

let make = 
  proc (maker) 
    proc (x)
      if zero?(x)
        then 1
      else 
        if ((maker maker) -(x,1))
          0
        else
          1
in let even =
  proc (x)
    ((make make) x)
in let odd =
  proc (x)
    (even -(x,1))

;;; mutually recursive approach

let make-even = 
  proc (o)
    proc (e)
      proc (num)
        if zero?(x)
          then 1
        else
          (((o o) e) -(num,1))
in let make-odd = 
  proc (o)
    proc (e)
      proc (num)
        if zero?(x)
          then 0
        else
          (((e o) e) -(num,1))
in let odd = 
  proc (num)
    (((make-odd make-odd) make-even) num)
in let even = 
  proc (num)
    (((make-even make-odd) make-even) num)

    