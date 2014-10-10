((lambda (x y)
; TODO: make sure x/y are not lost in a nested closure
 ((lambda ()
  ((lambda () (- x y)))) 5 4)
 ))
