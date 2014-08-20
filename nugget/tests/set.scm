;(set! x 1)
(letrec ((x #f))
;((lambda (x)
    (set! x (+ 2 (* 3 4)))
    (display x))
; #f)

