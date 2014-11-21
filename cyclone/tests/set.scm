;(set! x 1)
;(letrec ((x #f))
((lambda (x)
    (set! x #t) ; (+ 2 (* 3 4)))
    (display x))
 #f)

(define a '(#f #f))
(define b '(#f . #f))

(set-car! a 1)
(set-cdr! a '(2))
(write a)
(set-cdr! a 2)

(set-car! b '(#t))
(set-cdr! b '#t)

(write a)
(write b)
