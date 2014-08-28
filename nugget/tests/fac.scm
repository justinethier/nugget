(define (fac n)
    (if (zero? n)
        1
        (* n (fac (- n 1)))))
(display (fac 10))
;(newline)
