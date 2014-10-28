; TODO: once this works, make it work with (define) syntax
;(define (fac n)
(set! fac (lambda (n)
    (if (zero? n)
        1
        (* n (fac (- n 1))))))
(display (fac 10))
;(newline)
