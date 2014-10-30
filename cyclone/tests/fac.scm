; TODO: once this works, make it work with (define) syntax
;(define (fac n)
(set! fac (lambda (n)
    (if (= 0 n)
    ;; TODO: compiler should be able to detect an error using
    ;; below, since (zero?) is not defined
    ;(if (zero? n)
        1
        (* n (fac (- n 1))))))
(display (fac 10))
; TODO: crashes the application:
;(display (fac 1000000))
;(newline)
