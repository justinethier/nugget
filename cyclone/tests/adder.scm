;(define make-adder
;    (lambda (x)
;        (lambda (y) (+ x  y))))
;(define increment (make-adder 1))
;(increment 41) ; => 42
;(define decrement (make-adder -1))
;(decrement 42) ; => 41

(set! make-adder
    (lambda (x)
        (lambda (y) (+ x  y))))
(set! increment (make-adder 1))
(display (increment 41))
