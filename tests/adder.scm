; Closure conversion example from:
; http://lampwww.epfl.ch/teaching/archive/advanced_compiler/2007/resources/slides/act-2007-05-closure-conversion.pdf
(define make-adder
 (lambda (x)
 (lambda (y) (+ x y))))
(define increment (make-adder 1))
(increment 41) ; => 42
(define decrement (make-adder -1))
(decrement 42) ; => 41
