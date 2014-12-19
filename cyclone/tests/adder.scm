;; Demonstrates short-hand syntax for defining a function.
;; not currently handled by cyclone
(define (make-adder x)
  (lambda (y) (+ x  y)))
;(define make-adder
;    (lambda (x)
;        (lambda (y) (+ x  y))))

;; You know, one hack for this is, if define introduces a list
;; that is not a lambda, split it up into a (define x #f) and a set!
(define test (if #t (make-adder 1 1) (make-adder 2 2))) ;; This line demonstrates issues with define and CPS


(define increment (make-adder +1))
(display (increment 41)) ; => 42
(define decrement (make-adder -1))
(display (decrement 42)) ; => 41

;(set! make-adder
;    (lambda (x)
;        (lambda (y) (+ x  y))))
;(set! increment (make-adder 1))
;(display (increment 41))
