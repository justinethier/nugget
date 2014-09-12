;(begin
;  (set! make-adder
;    (lambda (x.1) (lambda (y.2) (%+ x.1 y.2))))
;  (set! increment (make-adder 1))
;  (%display (increment 41)))
(letrec ((make-adder
        (lambda (x.1) (lambda (y.2) (cons x.1 y.2))))
      (increment (make-adder #t)))
  (display (increment #f)))
