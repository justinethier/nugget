;; Test cases for closure-conversion phase, based on
;; results from using the 90 min scheme->c compiler
(import (husk pretty-print))
(load "compiler.scm")
(define (test label ast expected)
  (let ((actual (closure-convert ast)))

  (if (equal? expected actual)
    (write `(PASSED ,label))
    (begin
        (display `(FAILED ,label))
        (newline)
        (display "Expected: ")
        (display (pretty-print expected))
        (display "Actual: ")
        (display (pretty-print actual))))))

(test
  "add.scm"
  '((lambda (r.3) ((lambda (x.1) ((lambda (r.2) (%halt r.2)) x.1)) r.3)) (+ 41 1))
  ' (lambda () ((lambda (r.3) ((lambda (x.1) ((lambda (r.2) (%halt r.2)) x.1)) r.3)) (+ 41 1))))

(test
  "begin.scm"
  '((lambda (r.2) ((lambda (r.3) ((lambda (r.4) ((lambda (r.1) (%halt r.1)) (+ 3 4))) (+ 1 2))) 2)) 1)
  '(lambda () ((lambda (r.2) ((lambda (r.3) ((lambda (r.4) ((lambda (r.1) (%halt r.1)) (+ 3 4))) (+ 1 2))) 2)) 1)))

(test
  "display.scm"
  '((lambda (r.1) (%halt r.1)) (display #t))
  '(lambda () ((lambda (r.1) (%halt r.1)) (display #t))))

(test
  "if.scm"
  '((lambda (k.6) ((lambda (r.7) (if r.7 (k.6 (+ 3 4)) (k.6 (* 3 4)))) (+ 1 2)))
   (lambda (r.3) ((lambda (k.4) ((lambda (x.1) ((lambda (r.5) (if r.5 (k.4 (+ 1 1)) (k.4 (* 0 0)))) (+ x.1 1))) 0)) (lambda (r.2) (%halt r.2)))))
  '(lambda ()
    ((lambda (k.6) ((lambda (r.7) (if r.7 ((%closure-ref k.6 0) k.6 (+ 3 4)) ((%closure-ref k.6 0) k.6 (* 3 4)))) (+ 1 2)))
     (%closure
      (lambda (self$1 r.3)
        ((lambda (k.4) ((lambda (x.1) ((lambda (r.5) (if r.5 ((%closure-ref k.4 0) k.4 (+ 1 1)) ((%closure-ref k.4 0) k.4 (* 0 0)))) (+ x.1 1))) 0))
         (%closure (lambda (self$2 r.2) (%halt r.2)))))))))

(test 
    "prim.scm"
    '((lambda (r.2) ((lambda (r.3) ((lambda (r.1) (%halt r.1)) (+ r.2 r.3))) (* 3 4))) (+ 1 1))
    '(lambda () ((lambda (r.2) ((lambda (r.3) ((lambda (r.1) (%halt r.1)) (+ r.2 r.3))) (* 3 4))) (+ 1 1))))

(test
    "set.scm"
    '((lambda (r.3) ((lambda (r.2) ((lambda (r.1) (%halt r.1)) (set! x r.2))) (+ 2 r.3))) (* 3 4))
    '(lambda () ((lambda (r.3) ((lambda (r.2) ((lambda (r.1) (%halt r.1)) (set! x r.2))) (+ 2 r.3))) (* 3 4))))

(test
    "adder.scm"
   '((lambda (r.8)
       ((lambda (r.4) (make-adder (lambda (r.7) ((lambda (r.5) (increment (lambda (r.6) ((lambda (r.3) (%halt r.3)) (display r.6))) 41)) (set! increment r.7))) 1)) (set! make-adder r.8)))
     (lambda (k.9 x.1) (k.9 (lambda (k.10 y.2) (k.10 (+ x.1 y.2))))))
   '(lambda ()
      ((lambda (r.8)
         ((lambda (r.4)
            ((%closure-ref make-adder 0)
             make-adder
             (%closure
              (lambda (self$5 r.7) ((lambda (r.5) ((%closure-ref increment 0) increment (%closure (lambda (self$6 r.6) ((lambda (r.3) (%halt r.3)) (display r.6)))) 41)) (set! increment r.7))))
             1))
          (set! make-adder r.8)))
       (%closure (lambda (self$3 k.9 x.1) ((%closure-ref k.9 0) k.9 (%closure (lambda (self$4 k.10 y.2) ((%closure-ref k.10 0) k.10 (+ (%closure-ref self$4 1) y.2))) x.1)))))))
    
