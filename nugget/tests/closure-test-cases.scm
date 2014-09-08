;; Test cases for closure-conversion phase, based on
;; results from using the 90 min scheme->c compiler
(load "compiler.scm")
(define (test label ast expected)
  (let ((actual (closure-convert ast)))

  (if (equal? expected actual)
    (write `(PASSED ,test))
    (write `(FAILSED ,test ,actual)))))

(test
  "add.scm"
  '((lambda (r.3) ((lambda (x.1) ((lambda (r.2) (%halt r.2)) x.1)) r.3)) (+ 41 1))
  ' (lambda () ((lambda (r.3) ((lambda (x.1) ((lambda (r.2) (%halt r.2)) x.1)) r.3)) (+ 41 1))))

