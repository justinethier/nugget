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

;;"prim.scm"
;;((lambda (r.2) ((lambda (r.3) ((lambda (r.1) (%halt r.1)) (+ r.2 r.3))) (* 3 4))) (+ 1 1))
;;-------------------------- AST AFTER CLOSURE-CONVERSION:
;;(lambda () ((lambda (r.2) ((lambda (r.3) ((lambda (r.1) (%halt r.1)) (+ r.2 r.3))) (* 3 4))) (+ 1 1)))
;;
;;"set.scm"
;;(x (lambda (r.7)
;;     (r.7 (lambda (r.2)
;;            ((lambda (r.6)
;;               ((lambda (r.5) ((lambda (r.3) ((lambda (r.4) (letrec (lambda (r.1) (%halt r.1)) r.2 r.3 r.4)) (%display x))) (set! x r.5))) (+ 2 r.6)))
;;             (* 3 4)))))
;;   #f)
;;-------------------------- AST AFTER CLOSURE-CONVERSION:
;;(lambda ()
;;  ((%closure-ref x 0)
;;   x
;;   (%closure
;;    (lambda (self.8 r.7)
;;      ((%closure-ref r.7 0)
;;       r.7
;;       (%closure
;;        (lambda (self.9 r.2)
;;          ((lambda (r.6)
;;             ((lambda (r.5)
;;                ((lambda (r.3) ((lambda (r.4) ((%closure-ref letrec 0) letrec (%closure (lambda (self.10 r.1) (%halt r.1))) r.2 r.3 r.4)) (%display x)))
;;                 (set! x r.5)))
;;              (+ 2 r.6)))
;;           (* 3 4)))))))
;;   #f))
;;
