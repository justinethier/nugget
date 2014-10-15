;; This should run forever using a constant amount of memory
;; and max CPU:

;; Original program:
;; (define (foo) (bar))
;; (define (bar) (foo))
;; (foo)

; TODO: this is broken since length is supposed to work on a list, no pair
(letrec ((foo (lambda (x) (display (length x)) (bar (cons 1 x))))
         (bar (lambda (x) (foo (cons 1 x)))))
    (foo (cons 1 2)))
