(write (apply length '((#t #f))))
(write (apply cons '(#t #f)))
(apply cadr (list (list 1 2 3 4)))
(write (apply null? (list '())))
;(write (apply + '(10 20))) ; may need to change representation of symbols to make this work

;; Varargs
(define (list . objs)  objs)
(define (list2 a b . objs) objs)
(write (list 42 1))
(write (list 42 1 2))
(write (list2 42 1))
(write (list2 42 1 2))
