;; ; Adding examples here as one-line baby steps for the compiler
;; ;#t ; Bare minimum 
;; ;(display ((lambda (x) x) (cons #t #f)))
;; ;; TODO: (display "\n")
;; ;(display ((lambda (x) x) (cons #t (cons #t #f))))
;; ; (display #t)
;; ; (display #f)
;; ;(if #t (display #t) (display #f))
;; ; 1
;; ; 'done
;; ;(display 1)
;; ;(display (cons (cons 1 2) 3))
;; (display (+ 1 (* 2 (- 3 4))))
;; ;(display (+ 1 2 3))
;; ; (display '(1 2 3)
;; ; (display '(a b c)
;; ; TODO: actual closure demonstration
;; ; TODO: variable demonstration
;; ; TODO: if demonstration with a variable
;; ; TODO: loop demonstration
;; ;(lambda (x) (tautology (rewrite x) 1 2)) ; Incomplete snippet from boyer
;; (display 'hello)
;; (display 'hello-world)
;; (display (car '(1 . 2)))
;; (display (cdr '(1 . 2)))
;; (display (cdr '(1 2)))
;; (display (cadr '(1 2)))
;; ;TODO: (display (eval 2))
;; 
;; (display ''''test)
;; (display 'test)

;(define (list objs)  objs)
;(define (list . objs)  objs)
(define list (lambda objs  objs))
;; Equivalent to:
;;  (define <variable>
;;    (lambda <formal> <body>))
;; So => (define list (lambda objs objs))
;;
;; We also have syntax with req'd / optional args, EG: 
;;(define (list obj1 . objs)  objs)
;; Equivalent to:
;;  (define <variable>
;;    (lambda (<formals>) <body>))
;; So => (define list (lambda (obj1 . objs) objs))
;;
;; Previous notes on variadic functions:
;;
;; TODO: this could expand into (lambda objs objs)
;; also possible (but not for above) - (lambda (a b . c) ...)
;; a and b are required, c is optional.
;;
;; but, in order to call into a varargs func, num args must
;; be provided... will need to provide that. this may eventually
;; require passing numargs to all functions, like how Chicken
;; structures its functions.
(write (list 42 1))
(write (list 42 1 2))
;(write (list 1 2 3))
;(write (list))
;(write (list 'a '((b)) 'c))
