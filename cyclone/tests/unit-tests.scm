(define *num-passed* 0)
(define (assert msg actual expected)
  (if (not (equal? actual expected))
      (error "Unit test failed [" msg "] actual [" actual "] expected [" expected "]")
      (set! *num-passed* (+ *num-passed* 1))))

;; Adder example
(define (make-adder x)
  (lambda (y) (+ x  y)))
(define increment (make-adder +1))
(assert "Adder #1" (increment 41) 42)
(define decrement (make-adder -1))
(assert "Adder #2" (decrement 42) 41)

(assert "Application example"
  ((lambda (x) x) (+ 41 1))
  42)

;; Apply section
(assert "" (apply length '((#t #f))) 2)
(assert "" (apply cons '(#t #f)) '(#t . #f))
(assert "" (apply cadr (list (list 1 2 3 4))) 2)
(assert "" (apply null? (list '())) #t)
;; Varargs
(define (list2 a b . objs) objs)
(assert "apply varargs" (list 42 1) '(42 1))
(assert "apply varargs" (list 42 1 2) '(42 1 2))
(assert "apply varargs" (list2 42 1) '())
(assert "apply varargs" (list2 42 1 2) '(2))

(assert "begin" (begin 1 2 (+ 1 2) (+ 3 4)) 7)

;; Closure section
(assert "simple closure"
  (((lambda (x.1) 
    (lambda (y.2) 
      (cons x.1 y.2))) #t) #f)
 '(#t . #f))
(assert "closure #2"
  ((lambda (x y)
    ((lambda () (- x y)))) 5 4)
  1)

;; Factorial
(define (fac n) (if (= n 0) 1 (* n (fac (- n 1)))))
(assert "Factorial example" (fac 10) 3628800)

;; If section
(assert "if example" (if #t 1 2) 1)
(assert "if example" (if #f 1 2) 2)
(assert "if example" (if (+ 1 2) (+ 3 4) (* 3 4)) 7)
(assert "if" (if ((lambda (x) (+ x 1)) 0) (+ 1 1) (* 0 0)) 2)
(assert "no else clause" (if #t 'no-else-clause) 'no-else-clause)

(assert "" (+ (+ 1 1) (* 3 4)) 14)

;; Set section
((lambda (x)
    (set! x #t) ; (+ 2 (* 3 4)))
    (assert "set local x" x #t))
 #f)

(define a '(#f #f))
(define b '(#f . #f))

(set-car! a 1)
(set-cdr! a '(2))
(assert "set car/cdr a" a '(1 2))
(set-cdr! a 2)
(set-car! b '(#t))
(set-cdr! b '#t)

(assert "set a" a '(1 . 2))
(assert "set b" b '((#t) . #t))

;; Square example
(let ((x 10) 
      (y 20) 
      (square (lambda (x) (* x x)))) 
  (begin 
    (assert "square x" (square x) 100) 
    (assert "square y" (square y) 400)))

(assert "numeric small reverse" (reverse '(1 2)) '(2 1))
(assert "small reverse" (reverse '(a b c)) '(c b a))
(assert "larger reverse" (reverse '(1 2 3 4 5 6 7 8 9 10)) '(10 9 8 7 6 5 4 3 2 1))
;;  ;TODO: improper list, this is an error: (reverse '(1 . 2))
(assert "char whitespace" (char-whitespace? #\space) #t)
(assert "char whitespace" (char-whitespace? #\a) #f)
(assert "char numeric" (char-numeric? #\1) #t)
(assert "char numeric" (char-numeric? #\newline) #f)
(assert "" (and 1 2 3) 3)
(assert "" (and #t #f 'a 'b 'c) #f)
(assert "" (or 1 2 3) 1)
(assert "" (or #f 'a 'b 'c) 'a)
(assert "" (string-append "") "")
;error - (string-append 1)
(assert "" (string-append "test") "test")
(assert "" (string-append "ab" "cdefgh ij" "klmno" "p" "q" "rs  " "tuv" "w" " x " "yz")
  "abcdefgh ijklmnopqrs  tuvw x yz")
(assert "" (string->number "0") 0)
(assert "" (string->number "42") 42)
;(assert "" (string->number "343243243232") ;; Note no bignum support
(assert "" (string->number "3.14159") 3) ;; Currently no float support
(assert "" (list->string (list #\A #\B #\C)) "ABC")
(assert "" (list->string (list #\A)) "A")
(assert "" (list->string (list)) "") 
(assert "" (integer->char 65) #\A)
(assert "" (char->integer #\a) 97)

(assert "" (number->string (+ 1 2)) "3")
(assert "" (string->list "test") '(#\t #\e #\s #\t))
(assert "" (string->symbol "a-b-c-d") 'a-b-c-d)
(assert "" (symbol->string 'a/test-01) "a/test-01")
(assert "" (eq? 'a-1 'a-1) #t)
(assert "" (eq? (string->symbol "aa") 'aa) #t)
(assert "" (equal? (string->symbol "aa") 'aa) #t)

;; Map
(assert "map 1" (map (lambda (x) (car x)) '((a . b) (1 . 2) (#\h #\w))) '(a 1 #\h))
(assert "map 2" (map car '((a . b) (1 . 2) (#\h #\w))) '(a 1 #\h))
(assert "map 3" (map cdr '((a . b) (1 . 2) (#\h #\w))) '(b 2 (#\w)))
(assert "map length"
  (map length '(() (1) (1 2) (1 2 3) (1 2 3 4)))
 '(0 1 2 3 4))

;; Prove internal defines are compiled properly
;;
;; Illustrates an old problem with compiling parser.
;; how to handle the internal define p?
;; trans was trying to wrap p with a lambda, which is not going to
;; work because callers want to pass a,b,c directly.
(define (glob a b c)
  (define (p d)
    (list a b c d))
  (p 4))
(assert "internal defs for global funcs"
        (glob 1 2 3)
       '(1 2 3 4))

;; Global shadowing issue
;; Do not allow global define to shadow local ones
(define x 'global)
((lambda ()
  (define x 1)
  ((lambda ()
    (define x 2)
    (assert "local define of x" x 2)))
  (assert "another local define of x" x 1)))
(assert "global define of x" x 'global)

; TODO: could add parser tests for these
;(
;123(list)
;1'b
;(write
;  (list
;  1;2
;  ))
;1;2
;3"four five"
;#\space
;)

;; EVAL section
(define x 1)
(define y 2)
(define *z* 3)
;(write (eval '(Cyc-global-vars)))
(assert "eval compiled - x" (eval 'x) x)
(eval '(set! x 'mutated-x))
(assert "Access var with a mangled name" (eval '*z*) *z*)
(assert "Access compile var mutated by eval" x 'mutated-x)
;; END eval

; TODO: use display, output without surrounding quotes
(write (list *num-passed* " tests passed with no errors"))
;;
