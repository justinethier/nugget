(define *num-passed* 0)
(define (assert msg actual expected)
  (if (not (equal? actual expected))
      (error "Unit test failed [" msg "] actual [" actual "] expected [" expected "]")
      (set! *num-passed* (+ *num-passed* 1))))

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

; TODO: use display, output without surrounding quotes
(write (list *num-passed* " tests passed with no errors"))
;;
