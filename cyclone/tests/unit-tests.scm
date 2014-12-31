(define (assert msg actual expected)
  (if (not (equal? actual expected))
      (error "Unit test failed [" msg "] actual [" actual "] expected [" expected "]")))

; TODO: this should pass:
;(assert "numeric small reverse" (reverse '(1 2)) '(2 1))
(assert "small reverse" (reverse '(a b c)) '(c b a))
;TODO: same as above -(assert "larger reverse" (reverse '((1 2 3 4 5 6 7 8 9 10))) '(10 9 8 7 6 5 4 3 2 1))
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

; TODO: use display, output without surrounding quotes
(write "All test passed")
;;
