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
; TODO: use display, output without surrounding quotes
(write "All test passed")
;;
