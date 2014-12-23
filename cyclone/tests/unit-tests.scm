(define (assert msg actual expected)
  (if (not (equal? actual expected))
      (error "Unit test failed [" msg "] actual [" actual "] expected [" expected "]")))

(assert "small reverse" (reverse '(1 2)) '(2 1))
;;  (reverse '(a b c))
;;  (reverse '(1 2 3 4 5 6 7 8 9 10))
;;  ;TODO: improper list, this is an error: (reverse '(1 . 2))
;;  (char-whitespace? #\space)
;;  (char-whitespace? #\a)
;;  (char-numeric? #\1)
;;  (char-numeric? #\newline)))
;;
