;; This file is a staging/integration area for functionality that is not 
;; supported yet by cyclone
;;

;; Illustrates the problem we are having with compiling parser.
;; how to handle the internal define p?
;; CPS conversion is trying to wrap p with a lambda, which is not going to
;; work because callers want to pass a,b,c directly.
(define test 1)
(define (glob a b c)
  (define (p d)
    (write (list a b c d)))
  (p 4))
(glob 1 2 3)
test

;'a/test
;(write 'a/test)
;'a
;'b
;'c
;(write
;  (list
;    (number->string (+ 1 2))
;    (string->list "test")
;    (string->symbol "a-b-c-d")
;    (symbol->string 'a/test-01)
;    (eq? 'a-1 'a-1)
;    (eq? (string->symbol "aa") 'aa)
;    (equal? (string->symbol "aa") 'aa)
;  ))
;
;(let ((fp (open-input-file "dev.scm")))
;  (display (read-char fp))
;  (display (peek-char fp))
;  (display (read-char fp))
;  (display (read-char fp))
;  (display (peek-char fp))
;  (display (read-char fp))
;  (display (read-char fp))
;  (display (read-char fp))
;  (display (read-char fp))
;  (close-input-port fp))
;
; WIP TODO: move working functions above into the unit-test file
;
; read - can this be derived from parser.scm implementation???

; idea - booleans
; do we want to have a separate def macro for them? IE, have the
; literals be something like bool_t / bool_f to prevent collisions
; with, for example, 't ??
;
; farther off but along the same lines, how to support compilation of
; multiple scheme files into multiple C modules?
;
; also, there is a TON of stuff in the TODO file. need to get back to that
