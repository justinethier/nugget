(define a "0123456789")
(define b "abcdefghijklmnopqrstuvwxyz")
(define c "hello, world!")

(write c)
(write a)
(write b)
; (write (string-append a b c))

(set! a "hello 2")
(write a)
(write b)
(write c)
