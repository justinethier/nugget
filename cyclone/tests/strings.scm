(define a "0123456789")
(define b "abcdefghijklmnopqrstuvwxyz")
(define c "hello, world!")
(define d '(#\( #\" #\a #\b #\c #\" #\)))

(write c)
(write a)
(write b)
(write d)
(write d) ;; Test GC
(write d) ;; Test GC
; (write (string-append a b c))

(set! a "hello 2")
(write a)
(write b)
(write c)
