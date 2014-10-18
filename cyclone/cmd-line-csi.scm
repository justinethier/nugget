#!/usr/bin/env csi
;(define (main arguments)
;  (for-each display-file (cdr arguments))
;  0)

(display (command-line-arguments))
(display "\n")
(exit)

;(define (display-file filename)
;  (call-with-input-file filename
;    (lambda (port)
;      (let loop ()
;    (let ((thing (read-char port)))
;      (if (not (eof-object? thing))
;          (begin
;        (write-char thing)
;        (loop))))))))
