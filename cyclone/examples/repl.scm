(define (loop)
  (display (eval (read (current-input-port))))
  (display #\newline)
  (loop))

(loop)
