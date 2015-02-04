(define (loop)
  (display (eval (read)))
  (display #\newline)
  (loop))

(loop)
