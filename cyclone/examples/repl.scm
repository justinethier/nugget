(write *Cyc-version-banner*)
(define (loop)
  (display "cyclone> ")
  (display (eval (read (current-input-port))))
  (display #\newline)
  (loop))

(loop)
