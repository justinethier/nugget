
(call/cc
  (lambda (k) ; call it 'return' and things break... uh oh
    (begin
      (display 1)
      (display 2)
      (k 2)
      (display 3)
      (display 4))))
