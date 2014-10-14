;; Example of an escape continuation
(call/cc
  (lambda (return)
    (begin
      (display 1)
      (display 2)
      (return 2)
      (display 3)
      (display 4))))

