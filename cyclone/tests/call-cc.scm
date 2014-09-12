;; Example of an escape continuation
(call/cc
  (lambda (k) ; call it 'return' and things break... uh oh
              ; real compiler needs to prevent creating identifiers
              ; that are C keywords
    (begin
      (display 1)
      (display 2)
      (k 2)
      (display 3)
      (display 4))))

