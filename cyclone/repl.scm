
(define (read-all fp)
  (define (loop fp result)
    (let ((obj (read fp)))
      (if (eof-object? obj)
        (reverse result)
        (loop fp (cons obj result)))))
  (loop fp '()))

(define fp (open-input-file "repl.scm"))
(write (read-all fp))
