((define fp (open-input-file "dev.scm")))
(let loop ((obj (read fp)))
  (if (not (eof-object? obj))
    (begin
      (write obj)
      (loop (read fp)))))
