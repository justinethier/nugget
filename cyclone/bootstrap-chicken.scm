;; CHICKEN imports
(require-extension extras) ;; pretty-print
(require-extension chicken-syntax) ;; when

;; Read all s-expressions from file
(define (read-all)
  (letrec ((read-next-sexp 
    (lambda (result)
      (let ((obj (read)))
        (if (eof-object? obj)
            ;(begin
            ;  (close-input-port fp)
            ;  result)
            result
            (read-next-sexp 
              (cons obj result)))))))
    (reverse
      (read-next-sexp (list)))))

