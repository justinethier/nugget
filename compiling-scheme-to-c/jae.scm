; TODO: copy scheme-to-c here and begin to integrate

(define (read-lines ast)
    (let ((x (read)))
        (if (eof-object? x)
            (reverse ast)
            (begin
                (write x)
                (read-lines (cons x ast))))))

