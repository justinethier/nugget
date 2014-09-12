;; Recursion example:
(letrec ((fnc (lambda (i) 
                (begin
                    (display i)
                    (if i (fnc (- i 1)) 0)))))
    (fnc 10))
