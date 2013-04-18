;(lambda ()
    (define square
      (lambda (x)
        (* x x)))
    
    (+ (square 5) 1)
;)

; After CPS:
;(let ((r.2 (lambda (k.3) 
;            (let ((r.6 (lambda (k.7 x.1) 
;                         (k.7 (%* x.1 x.1))))) 
;                 (let ((r.4 (set! square r.6))) 
;                    (square (lambda (r.5) 
;                                (k.3 (%+ r.5 1))) 
;                            5))))))
;  (%halt r.2))
