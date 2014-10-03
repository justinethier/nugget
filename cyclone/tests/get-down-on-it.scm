(define (get-down-on-it x)
    (define (out-of-sight) (+ x y))
    (set! x (* x 2))
    out-of-sight)
