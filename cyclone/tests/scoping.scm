;; Test scoping when vars have conflicting names
;; currently blows up in cyclone
;(define x #f)
(set! x #f)
(display 1)
(write x)
((lambda (x)
    (display 2)
    (write x)
 ) #t)
