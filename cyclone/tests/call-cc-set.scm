;; TODO: work in progress, none of this works in compiler

; General function application
; Tests from: http://tech.phillipwright.com/2010/05/23/continuations-in-scheme/
;(define handle #f)
;(define test-value #f)
;(set! test-value (+ 2 (call/cc (lambda (k) (set! handle k) 2))))
;(set! test-value (handle 20))
;(assert/equal test-value 22)
;(set! test-value (handle 100))
;(assert/equal test-value 102)

;(let ((handle #f)
;      (test-value #f))
;    (set! test-value (+ 2 (call/cc (lambda (k) (set! handle k) 2))))
;    (set! test-value (handle 20))
;    (display test-value) ;;(assert/equal test-value 22)
;    ;(set! test-value (handle 100))
;    ;; TODO: (display "\n")
;    ;(display test-value) ;;(assert/equal test-value 102)
;    )

(let ((test-cont #f)
    (a #f))
    (set! a (call/cc (lambda (c) (set! test-cont c) 1)))
    (write a) ;;(assert/equal a 1)
    ;(test-cont 2)
    (write a) ;;(assert/equal a 2)
    )
