(define (%closure . args)
  (let ((clo (list->vector args)))
    

(define (%closure-ref clo idx
(define (%halt x)
  (exit))

; Test code from matt might, may need to tweak per corresponding
; functionality in the MTA C runtime
; Suitable definitions for the cell functions:
(define (cell value) (lambda (get? new-value) 
                       (if get? value (set! value new-value))))
(define (set-cell! c v) (c #f v))
(define (cell-get c) (c #t #t))
; END matt might

(define (test-set)
  ((lambda (x$684)
     ((lambda (x$684)
        ((%closure
           (lambda (self$687 r$686)
             ((lambda (r$685) (%halt r$685))
              (display (cell-get (%closure-ref self$687 1)))))
           x$684)
         (set-cell! x$684 #t)))
      (cell x$684)))
   #f))

;; Transformed scheme code from if.scm
(define (test-if)
    ((lambda (k$699) (if #t (k$699 1) (k$699 2)))
     (lambda (r$698)
       ((lambda (r$689)
          ((lambda ($_$684)
             ((lambda (k$697) (if #f (k$697 1) (k$697 2)))
              (lambda (r$696)
                ((lambda (r$690)
                   ((lambda ($_$685)
                      ((lambda (k$694)
                         ((lambda (r$695)
                            (if r$695 (k$694 (+ 3 4)) (k$694 (* 3 4))))
                          (+ 1 2)))
                       (lambda (r$691)
                         ((lambda ($_$686)
                            ((lambda (k$692)
                               ((lambda (x$687)
                                  ((lambda (r$693)
                                     (if r$693 (k$692 (+ 1 1)) (k$692 (* 0 0))))
                                   (+ x$687 1)))
                                0))
                             (lambda (r$688) (%halt r$688))))
                          r$691))))
                    r$690))
                 (display r$696)))))
           r$689))
        (display r$698)))))

