;;
;; A test framework to attempt to make it easier to debug generated programs.
;; The idea is to allow execution of Scheme code that has been transformed
;; using cyclone's source-to-source transformations. If the code executes
;; OK here, then it should execute fine after being transformed into C code.
;; Unless of course there is a bug here (hopefully not) or in the Scheme->C
;; compiler.
;;

;; Return a function that can be called directly to 
;; invoke the closure, or indirectly to access closure
;; elements.
;;
;; When called directly, the first arg is the closure
;; itself (self), followed by args passed when the 
;; closure was defined.
(define (%closure . clo-args)
  (define clo-data (list->vector clo-args))
  (define clo
    (lambda args
      (cond 
        ((and (> (length args) 1)
              (equal? 'ref (car args)))
         (vector-ref clo-data (cadr args)))
        (else
         (apply 
           (car clo-args) 
           (cons clo args))))))
   clo)

(define (%closure-ref clo idx)
  (clo 'ref idx))
(define (%halt x)
  (exit))

;; Test code from matt might, may need to tweak per corresponding
;; functionality in the MTA C runtime
;; Suitable definitions for the cell functions:
(define (cell value) (lambda (get? new-value) 
                       (if get? value (set! value new-value))))
(define (set-cell! c v) (c #f v))
(define (cell-get c) (c #t #t))
;; END matt might

(define (test-fac)
((lambda (fac)
   ((lambda (fac)
      ((%closure
         (lambda (self$698 r$689)
           ((%closure
              (lambda (self$699 r$687)
                ((%closure
                   (lambda (self$700 $_$684)
                     ((%closure-ref (cell-get (%closure-ref self$700 1)) 0)
                      (cell-get (%closure-ref self$700 1))
                      (%closure
                        (lambda (self$701 r$688)
                          ((lambda (r$686) (%halt r$686)) (display r$688))))
                      10))
                   (%closure-ref self$699 1))
                 r$687))
              (%closure-ref self$698 1))
            (set-cell! (%closure-ref self$698 1) r$689)))
         fac)
       (%closure
         (lambda (self$694 k$690 n$685)
           ((%closure
              (lambda (self$695 r$691)
                (if r$691
                  ((%closure-ref (%closure-ref self$695 2) 0)
                   (%closure-ref self$695 2)
                   1)
                  ((%closure
                     (lambda (self$696 r$693)
                       ((%closure-ref (cell-get (%closure-ref self$696 1)) 0)
                        (cell-get (%closure-ref self$696 1))
                        (%closure
                          (lambda (self$697 r$692)
                            ((%closure-ref (%closure-ref self$697 1) 0)
                             (%closure-ref self$697 1)
                             (* (%closure-ref self$697 2) r$692)))
                          (%closure-ref self$696 2)
                          (%closure-ref self$696 3))
                        r$693))
                     (%closure-ref self$695 1)
                     (%closure-ref self$695 2)
                     (%closure-ref self$695 3))
                   (- (%closure-ref self$695 3) 1))))
              (%closure-ref self$694 1)
              k$690
              n$685)
            (= 0 n$685)))
         fac)))
    (cell fac)))
 #f))
(test-fac)

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
;(test-set)

(define (test-adder)
    ((lambda (increment make-adder)
       ((%closure
          (lambda (self$696 increment)
            ((%closure
               (lambda (self$697 make-adder)
                 ((%closure
                    (lambda (self$700 r$693)
                      ((%closure
                         (lambda (self$701 r$689)
                           ((%closure
                              (lambda (self$702 $_$684)
                                ((%closure-ref
                                   (cell-get (%closure-ref self$702 2))
                                   0)
                                 (cell-get (%closure-ref self$702 2))
                                 (%closure
                                   (lambda (self$703 r$692)
                                     ((%closure
                                        (lambda (self$704 r$690)
                                          ((%closure
                                             (lambda (self$705 $_$685)
                                               ((%closure-ref
                                                  (cell-get (%closure-ref self$705 1))
                                                  0)
                                                (cell-get (%closure-ref self$705 1))
                                                (%closure
                                                  (lambda (self$706 r$691)
                                                    ((lambda (r$688) (%halt r$688)) (display r$691))))
                                                41))
                                             (%closure-ref self$704 1))
                                           r$690))
                                        (%closure-ref self$703 1))
                                      (set-cell! (%closure-ref self$703 1) r$692)))
                                   (%closure-ref self$702 1))
                                 1))
                              (%closure-ref self$701 1)
                              (%closure-ref self$701 2))
                            r$689))
                         (%closure-ref self$700 1)
                         (%closure-ref self$700 2))
                       (set-cell! (%closure-ref self$700 2) r$693)))
                    (%closure-ref self$697 1)
                    make-adder)
                  (%closure
                    (lambda (self$698 k$694 x$686)
                      ((%closure-ref k$694 0)
                       k$694
                       (%closure
                         (lambda (self$699 k$695 y$687)
                           ((%closure-ref k$695 0)
                            k$695
                            (+ (%closure-ref self$699 1) y$687)))
                         x$686))))))
               increment)
             (cell (%closure-ref self$696 1))))
          make-adder)
        (cell increment)))
     #f
     #f))
;(test-adder)

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

