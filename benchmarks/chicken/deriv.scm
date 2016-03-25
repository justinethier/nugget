;;; DERIV -- Symbolic derivation.

#; (import (scheme base)
        (scheme cxr)
        (scheme read)
        (scheme write)
        (scheme time))

;;; Returns the wrong answer for quotients.
;;; Fortunately these aren't used in the benchmark.

(define (deriv a)
  (cond ((not (pair? a))
         (if (eq? a 'x) 1 0))
        ((eq? (car a) '+)
         (cons '+
               (map deriv (cdr a))))
        ((eq? (car a) '-)
         (cons '-
               (map deriv (cdr a))))
        ((eq? (car a) '*)
         (list '*
                a
                (cons '+
                      (map (lambda (a) (list '/ (deriv a) a)) (cdr a)))))
        ((eq? (car a) '/)
         (list '-
               (list '/
                     (deriv (cadr a))
                     (caddr a))
               (list '/
                     (cadr a)
                     (list '*
                           (caddr a)
                           (caddr a)
                           (deriv (caddr a))))))
        (else
         (error #f "No derivation method available"))))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s (number->string count))
         (name "deriv"))
    (run-r7rs-benchmark
     (string-append name ":" s)
     count
     (lambda () (deriv (hide count input1)))
     (lambda (result) (equal? result output)))))

;;; The following code is appended to all benchmarks.

;;; Given an integer and an object, returns the object
;;; without making it too easy for compilers to tell
;;; the object will be returned.

(define (hide r x)
  (call-with-values
   (lambda ()
     (values (vector values (lambda (x) x))
             (if (< r 100) 0 1)))
   (lambda (v i)
     ((vector-ref v i) x))))

;;; Given the name of a benchmark,
;;; the number of times it should be executed,
;;; a thunk that runs the benchmark once,
;;; and a unary predicate that is true of the
;;; correct results the thunk may return,
;;; runs the benchmark for the number of specified iterations.

(define (run-r7rs-benchmark name count thunk ok?)

  ;; Rounds to thousandths.
  (define (rounded x)
    (/ (round (* 1000 x)) 1000))

  (display "Running ")
  (display name)
  (newline)
  ;(flush-output-port)
  (let* ((j/s 1 #;(jiffies-per-second))
         (t0 1 #;(current-second))
         (j0 1 #;(current-jiffy)))
    (let loop ((i 0)
               (result (if #f #f)))
      (cond ((< i count)
             (loop (+ i 1) (thunk)))
            ((ok? result)
             (let* ((j1 1 #;(current-jiffy))
                    (t1 1 #;(current-second))
                    (jifs (- j1 j0))
                    (secs (exact->inexact (/ jifs j/s)))
                    (secs2 (rounded (- t1 t0))))
               (display "Elapsed time: ")
               (write secs)
               (display " seconds (")
               (write secs2)
               (display ") for ")
               (display name)
               (newline))
             result)
            (else
             (display "ERROR: returned incorrect result: ")
             (write result)
             (newline)
             result)))))

(main)
