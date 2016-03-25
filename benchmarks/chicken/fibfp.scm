;;; FIBFP -- Computes fib(35) using floating point

#; (import (scheme base)
        (scheme read)
        (scheme write)
        (scheme time))

(define (fibfp n)
  (if (< n 2.)
    n
    (+ (fibfp (- n 1.))
       (fibfp (- n 2.)))))

(define (main)
  (let* ((count (read))
         (input (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input))
         (name "fibfp"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (fibfp (hide count input)))
     (lambda (result) (= result output)))))


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
