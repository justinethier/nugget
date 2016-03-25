;;; TAK -- A vanilla version of the TAKeuchi function.
 
#; (import (scheme base)
        (scheme read)
        (scheme write)
        (scheme time))

(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (input3 (read))
         (output (read))
         (s4 (number->string count))
         (s3 (number->string input3))
         (s2 (number->string input2))
         (s1 (number->string input1))
         (name "tak"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2 ":" s3 ":" s4)
     count
     (lambda ()
       (tak (hide count input1) (hide count input2) (hide count input3)))
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
