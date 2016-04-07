;;; TAIL -- One of the Kernighan and Van Wyk benchmarks.
;;;
;;; Modified for R7RS by Will Clinger.
;;;
;;; The key idea of this benchmark is that, for each iteration,
;;; the entire input is read line by line before any output
;;; is produced, and the lines are then written to the output
;;; in the reverse of the order in which they were read.

#;(import (scheme base)
        (scheme file)
        (scheme read)
        (scheme write)
        (scheme time))

(define (tail-r-aux port file-so-far)
  (let ((x (read-line port)))
    (if (eof-object? x)
        file-so-far
        (tail-r-aux port (cons x file-so-far)))))

(define (echo-lines-in-reverse-order in out)
  (for-each (lambda (line) (write-string line out) (newline out))
            (tail-r-aux in '())))

(define (go input output)
  (call-with-input-file
   input
   (lambda (in)
     (if (file-exists? output) (delete-file output))
     (call-with-output-file
      output
      (lambda (out)
        (echo-lines-in-reverse-order in out))))))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (output (read))
         (s3 (number->string count))
         (s2 input2)
         (s1 input1)
         (name "tail"))
    (run-r7rs-benchmark
     (string-append name ":" s3)
     count
     (lambda () (go (hide count input1) (hide count input2)))
     (lambda (result) #t))))

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
