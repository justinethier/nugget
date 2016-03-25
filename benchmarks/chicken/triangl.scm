;;; TRIANGL -- Board game benchmark.
   
#; (import (scheme base)
        (scheme read)
        (scheme write)
        (scheme time))

(define *board*
  (list->vector '(1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1)))

(define *sequence*
  (list->vector '(0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

(define *a*
  (list->vector '(1 2 4 3 5 6 1 3 6 2 5 4 11 12
                  13 7 8 4 4 7 11 8 12 13 6 10
                  15 9 14 13 13 14 15 9 10
                  6 6)))

(define *b*
  (list->vector '(2 4 7 5 8 9 3 6 10 5 9 8
                  12 13 14 8 9 5 2 4 7 5 8
                  9 3 6 10 5 9 8 12 13 14
                  8 9 5 5)))

(define *c*
  (list->vector '(4 7 11 8 12 13 6 10 15 9 14 13
                  13 14 15 9 10 6 1 2 4 3 5 6 1
                  3 6 2 5 4 11 12 13 7 8 4 4)))

(define *answer* '())
 
(define (attempt i depth)
  (cond ((= depth 14)
         (set! *answer*
               (cons (cdr (vector->list *sequence*)) *answer*))
         #t)
        ((and (= 1 (vector-ref *board* (vector-ref *a* i)))
              (= 1 (vector-ref *board* (vector-ref *b* i)))
              (= 0 (vector-ref *board* (vector-ref *c* i))))
         (vector-set! *board* (vector-ref *a* i) 0)
         (vector-set! *board* (vector-ref *b* i) 0)
         (vector-set! *board* (vector-ref *c* i) 1)
         (vector-set! *sequence* depth i)
         (do ((j 0 (+ j 1))
              (depth (+ depth 1)))
             ((or (= j 36) (attempt j depth)) #f))
         (vector-set! *board* (vector-ref *a* i) 1)
         (vector-set! *board* (vector-ref *b* i) 1)
         (vector-set! *board* (vector-ref *c* i) 0) #f)
        (else #f)))
 
(define (test i depth)
  (set! *answer* '())
  (attempt i depth)
  (car *answer*))
 
(define (main)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (output (read))
         (s3 (number->string count))
         (s2 (number->string input2))
         (s1 (number->string input1))
         (name "triangl"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2 ":" s3)
     count
     (lambda () (test (hide count input1) (hide count input2)))
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
