;; Cyclone Scheme
;; Copyright (c) 2014, Justin Ethier
;; All rights reserved.
;;
;; This module contains the s-expression parser and supporting functions.
;;
;;
;; FUTURE: if this was a module/library, would probably only want to export
;;         read and read-all
;;
;; TODO: issue list
;;  The root cause of all these issues is when we read an extra char that
;;  causes the current token to end, but then the char that was read is lost
;;
;;  - A quote mid-object restarts the parser
;;    EG: for (read), a'b ==> (quote b)
;;  - A comment immediately after an object causes the ; to be lost
;;    EG: 1;2 ==> causes 2 to be read even though it is inside a comment
;;    This is a problem for (read-all)
;;  - abc(list) ==> (list) 


; TODO: need to address TODO's below, and try to clean up new code.
; there are still performance issues (~10%) compared to old code on 
; master. want to get this in good shape before moving on


;; Helper functions
(define (add-tok tok toks quotes)
  (define (loop i)
    (if (= quotes i)
      tok
      (cons 'quote (cons (loop (+ i 1)) '()))))
  (if quotes
     (cons
       (loop 0)
       toks)
     (cons tok toks)))

;; Get completed list of tokens
(define (get-toks tok toks quotes)
  (if (null? tok)
    toks
    (add-tok (->tok tok) toks quotes)))

;; Add a token to the list, quoting it if necessary
(define (->tok lst)
  (parse-atom (reverse lst)))

;; Did we read a dotted list
(define (dotted? lst) 
  (and (> (length lst) 2)
       (equal? (cadr (reverse lst)) (string->symbol "."))))

;; Convert a list read by the reader into an improper list
(define (->dotted-list lst)
  (cond
    ((null? lst) '())
    ((equal? (car lst) (string->symbol "."))
     (cadr lst))
    (else
      (cons (car lst) (->dotted-list (cdr lst))))))

(define (parse-error msg lnum cnum)
  (error
    (string-append
      "Error (line "
      (number->string lnum)
      ", char "
      (number->string cnum)
      "): "
      msg)))

;; TODO: would be best if these did not have to be global
(define *line-num* 1)
(define *char-num* 0)


;; Add finished token, if there is one, and continue parsing
(define (parse/tok fp tok toks all? comment? quotes parens)
  (cond
   ((null? tok)
    (parse fp '() toks all? comment? quotes parens))
   (all?
    (parse fp '() 
           (add-tok (->tok tok) toks quotes) 
           all?
           comment? 
           #f  ; read tok, no more quote
           parens))
   (else
     (car (add-tok (->tok tok) toks quotes)))))
     ;(reverse (add-tok (->tok tok) toks quotes)))))

;; Parse input from stream
;; Input:
;; - Port object
;; - Current token
;; - List of tokens read (if applicable)
;; - Bool - Read-all mode, or just read the next object?
;; - Bool - Are we inside a comment?
;; - Quote level
;; - Level of nested parentheses 
;; Output: next object, or list of objects (if read-all mode)
;; 
(define (parse fp tok toks all? comment? quotes parens)
;; TODO: peek-char, if it is start of a comment and we have any toks,
;; need to return those toks (if we are in a read). otherwise can get
;; into a situation where the ; is lost
  (set! *char-num* (+ 1 *char-num*))
  (let ((c (read-char fp)))
;; DEBUGGING
;(write `(DEBUG read ,tok ,c))
;(write (newline))
;; END DEBUG
    (cond
      ((eof-object? c) 
       (if (> parens 0)
           (parse-error "missing closing parenthesis" *line-num* *char-num*))
       (if all?
         (reverse (get-toks tok toks quotes))
         (let ((last (get-toks tok toks quotes)))
           (if (> (length last) 0)
             (car last)
             c)))) ;; EOF
      (comment?
       (if (eq? c #\newline)
           (begin
              (set! *line-num* (+ 1 *line-num*))
              (set! *char-num* 0)
              (parse fp '() toks all? #f quotes parens))
           (parse fp '() toks all? #t quotes parens)))
      ((char-whitespace? c)
       (if (equal? c #\newline) (set! *line-num* (+ 1 *line-num*)))
       (if (equal? c #\newline) (set! *char-num* 0))
       (parse/tok fp tok toks all? #f quotes parens))
      ((eq? c #\;)
       (parse/tok fp tok toks all? #t quotes parens))
      ((eq? c #\')
       (let ((quote-level (if quotes
                              (+ quotes 1)
                              1)))
         (if (null? tok)
             (parse fp '() toks all? comment? quote-level parens)
;; TODO: is this what we want to do if !all? or do we need to peek
;;       for the quote and return, instead of trying to read a second obj??
             (parse fp '() (add-tok (->tok tok) toks quotes) 
                           all? comment? quote-level parens))))
      ((eq? c #\()
       (let ((sub ;(_cyc-read-all fp (+ parens 1)))
                  (parse fp '() '() #t #f #f (+ parens 1)))
             (toks* (get-toks tok toks quotes)))
         (define new-toks (add-tok 
                            (if (dotted? sub)
                                (->dotted-list sub)
                                sub)
                            toks* 
                            quotes)) 
         (if all?
          (parse fp '() new-toks all? #f #f parens)
          (car new-toks))))
          ;(reverse new-toks))))
      ((eq? c #\))
       (if (= parens 0)
           (parse-error "unexpected closing parenthesis" *line-num* *char-num*))
       (reverse (get-toks tok toks quotes)))
      ((eq? c #\")
       (let ((str (read-str fp '()))
             (toks* (get-toks tok toks quotes)))
         (define new-toks (add-tok str toks* quotes))
         (if all?
          (parse fp '() new-toks all? #f #f parens)
          (reverse new-toks))))
      ((eq? c #\#)
       (if (null? tok)
         ;; # reader
         (let ((next-c (read-char fp)))
            (set! *char-num* (+ 1 *char-num*))
            (cond
              ;; Do not use add-tok below, no need to quote a bool
              ((eq? #\t next-c) (parse fp '() (cons #t toks) all? #f #f parens))
              ((eq? #\f next-c) (parse fp '() (cons #f toks) all? #f #f parens))
              ((eq? #\\ next-c)
               (let ((new-toks (cons (read-pound fp) toks)))
                 (if all?
                   (parse fp '() new-toks all? #f #f parens)
                   (reverse new-toks))))
              (else
                (parse-error "Unhandled input sequence" *line-num* *char-num*))))
         ;; just another char...
         (parse fp (cons c tok) toks all? #f quotes parens)))
      (else
        (parse fp (cons c tok) toks all? #f quotes parens)))))

;(define (_cyc-read-all fp parens)
;   (parse fp '() '() #f #f parens))

;; Read chars past a leading #\
(define (read-pound fp)
  (define (done raw-buf)
    (let ((buf (reverse raw-buf)))
      (cond 
        ((= 0 (length buf))
         (parse-error "missing character" *line-num* *char-num*))
        ((= 1 (length buf))
         (car buf))
        ((equal? buf '(#\a #\l #\a #\r #\m))
         (integer->char 7))
        ((equal? buf '(#\b #\a #\c #\k #\s #\p #\a #\c #\e))
         (integer->char 8))
        ((equal? buf '(#\d #\e #\l #\e #\t #\e))
         (integer->char 127))
        ((equal? buf '(#\e #\s #\c #\a #\p #\e))
         (integer->char 27))
        ((equal? buf '(#\n #\e #\w #\l #\i #\n #\e))
         (integer->char 10))
        ((equal? buf '(#\n #\u #\l #\l))
         (integer->char 0))
        ((equal? buf '(#\r #\e #\t #\u #\r #\n))
         (integer->char 13))
        ((equal? buf '(#\s #\p #\a #\c #\e))
         (integer->char 32))
        ((equal? buf '(#\t #\a #\b))
         (integer->char 9))
        (else
         (parse-error (string-append 
                        "unable to parse character: "
                        (list->string buf))
                      *line-num* *char-num*)))))
  (define (loop buf)
    (let ((c (peek-char fp)))
      (if (or (eof-object? c)
              (char-whitespace? c)
              (and (> (length buf) 0)
                   (equal? c #\))))
         (done buf)
         (loop (cons (read-char fp) buf)))))
  (loop '()))

(define (read-str fp buf)
  (let ((c (read-char fp)))
    ;; TODO: for now, end on raw double-quote. real scheme
    ;; strings are not quite this simple - see spec.
    (cond
      ((eof-object? c)
       (parse-error "missing closing double-quote" *line-num* *char-num*))
      ((equal? #\" c)
       (list->string (reverse buf)))
      (else
        (read-str fp (cons c buf))))))

(define (sign? c)
  (or
    (equal? c #\+)
    (equal? c #\-)))

;; parse-atom -> [chars] -> literal
(define (parse-atom a)
  (cond 
    ((or
        (char-numeric? (car a))
        (and (> (length a) 1)
             (char-numeric? (cadr a))
             (sign? (car a))))
     (string->number  ;; TODO: this is cheating! need to do this, too.
                      ;; but, it could be done by a library function
                      ;; exposed as string->number... so, ok here
       (list->string a)))
    (else
     (string->symbol
       (list->string a)))))

;; Main lexer/parser
;(define (cyc-read-all fp)
;  (set! *line-num* 1)
;  (set! *char-num* 0)
;  (_cyc-read-all fp 0))

(define cyc-read ;; TODO: should be (read), but that is breaking on csi 4.8.0.5
  (lambda (fp)
    (parse fp '() '() #f #f #f 0)))

(define (read-all fp)
  (set! *line-num* 1)
  (set! *char-num* 0)
  (define (loop fp result)
    (let ((obj (cyc-read fp)))
      (if (eof-object? obj)
        (reverse result)
        (loop fp (cons obj result)))))
  (loop fp '()))

;(let ((fp (open-input-file "tests/begin.scm")))
;(let ((fp (open-input-file "tests/strings.scm")))
;(let ((fp (open-input-file "dev.scm")))
;(let ((fp (open-input-file "eval.scm")))
;  (write (read-all fp)))
;  (write (cyc-read-all fp)))
;(let ((fp (current-input-port)))
; (write (cyc-read fp)))
; (write (cyc-read-all fp)))


;(define (display-file filename)
;  (call-with-input-file filename
;    (lambda (port)
;      (let loop ()
;    (let ((thing (read-char port)))
;      (if (not (eof-object? thing))
;          (begin
;        (write-char thing)
;        (loop))))))))
;
