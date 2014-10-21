;; Helper functions
(define (add-tok tok toks quoted?)
  (if quoted?
     (cons (cons 'quote (cons tok '())) toks)
     (cons tok toks)))

;; Add a token to the list, quoting it if necessary
(define (->tok lst)
  (parse-atom (reverse lst)))

;; Get completed list of tokens
;; TODO: better name for this function?
(define (with-tok tok toks quoted?)
  (if (null? tok)
    toks
    (add-tok (->tok tok) toks quoted?)))

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

;; Main lexer/parser
;; TODO: line-num, char-num
(define (cyc-read-all fp)
  (letrec (
   ;; Keep looping, adding finished token if there is one
   (loop/tok (lambda (tok toks comment? quoted?)
                (if (null? tok)
                    (loop '() toks comment? quoted?)
                    (loop '() (add-tok (->tok tok) toks quoted?) comment? #f)))) ; read tok, no more '
   ;; Loop over input
   (loop (lambda (tok toks comment? quoted?)
    (let ((c (read-char fp)))
      (cond
        ((eof-object? c) 
         (reverse (with-tok tok toks quoted?)))
        (comment?
         (if (eq? c #\newline)
             (loop '() toks #f quoted?)
             (loop '() toks #t quoted?)))
        ((char-whitespace? c)
         (loop/tok tok toks #f quoted?))
        ((eq? c #\;)
         (loop/tok tok toks #t quoted?))
        ((eq? c #\')
         (if (null? tok)
             (loop '() toks comment? #t)
             (loop '() (add-tok (->tok tok) toks quoted?) comment? #t)))
        ((eq? c #\()
;idea is to form a new list when open paren encountered
;and to end that list upon close paren
         ;; TODO: need to error if close paren never found
         (let ((sub (cyc-read-all fp))
               (toks* (with-tok tok toks quoted?)))
            (loop 
              '() 
              (add-tok 
                (if (dotted? sub)
                    (->dotted-list sub)
                    sub)
                toks* 
                quoted?) 
              #f #f)))
        ((eq? c #\))
         ;; TODO: what if too many close parens??
         (reverse (with-tok tok toks quoted?)))
        ((eq? c #\")
         (error `(Unable to parse strings at this time)))
        ((eq? c #\#)
         (if (null? tok)
           ;; # reader
           (let ((next-c (read-char fp)))
              (cond
                ;; Do not use add-tok below, no need to quote a bool
                ((eq? #\t next-c) (loop '() (cons #t toks) #f #f))
                ((eq? #\f next-c) (loop '() (cons #f toks) #f #f))
                (else
                  (error `(Unhandled input sequence ,c ,next-c)))))
           ;; just another char...
           (loop (cons c tok) toks #f quoted?)))
        (else
          (loop (cons c tok) toks #f quoted?)))))))
   (loop '() '() #f #f)))

;; parse-atom -> [chars] -> literal
(define (parse-atom a)
  (cond 
    ((char-numeric? (car a))
     (string->number  ;; TODO: this is cheating! need to do this, too.
                      ;; but, it could be done by a library function
                      ;; exposed as string->number... so, ok here
       (list->string a)))
    (else
     (string->symbol
       (list->string a)))))

;(let ((fp (open-input-file "tests/begin.scm")))
;(let ((fp (open-input-file "tests/quote.scm")))
;  (write (cyc-read-all fp)))

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
