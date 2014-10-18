;; TODO: line-num, char-num
(define (cyc-read-all fp)
  (letrec (
   (->tok (lambda (lst)
            (parse-atom (reverse lst))))
   (with-tok (lambda (tok toks)
                 (if (null? tok)
                   toks
                   (cons (->tok tok) toks))))
   (loop/tok (lambda (tok toks comment? quoted?)
                (if (null? tok)
                    (loop '() toks comment? quoted?)
                    (loop '() (cons (->tok tok) toks) comment? #f)))) ; read tok, no more '
   (loop (lambda (tok toks comment? quoted?)
    (let ((c (read-char fp)))
      (cond
        ((eof-object? c) 
         (reverse (with-tok tok toks)))
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
             (loop '() (cons (->tok tok) toks) comment? #t)))
        ((eq? c #\()
;idea is to form a new list when open paren encountered
;and to end that list upon close paren
         ;; TODO: need to error if close paren never found
         (let ((sub (cyc-read-all fp))
               (toks* (with-tok tok toks)))
;; TODO: if quoted, then pack up sub in quote
idea is mark if quoted, then the next time we peel off a tok,
quote it (and set quoted to false).
when ' is encountered, need to quote next expression
            (loop '() (cons sub toks*) #f #f)))
        ((eq? c #\))
         ;; TODO: what if too many close parens??
         (reverse (with-tok tok toks)))
        ((eq? c #\")
         (error `(Unable to parse strings at this time)))
        ((eq? c #\#)
         (if (null? tok)
           ;; # reader
           (let ((next-c (read-char fp)))
              (cond
                ((eq? #\t next-c) (loop '() (cons #t toks) #f))
                ((eq? #\f next-c) (loop '() (cons #f toks) #f))
                (else
                  (error `(Unhandled input sequence ,c ,next-c)))))
           ;; just another char...
           (loop (cons c tok) toks #f)))
        (else
          (loop (cons c tok) toks #f)))))))
   (loop '() '() #f)))

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

(let ((fp (open-input-file "tests/quote.scm")))
  (write (cyc-read-all fp)))

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
