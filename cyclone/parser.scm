;; TODO: parser barfs on this, never reads second begin
;;1
;;(begin 1 2 (+ 1 2) (+ 3 4))
;;;(begin 1 2 (+ 1 2) (+ 3 4))
;;(begin 1 2 (+ 1 2) (+ 3 4))

;; TODO: line-num, char-num
(define (lex fp)
  (letrec (
   (->tok (lambda (lst)
            (list->string ;; TODO: may just be for temp debugging
              (reverse lst))))
   (with-tok (lambda (tok toks)
                 (if (null? tok)
                   toks
                   (cons (->tok tok) toks))))
   (loop/tok (lambda (tok toks comment?)
                (if (null? tok)
                    (loop '() toks comment?)
                    (loop '() (cons (->tok tok) toks) comment?))))
   (loop (lambda (tok toks comment?)
    (let ((c (read-char fp)))
      (cond
        ((eof-object? c) 
         (reverse (with-tok tok toks)))
        (comment?
         (if (eq? c #\newline)
             (loop '() toks #f)
             (loop '() toks #t)))
        ((char-whitespace? c)
         (loop/tok tok toks #f))
        ((eq? c #\;)
         (loop/tok tok toks #t))
        ((eq? c #\()
;idea is to form a new list when open paren encountered
;and to end that list upon close paren
         ;; TODO: need to error if close paren never found
         (let ((sub (lex fp))
               (toks* (with-tok tok toks)))
            (loop '() (cons sub toks*) #f)))
        ((eq? c #\))
         ;; TODO: what if too many close parens??
         ;(loop '() (cons 'TOK-cparen toks) #f))
         (reverse (with-tok tok toks)))
        ; TODO: # - need to start char reader
        ;           or vector, or... ???
        ; TODO: " - need to start string reader
        (else
          (loop (cons c tok) toks #f)))))))
   (loop '() '() #f)))

(let ((fp (open-input-file "tests/begin.scm")))
  (write (lex fp)))
