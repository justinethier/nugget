;; This file is a staging/integration area for functionality that is not 
;; supported yet by cyclone
;;


;;; GLOBAL SHADOWING ISSUE
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

(define (loop)
  (loop))
(loop)
add-tok

; loop version:
; (define add-tok
;   (lambda (tok toks quotes)
;     (set! loop
;       (lambda (i$896)
;         (if (= quotes i$896)
;           tok
;           (cons 'quote (cons (loop (+ i$896 1)) '())))))
;     (if quotes (cons (loop 0) toks) (cons tok toks))))

; loop1 (no shadowing) version:
; (define add-tok
;   (lambda (tok toks quotes)
;     ((lambda (loop$896)
;        (set! loop$896
;          (lambda (i$897)
;            (if (= quotes i$897)
;              tok
;              (cons 'quote (cons (loop$896 (+ i$897 1)) '())))))
;        (if quotes (cons (loop$896 0) toks) (cons tok toks)))
;      #f)))
;;; END GLOBAL SHADOWING ISSUE


;(
;123(list)
;1'b
;'a'c  ;; TODO: this is still an issue, try it
;(write
;  (list
;  1;2
;  ))
;1;2
;3"four five"
;#\space
;)

;(let ((fp (open-input-file "dev.scm")))
;  (display (read-char fp))
;  (display (peek-char fp))
;  (display (read-char fp))
;  (display (read-char fp))
;  (display (peek-char fp))
;  (display (read-char fp))
;  (display (read-char fp))
;  (display (read-char fp))
;  (display (read-char fp))
;  (close-input-port fp))

; WIP TODO: move working functions above into the unit-test file
;
; Goals
; 0) cleanup parser.scm from rework
;    fix local/global shadowing issue; it is present in parser.scm when a top-level REPL is used (w/loop)
; 1) read - can this be derived from parser.scm implementation???
; 2) simple repl from parser module - read, print AST
; 3 or 4) multiple module compilation
; 3 or 4) actual REPL by combining eval.scm and parser.scm.
;         can use load or just cat to combine initially, though
;         longer term they need to be distinct modules

; idea - booleans
; do we want to have a separate def macro for them? IE, have the
; literals be something like bool_t / bool_f to prevent collisions
; with, for example, 't ??
;
; farther off but along the same lines, how to support compilation of
; multiple scheme files into multiple C modules?
;
; also, there is a TON of stuff in the TODO file. need to get back to that
