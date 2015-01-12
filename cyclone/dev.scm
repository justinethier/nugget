;; This file is a staging/integration area for functionality that is not 
;; supported yet by cyclone
;;

'a/test
(write 'a/test)
(write #\space)
'a
'b
'c
(write
  (list
  ))

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
