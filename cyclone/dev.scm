;; This file is a staging/integration area for functionality that is not 
;; supported yet by cyclone
;;


;TODO: should move working functions below into a unit-test file
(write 
 (list
  (reverse '(1 2))
  (reverse '(a b c))
  (reverse '(1 2 3 4 5 6 7 8 9 10))
  ;TODO: improper list, this is an error: (reverse '(1 . 2))
  (char-whitespace? #\space)
  (char-whitespace? #\a)
  (char-numeric? #\1)
  (char-numeric? #\newline)))
;(eof-object?)
;(read-char fp)
; (char->integer char) 
; (integer->char n) 
; or
;list->string
; reverse
;string-append
;string->number
;string->symbol - TODO: can runtime allow dynamically-created symbols?
; letrec - TBD, may just restructure parser code to use defines
;open-input-file (and closing too, I suppose)
; read - can this be derived from parser.scm implementation???

; idea - booleans
; do we want to have a separate def macro for them? IE, have the
; literals be something like bool_t / bool_f to prevent collisions
; with, for example, 't ??
;
; farther off but along the same lines, how to support compilation of
; multiple scheme files into multiple C modules?
;
