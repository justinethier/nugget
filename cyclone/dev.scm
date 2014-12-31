;; This file is a staging/integration area for functionality that is not 
;; supported yet by cyclone
;;

(write
  (list
     (and 1 2 3)
     (and #t #f 'a 'b 'c)
     (or 1 2 3)
     (or #f 'a 'b 'c)
     (string-append "")
     ;error - (string-append 1)
     (string-append "test")
     (string-append "ab" "cdefgh ij" "klmno" "p" "q" "rs  " "tuv" "w" " x " "yz")
;    (string->number "0")
;    (string->number "42")
;    (string->number "343243243232") ;; Note no bignum support
;    (string->number "3.14159")
;    (list->string (list #\A #\B #\C))
;    (list->string (list #\A))
;    (list->string (list))
;;TODO: string->list
;    (integer->char 65)
;    (char->integer #\a)
  ))

;TODO: should move working functions above into the unit-test file
;
; equality testing is all messed up - EG: (equal? 1 1)
; need to improve this area
;
; port type (see below):
; consider http://stackoverflow.com/questions/6206893/how-to-implement-char-ready-in-c
; FILE* may not be good enough
;
;(eof-object?)
;(read-char fp)
;
;string->symbol - TODO: can runtime allow dynamically-created symbols?
;and
;or
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
