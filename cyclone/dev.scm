;; This file is a staging/integration area for functionality that is not 
;; supported yet by cyclone
;;

(write
  (list
    (string->number "0")
    (string->number "42")
    (string->number "343243243232") ;; Note no bignum support
    (string->number "3.14159")
    (list->string (list #\A #\B #\C))
    (list->string (list #\A))
    (list->string (list))
;TODO: string->list
    (integer->char 65)
    (char->integer #\a)))

;TODO: should move working functions below into a unit-test file
; port type (see below):
;(eof-object?)
;(read-char fp)
;
;string-append
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
