;; This file is a staging/integration area for functionality that is not 
;; supported yet by cyclone
;;


;TODO: should move working functions below into a unit-test file
;(eof-object?)
;(read-char fp)
; (char->integer char) 
; (integer->char n) 
; or
;list->string
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
