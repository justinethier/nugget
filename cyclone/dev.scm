;; This file is a staging/integration area for functionality that is not 
;; supported yet by cyclone
;;

(char-whitespace? #\space)
(char-whitespace? #\a)
(char-numeric? #\1)
(char-numeric? #\newline)
;(eof-object?)
;(read-char fp)
;list->string
;string->number
;string->symbol - TODO: can runtime allow dynamically-created symbols?
; letrec - TBD, may just restructure parser code to use defines
;open-input-file
; read - can this be derived from parser.scm implementation???

; idea - booleans
; do we want to have a separate def macro for them? IE, have the
; literals be something like bool_t / bool_f to prevent collisions
; with, for example, 't ??
