;; This file is a staging/integration area for functionality that is not 
;; supported yet by cyclone
;;

;; TODO: write these char functions as lisp built-ins
;;       BUT, let's change that code to scan for built-ins, and
;;       only include those that are actually used. may want to just
;;       make a single scan across the program for all fv's, and then
;;       reconcile that list with built-ins, and only include those that
;;       might be used. IE, any fv == map, include the map built-in.

(char-whitespace? #\space)
(char-whitespace? #\a)
(char-numeric? #\1)
(char-numeric? #\newline)
;(eof-object?)
;(read-char fp)
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
