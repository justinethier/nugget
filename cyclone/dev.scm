;; This file is a staging/integration area for functionality that is not 
;; supported yet by cyclone
;;

(write
  (list
;;TODO: string->list
  ))

; WIP TODO: move working functions above into the unit-test file
;
; port type (see below):
; consider http://stackoverflow.com/questions/6206893/how-to-implement-char-ready-in-c
; FILE* may not be good enough
;
;(eof-object?)
;(read-char fp)
;
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
; also, there is a TON of stuff in the TODO file. need to get back to that
