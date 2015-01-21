;; This file is a staging/integration area for functionality that is not 
;; supported yet by cyclone
;;

; WIP TODO: move working functions above into the unit-test file
;
; Goals
; 3 or 4) multiple module compilation. actually, do not really need that yet.
;         just need a way of saying OK, eval was used so load eval.scm and include
;         that with the complilation. would be the same for read or even (actually)
;         any of the lib functions/macros which could live in a separate scm file.
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
