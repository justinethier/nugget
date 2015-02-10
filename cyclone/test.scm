;; A temporary test file, move everything to a test suite and/or docs once it works!

;; The purpose of this file is to test interactions between the interpreter
;; and compiled code.
;;
;; Some requirements and notes:
;; - The interpreter can create new variables, but those new vars are only 
;;   accessible by the interpreter (otherwise code would not compile)
;; - The interpreter should be able to access compiled variables, including
;;   functions
;; - The interpreter should be able to change variables that originate
;;   in compiled code
;; - If eval is never called, compiled code can be more efficient by omitting
;;   information that is only required by the interpreter.
;; 
;; How to represent environments? 
;; Presumably the interpreter's global environment needs to include compiled globals as well. It should also be extended to include local vars, presumably prior to each call to eval??
;; 
;; case #1 - pass a global variable to the interpreter
(define x 1)
(write (eval 'x))

;; case #2 - pass a local (IE, lambda var)

;; case #3 - mutate global/local. or is this the same as previous?

;; case #4 - introduce new vars in interpreter, then use them later on??
