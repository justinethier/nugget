;; A temporary test file, delete this once it works!

;; case #1 - pass a global variable to the interpreter
(define x 1)
(write (eval 'x))

;; case #2 - pass a local (IE, lambda var)

;; case #3 - mutate global/local. or is this the same as previous?

;; case #4 - introduce new vars in interpreter, then use them later on??
