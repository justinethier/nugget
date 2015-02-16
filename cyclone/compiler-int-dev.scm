;; This is a temporary test file, move everything to a test suite and/or docs once it works!


;TODO: next step - create a function to export global (vars, values) to interpreter
;want to be able to call this when init'ing the global-env
;crazy idea - can apply be modified to take a closure as the first arg?
;that way we can allocate whatever we want when calling apply, and
;could easily return these lists to the interpreter
;
;it may be possible to modify c-compile-args for this purpose, by passing the continuation there and then modifying how apply is compiled. might be a big change though - htf does the cont get down to c-compile-prim?
;
;what is the cont parameter being used for? can we use it?
;
;test code:
;(let ((x (apply length '((#t #f))))
;      (y (apply length '((#t #f)))))
;(if (apply length '((#t #f)))
;  2
;  #f))

(write (cons (apply cons '(1 2)) (apply cons '(3 4))))

;; The purpose of this file is to test interactions between the interpreter
;; and compiled code.
;;
;; Some requirements and notes:
;; - The interpreter can create new variables, but those new vars are only 
;;   accessible by the interpreter (otherwise code would not compile)
;; - The interpreter should be able to access compiled variables, including
;;   functions
;; - The interpreter should be able to call compiled functions
;; - The interpreter should be able to change variables that originate
;;   in compiled code
;; - If eval is never called, compiled code can be more efficient by omitting
;;   information that is only required by the interpreter.
;; 
;; How to represent environments? 
;; Presumably the interpreter's global environment needs to include compiled globals as well. It should also be extended to include local vars, presumably prior to each call to eval??
;;
;; global env can be extended to include C globals and locals. their representations will be:
;;
;; - globals are just C variables. problem is, we may need to include the locations of those vars. otherwise how can the interpreter mutate them? IE, if global 'x' is a list, need the memory location of 'x' not the list, if we want to mutate the list
;;   simple - add a new type for globals that includes the memory address,
;;            but whenever we look one up, return the obj at that address.
;;     we want to avoid 'leaking' global objs outside of the env and associated
;;     code. then when a set comes in, change the var at the memory address.
;;
;;     obviously we want to just load the globals once when *global-env* is
;;     built, and do not want to load them ever again.
;;
;;     does any of this apply to locals?
;;
;; - locals are ... ? cells?
;;   presumably we need to extend global-env to include locals, then pass that
;;   extended env as the parameter to eval, each time it is called.
;;   this obviously would happen in the compiled code generated by cyclone.
;;
;;   local can be a local C variable:
;;        ((lambda (x) (display x)) 1)
;;        ...
;;        static void __lambda_11(int argc, closure _,object x_737) {
;;          return_check1(__lambda_10,Cyc_display(x_737));; 
;;        }
;;   but it can also be a closure:
;;      ((lambda (x) 
;;         ((lambda () (display x))))
;;       1)
;;      ...
;;      static void __lambda_12(int argc, closure _,object x_737) {
;;        
;;      closureN_type c_7380;
;;      c_7380.tag = closureN_tag;
;;       c_7380.fn = __lambda_11;
;;      c_7380.num_elt = 1;
;;      c_7380.elts = (object *)alloca(sizeof(object) * 1);
;;      c_7380.elts[0] = x_737;
;;      
;;      return_funcall0((closure)&c_7380);; 
;;      }
;;      
;;      static void __lambda_11(int argc, object self_7331) {
;;        return_check1(__lambda_10,Cyc_display(((closureN)self_7331)->elts[0]));; 
;;      }
;; 
;; case #1 - pass a global variable to the interpreter
(define x 1)
(define y 2)
(write (eval '(Cyc-global-vars)))
TODO: is quote_x not the same as what find_or_add returns? need to run
the debugging and see what is going on 
(write (eval 'x))
(write (list 'after-eval 'x x 'y y))
x ;; oh shit, need to reference x/y otherwise they get optimized out!

;; case #2 - pass a local (IE, lambda var)

;; case #3 - mutate global/local. or is this the same as previous?

;; case #4 - introduce new vars in interpreter, then use them later on??
