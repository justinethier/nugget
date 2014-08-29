;; TODO: switch to the MTA runtime
;
; REMEMBER, the original cgen has no concept of continuations (the parameter k).
; this will have to be added!!!
;
; JAE observations:
;
; - a primitive can be executed inline, EG:
;   static void apply_subst(cont,alist,term) closure cont; list alist,term;
;   {if (atom(term))
;      {list temp_temp = assq(term,alist);
;       if (!nullp(temp_temp)) return_funcall1(cont,cdr(temp_temp));
;       return_funcall1(cont,term);}
; - a function can return a value to its continuation, eg:
;   return_funcall1(cont, cdr(temp_temp))
; - or if you have a new continuation to call into (EG: function to call),
;   use return_check to do it 
; - but how is closure allocated (is it for another continuation?) and what func
;   is called into using return_check?

(define (emit line)
  (display line)
  (newline))

;; Compilation routines.

;; c-compile-program : exp -> string
(define (c-compile-program exp)
  (let* ((preamble "")
         (append-preamble (lambda (s)
                            (set! preamble (string-append preamble "  " s "\n"))))
         (body (c-compile-exp exp append-preamble)))
    (string-append 
     preamble 
;     "int main (int argc, char* argv[]) {\n"
     "  " body ;" ;\n"
;     "  return 0;\n"
;     " }\n"
)))

;; JAE - for ref, input is (display #t)
;;"---------------- after closure-convert:"
;; ((closure
;;    (lambda (env$2 r$1) (%halt r$1))
;;       (env-make 0))
;;  (display #t))
;; TODO: how does matt-m handle this?

;; c-compile-exp : exp (string -> void) -> string
(define (c-compile-exp exp append-preamble)
  (cond
    ; Core forms:
    ((const? exp)       (c-compile-const exp))
    ((prim?  exp)       (c-compile-prim exp))
    ((ref?   exp)       (c-compile-ref exp))
;    ((if? exp)          (c-compile-if exp append-preamble))
;
;    ; IR (1):
;    ((cell? exp)        (c-compile-cell exp append-preamble))
;    ((cell-get? exp)    (c-compile-cell-get exp append-preamble))
;    ((set-cell!? exp)   (c-compile-set-cell! exp append-preamble))
;    
;    ; IR (2):
    ((closure? exp)     (c-compile-closure exp append-preamble))
    ((env-make? exp)    (c-compile-env-make exp append-preamble))
;    ((env-get? exp)     (c-compile-env-get exp append-preamble))
;    
;    ; Application:      
    ((app? exp)         (c-compile-app exp append-preamble))
    (else               (error "unknown exp in c-compile-exp: " exp))))

;; c-compile-const : const-exp -> string
(define (c-compile-const exp)
  (cond
    ((integer? exp) (string-append 
                     "MakeInt(" (number->string exp) ")"))
    ((boolean? exp) (string-append
                     (if exp "quote_t" "quote_f")))
                     ;"MakeBoolean(" (if exp "1" "0") ")"))
    (else           (error "unknown constant: " exp))))

;; c-compile-prim : prim-exp -> string
(define (c-compile-prim p)
  (cond
;    ((eq? '+ p)       "__sum")
;    ((eq? '- p)       "__difference")
;    ((eq? '* p)       "__product")
;    ((eq? '= p)       "__numEqual")
    ((eq? '%halt p)   "__halt")
;    ((eq? 'display p) "__display")
    ((eq? 'display p) "prin1")
    ((eq? 'cons p) "make_cons") ;; TODO: when to use mcons vs make_cons ?
    (else             (error "unhandled primitive: " p))))

; c-compile-ref : ref-exp -> string
(define (c-compile-ref exp)
  (mangle exp))
  
; c-compile-args : list[exp] (string -> void) -> string
(define (c-compile-args args append-preamble prefix)
  (if (not (pair? args))
      ""
      (string-append
       prefix 
       (c-compile-exp (car args) append-preamble)
       (if (pair? (cdr args))
           (string-append (c-compile-args (cdr args) append-preamble ", "))
           ""))))

;; c-compile-app : app-exp (string -> void) -> string
(define (c-compile-app exp append-preamble)
  (trace:debug `(c-compile-app: ,exp))
  (let (($tmp (mangle (gensym 'tmp))))
    
;    (append-preamble (string-append
;                      "Value " $tmp " ; "))
    
    (let* ((args     (app->args exp))
           (fun      (app->fun exp)))
;TODO: may be special cases depending upon what we are calling (prim, lambda, etc)
      (cond
        ((prim? fun)
         (string-append
          (c-compile-exp fun append-preamble)
          "("
          (if (prim/cvar? fun) ; prim creates local c var
            "c, "
            "")
          (c-compile-args args append-preamble "")
          ");"))
;; TODO: closure?  may need to check whether args need to be pre-computed
;; eg: mcons(c); return_check(.., &c);
;; 
        ((closure? fun)
          (cond
           ((and (list? (car args))
                 (prim/cvar? (caar args)))
            (let ((cvar (c-compile-exp (car args) append-preamble)))
                (string-append
                  cvar "\n  "
                  (c-compile-exp fun append-preamble)
                  ", &c));")))
           (else
            (string-append
             (c-compile-exp fun append-preamble)
             (c-compile-args args append-preamble ", ")
             "));" ))))
        (else
         (string-append
          (c-compile-exp fun append-preamble)
          (c-compile-args args append-preamble ", ")
          "));" ))))))

; Does primitive create a c variable?
(define (prim/cvar? exp)
    (and (prim? exp)
         (member exp '(cons))))

;; c-compile-if : if-exp -> string
;(define (c-compile-if exp append-preamble)
;  (string-append
;   "(" (c-compile-exp (if->condition exp) append-preamble) ").b.value ? "
;   "(" (c-compile-exp (if->then exp) append-preamble)      ") : "
;   "(" (c-compile-exp (if->else exp) append-preamble)      ")"))
;
;; c-compile-set-cell! : set-cell!-exp (string -> void) -> string 
;(define (c-compile-set-cell! exp append-preamble)
;  (string-append
;   "(*"
;   "(" (c-compile-exp (set-cell!->cell exp) append-preamble) ".cell.addr)" " = "
;   (c-compile-exp (set-cell!->value exp) append-preamble)
;   ")"))
;
;; c-compile-cell-get : cell-get-exp (string -> void) -> string 
;(define (c-compile-cell-get exp append-preamble)
;  (string-append
;   "(*("
;   (c-compile-exp (cell-get->cell exp) append-preamble)
;   ".cell.addr"
;   "))"))
;
;; c-compile-cell : cell-exp (string -> void) -> string
;(define (c-compile-cell exp append-preamble)
;  (string-append
;   "NewCell(" (c-compile-exp (cell->value exp) append-preamble) ")"))

; c-compile-env-make : env-make-exp (string -> void) -> string
(define (c-compile-env-make exp append-preamble)
  (string-append
   ; "MakeEnv(__alloc_env" (number->string (env-make->id exp))
   ; "(" 
   (c-compile-args (env-make->values exp) append-preamble "")
   ;"))"
   ))

;; c-compile-env-get : env-get (string -> void) -> string
;(define (c-compile-env-get exp append-preamble)
;  (string-append
;   "((struct __env_"
;   (number->string (env-get->id exp)) "*)" 
;   (c-compile-exp (env-get->env exp) append-preamble) ".env.env)->" 
;   (mangle (env-get->field exp))))
;



;; Lambda compilation.

;; Lambdas get compiled into procedures that, 
;; once given a C name, produce a C function
;; definition with that name.

;; These procedures are stored up an eventually 
;; emitted.

; type lambda-id = natural

; num-lambdas : natural
(define num-lambdas 0)

; lambdas : alist[lambda-id,string -> string]
(define lambdas '())

; allocate-lambda : (string -> string) -> lambda-id
(define (allocate-lambda lam)
  (let ((id num-lambdas))
    (set! num-lambdas (+ 1 num-lambdas))
    (set! lambdas (cons (list id lam) lambdas))
    id))

; get-lambda : lambda-id -> (symbol -> string)
(define (get-lambda id)
  (cdr (assv id lambdas)))

; c-compile-closure : closure-exp (string -> void) -> string
(define (c-compile-closure exp append-preamble)
  (let* ((lam (closure->lam exp))
         (env (closure->env exp))
         (num-fv (- (length env) 2))
         (lid (allocate-lambda (c-compile-lambda lam))))
;; JAE TODO: looks like we need to make a closure before calling
;;           a function in the MTA runtime. but is that done here??
;; IE: which closure is built here, in reference to the lambda?
;; see app and display examples
;
; TODO: if there is an env, pack it up and pass it along as an arg
; to the function, since it is the function's closure:
;     (c-compile-exp env append-preamble)

    (string-append
    ; TODO: may not be appropriate place to return check
    ;       (or maybe it is with env construction??)
     "return_check(__lambda_" (number->string lid)
     "(cont"
;     "mclosure" (number->string (+ 1 num-fv)) "(cont1,"
;     "__lambda_" (number->string lid)
     (if (> num-fv 0) "," "")
;     (c-compile-exp env append-preamble)
;     "));\n"
)))

; c-compile-formals : list[symbol] -> string
(define (c-compile-formals formals)
  (if (not (pair? formals))
      ""
      (string-append
       "object "
       (mangle (car formals))
       (if (pair? (cdr formals))
           (string-append ", " (c-compile-formals (cdr formals)))
           ""))))

; c-compile-lambda : lamda-exp (string -> void) -> (string -> string)
(define (c-compile-lambda exp)
  (let* ((preamble "")
         (append-preamble (lambda (s)
                            (set! preamble (string-append preamble "  " s "\n")))))
    (let ((formals (c-compile-formals (lambda->formals exp)))
          (body    (c-compile-exp     (car (lambda->exp exp)) append-preamble))) ;; car ==> assume single expr in lambda body after CPS
      (lambda (name)
        (string-append "static void " name "(" formals ") {\n"
                       preamble
                       "  " body " \n"
                       "}\n")))))
  
;; c-compile-env-struct : list[symbol] -> string
;(define (c-compile-env-struct env)
;  (let* ((id     (car env))
;         (fields (cdr env))
;         (sid    (number->string id))
;         (tyname (string-append "struct __env_" sid)))
;    (string-append 
;     "struct __env_" (number->string id) " {\n" 
;     (apply string-append (map (lambda (f)
;                                 (string-append
;                                  " Value "
;                                  (mangle f) 
;                                  " ; \n"))
;                               fields))
;     "} ;\n\n"
;     tyname "*" " __alloc_env" sid 
;     "(" (c-compile-formals fields) ")" "{\n"
;     "  " tyname "*" " t = malloc(sizeof(" tyname "))" ";\n"
;     (apply string-append 
;            (map (lambda (f)
;                   (string-append "  t->" (mangle f) " = " (mangle f) ";\n"))
;                 fields))
;     "  return t;\n"
;     "}\n\n"
;     )))
;    
(define (mta:code-gen input-program)
  (define compiled-program 
    (c-compile-program input-program))

  ; emit prelude for this runtime
  (if *do-c-runtime* (emit *mta:header*))
  
  ;; Emit lambdas:
  ; Print the prototypes:
  (for-each
   (lambda (l)
     (emit (string-append "static void __lambda_" (number->string (car l)) "() ;")))
   lambdas)
  
  (emit "")
  
  ; Print the definitions:
  (for-each
   (lambda (l)
     (emit ((cadr l) (string-append "__lambda_" (number->string (car l))))))
   lambdas)

  (emit "
static void test(env,cont) closure env,cont; { ")
  (emit compiled-program)
  (emit "}")
  (if *do-c-runtime* (emit *mta:footer*)))


