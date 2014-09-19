; JAE notes:
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

;; Echo file to stdout
;
; TODO: enhance and take a block of compiled code, and insert when the token is found:
;/** SCHEME CODE ENTRY POINT **/
;
(define (emit-fp fp)
    (let ((l (read-line fp)))
        (if (eof-object? l)
            (close-port fp)
            (begin 
                (display l) 
                (newline)
                (emit-fp fp)))))

(define (read-runtime fp)
  (letrec* 
    ((break "/** SCHEME CODE ENTRY POINT **/")
     (read-fp (lambda (header footer on-header?)
       (let ((l (read-line fp)))
         (cond
           ((eof-object? l)
             (close-port fp)
             (cons (reverse header) (reverse footer)))
           (else 
             (cond
               ((equal? l break)
                 (read-fp header footer #f))
               (else
                 (if on-header?
                   (read-fp (cons l header) footer on-header?)
                   (read-fp header (cons l footer) on-header?))))))))))

   (read-fp (list) (list) #t)))

(define (string-join lst delim)
  (cond
    ((null? lst) 
      "")
    ((= (length lst) 1) 
      (car lst))
    (else
      (string-append 
        (car lst) 
        delim 
        (string-join (cdr lst) delim)))))

;; Compilation routines.

;; c-compile-program : exp -> string
(define (c-compile-program exp)
  (let* ((preamble "")
         (append-preamble (lambda (s)
                            (set! preamble (string-append preamble "  " s "\n"))))
         (body (c-compile-exp exp append-preamble "cont" '() "")))
    (string-append 
     preamble 
;     "int main (int argc, char* argv[]) {\n"
     "  " body ;" ;\n"
;     "  return 0;\n"
;     " }\n"
)))

;; c-compile-exp : exp (string -> void) -> string
;;
;; exp - expression to compiler
;; append-preamble - ??
;; cont - name of the next continuation
;;        this is experimental and probably needs refinement
;; free-var-lst - list of free variables, for creating closures
;;                 this is experimental but based off of closure-convert
;; cv-name - String name of a C variable that is being created by this
;;           expression. Blank if not applicable.
(define (c-compile-exp exp append-preamble cont free-var-lst cv-name)
  (cond
    ; Core forms:
    ((const? exp)       (c-compile-const exp))
    ((prim?  exp)       (c-compile-prim exp))
    ((ref?   exp)       (c-compile-ref exp))
    ((if? exp)          (c-compile-if exp append-preamble cont free-var-lst))
;
;    ; IR (1):
;    ((cell? exp)        (c-compile-cell exp append-preamble))
;    ((cell-get? exp)    (c-compile-cell-get exp append-preamble))
;    ((set-cell!? exp)   (c-compile-set-cell! exp append-preamble))
;    
;    ; IR (2):
    ((tagged-list? '%closure exp)
     (c-compile-closure exp append-preamble cont free-var-lst cv-name))
;    ((env-get? exp)     (c-compile-env-get exp append-preamble))
;    
;    ; Application:      
    ((app? exp)         (c-compile-app exp append-preamble cont free-var-lst cv-name))
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
(define (c-compile-args args append-preamble prefix cont free-var-lst)
  (if (not (pair? args))
      ""
      (string-append
       prefix 
       (c-compile-exp (car args) append-preamble cont free-var-lst "")
       (if (pair? (cdr args))
           (string-append (c-compile-args (cdr args) append-preamble ", " cont free-var-lst))
           ""))))

;; c-compile-app : app-exp (string -> void) -> string
(define (c-compile-app exp append-preamble cont free-var-lst cv-name)
  (trace:debug `(c-compile-app: ,exp))
  (let (($tmp (mangle (gensym 'tmp))))
    
;    (append-preamble (string-append
;                      "Value " $tmp " ; "))
    
    (let* ((args     (app->args exp))
           (fun      (app->fun exp)))
;TODO: may be special cases depending upon what we are calling (prim, lambda, etc)
      (cond
        ((lambda? fun)
         (let* ((lid (allocate-lambda (c-compile-lambda fun)))) ;; TODO: pass in free vars? may be needed to track closures
                                                                ;; properly, wait until this comes up in an example
          (cond
           ((exp/cvar? (car args))
            (let* ((cvar-name (mangle (gensym 'c)))
                   (cvar (c-compile-exp 
                             (car args) 
                             append-preamble 
                             cont 
                             free-var-lst
                             cvar-name)))
                (string-append
                  cvar "\n  "
                  "return_check(__lambda_" (number->string lid)
                  "(" ; TODO: how to propagate continuation - cont " "
                  ;", "
                  "&" cvar-name "));")))
           (else
            (string-append
              "return_check(__lambda_" (number->string lid)
              "(" ; TODO: how to propagate continuation - cont " "
               (c-compile-args args append-preamble "" cont free-var-lst) ;", " cont free-var-lst)
              "));" )))))

        ((prim? fun)
         (string-append
          (c-compile-exp fun append-preamble cont free-var-lst cv-name)
          "("
          (if (prim/cvar? fun) ; prim creates local c var
            (string-append cv-name ", ")
            "")
          (c-compile-args args append-preamble "" cont free-var-lst)
          ")"
          (if (prim/cvar? fun) ";" "")))

        ((equal? '%closure-ref fun)
         (string-append
            "("
            ;; TODO: probably not the ideal solution, but works for now
            "(closure" (number->string (cadr args)) ")"
            (mangle (car args))
            ")->elt"
            (number->string (cadr args))))

        ;; TODO: may not be good enough, closure app could be from an elt
        ((tagged-list? '%closure-ref fun)
         (let* ((comp-pairs
                  (c-compile-args/cvars 
                     args append-preamble cont free-var-lst))
                (comp-args 
                  (string-join
                    (map car comp-pairs)
                    ", "))
                (comp-cvars
                  (string-join
                    (filter 
                      (lambda (s) 
                         (and (string? s) (not (equal? s ""))))
                      (map cadr comp-pairs))
                    "\n")))
;(trace:debug `(JAE pairs ,comp-pairs scheme ,fun ,args))
         (string-append
          comp-cvars
          (if (> (string-length comp-cvars) 0) "\n" "")
          "return_funcall1"
          "("
          comp-args
          ");")))
        ((tagged-list? '%closure fun)
         (write `(TODO app %closure ,fun)))
        (else
         (string-append
          (c-compile-exp fun append-preamble cont free-var-lst cv-name)
          (c-compile-args args append-preamble ", " cont free-var-lst)
          "));" ))))))

;; Compile args to a function a return a list of pairs:
;;
;; - Code to pass as the arg
;; - Code to insert before the function call to introduce a C variable,
;;   for example to create a closure to pass as an arg
(define (c-compile-args/cvars args append-preamble cont free-var-lst)
    (map
        (lambda (a)
          (if (exp/cvar? a)
              ;; exp introduces a C variable
              (let ((cvar (gensym 'c)))
                (list
                    (string-append "&" (mangle cvar)) ; pass by ref
                    (c-compile-exp a append-preamble cont free-var-lst (mangle cvar))))
              (list
                (c-compile-exp a append-preamble cont free-var-lst "")
                ""))) ; Not assigning a C variable
        args))

; Does primitive create a c variable?
(define (prim/cvar? exp)
    (and (prim? exp)
         (member exp '(cons))))

; Does compiling exp create a c variable?
(define (exp/cvar? exp)
  (and (list? exp)
       (or (prim/cvar? (car exp))
           (tagged-list? '%closure exp))))

; c-compile-if : if-exp -> string
(define (c-compile-if exp append-preamble cont free-var-lst)
  (string-append
   "if(" (c-compile-exp 
           (if->condition exp) append-preamble cont free-var-lst "") "){ \n"
   "" (c-compile-exp 
         (if->then exp) append-preamble cont free-var-lst "")
   "\n} else { \n"
   "" (c-compile-exp 
         (if->else exp) append-preamble cont free-var-lst "")
   "}\n"))

; c-compile-set-cell! : set-cell!-exp (string -> void) -> string 
;(define (c-compile-set-cell! exp append-preamble)
;  (string-append
;   "(*"
;   "(" (c-compile-exp (set-cell!->cell exp) append-preamble) ".cell.addr)" " = "
;   (c-compile-exp (set-cell!->value exp) append-preamble)
;   ")"))

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

(define (lambda->env exp)
    (let ((formals (lambda->formals exp)))
        (car formals)))

; c-compile-closure : closure-exp (string -> void) -> string
(define (c-compile-closure exp append-preamble cont free-var-lst cv-name)
  (let* ((lam (closure->lam exp))
         ;(env (closure->env exp))
         ;(num-fv (- (length env) 2))
         (num-args (length (lambda->formals lam)))
         (lid (allocate-lambda (c-compile-lambda lam))))
;; JAE TODO: looks like we need to make a closure before calling
;;           a function in the MTA runtime. but is that done here??
;; IE: which closure is built here, in reference to the lambda?
;; see app and display examples
;
; TODO: if there is an env, pack it up and pass it along as an arg
; to the function, since it is the function's closure:
;     (c-compile-exp env append-preamble)

(trace:debug `(,exp fv: ,free-var-lst))

    (string-append
     "mclosure" (number->string (length free-var-lst)) "(" cv-name ", "
     "__lambda_" (number->string lid)
     (if (> (length free-var-lst) 0) "," "")
     (string-join free-var-lst ", ")
     ");" ;(if (> num-fv 0) "," "")
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
    (let* ((formals (c-compile-formals (lambda->formals exp)))
           (env-closure (lambda->env exp))
           (body    (c-compile-exp     
                        (car (lambda->exp exp)) ;; car ==> assume single expr in lambda body after CPS
                        append-preamble
                        (mangle env-closure)
                        (map mangle (lambda->formals exp))
                        ""
                        )))
      (lambda (name)
        (string-append "static void " name "(" formals ") {\n"
                       preamble
                       "  " body "; \n"
                       "}\n")))))
  
(define (mta:code-gen input-program)
  (let ((compiled-program (c-compile-program input-program)))
  
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
  (emit "}")))

