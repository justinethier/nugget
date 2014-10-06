;;
;; Compile scheme code to a Cheney-on-the-MTA C runtime
;;
(define (emit line)
  (display line)
  (newline))

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

;;; Compilation routines.

;; Return generated code that also requests allocation of C variables on stack
(define (c-code/vars str cvars)
  (list str
        cvars))

;; Return generated code with no C variables allocated on the stack
(define (c-code str) (c-code/vars str (list)))

;; Append arg count to a C code pair
(define (c:tuple/args cp num-args)
  (append cp (list num-args)))

;; Functions to work with data structures that contain C code:
;;
;; body - The actual body of C code
;; allocs - Allocations made by C code, eg "int c"
;; num-args - Number of function arguments combined in the tuple (optional)
;;
(define (c:body c-pair) (car c-pair))
(define (c:allocs c-pair) (cadr c-pair))
(define (c:num-args c-tuple) (caddr c-tuple))

(define (c:allocs->str c-allocs . prefix)
  (apply
    string-append
    (map
        (lambda (c)
           (string-append 
            (if (null? prefix)
                ""
                (car prefix))
            c 
            "\n"))
        c-allocs)))

(define (c:append cp1 cp2)
  (c-code/vars 
    (string-append (c:body cp1) (c:body cp2))
    (append (c:allocs cp1) (c:allocs cp2))))

(define (c:append/prefix prefix cp1 cp2)
  (c-code/vars 
    (string-append prefix (c:body cp1) (c:body cp2))
    (append (c:allocs cp1) (c:allocs cp2))))

(define (c:serialize cp prefix)
    (string-append
        (c:allocs->str (c:allocs cp) prefix)
        prefix
        (c:body cp)))

;; c-compile-program : exp -> string
(define (c-compile-program exp)
  (let* ((preamble "")
         (append-preamble (lambda (s)
                            (set! preamble (string-append preamble "  " s "\n"))))
         (body (c-compile-exp exp append-preamble "cont" '())))
    (string-append 
     preamble 
     (c:serialize body "  ") ;" ;\n"
;     "int main (int argc, char* argv[]) {\n"
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
(define (c-compile-exp exp append-preamble cont free-var-lst)
  (cond
    ; Core forms:
    ((const? exp)       (c-compile-const exp))
    ((prim?  exp)       (c-compile-prim exp))
    ((ref?   exp)       (c-compile-ref exp))
    ((if? exp)          (c-compile-if exp append-preamble cont free-var-lst))

    ; IR (2):
    ((tagged-list? '%closure exp)
     (c-compile-closure exp append-preamble cont free-var-lst))
    
    ; Application:      
    ((app? exp)         (c-compile-app exp append-preamble cont free-var-lst))
    (else               (error "unknown exp in c-compile-exp: " exp))))

;; c-compile-const : const-exp -> string
(define (c-compile-const exp)
  (cond
    ((integer? exp) 
      (let ((cvar-name (mangle (gensym 'c))))
        (c-code/vars
            (string-append "&" cvar-name) ; Code is just the variable name
            (list     ; Allocate integer on the C stack
              (string-append 
                "make_int(" cvar-name ", " (number->string exp) ");")))))
    ((boolean? exp) 
      (c-code (string-append
                (if exp "quote_t" "quote_f"))))
    (else
      (error "unknown constant: " exp))))

;; c-compile-prim : prim-exp -> string
(define (c-compile-prim p)
  (let ((c-func
          (cond
            ((eq? p '+)       "__sum")
            ((eq? p '-)       "__sub")
            ((eq? p '*)       "__mul")
            ((eq? p '/)       "__div")
            ;((eq? p '=)       "__numEqual")
            ((eq? p '%halt)     "__halt")
            ((eq? p 'display)   "prin1")
            ((eq? p 'cons)      "make_cons") ;; mcons? maybe n/a since malloc
            ((eq? p 'cell)      "make_cell")
            ((eq? p 'cell-get)  "cell_get")
            ((eq? p 'set-cell!) "cell_set")
            (else
              (error "unhandled primitive: " p)))))
;;    Note: what about code like (cons (cons - only want to insert one var, and use the other one in the second make_cons, right?
    (if (prim/cvar? p)
        (let ((cv-name (mangle (gensym 'c))))
           (c-code/vars 
            (string-append "&" cv-name)
            (list
                (string-append c-func "(" cv-name))))
        (c-code (string-append c-func "(")))))

; Does primitive create a c variable?
(define (prim/cvar? exp)
    (and (prim? exp)
         (member exp '(+ - * / cons cell))))

; c-compile-ref : ref-exp -> string
(define (c-compile-ref exp)
  (c-code (mangle exp)))

; c-compile-args : list[exp] (string -> void) -> string
(define (c-compile-args args append-preamble prefix cont free-var-lst)
  (letrec ((num-args 0)
         (_c-compile-args 
          (lambda (args append-preamble prefix cont free-var-lst)
            (if (not (pair? args))
                (c-code "")
                (begin
                  (trace:debug `(c-compile-args ,(car args)))
                  (set! num-args (+ 1 num-args))
                  (c:append/prefix
                    prefix 
                    (c-compile-exp (car args) 
                      append-preamble cont free-var-lst)
                    (_c-compile-args (cdr args) 
                      append-preamble ", " cont free-var-lst)))))))
  (c:tuple/args
    (_c-compile-args args 
      append-preamble prefix cont free-var-lst)
    num-args)))

;; c-compile-app : app-exp (string -> void) -> string
(define (c-compile-app exp append-preamble cont free-var-lst)
  (trace:debug `(c-compile-app: ,exp))
  (let (($tmp (mangle (gensym 'tmp))))
    (let* ((args     (app->args exp))
           (fun      (app->fun exp)))
      (cond
        ((lambda? fun)
         (let* ((lid (allocate-lambda (c-compile-lambda fun))) ;; TODO: pass in free vars? may be needed to track closures
                                                               ;; properly, wait until this comes up in an example
                (cgen 
                  (c-compile-args
                     args 
                     append-preamble 
                     ""
                     cont 
                     free-var-lst))
               )
              (c-code
                (string-append
                  (c:allocs->str (c:allocs cgen))
                  "return_check1(__lambda_" (number->string lid)
                  "," ; TODO: how to propagate continuation - cont " "
                  (c:body cgen) ");"))))

        ((prim? fun)
         (let ((c-fun 
                (c-compile-exp fun append-preamble cont free-var-lst))
               (c-args
                (c-compile-args args append-preamble "" cont free-var-lst)))
            (if (prim/cvar? fun)
              ;; Args need to go with alloc function
              (c-code/vars
                (c:body c-fun)
                (append
                  (c:allocs c-args) ;; fun alloc depends upon arg allocs
                  (list (string-append 
                    (car (c:allocs c-fun)) "," (c:body c-args) ");"))))
              ;; Args stay with body
              (c:append
                (c:append c-fun c-args)
                (c-code ")")))))

        ((equal? '%closure-ref fun)
         (c-code (apply string-append (list
            "("
            ;; TODO: probably not the ideal solution, but works for now
            "(closure" (number->string (cadr args)) ")"
            (mangle (car args))
            ")->elt"
            (number->string (cadr args))))))

        ;; TODO: may not be good enough, closure app could be from an elt
        ((tagged-list? '%closure-ref fun)
         (let* ((comp (c-compile-args 
                         args append-preamble "  " cont free-var-lst)))
           (c-code 
             (string-append
               (c:allocs->str (c:allocs comp) "\n")
               "return_funcall" (number->string (- (c:num-args comp) 1))
               "("
               (c:body comp)
               ");"))))

        ((tagged-list? '%closure fun)
         (let* ((cfun (c-compile-closure 
                        fun append-preamble cont free-var-lst))
                (cargs (c-compile-args
                         args append-preamble "  " cont free-var-lst)))
           (c-code
             (string-append
                (c:allocs->str (c:allocs cfun) "\n")
                (c:allocs->str (c:allocs cargs) "\n")
                "return_funcall" (number->string (- (c:num-args cargs) 0))
                "("
                "(closure)" (c:body cfun) ","
                (c:body cargs)
                ");"))))

        (else
         (error `(Unsupported function application ,exp))
         ;(string-append
         ; (c-compile-exp fun append-preamble cont free-var-lst cv-name)
         ; (c-compile-args args append-preamble ", " cont free-var-lst)
         ; "));" )
         )))))

; c-compile-if : if-exp -> string
(define (c-compile-if exp append-preamble cont free-var-lst)
  (let* ((compile (lambda (exp)
                    (c-compile-exp exp append-preamble cont free-var-lst)))
         (test (compile (if->condition exp)))
         (then (compile (if->then exp)))
         (els (compile (if->else exp))))
  (c-code (string-append
   "if("
   (c:serialize test "  ")
    "){ \n"
   "" 
   (c:serialize then "  ")
   "\n} else { \n"
   "" 
   (c:serialize els "  ")
   "}\n"))))


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
(define (c-compile-closure exp append-preamble cont free-var-lst)
  (let* ((lam (closure->lam exp))
         (env (if (> (length exp) 2)
                  (closure->env exp) ; arg to closure
                  #f))
         (env-var (if (and env
                           (tagged-list? '%closure-ref env))
                      (cadr env)
                      #f))
         (env-seq (if (and env
                           (tagged-list? '%closure-ref env))
                      (caddr env)
                      #f))
         (free-var-lst* 
           (map 
             (lambda (free-var) 
              ;; We may want to reference a closure variable, in which
              ;; case we need to reference it instead of the whole closure
              (if (and env-var env-seq (> env-seq 0)
                       (equal? free-var (mangle env-var)))
                (let ((clo-len (number->string env-seq)))
                  (string-append 
                    "((closure" clo-len ")" free-var ")->elt" clo-len))
                free-var))
             free-var-lst))
         (num-args (length (lambda->formals lam)))
         (cv-name (mangle (gensym 'c)))
         (lid (allocate-lambda (c-compile-lambda lam))))

; OBSOLETE? not sure what to make of this old comment: 
; if there is an env, pack it up and pass it along as an arg
; to the function, since it is the function's closure:
;     (c-compile-exp env append-preamble)

(trace:debug `(,exp fv: ,free-var-lst*))
  (c-code/vars
    (string-append "&" cv-name)
    (list (string-append
     "mclosure" (number->string (length free-var-lst*)) "(" cv-name ", "
     "__lambda_" (number->string lid)
     (if (> (length free-var-lst*) 0) "," "")
     (string-join free-var-lst* ", ")
     ");")))))

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
           (has-closure? 
             (equal? 
                "self" 
                (substring (mangle (car (lambda->formals exp))) 0 4)))
           (env-closure (lambda->env exp))
           (body    (c-compile-exp     
                        (car (lambda->exp exp)) ;; car ==> assume single expr in lambda body after CPS
                        append-preamble
                        (mangle env-closure)
                        (map mangle (lambda->formals exp))
                        )))
      (lambda (name)
        (string-append "static void " name 
                       "(" 
                       (if has-closure? "" "closure _,")
                        formals 
                       ") {\n"
                       preamble
                       ;"  " body "; \n"
                       (c:serialize body "  ") "; \n"
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

; Unused -
;;; Echo file to stdout
;(define (emit-fp fp)
;    (let ((l (read-line fp)))
;        (if (eof-object? l)
;            (close-port fp)
;            (begin 
;                (display l) 
;                (newline)
;                (emit-fp fp)))))
;
;(define (read-runtime fp)
;  (letrec* 
;    ((break "/** SCHEME CODE ENTRY POINT **/")
;     (read-fp (lambda (header footer on-header?)
;       (let ((l (read-line fp)))
;         (cond
;           ((eof-object? l)
;             (close-port fp)
;             (cons (reverse header) (reverse footer)))
;           (else 
;             (cond
;               ((equal? l break)
;                 (read-fp header footer #f))
;               (else
;                 (if on-header?
;                   (read-fp (cons l header) footer on-header?)
;                   (read-fp header (cons l footer) on-header?))))))))))
;
;   (read-fp (list) (list) #t)))
