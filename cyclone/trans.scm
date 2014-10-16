;;
;; Scheme-to-scheme transformations and utility functions used by the compiler
;;

;; Tuning
(define *do-desugar* #t) ; Eventually replace w/a macro system
(define *do-cps* #t)    ; Turn off to reduce code size, but no call/cc. 
                        ; Also not sure if later phases work without CPS
(define *do-code-gen* #t) ; Generate C code?
(define *do-c-runtime* #t) ; Generate code for the C runtime?

;; Trace
(define *trace-level* 4)
(define (trace level msg pp prefix)
    (if (>= *trace-level* level)
      (begin
        (display "/* ")
        (newline)
        (display prefix)
        (pp msg)
        (display " */")
        (newline))))
(define (trace:error msg) (trace 1 msg pretty-print ""))
(define (trace:warn msg)  (trace 2 msg pretty-print ""))
(define (trace:info msg)  (trace 3 msg pretty-print ""))
(define (trace:debug msg) (trace 4 msg display "DEBUG: "))

;; Utilities.

; void : -> void
(define (void) (if #f #t))

; tagged-list? : symbol value -> boolean
(define (tagged-list? tag l)
  (and (pair? l)
       (eq? tag (car l))))

; char->natural : char -> natural
(define (char->natural c)
  (let ((i (char->integer c)))
    (if (< i 0)
        (* -2 i)
        (+ (* 2 i) 1))))

; integer->char-list : integer -> string
(define (integer->char-list n)
  (string->list (number->string n)))

; gensym-count : integer
(define gensym-count 0)

; gensym : symbol -> symbol
(define gensym (lambda params
                 (if (null? params)
                     (begin
                       (set! gensym-count (+ gensym-count 1))
                       (string->symbol (string-append
                                        "$"
                                        (number->string gensym-count))))
                     (begin
                       (set! gensym-count (+ gensym-count 1))
                       (string->symbol (string-append 
                                        (if (symbol? (car params))
                                            (symbol->string (car params))
                                            (car params))
                                        "$"
                                        (number->string gensym-count)))))))

; member : symbol sorted-set[symbol] -> boolean
(define (member sym S)
  (if (not (pair? S))
      #f
      (if (eq? sym (car S))
          #t
          (member sym (cdr S)))))

; symbol<? : symbol symobl -> boolean
(define (symbol<? sym1 sym2)
  (string<? (symbol->string sym1)
            (symbol->string sym2)))

; insert : symbol sorted-set[symbol] -> sorted-set[symbol]
(define (insert sym S)
  (if (not (pair? S))
      (list sym)
      (cond
        ((eq? sym (car S))       S)
        ((symbol<? sym (car S))  (cons sym S))
        (else (cons (car S) (insert sym (cdr S)))))))

; remove : symbol sorted-set[symbol] -> sorted-set[symbol]
(define (remove sym S)
  (if (not (pair? S))
      '()
      (if (eq? (car S) sym)
          (cdr S)
          (cons (car S) (remove sym (cdr S))))))
          
; union : sorted-set[symbol] sorted-set[symbol] -> sorted-set[symbol]
(define (union set1 set2)
  ; NOTE: This should be implemented as merge for efficiency.
  (if (not (pair? set1))
      set2
      (insert (car set1) (union (cdr set1) set2))))

; difference : sorted-set[symbol] sorted-set[symbol] -> sorted-set[symbol]
(define (difference set1 set2)
  ; NOTE: This can be similarly optimized.
  (if (not (pair? set2))
      set1
      (difference (remove (car set2) set1) (cdr set2))))

; reduce : (A A -> A) list[A] A -> A
(define (reduce f lst init)
  (if (not (pair? lst))
      init
      (reduce f (cdr lst) (f (car lst) init))))

; azip : list[A] list[B] -> alist[A,B]
(define (azip list1 list2)
  (if (and (pair? list1) (pair? list2))
      (cons (list (car list1) (car list2))
            (azip (cdr list1) (cdr list2)))
      '()))

; assq-remove-key : alist[A,B] A -> alist[A,B]
(define (assq-remove-key env key)
  (if (not (pair? env))
      '()
      (if (eq? (car (car env)) key)
          (assq-remove-key (cdr env) key)
          (cons (car env) (assq-remove-key (cdr env) key)))))

; assq-remove-keys : alist[A,B] list[A] -> alist[A,B]
(define (assq-remove-keys env keys)
  (if (not (pair? keys))
      env
      (assq-remove-keys (assq-remove-key env (car keys)) (cdr keys))))




;; Data type predicates and accessors.

; const? : exp -> boolean
(define (const? exp)
  (or (integer? exp)
      (boolean? exp)))

; ref? : exp -> boolean
(define (ref? exp)
  (symbol? exp))

; quote? : exp -> boolean
(define (quote? exp)
  (tagged-list? 'quote exp))

; let? : exp -> boolean
(define (let? exp)
  (tagged-list? 'let exp))

; let->bindings : let-exp -> alist[symbol,exp]
(define (let->bindings exp)
  (cadr exp))

; let->exp : let-exp -> exp
(define (let->exp exp)
  (cddr exp))

; let->bound-vars : let-exp -> list[symbol]
(define (let->bound-vars exp)
  (map car (cadr exp)))

; let->args : let-exp -> list[exp]
(define (let->args exp)
  (map cadr (cadr exp)))

; letrec? : exp -> boolean
(define (letrec? exp)
  (tagged-list? 'letrec exp))

; letrec->bindings : letrec-exp -> alist[symbol,exp]
(define (letrec->bindings exp)
  (cadr exp))

; letrec->exp : letrec-exp -> exp
(define (letrec->exp exp)
  (cddr exp))

; letrec->exp : letrec-exp -> list[symbol]
(define (letrec->bound-vars exp)
  (map car (cadr exp)))

; letrec->exp : letrec-exp -> list[exp]
(define (letrec->args exp)
  (map cadr (cadr exp)))

; lambda? : exp -> boolean
(define (lambda? exp)
  (tagged-list? 'lambda exp))

; lambda->formals : lambda-exp -> list[symbol]
(define (lambda->formals exp)
  (cadr exp))

; lambda->exp : lambda-exp -> exp
(define (lambda->exp exp)
  (cddr exp)) ;; JAE - changed from caddr, so we can handle multiple expressions

; if? : exp -> boolean
(define (if? exp)
  (tagged-list? 'if exp))

; if->condition : if-exp -> exp
(define (if->condition exp)
  (cadr exp))

; if->then : if-exp -> exp
(define (if->then exp)
  (caddr exp))

; if->else : if-exp -> exp
(define (if->else exp)
  (cadddr exp))

; app? : exp -> boolean
(define (app? exp)
  (pair? exp))

; app->fun : app-exp -> exp
(define (app->fun exp)
  (car exp))

; app->args : app-exp -> list[exp]
(define (app->args exp)
  (cdr exp))
  
; prim? : exp -> boolean
(define (prim? exp)
  (member exp '(
     +
     -
     *
     /
     =
     %halt
     cons
     cell-get
     set-cell!
     cell
     length
     display)))

(define (prim-call? exp)
  (and (list? exp) (prim? (car exp))))

; begin? : exp -> boolean
(define (begin? exp) 
  (tagged-list? 'begin exp))

; begin->exps : begin-exp -> list[exp]
(define (begin->exps exp)
  (cdr exp))

; set! : exp -> boolean
(define (set!? exp)
  (tagged-list? 'set! exp))

; set!->var : set!-exp -> var
(define (set!->var exp)
  (cadr exp))

; set!->exp : set!-exp -> exp
(define (set!->exp exp)
  (caddr exp))

; closure? : exp -> boolean
(define (closure? exp) 
  (tagged-list? 'closure exp))

; closure->lam : closure-exp -> exp
(define (closure->lam exp) 
  (cadr exp))

; closure->env : closure-exp -> exp
(define (closure->env exp) 
  (caddr exp))

(define (closure->fv exp) 
  (cddr exp))

; env-make? : exp -> boolean
(define (env-make? exp) 
  (tagged-list? 'env-make exp))

; env-make->id : env-make-exp -> env-id
(define (env-make->id exp)
  (cadr exp))

; env-make->fields : env-make-exp -> list[symbol]
(define (env-make->fields exp)
  (map car (cddr exp)))
  
; env-make->values : env-make-exp -> list[exp]
(define (env-make->values exp)
  (map cadr (cddr exp)))

; env-get? : exp -> boolen
(define (env-get? exp)
  (tagged-list? 'env-get exp))

; env-get->id : env-get-exp -> env-id
(define (env-get->id exp)
  (cadr exp))
  
; env-get->field : env-get-exp -> symbol
(define (env-get->field exp)
  (caddr exp))

; env-get->env : env-get-exp -> exp
(define (env-get->env exp)
  (cadddr exp)) 

; set-cell!? : set-cell!-exp -> boolean
(define (set-cell!? exp)
  (tagged-list? 'set-cell! exp))

; set-cell!->cell : set-cell!-exp -> exp
(define (set-cell!->cell exp)
  (cadr exp))

; set-cell!->value : set-cell!-exp -> exp
(define (set-cell!->value exp)
  (caddr exp))

; cell? : exp -> boolean
(define (cell? exp)
  (tagged-list? 'cell exp))

; cell->value : cell-exp -> exp
(define (cell->value exp)
  (cadr exp))

; cell-get? : exp -> boolean
(define (cell-get? exp)
  (tagged-list? 'cell-get exp))

; cell-get->cell : cell-exp -> exp
(define (cell-get->cell exp)
  (cadr exp))



;; Syntax manipulation.

; substitute-var : alist[var,exp] ref-exp -> exp
(define (substitute-var env var)
  (let ((sub (assq var env)))
    (if sub
        (cadr sub)
        var)))

; substitute : alist[var,exp] exp -> exp
(define (substitute env exp)
  
  (define (substitute-with env)
    (lambda (exp)
      (substitute env exp)))

  (cond
    ; Core forms:    
    ((null? env)        exp)
    ((const? exp)       exp)
    ((prim? exp)        exp)
    ((ref? exp)         (substitute-var env exp))
    ((lambda? exp)      `(lambda ,(lambda->formals exp)
                           ,@(map (lambda (body-exp) 
                                    ;; TODO: could be more efficient
                                    (substitute 
                                        (assq-remove-keys env (lambda->formals exp)) 
                                        body-exp))
                                 (lambda->exp exp))))
    ((set!? exp)        `(set! ,(substitute-var env (set!->var exp))
                               ,(substitute env (set!->exp exp))))
    ((if? exp)          `(if ,(substitute env (if->condition exp))
                             ,(substitute env (if->then exp))
                             ,(substitute env (if->else exp))))
    
    ; Sugar:
    ((let? exp)         `(let ,(azip (let->bound-vars exp)
                                     (map (substitute-with env) (let->args exp)))
                           ,(substitute (assq-remove-keys env (let->bound-vars exp))
                                        (car (let->exp exp)))))
    ((letrec? exp)      (let ((new-env (assq-remove-keys env (letrec->bound-vars exp))))
                          `(letrec ,(azip (letrec->bound-vars exp) 
                                          (map (substitute-with new-env) 
                                               (letrec->args exp)))
                             ,(substitute new-env (car (letrec->exp exp))))))
    ((begin? exp)       (cons 'begin (map (substitute-with env) (begin->exps exp))))

    ; IR (1):
    ((cell? exp)        `(cell ,(substitute env (cell->value exp))))
    ((cell-get? exp)    `(cell-get ,(substitute env (cell-get->cell exp))))
    ((set-cell!? exp)   `(set-cell! ,(substitute env (set-cell!->cell exp))
                                    ,(substitute env (set-cell!->value exp))))
    
    ; IR (2):
    ((closure? exp)     `(closure ,(substitute env (closure->lam exp))
                                  ,(substitute env (closure->env exp))))
    ((env-make? exp)    `(env-make ,(env-make->id exp) 
                                   ,@(azip (env-make->fields exp)
                                           (map (substitute-with env)
                                                (env-make->values exp)))))
    ((env-get? exp)     `(env-get ,(env-get->id exp)
                                  ,(env-get->field exp)
                                  ,(substitute env (env-get->env exp))))
    
    ; Application:
    ((app? exp)         (map (substitute-with env) exp))
    (else               (error "unhandled expression type in substitution: " exp))))




;; Desugaring.

; let=>lambda : let-exp -> app-exp
(define (let=>lambda exp)
  (if (let? exp)
      (let ((vars (map car (let->bindings exp)))
            (args (map cadr (let->bindings exp))))
        `((lambda (,@vars) ,@(let->exp exp)) ,@args))
      exp))

; letrec=>lets+sets : letrec-exp -> exp
(define (letrec=>lets+sets exp)
  (if (letrec? exp)
      (let* ((bindings  (letrec->bindings exp))
             (namings   (map (lambda (b) (list (car b) #f)) bindings))
             (names     (letrec->bound-vars exp))
             (sets      (map (lambda (binding) 
                               (cons 'set! binding))
                             bindings))
             (args      (letrec->args exp)))
        `(let ,namings
           (begin ,@(append sets (letrec->exp exp)))))))

; begin=>let : begin-exp -> let-exp
(define (begin=>let exp)
  (define (singlet? l)
    (and (list? l)
         (= (length l) 1)))
  
  (define (dummy-bind exps)
    (cond
      ((singlet? exps)  (car exps))
      
      ((pair? exps)     `(let (($_ ,(car exps)))
                          ,(dummy-bind (cdr exps))))))
  (dummy-bind (begin->exps exp)))

; desugar : exp -> exp
(define (desugar exp)
  (cond
    ; Core forms:
    ((const? exp)      exp)
    ((prim? exp)       exp)
    ((ref? exp)        exp)
    ((quote? exp)      exp)
    ((lambda? exp)     `(lambda ,(lambda->formals exp)
                          ,@(map desugar (lambda->exp exp))))
    ((set!? exp)       `(set! ,(set!->var exp) ,(set!->exp exp)))
    ((if? exp)         `(if ,(if->condition exp)
                            ,(if->then exp)
                            ,(if->else exp)))
    
    ; Sugar:
    ((let? exp)        (desugar (let=>lambda exp)))
    ((letrec? exp)     (desugar (letrec=>lets+sets exp)))
    ((begin? exp)      (desugar (begin=>let exp)))
    
    ; IR (1):
    ((cell? exp)       `(cell ,(desugar (cell->value exp))))
    ((cell-get? exp)   `(cell-get ,(desugar (cell-get->cell exp))))
    ((set-cell!? exp)  `(set-cell! ,(desugar (set-cell!->cell exp)) 
                                   ,(desugar (set-cell!->value exp))))
    
    ; IR (2): 
    ((closure? exp)    `(closure ,(desugar (closure->lam exp))
                                 ,(desugar (closure->env exp))))
    ((env-make? exp)   `(env-make ,(env-make->id exp)
                                  ,@(azip (env-make->fields exp)
                                          (map desugar (env-make->values exp)))))
    ((env-get? exp)    `(env-get ,(env-get->id exp)
                                 ,(env-get->field exp)
                                 ,(env-get->env exp)))
    
    ; Applications:
    ((app? exp)        (map desugar exp))    
    (else              (error "unknown exp: " exp))))
    




;; Syntactic analysis.

; free-vars : exp -> sorted-set[var]
(define (free-vars exp)
  (cond
    ; Core forms:
    ((const? exp)    '())
    ((prim? exp)     '())    
    ((quote? exp)    '())    
    ((ref? exp)      (list exp))
    ((lambda? exp)   (difference (reduce union (map free-vars (lambda->exp exp)) '())
                                 (lambda->formals exp)))
    ((if? exp)       (union (free-vars (if->condition exp))
                            (union (free-vars (if->then exp))
                                   (free-vars (if->else exp)))))
    ((set!? exp)     (union (list (set!->var exp)) 
                            (free-vars (set!->exp exp))))
    
    ; Sugar:
    ((let? exp)      (free-vars (let=>lambda exp)))
    ((letrec? exp)   not-handled)
    ((begin? exp)    (reduce union (map free-vars (begin->exps exp)) '()))

    ; IR (1):
    ((cell-get? exp)  (free-vars (cell-get->cell exp)))
    ((cell? exp)      (free-vars (cell->value exp)))
    ((set-cell!? exp) (union (free-vars (set-cell!->cell exp))
                             (free-vars (set-cell!->value exp))))
    
    ; IR (2):
    ((closure? exp)   (union (free-vars (closure->lam exp))
                             (free-vars (closure->env exp))))
    ((env-make? exp)  (reduce union (map free-vars (env-make->values exp)) '()))
    ((env-get? exp)   (free-vars (env-get->env exp)))

    ; Application:
    ((app? exp)       (reduce union (map free-vars exp) '()))
    (else             (error "unknown expression: " exp))))





;; Mutable variable analysis and elimination.

;; Mutables variables analysis and elimination happens
;; on a desugared Intermediate Language (1).

;; Mutable variable analysis turns mutable variables 
;; into heap-allocated cells:

;; For any mutable variable mvar:

;; (lambda (... mvar ...) body) 
;;           =>
;; (lambda (... $v ...) 
;;  (let ((mvar (cell $v)))
;;   body))

;; (set! mvar value) => (set-cell! mvar value)

;; mvar => (cell-get mvar)

; mutable-variables : list[symbol]
(define mutable-variables '())

; mark-mutable : symbol -> void
(define (mark-mutable symbol)
  (set! mutable-variables (cons symbol mutable-variables)))

; is-mutable? : symbol -> boolean
(define (is-mutable? symbol)
  (define (is-in? S)
    (if (not (pair? S))
        #f
        (if (eq? (car S) symbol)
            #t
            (is-in? (cdr S)))))
  (is-in? mutable-variables))

; analyze-mutable-variables : exp -> void
(define (analyze-mutable-variables exp)
  (cond 
    ; Core forms:
    ((const? exp)    (void))
    ((prim? exp)     (void))
    ((ref? exp)      (void))
    ((quote? exp)    (void))
    ((lambda? exp)   (begin
                        (map analyze-mutable-variables (lambda->exp exp))
                        (void)))
    ((set!? exp)     (begin (mark-mutable (set!->var exp))
                            (analyze-mutable-variables (set!->exp exp))))
    ((if? exp)       (begin
                       (analyze-mutable-variables (if->condition exp))
                       (analyze-mutable-variables (if->then exp))
                       (analyze-mutable-variables (if->else exp))))
    
    ; Sugar:
    ((let? exp)      (begin
                       (map analyze-mutable-variables (map cadr (let->bindings exp)))
                       (map analyze-mutable-variables (let->exp exp))
                       (void)))
    ((letrec? exp)   (begin
                       (map analyze-mutable-variables (map cadr (letrec->bindings exp)))
                       (map analyze-mutable-variables (letrec->exp exp))
                       (void)))
    ((begin? exp)    (begin
                       (map analyze-mutable-variables (begin->exps exp))
                       (void)))
    
    ; Application:
    ((app? exp)      (begin 
                       (map analyze-mutable-variables exp)
                       (void)))
    (else            (error "unknown expression type: " exp))))


; wrap-mutables : exp -> exp
(define (wrap-mutables exp)
  
  (define (wrap-mutable-formals formals body-exp)
    (if (not (pair? formals))
        body-exp
        (if (is-mutable? (car formals))
            `(let ((,(car formals) (cell ,(car formals))))
               ,(wrap-mutable-formals (cdr formals) body-exp))
            (wrap-mutable-formals (cdr formals) body-exp))))
  
  (cond
    ; Core forms:
    ((const? exp)    exp)
    ((ref? exp)      (if (is-mutable? exp)
                         `(cell-get ,exp)
                         exp))
    ((prim? exp)     exp)
    ((quote? exp)    exp)
    ((lambda? exp)   `(lambda ,(lambda->formals exp)
                        ,(wrap-mutable-formals (lambda->formals exp)
                                               (wrap-mutables (car (lambda->exp exp)))))) ;; Assume single expr in lambda body, since after CPS phase
    ((set!? exp)     `(set-cell! ,(set!->var exp) ,(wrap-mutables (set!->exp exp))))
    ((if? exp)       `(if ,(wrap-mutables (if->condition exp))
                          ,(wrap-mutables (if->then exp))
                          ,(wrap-mutables (if->else exp))))
    
    ; Application:
    ((app? exp)      (map wrap-mutables exp))
    (else            (error "unknown expression type: " exp))))


;; CPS conversion 
;;
;; This is a port of code from the 90-minute Scheme->C Compiler by Marc Feeley
;;
;; Convert intermediate code to continuation-passing style, to allow for
;; first-class continuations and call/cc
;;

(define (cps-convert ast)

  (define (cps ast cont-ast)
    (cond
          ((const? ast)
           (list cont-ast ast))

          ((ref? ast)
           (list cont-ast ast))

          ((quote? ast)
           (list cont-ast ast))

          ((set!? ast)
           (cps-list (cddr ast) ;; expr passed to set
                     (lambda (val)
                       (list cont-ast 
                         `(set! ,(cadr ast) ,@val))))) ;; cadr => variable

          ((if? ast)
           (let ((xform
                  (lambda (cont-ast)
                    (cps-list (list (cadr ast))
                              (lambda (test)
                                 (list 'if
                                       (car test)
                                       (cps (caddr ast)
                                            cont-ast)
                                       (cps (cadddr ast)
                                            cont-ast)))))))
             (if (ref? cont-ast) ; prevent combinatorial explosion
                 (xform cont-ast)
                 (let ((k (gensym 'k)))
                    (list (list 'lambda
                           (list k)
                           (xform k))
                          cont-ast)))))

          ((prim-call? ast)
           (cps-list (cdr ast) ; args to primitive function
                     (lambda (args)
                        (list cont-ast
                            `(,(car ast) ; op
                              ,@args)))))

          ((lambda? ast)
           (let ((k (gensym 'k)))
             (list cont-ast
                   `(lambda
                      ,(cons k (cadr ast)) ; lam params
                      ,(cps-seq (cddr ast) k)))))

;
; TODO: begin is expanded already by desugar code... better to do it here?
;          ((seq? ast)
;           (cps-seq (ast-subx ast) cont-ast))

          ((app? ast)
           (let ((fn (app->fun ast)))
             (if (lambda? fn)
                 (cps-list (app->args ast)
                           (lambda (vals)
                             (cons (list
                                     'lambda
                                     (lambda->formals fn)
                                     (cps-seq (cddr fn) ;(ast-subx fn)
                                                    cont-ast))
                                    vals)))
                 (cps-list ast ;(ast-subx ast)
                           (lambda (args)
                              (cons (car args)
                                    (cons cont-ast
                                          (cdr args))))))))

          (else
           (error "unknown ast" ast))))

  (define (cps-list asts inner)
    (define (body x)
      (cps-list (cdr asts)
                (lambda (new-asts)
                  (inner (cons x new-asts)))))

    (cond ((null? asts)
           (inner '()))
          ((or (const? (car asts))
               (ref? (car asts)))
           (body (car asts)))
          (else
           (let ((r (gensym 'r))) ;(new-var 'r)))
             (cps (car asts)
                  `(lambda (,r) ,(body r)))))))

  (define (cps-seq asts cont-ast)
    (cond ((null? asts)
           (list cont-ast #f))
          ((null? (cdr asts))
           (cps (car asts) cont-ast))
          (else
           (let ((r (gensym 'r)))
             (cps (car asts)
                  `(lambda
                     (,r)
                    ,(cps-seq (cdr asts) cont-ast)))))))

  (let ((ast-cps
         (cps ast
            (let ((r (gensym 'r)))
                `(lambda (,r) (%halt ,r))))))
    (if (member 'call/cc (free-vars ast))
        ; add this definition for call/cc if call/cc is needed
        (list 
            (list
                'lambda
                (list 'call/cc)
                ast-cps)
           '(lambda (k f)
                (f k (lambda (_ result) (k result)))))
        ast-cps)

;    (if (lookup 'call/cc (fv ast))
;        ; add this definition for call/cc if call/cc is needed
;        (make-app
;         (list (make-lam
;                (list ast-cps)
;                (list (new-var '_)))
;               (xe '(set! call/cc
;                          (lambda (k f)
;                            (f k (lambda (_ result) (k result)))))
;                   '())))
;        ast-cps)
    ))


;; Closure-conversion.
;;
;; Closure conversion eliminates all of the free variables from every
;; lambda term.
;;
;; The code below is based on a fusion of a port of the 90-min-scc code by 
;; Marc Feeley and the closure conversion code in Matt Might's scheme->c 
;; compiler.

(define (pos-in-list x lst)
  (let loop ((lst lst) (i 0))
    (cond ((not (pair? lst)) #f)
          ((eq? (car lst) x) i)
          (else 
            (loop (cdr lst) (+ i 1))))))

(define (closure-convert exp)
 (define (convert exp self-var free-var-lst)
  (define (cc exp)
   (cond
    ((const? exp)        exp)
    ((quote? exp)        exp)
    ((ref? exp)
      (let ((i (pos-in-list exp free-var-lst)))
        (if i
            `(%closure-ref
              ,self-var
              ,(+ i 1))
            exp)))
    ((or
        (tagged-list? '%closure-ref exp)
        (tagged-list? '%closure exp)
        (prim-call? exp))
        `(,(car exp)
          ,@(map cc (cdr exp)))) ;; TODO: need to splice?
    ((set!? exp)  `(set! ,(set!->var exp)
                         ,(cc (set!->exp exp))))
    ((lambda? exp)
     (let* ((new-self-var (gensym 'self))
            (body  (lambda->exp exp))
            (new-free-vars (difference (free-vars body) (lambda->formals exp))))
       `(%closure
          (lambda
            ,(cons new-self-var (lambda->formals exp))
            ,(convert (car body) new-self-var new-free-vars)) ;; TODO: should this be a map??? was a list in 90-min-scc.
          ,@(map (lambda (v) ;; TODO: splice here?
                    (cc v))
            new-free-vars))))
    ((if? exp)  `(if ,@(map cc (cdr exp))))
    ((cell? exp)       `(cell ,(cc (cell->value exp))))
    ((cell-get? exp)   `(cell-get ,(cc (cell-get->cell exp))))
    ((set-cell!? exp)  `(set-cell! ,(cc (set-cell!->cell exp))
                                   ,(cc (set-cell!->value exp))))
    ((app? exp)
     (let ((fn (car exp))
           (args (map cc (cdr exp))))
       (if (lambda? fn)
           (let* ((body  (lambda->exp fn))
                  (new-free-vars (difference (free-vars body) (lambda->formals fn)))
                  (new-free-vars? (> (length new-free-vars) 0)))
               (if new-free-vars?
                 ; Free vars, create a closure for them
                 (let* ((new-self-var (gensym 'self)))
                   `((%closure 
                        (lambda
                          ,(cons new-self-var (lambda->formals fn))
                          ,(convert (car body) new-self-var new-free-vars))
                        ,@(map (lambda (v) (cc v))
                               new-free-vars))
                     ,@args))
                 ; No free vars, just create simple lambda
                 `((lambda ,(lambda->formals fn)
                           ,@(map cc body))
                   ,@args)))
           (let ((f (cc fn)))
            `((%closure-ref ,f 0)
              ,f
              ,@args)))))
    (else                
      (error "unhandled exp: " exp))))
  (cc exp))

 `(lambda ()
    ,(convert exp #f '())))

; Suitable definitions for the cell functions:
;(define (cell value) (lambda (get? new-value) 
;                       (if get? value (set! value new-value))))
;(define (set-cell! c v) (c #f v))
;(define (cell-get c) (c #t #t))
