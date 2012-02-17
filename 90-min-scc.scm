#!/usr/local/Gambit-C/bin/gsi

; Copyright (C) 2004 by Marc Feeley, All Rights Reserved.

; This is the "90 minute Scheme to C compiler" presented at the
; Montreal Scheme/Lisp User Group on October 20, 2004.

; Usage with Gambit-C 4.0:
;
;    % ./90-min-scc.scm test.scm
;    % gcc -O test.c
;    % ./a.out

;------------------------------------------------------------------------------

; abstract-syntax tree node structure:

(define-type ast
  extender: define-type-of-ast
  subx) ; the asts of the sub-expressions

(define-type-of-ast lit val)   ; literal             e.g. 99 or #f
(define-type-of-ast ref var)   ; variable reference  e.g. x
(define-type-of-ast set var)   ; variable assignment e.g. (set! x 99)
(define-type-of-ast cnd)       ; conditional         e.g. (if 1 2 3)
(define-type-of-ast prim op)   ; primitive op        e.g. (+ 1 2)
(define-type-of-ast app)       ; application         e.g. (f 1 2)
(define-type-of-ast lam params); lambda expression   e.g. (lambda (x) x)
(define-type-of-ast seq)       ; sequence            e.g. (begin 1 2)

;------------------------------------------------------------------------------

; representation of environments:

(define-type binding
  extender: define-type-of-binding
  id)

(define-type-of-binding var uid)
(define-type-of-binding macro expander)

(define (extend bindings env)
  (append bindings env))

(define (lookup id env)
  (cond ((null? env)                     #f)
        ((eq? (binding-id (car env)) id) (car env))
        (else                            (lookup id (cdr env)))))

(define seq-num 0)

(define (new-var id)
  (set! seq-num (+ seq-num 1))
  (make-var
   id
   (string->symbol
    (string-append (symbol->string id)
                   "."
                   (number->string seq-num)))))

(define (new-global-var id)
  (make-var id id))

(define (global-var? var)
  (eq? (binding-id var) (var-uid var)))

;------------------------------------------------------------------------------

; xe = expand expression     xe :: (expr,cte) -> ast

(define (xe e cte)
  (cond ((const-expr? e) (xe-const-expr e cte))
        ((ident-expr? e) (xe-ident-expr e cte))
        ((form-expr? e)  (xe-form-expr e cte))
        (else            (error "syntax-error" e))))

(define (const-expr? e) (or (boolean? e) (number? e)))
(define (ident-expr? e) (symbol? e))
(define (form-expr? e)  (and (not (null? e)) (list? e)))

(define (xe-const-expr e cte)
  (make-lit '() e))

(define (xe-ident-expr e cte)
  (let ((b (xe-lookup e cte)))
    (if (var? b)
        (make-ref '() b)
        (error "can't reference a nonvariable" e))))

(define (xe-form-expr e cte)
  (let ((h (car e)))
    (let ((b (and (ident-expr? h) (xe-lookup h cte))))
      (if (macro? b)
          ((macro-expander b) e cte)
          (make-app (xe-exprs e cte))))))

(define (xe-exprs le cte)
  (map (lambda (x) (xe x cte)) le))

(define (make-initial-cte)
  (list

   (make-macro '=   ; could have used %= instead
     (lambda (e cte)
       (if (= (length (cdr e)) 2)
           (make-prim (xe-exprs (cdr e) cte) '%=)
           (error "= expects 2 args"))))

   (make-macro '<   ; could have used %< instead
     (lambda (e cte)
       (if (= (length (cdr e)) 2)
           (make-prim (xe-exprs (cdr e) cte) '%<)
           (error "< expects 2 args"))))

   (make-macro '+   ; could have used %+ instead
     (lambda (e cte)
       (if (= (length (cdr e)) 2)
           (make-prim (xe-exprs (cdr e) cte) '%+)
           (error "+ expects 2 args"))))

   (make-macro '-   ; could have used %- instead
     (lambda (e cte)
       (if (= (length (cdr e)) 2)
           (make-prim (xe-exprs (cdr e) cte) '%-)
           (error "- expects 2 args"))))

   (make-macro '*   ; could have used %* instead
     (lambda (e cte)
       (if (= (length (cdr e)) 2)
           (make-prim (xe-exprs (cdr e) cte) '%*)
           (error "* expects 2 args"))))

   (make-macro 'display   ; could have used %display instead
     (lambda (e cte)
       (if (= (length (cdr e)) 1)
           (make-prim (xe-exprs (cdr e) cte) '%display)
           (error "display expects 1 arg"))))

   (make-macro 'set!
     (lambda (e cte)
       (if (= (length (cdr e)) 2)
           (let ((b (xe-lookup (cadr e) '())))
             (cond ((var? b)
                    (make-set (xe-exprs (cddr e) cte) b))
                   (else
                    (error "can't set! a nonvariable" e))))
           (error "set! expects 2 args"))))

   (make-macro 'define
     (lambda (e cte)
       (xe (cons 'set! (cdr e)) cte)))

   (make-macro 'if
     (lambda (e cte)
       (cond ((= (length (cdr e)) 3)
              (make-cnd (xe-exprs (cdr e) cte)))
             ((= (length (cdr e)) 2)
              (xe `(if ,(cadr e) ,(caddr e) #f) cte))
             (else
              (error "if expects 2 or 3 args")))))

   (make-macro 'lambda
     (lambda (e cte)
       (if (>= (length (cdr e)) 1)
           (let ((params (map new-var (cadr e))))
             (let ((new-cte (extend params cte)))
               (make-lam
                (list (xe (cons 'begin (cddr e)) new-cte))
                params)))
           (error "lambda expects a parameter list"))))

   (make-macro 'begin
     (lambda (e cte)
       (cond ((= (length (cdr e)) 0)
              (xe #f cte))
             ((= (length (cdr e)) 1)
              (xe (cadr e) cte))
             (else
              (make-seq (xe-exprs (cdr e) cte))))))

   (make-macro 'let
     (lambda (e cte)
       (if (>= (length (cdr e)) 1)
           (xe (cons (cons 'lambda
                           (cons (map car (cadr e))
                                 (cddr e)))
                     (map cadr (cadr e)))
               cte)
           (error "let expects a binding list"))))

   (make-macro 'or
     (lambda (e cte)
       (cond ((= (length (cdr e)) 0)
              (xe #f cte))
             ((= (length (cdr e)) 1)
              (xe (cadr e) cte))
             (else
              (xe `((lambda (t1 t2) (if t1 t1 (t2)))
                    ,(cadr e)
                    (lambda () (or ,@(cddr e))))
                  cte)))))

   (make-macro 'and
     (lambda (e cte)
       (cond ((= (length (cdr e)) 0)
              (xe #t cte))
             ((= (length (cdr e)) 1)
              (xe (cadr e) cte))
             (else
              (xe `((lambda (t1 t2) (if t1 (t2) t1))
                    ,(cadr e)
                    (lambda () (and ,@(cddr e))))
                  cte)))))))

(define (xe-lookup id cte)
  (or (lookup id cte)
      (lookup id xe-global-cte)
      (let ((v (new-global-var id)))
        (set! xe-global-cte (cons v xe-global-cte))
        v)))

(define xe-global-cte '())

(define (parse-file filename)
  (set! xe-global-cte (make-initial-cte))
  (xe (with-input-from-file filename
        (lambda () (cons 'begin (read-all))))
      '()))

;------------------------------------------------------------------------------

; utilities

(define (interval n m) ; returns the list (n n+1 n+2 ... m)
  (if (<= n m) (cons n (interval (+ n 1) m)) '()))

(define (keep f lst)
  (cond ((null? lst)   '())
        ((f (car lst)) (cons (car lst) (keep f (cdr lst))))
        (else          (keep f (cdr lst)))))

(define (diff s1 s2)
  (cond ((null? s1)         '())
        ((memq (car s1) s2) (diff (cdr s1) s2))
        (else               (cons (car s1) (diff (cdr s1) s2)))))

(define (union s1 s2)
  (cond ((null? s1)         s2)
        ((memq (car s1) s2) (union (cdr s1) s2))
        (else               (cons (car s1) (union (cdr s1) s2)))))

(define (union-multi sets) (foldl union '() sets))

(define (foldl f base lst)
  (if (null? lst)
      base
      (foldl f (f base (car lst)) (cdr lst))))

(define (pos-in-list x lst)
  (let loop ((lst lst) (i 0))
    (cond ((not (pair? lst)) #f)
          ((eq? (car lst) x) i)
          (else              (loop (cdr lst) (+ i 1))))))

;------------------------------------------------------------------------------

; free variables

(define (fv ast)
  (cond ((ref? ast)
         (list (ref-var ast)))
        ((set? ast)
         (union (fv (car (ast-subx ast)))
                (list (set-var ast))))
        ((lam? ast)
         (diff (fv (car (ast-subx ast)))
               (lam-params ast)))
        (else
         (union-multi (map fv (ast-subx ast))))))

;------------------------------------------------------------------------------

; CPS conversion

(define (cps-convert ast)

  (define (cps ast cont-ast)

    (cond ((lit? ast)
           (make-app (list cont-ast ast)))

          ((ref? ast)
           (make-app (list cont-ast ast)))

          ((set? ast)
           (cps-list (ast-subx ast)
                     (lambda (val)
                       (make-app
                        (list cont-ast
                              (make-set val
                                        (set-var ast)))))))

          ((cnd? ast)
           (let ((xform
                  (lambda (cont-ast)
                    (cps-list (list (car (ast-subx ast)))
                              (lambda (test)
                                (make-cnd
                                 (list (car test)
                                       (cps (cadr (ast-subx ast))
                                            cont-ast)
                                       (cps (caddr (ast-subx ast))
                                            cont-ast))))))))
             (if (ref? cont-ast) ; prevent combinatorial explosion
                 (xform cont-ast)
                 (let ((k (new-var 'k)))
                   (make-app
                    (list (make-lam
                           (list (xform (make-ref '() k)))
                           (list k))
                          cont-ast))))))

          ((prim? ast)
           (cps-list (ast-subx ast)
                     (lambda (args)
                       (make-app
                        (list cont-ast
                              (make-prim args
                                         (prim-op ast)))))))

          ((app? ast)
           (let ((fn (car (ast-subx ast))))
             (if (lam? fn)
                 (cps-list (cdr (ast-subx ast))
                           (lambda (vals)
                             (make-app
                              (cons (make-lam
                                     (list (cps-seq (ast-subx fn)
                                                    cont-ast))
                                     (lam-params fn))
                                    vals))))
                 (cps-list (ast-subx ast)
                           (lambda (args)
                             (make-app
                              (cons (car args)
                                    (cons cont-ast
                                          (cdr args)))))))))

          ((lam? ast)
           (let ((k (new-var 'k)))
             (make-app
              (list cont-ast
                    (make-lam
                     (list (cps-seq (ast-subx ast)
                                    (make-ref '() k)))
                     (cons k (lam-params ast)))))))

          ((seq? ast)
           (cps-seq (ast-subx ast) cont-ast))

          (else
           (error "unknown ast" ast))))

  (define (cps-list asts inner)

    (define (body x)
      (cps-list (cdr asts)
                (lambda (new-asts)
                  (inner (cons x new-asts)))))

    (cond ((null? asts)
           (inner '()))
          ((or (lit? (car asts))
               (ref? (car asts)))
           (body (car asts)))
          (else
           (let ((r (new-var 'r)))
             (cps (car asts)
                  (make-lam (list (body (make-ref '() r)))
                            (list r)))))))

  (define (cps-seq asts cont-ast)
    (cond ((null? asts)
           (make-app (list cont-ast #f)))
          ((null? (cdr asts))
           (cps (car asts) cont-ast))
          (else
           (let ((r (new-var 'r)))
             (cps (car asts)
                  (make-lam
                   (list (cps-seq (cdr asts) cont-ast))
                   (list r)))))))

  (let ((ast-cps
         (cps ast
              (let ((r (new-var 'r)))
                (make-lam
                 (list (make-prim (list (make-ref '() r))
                                  '%halt))
                 (list r))))))
    (if (lookup 'call/cc (fv ast))
        ; add this definition for call/cc if call/cc is needed
        (make-app
         (list (make-lam
                (list ast-cps)
                (list (new-var '_)))
               (xe '(set! call/cc
                          (lambda (k f)
                            (f k (lambda (_ result) (k result)))))
                   '())))
        ast-cps)))

;------------------------------------------------------------------------------

; closure conversion

(define (closure-convert ast)

  (define (convert ast self-var free-vars)

    (define (cc ast)
      (cond ((lit? ast)
             ast)
            ((ref? ast)
             (let ((i
                    (pos-in-list (ref-var ast)
                                 free-vars)))
               (if i
                   (make-prim
                    (list (make-ref '() self-var)
                          (make-lit '() (+ i 1)))
                    '%closure-ref)
                   ast)))
            ((set? ast)
             (make-set (map cc (ast-subx ast))
                       (set-var ast)))
            ((cnd? ast)
             (make-cnd (map cc (ast-subx ast))))
            ((prim? ast)
             (make-prim (map cc (ast-subx ast))
                        (prim-op ast)))
            ((app? ast)
             (let ((fn (car (ast-subx ast)))
                   (args (map cc (cdr (ast-subx ast)))))
               (if (lam? fn)
                   (make-app
                    (cons (make-lam
                           (list (cc (car (ast-subx fn))))
                           (lam-params fn))
                          args))
                   (let ((f (cc fn)))
                     (make-app
                      (cons (make-prim
                             (list f
                                   (make-lit '() 0))
                             '%closure-ref)
                            (cons f
                                  args)))))))
            ((lam? ast)
             (let ((new-free-vars
                    (keep (lambda (v)
                            (not (global-var? v)))
                          (fv ast)))
                   (new-self-var
                    (new-var 'self)))
               (make-prim
                (cons (make-lam
                       (list (convert (car (ast-subx ast))
                                      new-self-var
                                      new-free-vars))
                       (cons new-self-var
                             (lam-params ast)))
                      (map (lambda (v)
                             (cc (make-ref '() v)))
                           new-free-vars))
                '%closure)))
            ((seq? ast)
             ; this case is impossible after CPS-conversion
             (make-seq (map cc (ast-subx ast))))
            (else
             (error "unknown ast" ast))))

    (cc ast))

  (make-lam (list (convert ast #f '())) '()))

;------------------------------------------------------------------------------

; code generation

(define (code-generate ast)

  (define lambda-todo '())
  (define lambda-count 0)

  (define (add-lambda! lam)
    (let ((i lambda-count))
      (set! lambda-count (+ i 1))
      (set! lambda-todo (cons (cons i lam) lambda-todo))
      i))

  (define global-vars (fv ast))

  (define (code-gen ast stack-env)

    (define (cg-list asts vars stack-env sep cont)
      (if (null? asts)
          (cont "" stack-env)
          (let ((x (code-gen (car asts) stack-env)))
            (cg-list (cdr asts)
                     (cdr vars)
                     (cons (car vars) stack-env)
                     sep
                     (lambda (code stack-env)
                       (cont (list x sep code)
                             stack-env))))))

    (define (cg-args args stack-env)
      (cg-list args
               (interval 1 (length args))
               stack-env
               ""
               (lambda (code stack-env)
                 code)))

    (define (access-var var stack-env)
      (if (global-var? var)
          (let ((i (pos-in-list var global-vars)))
            (list "GLOBAL(" i "/*" (var-uid var) "*/)"))
          (let ((i (- (length stack-env)
                      (pos-in-list var stack-env)
                      1)))
            (list "LOCAL(" i "/*" (var-uid var) "*/)"))))

    (define (cg ast)

      (cond ((lit? ast)
             (let ((val (lit-val ast)))
               (case val
                 ((#f) (list " PUSH(FALSEOBJ));"))
                 ((#t) (list " PUSH(TRUEOBJ));"))
                 (else (list " PUSH(INT2OBJ(" val "));")))))

            ((ref? ast)
             (let ((var (ref-var ast)))
               (list " PUSH(" (access-var var stack-env) ");")))

            ((set? ast)
             (let ((var (set-var ast)))
               (list
                (cg (car (ast-subx ast)))
                " " (access-var var stack-env) " = TOS();")))

            ((cnd? ast)
             (let ((x (map cg (ast-subx ast))))
               (list (car x)
                     "\n if (POP()) {\n"
                     (cadr x)
                     "\n } else {\n"
                     (caddr x)
                     "\n }")))

            ((prim? ast)
             (let ((args (ast-subx ast)))
               (case (prim-op ast)
                 ((%= %< %+ %- %* %display %halt)
                  (list
                   (cg-args args stack-env)
                   (case (prim-op ast)
                     ((%=) " EQ();")
                     ((%<) " LT();")
                     ((%+) " ADD();")
                     ((%-) " SUB();")
                     ((%*) " MUL();")
                     ((%display) " DISPLAY();")
                     ((%halt) " HALT();"))))
                 ((%closure)
                  (let* ((i (add-lambda! (car args)))
                         (n (length (cdr args)))
                         (s (list "CLOSURE(" i "," n ");")))
                    (list
                     (cg-args (cdr args) stack-env)
                     " BEGIN_" s
                     (map (lambda (j)
                            (list " INICLO(" j ");"))
                          (reverse (interval 1 n)))
                     " END_" s)))
                 ((%closure-ref)
                  (let ((i (lit-val (cadr args))))
                    (list
                     (cg (car args))
                     " TOS() = CLOSURE_REF(TOS()," i ");")))
                 (else
                  (error "unknown primitive" (prim-op ast))))))

            ((app? ast)
             (let* ((fn (car (ast-subx ast)))
                    (args (cdr (ast-subx ast)))
                    (n (length args)))
               (if (lam? fn)
                   (cg-list args
                            (lam-params fn)
                            stack-env
                            "\n"
                            (lambda (code new-stack-env)
                              (list
                               code
                               (code-gen (car (ast-subx fn))
                                         new-stack-env))))
                   (cg-list args
                            (interval 1 n)
                            stack-env
                            "\n"
                            (lambda (code new-stack-env)
                              (let* ((start (length stack-env))
                                     (s (list "JUMP(" n ");")))
                              (list
                               code
                               " BEGIN_" s
                               (map (lambda (j)
                                      (list " PUSH(LOCAL(" (+ j start) "));"))
                                    (interval 0 (- n 1)))
                               " END_" s)))))))

            ((lam? ast)
             ; this case is impossible after CPS-conversion
             (list " PUSH(INT2OBJ(" (add-lambda! ast) "));"))

            ((seq? ast)
             ; this case is impossible after CPS-conversion
             (map (lambda (ast)
                    (list (cg ast) "DROP();"))
                  (ast-subx ast)))

            (else
             (error "unknown ast" ast))))

    (cg ast))

  (define (compile-all-lambdas)
    (if (null? lambda-todo)
        ""
        (let* ((x (car lambda-todo))
               (ast (cdr x)))
          (set! lambda-todo (cdr lambda-todo))
          (list
           "case " (car x) ": /* " (object->string (source ast) 60) " */\n\n"
           (code-gen (car (ast-subx ast))
                     (reverse (lam-params ast)))
           "\n\n"
           (compile-all-lambdas)))))

  (add-lambda! ast)

  (let ((code (compile-all-lambdas)))
    (list
     (list
      "#define NB_GLOBALS " (length global-vars) "\n"
      "#define MAX_STACK " 100 "\n" ; could be computed...
      code-prefix)
     code
     code-suffix)))

(define code-prefix "
#include <stdio.h>
#include <stdlib.h>

#define HEAP_SIZE 1000000

typedef int obj;

obj global[NB_GLOBALS];
obj stack[MAX_STACK];
obj heap[HEAP_SIZE];

#define INT2OBJ(n) ((n) << 1)
#define OBJ2INT(o) ((o) >> 1)

#define PTR2OBJ(p) ((obj)(p) + 1)
#define OBJ2PTR(o) ((obj*)((o) - 1))

#define FALSEOBJ INT2OBJ(0)
#define TRUEOBJ INT2OBJ(1)

#define GLOBAL(i) global[i]
#define LOCAL(i) stack[i]
#define CLOSURE_REF(self,i) OBJ2PTR(self)[i]

#define TOS() sp[-1]
#define PUSH(x) *sp++ = x
#define POP() *--sp

#define EQ() { obj y = POP(); TOS() = INT2OBJ(TOS() == y); }
#define LT() { obj y = POP(); TOS() = INT2OBJ(TOS() < y); }
#define ADD() { obj y = POP(); TOS() = TOS() + y; }
#define SUB() { obj y = POP(); TOS() = TOS() - y; }
#define MUL() { obj y = POP(); TOS() = OBJ2INT(TOS()) * y; }
#define DISPLAY() printf (\"%d\", OBJ2INT(TOS()))
#define HALT() break

#define BEGIN_CLOSURE(label,nbfree) if (hp-(nbfree+1) < heap) hp = gc (sp);
#define INICLO(i) *--hp = POP()
#define END_CLOSURE(label,nbfree) *--hp = label; PUSH(PTR2OBJ(hp));

#define BEGIN_JUMP(nbargs) sp = stack;
#define END_JUMP(nbargs) pc = OBJ2PTR(LOCAL(0))[0]; goto jump;

obj *gc (obj *sp) { exit (1); } /* no GC! */

obj execute (void)
{
  int pc = 0;
  obj *sp = stack;
  obj *hp = &heap[HEAP_SIZE];

  jump: switch (pc) {

")

(define code-suffix "  }
  return POP();
}

int main () { printf (\"result = %d\\n\", OBJ2INT(execute ())); return 0; }
")

;------------------------------------------------------------------------------

; debugging

(define (source ast)
  (cond ((lit? ast)
         (lit-val ast))
        ((ref? ast)
         (var-uid (ref-var ast)))
        ((set? ast)
         (list 'set!
               (var-uid (set-var ast))
               (source (car (ast-subx ast)))))
        ((cnd? ast)
         (cons 'if (map source (ast-subx ast))))
        ((prim? ast)
         (cons (prim-op ast) (map source (ast-subx ast))))
        ((app? ast)
         (if (lam? (car (ast-subx ast)))
             (list 'let
                   (map (lambda (p a)
                          (list (var-uid p) (source a)))
                        (lam-params (car (ast-subx ast)))
                        (cdr (ast-subx ast)))
                   (source (car (ast-subx (car (ast-subx ast))))))
             (map source (ast-subx ast))))
        ((lam? ast)
         (list 'lambda
               (map var-uid (lam-params ast))
               (source (car (ast-subx ast)))))
        ((seq? ast)
         (cons 'begin (map source (ast-subx ast))))
        (else
         (error "unknown ast" ast))))

;------------------------------------------------------------------------------

(define (compile-file filename)
  (let ((ast (parse-file filename)))
    (display "-------------------------- AST:\n")
    (pretty-print (source ast))
    (let ((ast-after-cps (cps-convert ast)))
      (display "-------------------------- AST AFTER CPS-CONVERSION:\n")
      (pretty-print (source ast-after-cps))
      (let ((ast-after-cc (closure-convert ast-after-cps)))
        (display "-------------------------- AST AFTER CLOSURE-CONVERSION:\n")
        (pretty-print (source ast-after-cc))
        (let ((code (code-generate ast-after-cc)))
          (display "-------------------------- C CODE:\n")
          (display (cadr code))
          (with-output-to-file
              (string-append (path-strip-extension filename) ".c")
            (lambda ()
              (display code))))))))

(define (main filename)
  (compile-file filename))
