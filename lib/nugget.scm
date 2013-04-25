;------------------------------------------------------------------------------

; utilities
;(define (pos-in-list x lst)
;  (let loop ((lst lst) (i 0))
;    (cond ((not (pair? lst)) #f)
;          ((eq? (car lst) x) i)
;          (else              (loop (cdr lst) (+ i 1))))))
(define (keep f lst)
  (cond ((null? lst)   '())
        ((f (car lst)) (cons (car lst) (keep f (cdr lst))))
        (else          (keep f (cdr lst)))))

;------------------------------------------------------------------------------

; code generation


  (define lambda-todo '())
  (define lambda-count 0)

  (define (add-lambda! lam)
    ;(write `(debug ,lam))
    (let ((i lambda-count))
      (set! lambda-count (+ i 1))
      (set! lambda-todo (cons (cons i lam) lambda-todo))
      i))

; TODO:
;  (define (access-var var stack-env)
;    (if (global-var? var)
;        (let ((i (pos-in-list var global-vars)))
;          (list "GLOBAL(" i "/*" (var-uid var) "*/)"))
;        (let ((i (- (length stack-env)
;                    (pos-in-list var stack-env)
;                    1)))
;          (list "LOCAL(" i "/*" (var-uid var) "*/)"))))

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

