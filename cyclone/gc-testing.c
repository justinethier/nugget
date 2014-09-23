// JAE - based off of closure-1.scm example, experimenting with 
// generalizing/testing GC related functionality


/* STACK_GROWS_DOWNWARD is a machine-specific preprocessor switch. */
/* It is true for the Macintosh 680X0 and the Intel 80860. */
#define STACK_GROWS_DOWNWARD 1

/* STACK_SIZE is the size of the stack buffer, in bytes.           */
/* Some machines like a smallish stack--i.e., 4k-16k, while others */
/* like a biggish stack--i.e., 100k-500k.                          */
#define STACK_SIZE 100000

/* HEAP_SIZE is the size of the 2nd generation, in bytes. */
/* HEAP_SIZE should be at LEAST 225000*sizeof(cons_type). */
#define HEAP_SIZE 6000000

/* Define size of Lisp tags.  Options are "short" or "long". */
typedef long tag_type;

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <setjmp.h>
#include <string.h>
#include <stdarg.h>

#ifndef CLOCKS_PER_SEC
/* gcc doesn't define this, even though ANSI requires it in <time.h>.. */
#define CLOCKS_PER_SEC 0
#define setjmp _setjmp
#define longjmp _longjmp
#endif

/* The following sparc hack is courtesy of Roger Critchlow. */
/* It speeds up the output by more than a factor of THREE. */
/* Do 'gcc -O -S cboyer13.c'; 'perlscript >cboyer.s'; 'gcc cboyer.s'. */
#ifdef __GNUC__
#ifdef sparc
#define never_returns __attribute__ ((noreturn))
#else
#define never_returns /* __attribute__ ((noreturn)) */
#endif
#else
#define never_returns /* __attribute__ ((noreturn)) */
#endif

#if STACK_GROWS_DOWNWARD
#define check_overflow(x,y) ((x) < (y))
#else
#define check_overflow(x,y) ((x) > (y))
#endif

/* Return to continuation after checking for stack overflow. */
#define return_funcall1(cfn,a1) \
{char stack; \
 if (1 || check_overflow(&stack,stack_limit1)) { \
     object buf[1]; buf[0] = a1; \
     GC(cfn,buf,1); return; \
 } else {funcall1((closure) (cfn),a1); return;}}

/* TODO: need to check the stack, and figure out how to deal with
         second arg with GC */
#define return_funcall2(cfn,a1,a2) \
{char stack; \
 if (1 || check_overflow(&stack,stack_limit1)) { \
     object buf[2]; buf[0] = a1; buf[1] = a2; \
     GC(cfn,buf,2); return; \
 } else {funcall2((closure) (cfn),a1,a2); return;}}


/* Evaluate an expression after checking for stack overflow. */
/* (Overflow checking has been "optimized" away for this version). */
#define return_check(exp) {exp; return;}
#define return_check2(fn, count, args) { \
 char stack; \
 if (0 || check_overflow(&stack,stack_limit1)) { \
     mclosure0(c, &fn); \
     GC_after(c, count, args); return; \
 } else { (fn)(args); }}

/* Define tag values.  (I don't trust compilers to optimize enums.) */
#define cons_tag 0
#define symbol_tag 1
#define forward_tag 2
#define closure0_tag 3
#define closure1_tag 4
#define closure2_tag 5
#define closure3_tag 6
#define closure4_tag 7

#define nil NULL
#define eq(x,y) (x == y)
#define nullp(x) (x == NULL)
#define or(x,y) (x || y)
#define and(x,y) (x && y)

/* Define general object type. */

typedef void *object;

#define type_of(x) (((list) x)->tag)
#define forward(x) (((list) x)->cons_car)

/* Define function type. */

typedef void (*function_type)();

/* Define symbol type. */

typedef struct {const tag_type tag; const char *pname; object plist;} symbol_type;
typedef symbol_type *symbol;

#define symbol_plist(x) (((symbol_type *) x)->plist)

#define defsymbol(name) \
static symbol_type name##_symbol = {symbol_tag, #name, nil}; \
static const object quote_##name = &name##_symbol

/* Define cons type. */

typedef struct {tag_type tag; object cons_car,cons_cdr;} cons_type;
typedef cons_type *list;

#define car(x) (((list) x)->cons_car)
#define cdr(x) (((list) x)->cons_cdr)
#define caar(x) (car(car(x)))
#define cadr(x) (car(cdr(x)))
#define cdar(x) (cdr(car(x)))
#define cddr(x) (cdr(cdr(x)))
#define caddr(x) (car(cdr(cdr(x))))
#define cadddr(x) (car(cdr(cdr(cdr(x)))))

#define make_cons(n,a,d) \
cons_type n; n.tag = cons_tag; n.cons_car = a; n.cons_cdr = d;

#define atom(x) ((x == NULL) || (((cons_type *) x)->tag != cons_tag))

/* Closure types.  (I don't trust compilers to optimize vector refs.) */

typedef struct {tag_type tag; function_type fn;} closure0_type;
typedef struct {tag_type tag; function_type fn; object elt1;} closure1_type;
typedef struct {tag_type tag; function_type fn; object elt1,elt2;} closure2_type;
typedef struct {tag_type tag; function_type fn; object elt1,elt2,elt3;} closure3_type;
typedef struct {tag_type tag; function_type fn; object elt1,elt2,elt3,elt4;} closure4_type;

typedef closure0_type *closure0;
typedef closure1_type *closure1;
typedef closure2_type *closure2;
typedef closure3_type *closure3;
typedef closure4_type *closure4;
typedef closure0_type *closure;

#define mclosure0(c,f) closure0_type c; c.tag = closure0_tag; c.fn = f;
#define mclosure1(c,f,a) closure1_type c; c.tag = closure1_tag; \
   c.fn = f; c.elt1 = a;
#define mclosure2(c,f,a1,a2) closure2_type c; c.tag = closure2_tag; \
   c.fn = f; c.elt1 = a1; c.elt2 = a2;
#define mclosure3(c,f,a1,a2,a3) closure3_type c; c.tag = closure3_tag; \
   c.fn = f; c.elt1 = a1; c.elt2 = a2; c.elt3 = a3;
#define mclosure4(c,f,a1,a2,a3,a4) closure4_type c; c.tag = closure4_tag; \
   c.fn = f; c.elt1 = a1; c.elt2 = a2; c.elt3 = a3; c.elt4 = a4;
#define funcall0(cfn) ((cfn)->fn)(cfn)
#define funcall1(cfn,a1) ((cfn)->fn)(cfn,a1)
#define funcall2(cfn,a1,a2) ((cfn)->fn)(cfn,a1,a2)
#define setq(x,e) x = e

#define mlist1(e1) (mcons(e1,nil))
#define mlist2(e2,e1) (mcons(e2,mlist1(e1)))
#define mlist3(e3,e2,e1) (mcons(e3,mlist2(e2,e1)))
#define mlist4(e4,e3,e2,e1) (mcons(e4,mlist3(e3,e2,e1)))
#define mlist5(e5,e4,e3,e2,e1) (mcons(e5,mlist4(e4,e3,e2,e1)))
#define mlist6(e6,e5,e4,e3,e2,e1) (mcons(e6,mlist5(e5,e4,e3,e2,e1)))
#define mlist7(e7,e6,e5,e4,e3,e2,e1) (mcons(e7,mlist6(e6,e5,e4,e3,e2,e1)))

#define rule(lhs,rhs) (mlist3(quote_equal,lhs,rhs))

/* Prototypes for Lisp built-in functions. */

static list mcons(object,object);
static object terpri(void);
static object prin1(object);
static list assq(object,list);
static object get(object,object);
static object equalp(object,object);
static object memberp(object,list);
static char *transport(char *);
static void GC(closure,object*,int) never_returns;
static void GC_after(closure, int, ...);

static void main_main(long stack_size,long heap_size,char *stack_base) never_returns;
static long long_arg(int argc,char **argv,char *name,long dval);

/* Global variables. */

static clock_t start;   /* Starting time. */

static char *stack_begin;   /* Initialized by main. */
static char *stack_limit1;  /* Initialized by main. */
static char *stack_limit2;

static char *bottom;    /* Bottom of tospace. */
static char *allocp;    /* Cheney allocate pointer. */
static char *alloc_end;

static long no_gcs = 0; /* Count the number of GC's. */

static volatile object gc_cont;   /* GC continuation closure. */
static volatile object gc_ans[10];    /* argument for GC continuation closure. */
static volatile int gc_num_ans;
static jmp_buf jmp_main; /* Where to jump to. */

static object test_exp1, test_exp2; /* Expressions used within test. */

/* Define the Lisp atoms that we need. */

defsymbol(f);
defsymbol(t);
/* JAE TODO: computed by compiler(?)
defsymbol(add1);
defsymbol(and);
defsymbol(append);
defsymbol(cons);
defsymbol(delete);
defsymbol(difference);
defsymbol(equal);
defsymbol(fix);
defsymbol(flatten);
defsymbol(foo);
defsymbol(greatest_factor);
defsymbol(if);
defsymbol(implies);
defsymbol(intersect);
defsymbol(lemmas);
defsymbol(length);
defsymbol(lessp);
defsymbol(member);
defsymbol(nlistp);
defsymbol(not);
defsymbol(numberp);
defsymbol(one);
defsymbol(or);
defsymbol(plus);
defsymbol(quotient);
defsymbol(remainder);
defsymbol(reverse);
defsymbol(six);
defsymbol(sub1);
defsymbol(times);
defsymbol(two);
defsymbol(x1);
defsymbol(x2);
defsymbol(x3);
defsymbol(x4);
defsymbol(x5);
defsymbol(x6);
defsymbol(x7);
defsymbol(zero);
defsymbol(zerop);
*/

static object quote_list_f;  /* Initialized by main to '(f) */
static object quote_list_t;  /* Initialized by main to '(t) */

static volatile object unify_subst = nil; /* This is a global Lisp variable. */

/* These (crufty) printing functions are used for debugging. */
static object terpri() {printf("\n"); return nil;}

static object prin1(x) object x;
{if (nullp(x)) {printf("nil "); return x;}
 switch (type_of(x))
   {case closure0_tag:
    case closure1_tag:
    case closure2_tag:
    case closure3_tag:
    case closure4_tag:
      printf("<%p>",(void *)((closure) x)->fn);
      break;
    case symbol_tag:
      printf("%s ",((symbol_type *) x)->pname);
      break;
    case cons_tag:
      printf("("); prin1(car(x)); printf("."); prin1(cdr(x)); printf(")");
      break;
    default:
      printf("prin1: bad tag x=%ld\n", ((closure)x)->tag); getchar(); exit(0);}
 return x;}

/* Some of these non-consing functions have been optimized from CPS. */

static object memberp(x,l) object x; list l;
{for (; !nullp(l); l = cdr(l)) if (equalp(x,car(l))) return quote_t;
 return nil;}

static object get(x,i) object x,i;
{register object plist; register object plistd;
 if (nullp(x)) return x;
 if (type_of(x)!=symbol_tag) {printf("get: bad x=%ld\n",((closure)x)->tag); exit(0);}
 plist = symbol_plist(x);
 for (; !nullp(plist); plist = cdr(plistd))
   {plistd = cdr(plist);
    if (eq(car(plist),i)) return car(plistd);}
 return nil;}

static object equalp(x,y) object x,y;
{for (; ; x = cdr(x), y = cdr(y))
   {if (eq(x,y)) return quote_t;
    if (nullp(x) || nullp(y) ||
        type_of(x)!=cons_tag || type_of(y)!=cons_tag) return nil;
    if (!equalp(car(x),car(y))) return nil;}}

static list assq(x,l) object x; list l;
{for (; !nullp(l); l = cdr(l))
   {register list la = car(l); if (eq(x,car(la))) return la;}
 return nil;}

/* Examples:
static list add_lemma(term) list term;
{object plist = symbol_plist(car(cadr(term)));
 if (nullp(plist))
   {plist = mlist2(quote_lemmas,nil);
    symbol_plist(car(cadr(term))) = plist;}
 {object cell = cdr(plist);
  car(cell) = mcons(term,car(cell));}
 return term;}

static void apply_subst_cont1(closure2,list) never_returns;

static void apply_subst(cont,alist,term) closure cont; list alist,term;
{if (atom(term))
   {list temp_temp = assq(term,alist);
    if (!nullp(temp_temp)) return_funcall1(cont,cdr(temp_temp));
    return_funcall1(cont,term);}
 {mclosure2(c,apply_subst_cont1,cont,car(term));
  return_check(apply_subst_lst((closure) &c,alist,cdr(term)));}}

static void apply_subst_cont1(env,dterm) closure2 env; list dterm;
{make_cons(c,env->elt2,dterm);
 return_funcall1(env->elt1,&c);}
*/

static void my_exit(closure) never_returns;

static void my_exit(env) closure env;
{printf("my_exit: heap bytes allocated=%d  time=%ld ticks  no_gcs=%ld\n",
        allocp-bottom,clock()-start,no_gcs);
 printf("my_exit: ticks/second=%ld\n",(long) CLOCKS_PER_SEC);
 exit(0);}

static void __halt(object obj) {
    printf("halt: ");
    //prin1(obj);
    //exit(0);
    my_exit(obj);
}


/* 
"---------------- input program:"
 */
/* 
(begin
  (display
    (((lambda (x.1) (lambda (y.2) (cons x.1 y.2))) #t)
     #f)))
 */
/* 
"---------------- after desugar:"
 */
/* 
(display
  (((lambda (x.1) (lambda (y.2) (cons x.1 y.2))) #t)
   #f))
 */
/* 
"---------------- after CPS:"
 */
/* 
((lambda (x.1)
   ((lambda (r$3)
      (r$3 (lambda (r$2)
             ((lambda (r$1) (%halt r$1)) (display r$2)))
           #f))
    (lambda (k$4 y.2) (k$4 (cons x.1 y.2)))))
 #t)
 */
/* 
"---------------- after wrap-mutables:"
 */
/* 
((lambda (x.1)
   ((lambda (r$3)
      (r$3 (lambda (r$2)
             ((lambda (r$1) (%halt r$1)) (display r$2)))
           #f))
    (lambda (k$4 y.2) (k$4 (cons x.1 y.2)))))
 #t)
 */
/* 
"---------------- after closure-convert:"
 */
/* 
((lambda (x.1)
   ((lambda (r$3)
      ((%closure-ref r$3 0)
       r$3
       (%closure
         (lambda (self$6 r$2)
           ((lambda (r$1) (%halt r$1)) (display r$2))))
       #f))
    (%closure
      (lambda (self$5 k$4 y.2)
        ((%closure-ref k$4 0)
         k$4
         (cons (%closure-ref self$5 1) y.2)))
      x.1)))
 #t)
 */
/* 
"---------------- C code:"
 */
/* 
DEBUG: (c-compile-app: ((lambda (x.1) ((lambda (r$3) ((%closure-ref r$3 0) r$3 (%closure (lambda (self$6 r$2) ((lambda (r$1) (%halt r$1)) (display r$2)))) #f)) (%closure (lambda (self$5 k$4 y.2) ((%closure-ref k$4 0) k$4 (cons (%closure-ref self$5 1) y.2))) x.1))) #t)) */
/* 
DEBUG: (c-compile-app: ((lambda (r$3) ((%closure-ref r$3 0) r$3 (%closure (lambda (self$6 r$2) ((lambda (r$1) (%halt r$1)) (display r$2)))) #f)) (%closure (lambda (self$5 k$4 y.2) ((%closure-ref k$4 0) k$4 (cons (%closure-ref self$5 1) y.2))) x.1))) */
/* 
DEBUG: (c-compile-app: ((%closure-ref r$3 0) r$3 (%closure (lambda (self$6 r$2) ((lambda (r$1) (%halt r$1)) (display r$2)))) #f)) */
/* 
DEBUG: (c-compile-app: ((lambda (r$1) (%halt r$1)) (display r$2))) */
/* 
DEBUG: (c-compile-app: (%halt r$1)) */
/* 
DEBUG: (c-compile-app: (display r$2)) */
/* 
DEBUG: ((%closure (lambda (self$6 r$2) ((lambda (r$1) (%halt r$1)) (display r$2)))) fv: ("r_733")) */
/* 
DEBUG: (c-compile-app: ((%closure-ref k$4 0) k$4 (cons (%closure-ref self$5 1) y.2))) */
/* 
DEBUG: (c-compile-app: (cons (%closure-ref self$5 1) y.2)) */
/* 
DEBUG: (c-compile-app: (%closure-ref self$5 1)) */
/* 
DEBUG: ((%closure (lambda (self$5 k$4 y.2) ((%closure-ref k$4 0) k$4 (cons (%closure-ref self$5 1) y.2))) x.1) fv: ("x_931")) */
static void __lambda_4() ;
static void __lambda_3() ;
static void __lambda_2() ;
static void __lambda_1() ;
static void __lambda_0() ;

static void __lambda_4(object x_931) {
  mclosure1(c_7314, __lambda_3,x_931);
  //return_check(__lambda_2(&c_7314));; 
  return_check2(__lambda_2, 1, (&c_7314)); 
}

static void __lambda_3(object self_735, object k_734, object y_932) {
  make_cons(c_7316, ((closure1)self_735)->elt1, y_932);
return_funcall1(k_734, &c_7316);; 
}

static void __lambda_2(object r_733) {
  mclosure1(c_7310, __lambda_1,r_733);
return_funcall2(r_733, &c_7310, quote_f);; 
}

static void __lambda_1(object self_736, object r_732) {
  return_check2(__lambda_0, 1, (prin1(r_732)));; 
  //return_check(__lambda_0(prin1(r_732)));; 
}

static void __lambda_0(object r_731) {
  __halt(r_731); 
}


static void test(env,cont) closure env,cont; { 
  return_check2(__lambda_4, 1, (quote_t));
  //return_check(__lambda_4(quote_t));
}


static char *transport(x) char *x;
/* Transport one object.  WARNING: x cannot be nil!!! */
{switch (type_of(x))
   {case cons_tag:
      {register list nx = (list) allocp;
       type_of(nx) = cons_tag; car(nx) = car(x); cdr(nx) = cdr(x);
       forward(x) = nx; type_of(x) = forward_tag;
       allocp = ((char *) nx)+sizeof(cons_type);
       return (char *) nx;}
    case closure0_tag:
      {register closure0 nx = (closure0) allocp;
       type_of(nx) = closure0_tag; nx->fn = ((closure0) x)->fn;
       forward(x) = nx; type_of(x) = forward_tag;
       allocp = ((char *) nx)+sizeof(closure0_type);
       return (char *) nx;}
    case closure1_tag:
      {register closure1 nx = (closure1) allocp;
       type_of(nx) = closure1_tag; nx->fn = ((closure1) x)->fn;
       nx->elt1 = ((closure1) x)->elt1;
       forward(x) = nx; type_of(x) = forward_tag;
       x = (char *) nx; allocp = ((char *) nx)+sizeof(closure1_type);
       return (char *) nx;}
    case closure2_tag:
      {register closure2 nx = (closure2) allocp;
       type_of(nx) = closure2_tag; nx->fn = ((closure2) x)->fn;
       nx->elt1 = ((closure2) x)->elt1;
       nx->elt2 = ((closure2) x)->elt2;
       forward(x) = nx; type_of(x) = forward_tag;
       x = (char *) nx; allocp = ((char *) nx)+sizeof(closure2_type);
       return (char *) nx;}
    case closure3_tag:
      {register closure3 nx = (closure3) allocp;
       type_of(nx) = closure3_tag; nx->fn = ((closure3) x)->fn;
       nx->elt1 = ((closure3) x)->elt1;
       nx->elt2 = ((closure3) x)->elt2;
       nx->elt3 = ((closure3) x)->elt3;
       forward(x) = nx; type_of(x) = forward_tag;
       x = (char *) nx; allocp = ((char *) nx)+sizeof(closure3_type);
       return (char *) nx;}
    case closure4_tag:
      {register closure4 nx = (closure4) allocp;
       type_of(nx) = closure4_tag; nx->fn = ((closure4) x)->fn;
       nx->elt1 = ((closure4) x)->elt1;
       nx->elt2 = ((closure4) x)->elt2;
       nx->elt3 = ((closure4) x)->elt3;
       nx->elt4 = ((closure4) x)->elt4;
       forward(x) = nx; type_of(x) = forward_tag;
       x = (char *) nx; allocp = ((char *) nx)+sizeof(closure4_type);
       return (char *) nx;}
    case forward_tag:
       return (char *) forward(x);
    case symbol_tag:
    default:
      printf("transport: bad tag x=%p x.tag=%ld\n",(void *)x,type_of(x)); exit(0);}
 return x;}

/* Use overflow macro which already knows which way the stack goes. */
#define transp(p) \
temp = (p); \
if (check_overflow(low_limit,temp) && \
    check_overflow(temp,high_limit)) \
   (p) = (object) transport(temp);

// TODO: will args always be a list of objects?
void GC_after(closure cfn, int count, ...) {
    object ans[10];
    int i;
    va_list args;
    va_start(args, count);
    for (i = 0; i < count; i++) {
        ans[i] = va_arg(args, object);
    }
    va_end(args);
    GC(cfn, ans, count);
    return;
}

static void GC(cont,ans,num_ans) closure cont; object *ans; int num_ans;
{char foo;
 int i;
 register char *scanp = allocp; /* Cheney scan pointer. */
 register object temp;
 register object low_limit = &foo; /* Move live data above us. */
 register object high_limit = stack_begin;
 no_gcs++;                      /* Count the number of minor GC's. */
 /* Transport GC's continuation and its argument. */
 transp(cont);
 gc_cont = cont; 
 gc_num_ans = num_ans;
 for (i = 0; i < num_ans; i++){ 
     transp(ans[i]);
     gc_ans[i] = ans[i];
 }
 /* Transport global variable. */
 transp(unify_subst);
 while (scanp<allocp)       /* Scan the newspace. */
   switch (type_of(scanp))
     {case cons_tag:
        transp(car(scanp)); transp(cdr(scanp));
        scanp += sizeof(cons_type); break;
      case closure0_tag:
        scanp += sizeof(closure0_type); break;
      case closure1_tag:
        transp(((closure1) scanp)->elt1);
        scanp += sizeof(closure1_type); break;
      case closure2_tag:
        transp(((closure2) scanp)->elt1); transp(((closure2) scanp)->elt2);
        scanp += sizeof(closure2_type); break;
      case closure3_tag:
        transp(((closure3) scanp)->elt1); transp(((closure3) scanp)->elt2);
        transp(((closure3) scanp)->elt3);
        scanp += sizeof(closure3_type); break;
      case closure4_tag:
        transp(((closure4) scanp)->elt1); transp(((closure4) scanp)->elt2);
        transp(((closure4) scanp)->elt3); transp(((closure4) scanp)->elt4);
        scanp += sizeof(closure4_type); break;
      case symbol_tag: default:
        printf("GC: bad tag scanp=%p scanp.tag=%ld\n",(void *)scanp,type_of(scanp));
        exit(0);}
 longjmp(jmp_main,1); /* Return globals gc_cont, gc_ans. */
}

/* This heap cons is used only for initialization. */
static list mcons(a,d) object a,d;
{register cons_type *c = malloc(sizeof(cons_type));
 c->tag = cons_tag; c->cons_car = a; c->cons_cdr = d;
 return c;}

static void main_main (stack_size,heap_size,stack_base)
     long stack_size,heap_size; char *stack_base;
{char in_my_frame;
 mclosure0(clos_exit,&my_exit);  /* Create a closure for exit function. */
 gc_ans[0] = &clos_exit;            /* It becomes the argument to test. */
 gc_num_ans = 1;
 /* Allocate stack buffer. */
 stack_begin = stack_base;
#if STACK_GROWS_DOWNWARD
 stack_limit1 = stack_begin - stack_size;
 stack_limit2 = stack_limit1 - 2000;
#else
 stack_limit1 = stack_begin + stack_size;
 stack_limit2 = stack_limit1 + 2000;
#endif
 printf("main: sizeof(cons_type)=%ld\n",(long) sizeof(cons_type));
 if (check_overflow(stack_base,&in_my_frame))
   {printf("main: Recompile with STACK_GROWS_DOWNWARD set to %ld\n",
           (long) (1-STACK_GROWS_DOWNWARD)); exit(0);}
 printf("main: stack_size=%ld  stack_base=%p  stack_limit1=%p\n",
        stack_size,(void *)stack_base,(void *)stack_limit1);
 printf("main: Try different stack sizes from 4 K to 1 Meg.\n");
 /* Do initializations of Lisp objects and rewrite rules. */
 quote_list_f = mlist1(quote_f); quote_list_t = mlist1(quote_t);
 /* Make temporary short names for certain atoms. */
 {

  /* Define the rules, but only those that are actually referenced. */

  /* Create closure for the test function. */
  mclosure0(run_test,&test);
  gc_cont = &run_test;
  /* Initialize constant expressions for the test runs. */

  /* Allocate heap area for second generation. */
  /* Use calloc instead of malloc to assure pages are in main memory. */
  printf("main: Allocating and initializing heap...\n");
  bottom = calloc(1,heap_size);
  allocp = (char *) ((((long) bottom)+7) & -8);
  alloc_end = allocp + heap_size - 8;
  printf("main: heap_size=%ld  allocp=%p  alloc_end=%p\n",
         (long) heap_size,(void *)allocp,(void *)alloc_end);
  printf("main: Try a larger heap_size if program bombs.\n");
  printf("Starting...\n");
  start = clock(); /* Start the timing clock. */
  /* These two statements form the most obscure loop in the history of C! */
  setjmp(jmp_main); 
  if (gc_num_ans == 1) {
      funcall1((closure) gc_cont,gc_ans[0]);
  } else if (gc_num_ans == 2) {
      funcall2((closure) gc_cont,gc_ans[0],gc_ans[1]);
  } else {
      printf("Unsupported number of args from GC %d\n", gc_num_ans);
  }
  /*                                                                      */
  printf("main: your setjmp and/or longjmp are broken.\n"); exit(0);}}

static long long_arg(argc,argv,name,dval)
     int argc; char **argv; char *name; long dval;
{int j;
 for(j=1;(j+1)<argc;j += 2)
   if (strcmp(name,argv[j]) == 0)
     return(atol(argv[j+1]));
 return(dval);}

main(int argc,char **argv)
{long stack_size = long_arg(argc,argv,"-s",STACK_SIZE);
 long heap_size = long_arg(argc,argv,"-h",HEAP_SIZE);
 main_main(stack_size,heap_size,(char *) &stack_size);
 return 0;}
