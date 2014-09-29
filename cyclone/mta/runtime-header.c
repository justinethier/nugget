
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

long global_stack_size;
long global_heap_size;

/* Define size of Lisp tags.  Options are "short" or "long". */
typedef long tag_type;

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <setjmp.h>
// #include <stdarg.h>
#include <string.h>

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
 if (check_overflow(&stack,stack_limit1)) { \
     object buf[1]; buf[0] = a1; \
     GC(cfn,buf,1); return; \
 } else {funcall1((closure) (cfn),a1); return;}}

/* TODO: need to check the stack, and figure out how to deal with
         second arg with GC */
#define return_funcall2(cfn,a1,a2) \
{char stack; \
 if (check_overflow(&stack,stack_limit1)) { \
     object buf[2]; buf[0] = a1; buf[1] = a2; \
     GC(cfn,buf,2); return; \
 } else {funcall2((closure) (cfn),a1,a2); return;}}


/* Evaluate an expression after checking for stack overflow. */
#define return_check1(_fn, a1) { \
 char stack; \
 if (check_overflow(&stack,stack_limit1)) { \
     object buf[1]; buf[0] = a1; \
     mclosure0(c1, _fn); \
     GC(&c1, buf, 1); return; \
 } else { (_fn)((closure)_fn, a1); }}
//GC_after(&c1, count, args); return; 

/* Define tag values.  (I don't trust compilers to optimize enums.) */
#define cons_tag 0
#define symbol_tag 1
#define forward_tag 2
#define closure0_tag 3
#define closure1_tag 4
#define closure2_tag 5
#define closure3_tag 6
#define closure4_tag 7
#define integer_tag 8
#define double_tag 9

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

/* Define numeric types (experimental) */
typedef struct {tag_type tag; int value;} integer_type;
#define make_int(n,v) integer_type n; n.tag = integer_tag; n.value = v;
typedef struct {tag_type tag; double value;} double_type;
#define make_double(n,v) double_type n; n.tag = double_tag; n.value = v;

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

#define make_cell(n,a) make_cons(n,a,nil);
static object cell_get(object cell){
    return car(cell);
}
static object cell_set(object cell, object value){
    ((list) cell)->cons_car = value;
    return cell;
}

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

//static object test_exp1, test_exp2; /* Expressions used within test. */

/* Define the Lisp atoms that we need. */

defsymbol(f);
defsymbol(t);

//static object quote_list_f;  /* Initialized by main to '(f) */
//static object quote_list_t;  /* Initialized by main to '(t) */

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
    case integer_tag:
      printf("%d ", ((integer_type *) x)->value);
      break;
    case double_tag:
      printf("%lf ", ((double_type *) x)->value);
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

static object sum(object x, object y) {
}

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


