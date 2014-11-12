/* 
 * Cyclone Scheme
 * This file contains the C runtime used by compiled programs.
 */

#define DEBUG_ALWAYS_GC 1
#define DEBUG_SHOW_DIAG 1

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

/* Define tag values.  (I don't trust compilers to optimize enums.) */
#define cons_tag 0
#define symbol_tag 1
#define forward_tag 2
#define closure0_tag 3
#define closure1_tag 4
#define closure2_tag 5
#define closure3_tag 6
#define closure4_tag 7
#define closureN_tag 8
#define integer_tag 9
#define double_tag 10

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
typedef struct {tag_type tag; function_type fn; int num_elt; object *elts;} closureN_type;

typedef closure0_type *closure0;
typedef closure1_type *closure1;
typedef closure2_type *closure2;
typedef closure3_type *closure3;
typedef closure4_type *closure4;
typedef closureN_type *closureN;
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
#define setq(x,e) x = e

// TODO: no portable way to pass variable args to a macro, so, may need to
//       generate mclosureN code from scheme, instead of trying to have a
//       generic C function. Just doing the below as exploratory research
#define DEBUG_mclosure(c, f, a1, a2) closureN_type c; c.tag = closureN_tag; c.fn = f; \
  c.num_elt = 2; c.elts = (object *)alloca(sizeof(object) * c.num_elt); c.elts[0] = a1; c.elts[1] = a2;


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
{object tmp = nil;
 if (nullp(x)) {printf("()"); return x;}
 switch (type_of(x))
   {case closure0_tag:
    case closure1_tag:
    case closure2_tag:
    case closure3_tag:
    case closure4_tag:
    case closureN_tag:
      printf("<%p>",(void *)((closure) x)->fn);
      break;
    case symbol_tag:
      printf("%s",((symbol_type *) x)->pname);
      break;
    case integer_tag:
      printf("%d", ((integer_type *) x)->value);
      break;
    case double_tag:
      printf("%lf", ((double_type *) x)->value);
      break;
    case cons_tag:
      printf("("); 
      prin1(car(x));
      for (tmp = cdr(x); tmp && ((closure) tmp)->tag == cons_tag; tmp = cdr(tmp)) {
          printf(" ");
          prin1(car(tmp));
      }
      if (tmp) {
          printf(" . ");
          prin1(tmp);
      }
      printf(")");
      break;
    default:
      printf("prin1: bad tag x=%ld\n", ((closure)x)->tag); getchar(); exit(0);}
 return x;}

static object write(x) object x;
{prin1(x);
 printf("\n");
 return x;}

/* Some of these non-consing functions have been optimized from CPS. */

static object memberp(x,l) object x; list l;
{for (; !nullp(l); l = cdr(l)) if (quote_f != equalp(x,car(l))) return quote_t;
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
        type_of(x)!=cons_tag || type_of(y)!=cons_tag) return quote_f;
    if (quote_f == equalp(car(x),car(y))) return quote_f;}}

static list assq(x,l) object x; list l;
{for (; !nullp(l); l = cdr(l))
   {register list la = car(l); if (eq(x,car(la))) return la;}
 return nil;}


// TODO: generate these using macros???
static object __num_eq(x, y) object x, y;
{if (((integer_type *)x)->value == ((integer_type *)y)->value)
    return quote_t;
 return quote_f;}

static object __num_gt(x, y) object x, y;
{//printf("DEBUG cmp %d, x %d, y %d, x tag %d, y tag %d\n", 
 //   (((integer_type *)x)->value > ((integer_type *)y)->value),
 //   ((integer_type *)x)->value, ((integer_type *)y)->value,
 //   ((list)x)->tag, ((list)y)->tag);
 //exit(1);
 if (((integer_type *)x)->value > ((integer_type *)y)->value)
    return quote_t;
 return quote_f;}

static object __num_lt(x, y) object x, y;
{if (((integer_type *)x)->value < ((integer_type *)y)->value)
    return quote_t;
 return quote_f;}

static object __num_gte(x, y) object x, y;
{if (((integer_type *)x)->value >= ((integer_type *)y)->value)
    return quote_t;
 return quote_f;}

static object __num_lte(x, y) object x, y;
{if (((integer_type *)x)->value <= ((integer_type *)y)->value)
    return quote_t;
 return quote_f;}

// TODO: static object Cyc_is_eq(x, y) object x, y)
static object Cyc_is_boolean(object o){
    if (!nullp(o) && 
        ((list)o)->tag == symbol_tag &&
        (eq(quote_f, o) || eq(quote_t, o)))
        return quote_t;
    return quote_f;}

static object Cyc_is_cons(object o){
    if (!nullp(o) && ((list)o)->tag == cons_tag)
        return quote_t;
    return quote_f;}

static object Cyc_is_number(object o){
    if (!nullp(o) && ((list)o)->tag == integer_tag)
        return quote_t;
    return quote_f;}

static object Cyc_is_symbol(object o){
    if (!nullp(o) && ((list)o)->tag == symbol_tag)
        return quote_t;
    return quote_f;}

static integer_type Cyc_length(object l){
    make_int(len, 0);
    while(!nullp(l)){
        if (((list)l)->tag != cons_tag){
            printf("length - invalid parameter, expected list\n");
            exit(1);
        }
        l = cdr(l);
        len.value++;
    }
    return len;
}
/*static object sum(object x, object y) {}*/

static void my_exit(closure) never_returns;

static void my_exit(env) closure env; {
#if DEBUG_SHOW_DIAG
    printf("my_exit: heap bytes allocated=%d  time=%ld ticks  no_gcs=%ld\n",
        allocp-bottom,clock()-start,no_gcs);
 printf("my_exit: ticks/second=%ld\n",(long) CLOCKS_PER_SEC);
#endif
 exit(0);}

static void __halt(object obj) {
#if DEBUG_SHOW_DIAG
    printf("\nhalt: ");
    prin1(obj);
    printf("\n");
#endif
    my_exit(obj);
}

#define __sum(c,x,y) integer_type c; c.tag = integer_tag; c.value = (((integer_type *)(x))->value + ((integer_type *)(y))->value);
#define __mul(c,x,y) integer_type c; c.tag = integer_tag; c.value = (((integer_type *)(x))->value * ((integer_type *)(y))->value);
#define __sub(c,x,y) integer_type c; c.tag = integer_tag; c.value = (((integer_type *)(x))->value - ((integer_type *)(y))->value);
#define __div(c,x,y) integer_type c; c.tag = integer_tag; c.value = (((integer_type *)(x))->value / ((integer_type *)(y))->value);



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
    case closureN_tag:
      {register closureN nx = (closureN) allocp;
       int i;
       type_of(nx) = closureN_tag; nx->fn = ((closureN) x)->fn;
       nx->num_elt = ((closureN) x)->num_elt;
       nx->elts = (object *)(((char *)nx) + sizeof(closureN_type));
       for (i = 0; i < nx->num_elt; i++) {
         nx->elts[i] = ((closureN) x)->elts[i];
       }
       forward(x) = nx; type_of(x) = forward_tag;
       x = (char *) nx; allocp = ((char *) nx)+sizeof(closureN_type) + sizeof(object) * nx->num_elt;
       return (char *) nx;}
    case integer_tag:
      {register integer_type *nx = (integer_type *) allocp;
       type_of(nx) = integer_tag; nx->value = ((integer_type *) x)->value;
       forward(x) = nx; type_of(x) = forward_tag;
       x = (char *) nx; allocp = ((char *) nx)+sizeof(integer_type);
       return (char *) nx;}
    case forward_tag:
       return (char *) forward(x);
    case symbol_tag:
    default:
      printf("transport: bad tag x=%p x.tag=%ld\n",(void *)x,type_of(x)); exit(0);}
 return x;}

/* Use overflow macro which already knows which way the stack goes. */
/* Major collection, transport objects on stack or old heap */
#define transp(p) \
temp = (p); \
if ((check_overflow(low_limit,temp) && \
     check_overflow(temp,high_limit)) || \
    (check_overflow(old_heap_low_limit - 1, temp) && \
     check_overflow(temp,old_heap_high_limit + 1))) \
   (p) = (object) transport(temp);

static void GC_loop(int major, closure cont, object *ans, int num_ans)
{char foo;
 int i;
 register object temp;
 register object low_limit = &foo; /* Move live data above us. */
 register object high_limit = stack_begin;
 register char *scanp = allocp; /* Cheney scan pointer. */
 register object old_heap_low_limit = low_limit; // Minor-GC default
 register object old_heap_high_limit = high_limit; // Minor-GC default

 char *tmp_bottom = bottom;    /* Bottom of tospace. */
 char *tmp_allocp = allocp;    /* Cheney allocate pointer. */
 char *tmp_alloc_end = alloc_end;
 if (major) {
    // Initialize new heap (TODO: make a function for this)
    bottom = calloc(1,global_heap_size);
    allocp = (char *) ((((long) bottom)+7) & -8);
    alloc_end = allocp + global_heap_size - 8;
    scanp = allocp;
    old_heap_low_limit = tmp_bottom;
    old_heap_high_limit = tmp_alloc_end;
 }

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
      case closureN_tag:
       {int i;
        for (i = 0; i < ((closureN) scanp)->num_elt; i++) {
          transp(((closureN) scanp)->elts[i]);
        }
       }
       scanp += sizeof(closureN_type) + sizeof(object) * ((closureN) scanp)->num_elt;
       break;
      case integer_tag:
        scanp += sizeof(integer_type); break;
      case symbol_tag: default:
        printf("GC: bad tag scanp=%p scanp.tag=%ld\n",(void *)scanp,type_of(scanp));
        exit(0);}

 if (major) {
     free(tmp_bottom);
 }
}

static void GC(cont,ans,num_ans) closure cont; object *ans; int num_ans;
{
 // JAE - Only room for one more minor-GC, so do a major one.
 // Not sure this is the best strategy, it may be better to do major
 // ones sooner, perhaps after every x minor GC's.
 //
 // Also may need to consider dynamically increasing heap size, but
 // by how much (1.3x, 1.5x, etc) and when? I suppose when heap usage
 // after a collection is above a certain percentage, then it would be 
 // necessary to increase heap size the next time.
 //
 if (allocp >= (bottom + (global_heap_size - global_stack_size))) {
     //printf("Possibly only room for one more minor GC. no_gcs = %ld\n", no_gcs);
     GC_loop(1, cont, ans, num_ans);
 } else {
     no_gcs++; /* Count the number of minor GC's. */
     GC_loop(0, cont, ans, num_ans);
 }
 // /JAE
 longjmp(jmp_main,1); /* Return globals gc_cont, gc_ans. */
}

/* This heap cons is used only for initialization. */
static list mcons(a,d) object a,d;
{register cons_type *c = malloc(sizeof(cons_type));
 c->tag = cons_tag; c->cons_car = a; c->cons_cdr = d;
 return c;}

static void c_entry_pt(closure,closure);
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
#if DEBUG_SHOW_DIAG
 printf("main: sizeof(cons_type)=%ld\n",(long) sizeof(cons_type));
#endif
 if (check_overflow(stack_base,&in_my_frame))
   {printf("main: Recompile with STACK_GROWS_DOWNWARD set to %ld\n",
           (long) (1-STACK_GROWS_DOWNWARD)); exit(0);}
#if DEBUG_SHOW_DIAG
 printf("main: stack_size=%ld  stack_base=%p  stack_limit1=%p\n",
        stack_size,(void *)stack_base,(void *)stack_limit1);
 printf("main: Try different stack sizes from 4 K to 1 Meg.\n");
#endif
 /* Do initializations of Lisp objects and rewrite rules.
 quote_list_f = mlist1(quote_f); quote_list_t = mlist1(quote_t); */
 /* Make temporary short names for certain atoms. */
 {

  /* Define the rules, but only those that are actually referenced. */

  /* Create closure for the test function. */
  mclosure0(run_test,&c_entry_pt);
  gc_cont = &run_test;
  /* Initialize constant expressions for the test runs. */

  /* Allocate heap area for second generation. */
  /* Use calloc instead of malloc to assure pages are in main memory. */
#if DEBUG_SHOW_DIAG
  printf("main: Allocating and initializing heap...\n");
#endif
  bottom = calloc(1,heap_size);
  allocp = (char *) ((((long) bottom)+7) & -8);
  alloc_end = allocp + heap_size - 8;
#if DEBUG_SHOW_DIAG
  printf("main: heap_size=%ld  allocp=%p  alloc_end=%p\n",
         (long) heap_size,(void *)allocp,(void *)alloc_end);
  printf("main: Try a larger heap_size if program bombs.\n");
  printf("Starting...\n");
#endif
  start = clock(); /* Start the timing clock. */
  /* These two statements form the most obscure loop in the history of C! */
  setjmp(jmp_main);
  AFTER_LONGJMP
  /*                                                                      */
  printf("main: your setjmp and/or longjmp are broken.\n"); exit(0);}}

static long long_arg(argc,argv,name,dval)
     int argc; char **argv; char *name; long dval;
{int j;
 for(j=1;(j+1)<argc;j += 2)
   if (strcmp(name,argv[j]) == 0)
     return(atol(argv[j+1]));
 return(dval);}

