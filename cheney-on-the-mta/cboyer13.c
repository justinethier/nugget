/*--------------------- Cut Here -----------------------------------*/
/* cboyer-1-3.c version 1.3 February 20, 1994                       */
/* Changed since 1.2:                                               */
/* Print version number on herald.                                  */
/* Functions declared with 'void' returned value.                   */
/* Gnu declarations indicating that functions don't return.         */
/* cboyer-1-2.c version 1.2 February 16, 1994                       */
/* Changed since 1.1:                                               */
/* Command line args -s, -h for stack_size and heap_size, resp.     */
/* cboyer-1-1.c version 1.1 February 14, 1994                       */
/* Changed since 1.0:                                               */
/* Fixed 'static' problem.                                          */
/* Fixed message re setjmp/longjmp.                                 */
/* Fixed 80860 numbers due to incorrect timer info.                 */
/* Fixed stack direction and GC stack stuff.                        */
/* cboyer-1-0.c version 1.0 February 11, 1994                       */

/* Copyright (c) 1994 by Nimble Computer Corporation.               */
/* 16231 Meadow Ridge Way, Encino, CA 91436, U.S.A.                 */
/* (818) 986-1436.  FAX: (818) 986-1360.                            */
/* Written by Henry G. Baker.  hbaker1@pipeline.com                 */
/* This code is NOT public domain, but is distributed to            */
/* comp.lang.scheme.c Usenet mailing list readers for comments      */
/* and benchmark tests on various machines.                         */

/* This file is an ANSI C version of the Gabriel "Boyer Benchmark", */
/* which tests the concepts in my paper "Cheney on the MTA".        */
/* This CBOYER program should return 't'.                           */

/* Since this benchmark runs so fast on modern machines, Bob Boyer  */
/* (boyer@cli.com) has produced two scalable "upgrades".  The first */
/* "SuperBoyer" simply changes slightly the original tautology to   */
/* produce more difficult problems based on a parameter N.  The     */
/* second "SuperDuperBoyer" uses the same framework to prove the    */
/* very hard "pigeonhole principle"--N+1 pigeons cannot be placed   */
/* in N holes, without at least one hole receiving two pigeons.     */
/* These upgrades can easily be made to this C version, as well.    */

/* The total address space allocated is approximately               */
/* STACK_SIZE*no_gcs, which is about 40 Mbytes on the Mac.  The     */
/* memory space used is STACK_SIZE + 'heap bytes allocated', which  */
/* is a minimum when STACK_SIZE is about 256Kbytes.  CBOYER runs    */
/* best with optimizations which dispense with frame building.      */

/* On the Macintosh Plus w/ Radius 68020 Accelerator, CBOYER        */
/* reaches its maximum speed of 29.6 secs. with a 14kbyte stack     */
/* and uses 2,189,866 bytes of heap.  It does 2916 minor GC's.      */
/* This speed is IDENTICAL to that of native compiled Coral Common  */
/* Lisp 1.2 on the same configuration with no garbage collections.  */

/* On a 33 MHz Intel 80860, CBOYER reaches its maximum speed of     */
/* 1.71 secs. with a 300kbyte stack and uses 1,357,468 bytes of     */
/* heap.  It does 131 minor GC's.  This speed is 1.46 X faster      */
/* than Kyoto Common Lisp (KCL) which compiles into C.              */

/* References:
Appel, A.W.  "Garbage collection can be faster than stack allocation".
   Info. Proc. Letters 25,4 (1987), 275-279.
Appel, A.W.  "Simple Generational Garbage Collection and Fast Allocation".
   SW Prac. & Exper. 19,2 (1989), 171+.
Appel, A.W.  "A Runtime System".  Lisp&Symb Comp 3,4 (1990), 343-380.
Baker, H.G.  "List Processing in Real Time on a Serial Computer".
   CACM 21,4 (April 1978), 280-294.  Available in
   http://home.pipeline.com/~hbaker/RealTimeGC.ps.Z.
Baker, H.G.  "CONS Should Not CONS Its Arguments, Part II: Cheney
   on the M.T.A."  Draft Memorandum, Jan., 1994.  Available from
   http://home.pipeline.com/~hbaker1/CheneyMTA.ps.Z.
Baker, H.G.  "The Boyer Benchmark Meets Linear Logic".  ACM Lisp
   Pointers VI, 4 (Oct/Dec 1993), 3-10.  Available in
   http://home.pipeline.com/~hbaker1/LBoyer.ps.Z.
Baker, H.G.  "The Boyer Benchmark at Warp Speed".  ACM Lisp
   Pointers V,3 (Jul/Sep 1992), 13-14.  Available in
   http://home.pipeline.com/~hbaker1/BoyerB.ps.Z.
Baker, H.G.  "CONS Should Not CONS Its Arguments".  ACM Sigplan
   Notices 27,3 (Mar 1992), 371-398.  Available in
   http://home.pipeline.com/~hbaker1/LazyAlloc.ps.Z.
Cheney, C.J.  "A nonrecursive list compacting algorithm".  CACM 13,11
   (1970), 677-678.
Diwan, A., Tarditi, D., and Moss. E.  "Memory subsystem performance
   of programs using copying garbage collection".  ACM POPL 21, 1994.
Gabriel, R.P.  Performance and Evaluation of Lisp Systems.  MIT
   Press, 1986.
Tarditi, D., & Lee, P.  "No assembly required: Compiling standard ML
   to C".  ACM LOPLAS 1,2 (1992), 161-177.
*/

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
#ifdef THINK_C
typedef short tag_type;
#undef STACK_SIZE
#define STACK_SIZE 14000
#undef HEAP_SIZE
#define HEAP_SIZE 3000000
#define const  
#define volatile  
#else
typedef long tag_type;
#endif

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <setjmp.h>
#include <string.h>

#ifndef CLOCKS_PER_SEC
/* gcc doesn't define this, even though ANSI requires it in <time.h>.. */
#define CLOCKS_PER_SEC 0
#define setjmp _setjmp
#define longjmp _longjmp
#endif

/* The preprocessor symbol THINK_C is used for the Macintosh. */
/* (There apparently isn't a separate Macintosh Operating System switch). */
#ifdef THINK_C
#include <MemoryMgr.h>
#include <console.h>
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
 if (check_overflow(&stack,stack_limit1)) {GC(cfn,a1); return;} \
    else {funcall1((closure) (cfn),a1); return;}}

/* Evaluate an expression after checking for stack overflow. */
/* (Overflow checking has been "optimized" away for this version). */
#define return_check(exp) {exp; return;}

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
static void GC(closure,object) never_returns;

/* Prototypes for Boyer Benchmark functions. */

static list add_lemma(list);
static void apply_subst(closure,list,list) never_returns;
static void apply_subst_lst(closure,list,list) never_returns;
#define falsep(x,lst) (or(equalp(x,quote_list_f),memberp(x,lst)))
static void one_way_unify(closure,list,list) never_returns;
static void one_way_unify1(closure,list,list) never_returns;
static void one_way_unify1_lst(closure,list,list) never_returns;
static void rewrite(closure,list) never_returns;
static void rewrite_args(closure,list) never_returns;
static void rewrite_with_lemmas(closure,list,list) never_returns;
static void tautologyp(closure,object,object,object) never_returns;
static void tautp(closure,object) never_returns;
static void test(closure,closure) never_returns;
#define truep(x,lst) (or(equalp(x,quote_list_t),memberp(x,lst)))
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
static volatile object gc_ans;    /* argument for GC continuation closure. */
static jmp_buf jmp_main; /* Where to jump to. */

static object test_exp1, test_exp2; /* Expressions used within test. */

/* Define the Lisp atoms that we need. */

defsymbol(a);
defsymbol(b);
defsymbol(c);
defsymbol(d);
defsymbol(e);
defsymbol(f);
defsymbol(i);
defsymbol(j);
defsymbol(l);
defsymbol(p);
defsymbol(q);
defsymbol(t);
defsymbol(u);
defsymbol(w);
defsymbol(x);
defsymbol(y);
defsymbol(z);
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
      printf("<%ld>",((closure) x)->fn);
      break;
    case symbol_tag:
      printf("%s ",((symbol_type *) x)->pname);
      break;
    case cons_tag:
      printf("("); prin1(car(x)); printf("."); prin1(cdr(x)); printf(")");
      break;
    default:
      printf("prin1: bad tag x=%ld\n",x); getchar(); exit(0);}
 return x;}

/* Some of these non-consing functions have been optimized from CPS. */

static object memberp(x,l) object x; list l;
{for (; !nullp(l); l = cdr(l)) if (equalp(x,car(l))) return quote_t;
 return nil;}

static object get(x,i) object x,i;
{register object plist; register object plistd;
 if (nullp(x)) return x;
 if (type_of(x)!=symbol_tag) {printf("get: bad x=%ld\n",x); exit(0);}
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

static void apply_subst_lst_cont1(closure3,list) never_returns;

static void apply_subst_lst(cont,alist,lst) closure cont; list alist,lst;
{if (nullp(lst)) return_funcall1(cont,nil);
 {mclosure3(c,apply_subst_lst_cont1,cont,alist,cdr(lst));
  return_check(apply_subst((closure) &c,alist,car(lst)));}}

static void apply_subst_lst_cont2(closure2,list) never_returns;

static void apply_subst_lst_cont1(env,fst) closure3 env; list fst;
/* env = <cont,alist,lst> */
{mclosure2(c,apply_subst_lst_cont2,env->elt1,fst);
 return_check(apply_subst_lst((closure) &c,env->elt2,env->elt3));}

static void apply_subst_lst_cont2(env,rst) closure2 env; list rst;
/* env = <cont,fst> */
{make_cons(c,env->elt2,rst);      /* Allocate new cons cell. */
 return_funcall1(env->elt1,&c);}  /* Call continuation with new cons cell */

static void rewrite_cont1(closure2,list) never_returns;

static void rewrite(cont,term) closure cont; list term;
{if (atom(term)) return_funcall1(cont,term);
 {mclosure2(c,rewrite_cont1,cont,car(term));
  return_check(rewrite_args((closure) &c,cdr(term)));}}

static void rewrite_cont1(env,rargs) closure2 env; list rargs;
{register list aterm = env->elt2;
 make_cons(c,aterm,rargs);
 return_check(rewrite_with_lemmas(env->elt1,&c,get(aterm,quote_lemmas)));}

static void rewrite_args_cont1(closure2,list) never_returns;

static void rewrite_args(cont,lst) closure0 cont; list lst;
{if (nullp(lst)) return_funcall1(cont,lst);
 {mclosure2(c,rewrite_args_cont1,cont,cdr(lst));
  return_check(rewrite((closure) &c,car(lst)));}}

static void rewrite_args_cont2(closure2,list) never_returns;

static void rewrite_args_cont1(env,rlsta) closure2 env; list rlsta;
{mclosure2(c,rewrite_args_cont2,env->elt1,rlsta);
 return_check(rewrite_args((closure) &c,env->elt2));}

static void rewrite_args_cont2(env,rlstd) closure2 env; list rlstd;
{make_cons(c,env->elt2,rlstd);
 return_funcall1(env->elt1,&c);}

static void one_way_unify1_lst_cont1(closure3,list) never_returns;

static void one_way_unify1_lst(cont,lst1,lst2) closure cont; list lst1,lst2;
{if (nullp(lst1)) return_funcall1(cont,quote_t);
 {mclosure3(c,one_way_unify1_lst_cont1,cont,cdr(lst1),cdr(lst2));
  return_check(one_way_unify1((closure) &c,car(lst1),car(lst2)));}}

static void one_way_unify1_lst_cont1(env,ucars) closure3 env; list ucars;
{if (nullp(ucars)) return_funcall1(env->elt1,nil);
 return_check(one_way_unify1_lst(env->elt1,env->elt2,env->elt3));}

static void one_way_unify(cont,term1,term2) closure cont; list term1,term2;
{setq(unify_subst,nil);
 return_check(one_way_unify1(cont,term1,term2));}

static list assq(x,l) object x; list l;
{for (; !nullp(l); l = cdr(l))
   {register list la = car(l); if (eq(x,car(la))) return la;}
 return nil;}

static void one_way_unify1(cont,term1,term2) closure cont; list term1,term2;
{if (atom(term2))
   {/* inlined temp_temp = assq(term2,unify_subst); */
     register list l = unify_subst;
     for (; !nullp(l); l = cdr(l))
       if (eq(term2,caar(l))) return_funcall1(cont,equalp(term1,cdar(l)));
     {make_cons(c1,term2,term1);
      {make_cons(c2,&c1,unify_subst);
       setq(unify_subst,&c2);
       return_funcall1(cont,quote_t);}}}
 if (atom(term1)) return_funcall1(cont,nil);
 if (eq(car(term1),car(term2)))
   {return_check(one_way_unify1_lst(cont,cdr(term1),cdr(term2)));}
 return_funcall1(cont,nil);}

static void rewrite_with_lemmas_cont1(closure3,list) never_returns;

static void rewrite_with_lemmas(cont,term,lst) closure cont; list term,lst;
{if (nullp(lst)) return_funcall1(cont,term);
 {mclosure3(c,rewrite_with_lemmas_cont1,cont,term,lst);
  return_check(one_way_unify((closure) &c,term,cadr(car(lst))));}}

static void rewrite_with_lemmas_cont2(closure1,list) never_returns;

static void rewrite_with_lemmas_cont1(env,ut) closure3 env; list ut;
{register list lst = env->elt3;
 if (!nullp(ut))
   {mclosure1(c,rewrite_with_lemmas_cont2,env->elt1);
    return_check(apply_subst((closure) &c,unify_subst,caddr(car(lst))));}
 return_check(rewrite_with_lemmas(env->elt1,env->elt2,cdr(lst)));}

static void rewrite_with_lemmas_cont2(env,term) closure1 env; list term;
{unify_subst = nil; /* No longer need this stuff. */
 return_check(rewrite(env->elt1,term));}

static void tautologyp_cont1(closure4,object) never_returns;

static void tautologyp(cont,x,true_lst,false_lst)
     closure cont; object x,true_lst,false_lst;
{if (truep(x,true_lst)) return_funcall1(cont,quote_t);
 if (falsep(x,false_lst)) return_funcall1(cont,nil);
 if (atom(x)) return_funcall1(cont,nil);
 if (!eq(car(x),quote_if)) return_funcall1(cont,nil);
 {register object dx = cdr(x);
  register object adx = car(dx);
  if (truep(adx,true_lst))
    {return_check(tautologyp(cont,cadr(dx),true_lst,false_lst));}
  if (falsep(adx,false_lst))
    {return_check(tautologyp(cont,caddr(dx),true_lst,false_lst));}
  {mclosure4(c1,tautologyp_cont1,cont,dx,true_lst,false_lst);
   {make_cons(c2,adx,true_lst);
    return_check(tautologyp((closure) &c1,cadr(dx),&c2,false_lst));}}}}

static void tautologyp_cont1(env,tarm) closure4 env; object tarm;
{if (nullp(tarm)) return_funcall1(env->elt1,nil);
 {register object dx = env->elt2;
  make_cons(c,car(dx),env->elt4);
  return_check(tautologyp(env->elt1,caddr(dx),env->elt3,&c));}}

static void tautp_cont1(closure1,object) never_returns;

static void tautp(cont,x) closure cont; object x;
{mclosure1(c,tautp_cont1,cont);
 return_check(rewrite((closure) &c,x));}

static void tautp_cont1(env,rx) closure1 env; object rx;
{return_check(tautologyp(env->elt1,rx,nil,nil));}

static void test_cont1(closure1,object) never_returns;

static void test(env,cont) closure env,cont;
{mclosure1(cont1,test_cont1,cont);
 return_check(apply_subst((closure) &cont1,test_exp1,test_exp2));}

static void test_cont2(closure1,object) never_returns;

static void test_cont1(env,term) closure1 env; object term;
{mclosure1(c,test_cont2,env->elt1);
 return_check(tautp((closure) &c,term));}

static void test_cont2(env,ans) closure1 env; object ans;
{printf("Answer is: "); prin1(ans); terpri();
 return_funcall1(env->elt1,ans);}

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
    case forward_tag:	return (char *) forward(x);
    case symbol_tag:
    default:
      printf("transport: bad tag x=%ld x.tag=%ld\n",x,type_of(x)); exit(0);}
 return x;}

/* Use overflow macro which already knows which way the stack goes. */
#define transp(p) \
temp = (p); \
if (check_overflow(low_limit,temp) && \
    check_overflow(temp,high_limit)) \
   (p) = (object) transport(temp);

static void GC(cont,ans) closure cont; object ans;
{char foo;
 register char *scanp = allocp; /* Cheney scan pointer. */
 register object temp;
 register object low_limit = &foo; /* Move live data above us. */
 register object high_limit = stack_begin;
 no_gcs++;                      /* Count the number of minor GC's. */
 /* Transport GC's continuation and its argument. */
 transp(cont); transp(ans);
 gc_cont = cont; gc_ans = ans;
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
	printf("GC: bad tag scanp=%ld scanp.tag=%ld\n",scanp,type_of(scanp));
	exit(0);}
 longjmp(jmp_main,1); /* Return globals gc_cont, gc_ans. */
}

/* This heap cons is used only for initialization. */
static list mcons(a,d) object a,d;
{register cons_type *c = malloc(sizeof(cons_type));
 c->tag = cons_tag; c->cons_car = a; c->cons_cdr = d;
 return c;}

static void my_exit(closure) never_returns;

static void my_exit(env) closure env;
{printf("my_exit: heap bytes allocated=%ld  time=%ld ticks  no_gcs=%ld\n",
        allocp-bottom,clock()-start,no_gcs);
 printf("my_exit: ticks/second=%ld\n",(long) CLOCKS_PER_SEC);
 exit(0);}

static void main_main (stack_size,heap_size,stack_base)
     long stack_size,heap_size; char *stack_base;
{char in_my_frame;
 mclosure0(clos_exit,&my_exit);  /* Create a closure for exit function. */
 gc_ans = &clos_exit;            /* It becomes the argument to test. */
 /* Allocate stack buffer. */
#ifdef THINK_C
 SetApplLimit((Ptr) (GetApplLimit()-stack_size)); MaxApplZone();
#endif
 stack_begin = stack_base;
#if STACK_GROWS_DOWNWARD
 stack_limit1 = stack_begin - stack_size;
 stack_limit2 = stack_limit1 - 2000;
#else
 stack_limit1 = stack_begin + stack_size;
 stack_limit2 = stack_limit1 + 2000;
#endif
 printf("main: CBOYER version 1.3 Copyright (c) 1994 by Nimble Computer Corporation\n");
 printf("main: sizeof(cons_type)=%ld\n",(long) sizeof(cons_type));
 if (check_overflow(stack_base,&in_my_frame))
   {printf("main: Recompile with STACK_GROWS_DOWNWARD set to %ld\n",
           (long) (1-STACK_GROWS_DOWNWARD)); exit(0);}
 printf("main: stack_size=%ld  stack_base=%ld  stack_limit1=%ld\n",
        stack_size,stack_base,stack_limit1);
 printf("main: Try different stack sizes from 4 K to 1 Meg.\n");
 /* Do initializations of Lisp objects and rewrite rules. */
 quote_list_f = mlist1(quote_f); quote_list_t = mlist1(quote_t);
 /* Make temporary short names for certain atoms. */
 {object add1 = quote_add1;
  object and = quote_and;
  object append = quote_append;
  object cons = quote_cons;
  object delete = quote_delete;
  object difference = quote_difference;
  object equal = quote_equal;
  object fix = quote_fix;
  object flatten = quote_flatten;
  object foo = quote_foo;
  object greatest_factor = quote_greatest_factor;
  object implies = quote_implies;
  object intersect = quote_intersect;
  object length = quote_length;
  object lessp = quote_lessp;
  object member = quote_member;
  object nlistp = quote_nlistp;
  object not = quote_not;
  object numberp = quote_numberp;
  object one = quote_one;
  object or = quote_or;
  object plus = quote_plus;
  object quotient = quote_quotient;
  object remainder = quote_remainder;
  object reverse = quote_reverse;
  object six = quote_six;
  object sub1 = quote_sub1;
  object times = quote_times;
  object two = quote_two;
  object x1 = quote_x1;
  object x2 = quote_x2;
  object x3 = quote_x3;
  object x4 = quote_x4;
  object x5 = quote_x5;
  object x6 = quote_x6;
  object x7 = quote_x7;
  object zero = quote_zero;
  object zerop = quote_zerop;
  object a = quote_a;
  object b = quote_b;
  object c = quote_c;
  object d = quote_d;
  object e = quote_e;
  object f = quote_f;
  object i = quote_i;
  object j = quote_j;
  object l = quote_l;
  object p = quote_p;
  object q = quote_q;
  object u = quote_u;
  object w = quote_w;
  object x = quote_x;
  object y = quote_y;
  object z = quote_z;
  /* Define the rules, but only those that are actually referenced. */
  object rule_18 =
    add_lemma(rule(mlist3(and,p,q),
                   mlist4(quote_if,p,mlist4(quote_if,q,quote_list_t,
					    quote_list_f),
			  quote_list_f)));
  object rule_19 =
    add_lemma(rule(mlist3(or,p,q),
                   mlist4(quote_if,p,quote_list_t,
			  mlist4(quote_if,q,quote_list_t,quote_list_f))));
  object rule_21 =
    add_lemma(rule(mlist3(implies,p,q),
                   mlist4(quote_if,p,mlist4(quote_if,q,quote_list_t,
					    quote_list_f),
			  quote_list_t)));
  object rule_23 =
    add_lemma(rule(mlist4(quote_if,mlist4(quote_if,a,b,c),d,e),
                   mlist4(quote_if,a,mlist4(quote_if,b,d,e),
			  mlist4(quote_if,c,d,e))));
  object rule_25 =
    add_lemma(rule(mlist3(plus,mlist3(plus,x,y),z),
                   mlist3(plus,x,mlist3(plus,y,z))));
  object rule_26 =
    add_lemma(rule(mlist3(equal,mlist3(plus,a,b),mlist1(zero)),
                   mlist3(and,mlist2(zerop,a),mlist2(zerop,b))));
  object rule_27 =
    add_lemma(rule(mlist3(difference,x,x),mlist1(zero)));
  object rule_28 =
    add_lemma(rule(mlist3(equal,mlist3(plus,a,b),mlist3(plus,a,c)),
                   mlist3(equal,mlist2(fix,b),mlist2(fix,c))));
  object rule_29 =
    add_lemma(rule(mlist3(equal,mlist1(zero),mlist3(difference,x,y)),
                   mlist2(not,mlist3(lessp,y,x))));
  object rule_30 =
    add_lemma(rule(mlist3(equal,x,mlist3(difference,x,y)),
                   mlist3(and,mlist2(numberp,x),
			  mlist3(or,mlist3(equal,x,mlist1(zero)),
				 mlist2(zerop,y)))));
  object rule_33 =
    add_lemma(rule(mlist3(append,mlist3(append,x,y),z),
                   mlist3(append,x,mlist3(append,y,z))));
  object rule_34 =
    add_lemma(rule(mlist2(reverse,mlist3(append,a,b)),
                   mlist3(append,mlist2(reverse,b),mlist2(reverse,a))));
  object rule_35 =
    add_lemma(rule(mlist3(times,x,mlist3(plus,y,z)),
                   mlist3(plus,mlist3(times,x,y),mlist3(times,x,z))));
  object rule_36 =
    add_lemma(rule(mlist3(times,mlist3(times,x,y),z),
                   mlist3(times,x,mlist3(times,y,z))));
  object rule_37 =
    add_lemma(rule(mlist3(equal,mlist3(times,x,y),mlist1(zero)),
                   mlist3(or,mlist2(zerop,x),mlist2(zerop,y))));
  object rule_40 =
    add_lemma(rule(mlist3(member,x,mlist3(append,a,b)),
                   mlist3(or,mlist3(member,x,a),mlist3(member,x,b))));
  object rule_41 =
    add_lemma(rule(mlist3(member,x,mlist2(reverse,y)),
                   mlist3(member,x,y)));
  object rule_42 =
    add_lemma(rule(mlist2(length,mlist2(reverse,x)),
                   mlist2(length,x)));
  object rule_43 =
    add_lemma(rule(mlist3(member,a,mlist3(intersect,b,c)),
                   mlist3(and,mlist3(member,a,b),mlist3(member,a,c))));
  object rule_50 =
    add_lemma(rule(mlist3(equal,mlist3(append,a,b),mlist3(append,a,c)),
                   mlist3(equal,b,c)));
  object rule_51 =
    add_lemma(rule(mlist3(plus,mlist3(remainder,x,y),
			  mlist3(times,y,mlist3(quotient,x,y))),
                   mlist2(fix,x)));
  object rule_54 =
    add_lemma(rule(mlist3(remainder,y,one),mlist1(zero)));
  object rule_55 =
    add_lemma(rule(mlist3(lessp,mlist3(remainder,x,y),y),
                   mlist2(not,mlist2(zerop,y))));
  object rule_56 =
    add_lemma(rule(mlist3(remainder,x,x),mlist1(zero)));
  object rule_57 =
    add_lemma(rule(mlist3(lessp,mlist3(quotient,i,j),i),
                   mlist3(and,mlist2(not,mlist2(zerop,i)),
                          mlist3(or,mlist2(zerop,j),
                                 mlist2(not,mlist3(equal,j,one))))));
  object rule_58 =
    add_lemma(rule(mlist3(lessp,mlist3(remainder,x,y),x),
                   mlist4(and,mlist2(not,mlist2(zerop,y)),
                          mlist2(not,mlist2(zerop,x)),
                          mlist2(not,mlist3(lessp,x,y)))));
  object rule_63 =
    add_lemma(rule(mlist3(difference,mlist3(plus,x,y),x),
                   mlist2(fix,y)));
  object rule_64 =
    add_lemma(rule(mlist3(difference,mlist3(plus,y,x),x),
                   mlist2(fix,y)));
  object rule_65 =
    add_lemma(rule(mlist3(difference,mlist3(plus,x,y),mlist3(plus,x,z)),
                   mlist3(difference,y,z)));
  object rule_66 =
    add_lemma(rule(mlist3(times,x,mlist3(difference,c,w)),
                   mlist3(difference,mlist3(times,c,x),mlist3(times,w,x))));
  object rule_67 =
    add_lemma(rule(mlist3(remainder,mlist3(times,x,z),z),
                   mlist1(zero)));
  object rule_68 =
    add_lemma(rule(mlist3(difference,mlist3(plus,b,mlist3(plus,a,c)),a),
                   mlist3(plus,b,c)));
  object rule_69 =
    add_lemma(rule(mlist3(difference,mlist2(add1,mlist3(plus,y,z)),z),
                   mlist2(add1,y)));
  object rule_70 =
    add_lemma(rule(mlist3(lessp,mlist3(plus,x,y),mlist3(plus,x,z)),
                   mlist3(lessp,y,z)));
  object rule_71 =
    add_lemma(rule(mlist3(lessp,mlist3(times,x,z),mlist3(times,y,z)),
                   mlist3(and,mlist2(not,mlist2(zerop,z)),
			  mlist3(lessp,x,y))));
  object rule_72 =
    add_lemma(rule(mlist3(lessp,y,mlist3(plus,x,y)),
                   mlist2(not,mlist2(zerop,x))));
  object rule_75 =
    add_lemma(rule(mlist3(equal,mlist2(flatten,x),
			  mlist3(cons,y,mlist1(nil))),
                   mlist3(and,mlist2(nlistp,x),mlist3(equal,x,y))));
  object rule_78 =
    add_lemma(rule(mlist3(equal,mlist3(greatest_factor,x,y),mlist1(zero)),
                   mlist3(and,mlist3(or,mlist2(zerop,y),
				     mlist3(equal,y,one)),
			  mlist3(equal,x,mlist1(zero)))));
  object rule_79 =
    add_lemma(rule(mlist3(equal,mlist3(greatest_factor,x,y),one),
                   mlist3(equal,x,one)));
  object rule_83 =
    add_lemma(rule(mlist3(equal,z,mlist3(times,w,z)),
                   mlist3(and,mlist2(numberp,z),
                          mlist3(or,mlist3(equal,z,mlist1(zero)),
				 mlist3(equal,w,one)))));
  object rule_85 =
    add_lemma(rule(mlist3(equal,x,mlist3(times,x,y)),
                   mlist3(or,mlist3(equal,x,mlist1(zero)),
                          mlist3(and,mlist2(numberp,x),
				 mlist3(equal,y,one)))));
  object rule_86 =
    add_lemma(rule(mlist3(remainder,mlist3(times,y,x),y),
                   mlist1(zero)));
  object rule_87_1 = mlist2(not,mlist3(equal,a,mlist1(zero)));
  object rule_87_5 = mlist3(equal,mlist2(sub1,a),mlist1(zero));
  object rule_87_6 = mlist3(equal,mlist2(sub1,b),mlist1(zero));
  object rule_87 =
    add_lemma(rule(mlist3(equal,mlist3(times,a,b),one),
                   mlist7(and,
                          rule_87_1,
                          mlist2(not,mlist3(equal,b,mlist1(zero))),
                          mlist2(numberp,a),
                          mlist2(numberp,b),
                          rule_87_5,
                          rule_87_6)));
  object rule_88 =
    add_lemma(rule(mlist3(lessp,mlist2(length,mlist3(delete,x,l)),
			  mlist2(length,l)),
                   mlist3(member,x,l)));
  object rule_91_37 = mlist3(cons,x3,
                             mlist3(cons,x4,
                                    mlist3(cons,x5,
                                           mlist3(cons,x6,x7))));
  object rule_91 =
    add_lemma(rule(mlist2(length,
                          mlist3(cons,x1,
                                 mlist3(cons,x2,rule_91_37))),
                   mlist3(plus,six,mlist2(length,x7))));
  object rule_92 =
    add_lemma(rule(mlist3(difference,mlist2(add1,mlist2(add1,x)),two),
                   mlist2(fix,x)));
  object rule_95 =
    add_lemma(rule(mlist3(plus,x,mlist2(add1,y)),
                   mlist4(quote_if,mlist2(numberp,y),
			  mlist2(add1,mlist3(plus,x,y)),
			  mlist2(add1,x))));
  object rule_96 =
    add_lemma(rule(mlist3(equal,mlist3(difference,x,y),
			  mlist3(difference,z,y)),
                   mlist4(quote_if,mlist3(lessp,x,y),
			  mlist2(not,mlist3(lessp,y,z)),
			  mlist4(quote_if,mlist3(lessp,z,y),
				 mlist2(not,mlist3(lessp,y,x)),
				 mlist3(equal,mlist2(fix,x),
					mlist2(fix,z))))));
  object rule_98 =
    add_lemma(rule(mlist3(times,x,mlist2(add1,y)),
                   mlist4(quote_if,mlist2(numberp,y),
			  mlist3(plus,x,mlist3(times,x,y)),
			  mlist2(fix,x))));
  object rule_101 =
    add_lemma(rule(mlist3(equal,mlist3(lessp,x,y),z),
                   mlist4(quote_if,mlist3(lessp,x,y),
			  mlist3(equal,quote_list_t,z),
			  mlist3(equal,quote_list_f,z))));
  /* Create closure for the test function. */
  mclosure0(run_test,&test);
  gc_cont = &run_test;
  /* Initialize constant expressions for the test runs. */
  test_exp1 = mlist5(mlist3(x,f,mlist3(plus,mlist3(plus,a,b),
				       mlist3(plus,c,mlist1(zero)))),
                     mlist3(y,f,mlist3(times,mlist3(times,a,b),
				       mlist3(plus,c,d))),
                     mlist3(z,f,mlist2(reverse,mlist3(append,
						      mlist3(append,a,b),
						      mlist1(nil)))),
                     mlist4(u,equal,mlist3(plus,a,b),mlist3(difference,x,y)),
                     mlist4(w,lessp,mlist3(remainder,a,b),
			    mlist3(member,a,mlist2(length,b))));
  test_exp2 = mlist3(implies,
		     mlist3(and,mlist3(implies,x,y),
			    mlist3(and,mlist3(implies,y,z),
				   mlist3(and,mlist3(implies,z,u),
					  mlist3(implies,u,w)))),
		     mlist3(implies,x,w));
  /* Allocate heap area for second generation. */
  /* Use calloc instead of malloc to assure pages are in main memory. */
  printf("main: Allocating and initializing heap...\n");
  bottom = calloc(1,heap_size);
  allocp = (char *) ((((long) bottom)+7) & -8);
  alloc_end = allocp + heap_size - 8;
  printf("main: heap_size=%ld  allocp=%ld  alloc_end=%ld\n",
         (long) heap_size,allocp,alloc_end);
  printf("main: Try a larger heap_size if program bombs.\n");
  printf("Starting...\n");
  start = clock(); /* Start the timing clock. */
  /* These two statements form the most obscure loop in the history of C! */
  setjmp(jmp_main); funcall1((closure) gc_cont,gc_ans);
  /*                                                                      */
  printf("main: your setjmp and/or longjmp are broken.\n"); exit(0);}}

static long long_arg(argc,argv,name,dval)
     int argc; char **argv; char *name; long dval;
/* Thanks to George Carrette. */
{int j;
 for(j=1;(j+1)<argc;j += 2)
   if (strcmp(name,argv[j]) == 0)
     return(atol(argv[j+1]));
 return(dval);}

main(int argc,char **argv)
{
#ifdef THINK_C
 argc = ccommand(&argv);
#endif
 {long stack_size = long_arg(argc,argv,"-s",STACK_SIZE);
  long heap_size = long_arg(argc,argv,"-h",HEAP_SIZE);
  main_main(stack_size,heap_size,(char *) &stack_size);
  return 0;}}
/*--------------------- Cut Here -----------------------------------*/
