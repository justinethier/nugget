/* 
"---------------- input program:"
 */
/* 
(begin ((lambda (x) x) (+ 41 1)))
 */
/* 
"---------------- after desugar:"
 */
/* 
((lambda (x) x) (+ 41 1))
 */
/* 
"---------------- after CPS:"
 */
/* 
((lambda (r$2)
   ((lambda (x) ((lambda (r$1) (%halt r$1)) x)) r$2))
 (+ 41 1))
 */
/* 
"---------------- after wrap-mutables:"
 */
/* 
((lambda (r$2)
   ((lambda (x) ((lambda (r$1) (%halt r$1)) x)) r$2))
 (+ 41 1))
 */
/* 
"---------------- after closure-convert:"
 */
/* 
((closure
   (lambda (env$3 r$2)
     ((closure
        (lambda (env$4 x)
          ((closure
             (lambda (env$5 r$1) (%halt r$1))
             (env-make 0))
           x))
        (env-make 1))
      r$2))
   (env-make 2))
 (+ 41 1))
 */
#include <stdlib.h>
#include <stdio.h>
#include "c-matt-might/scheme.h"


Value __sum ;
Value __difference ;
Value __product ;
Value __halt ;
Value __display ;
Value __numEqual ;

struct __env_2 {
} ;

struct __env_2* __alloc_env2(){
  struct __env_2* t = malloc(sizeof(struct __env_2));
  return t;
}


struct __env_1 {
} ;

struct __env_1* __alloc_env1(){
  struct __env_1* t = malloc(sizeof(struct __env_1));
  return t;
}


struct __env_0 {
} ;

struct __env_0* __alloc_env0(){
  struct __env_0* t = malloc(sizeof(struct __env_0));
  return t;
}


Value __prim_sum(Value e, Value a, Value b) {
  return MakeInt(a.z.value + b.z.value) ;
}
Value __prim_product(Value e, Value a, Value b) {
  return MakeInt(a.z.value * b.z.value) ;
}
Value __prim_difference(Value e, Value a, Value b) {
  return MakeInt(a.z.value - b.z.value) ;
}
Value __prim_halt(Value e, Value v) {
  exit(0);
}
Value __prim_display(Value e, Value v) {
  printf("%i\n",v.z.value) ;
  return v ;
}
Value __prim_numEqual(Value e, Value a, Value b) {
  return MakeBoolean(a.z.value == b.z.value) ;
}
Value __lambda_2() ;
Value __lambda_1() ;
Value __lambda_0() ;

Value __lambda_2(Value env_733, Value r_732) {
  Value tmp_737 ; 
  return (tmp_737 = MakeClosure(__lambda_1,MakeEnv(__alloc_env1())),tmp_737.clo.lam(MakeEnv(tmp_737.clo.env),r_732)) ;
}

Value __lambda_1(Value env_734, Value x) {
  Value tmp_738 ; 
  return (tmp_738 = MakeClosure(__lambda_0,MakeEnv(__alloc_env0())),tmp_738.clo.lam(MakeEnv(tmp_738.clo.env),x)) ;
}

Value __lambda_0(Value env_735, Value r_731) {
  Value tmp_739 ; 
  return (tmp_739 = __halt,tmp_739.clo.lam(MakeEnv(tmp_739.clo.env),r_731)) ;
}

int main (int argc, char* argv[]) {
  Value tmp_736 ; 
  Value tmp_7310 ; 
  __sum         = MakePrimitive(__prim_sum) ;
  __product     = MakePrimitive(__prim_product) ;
  __difference  = MakePrimitive(__prim_difference) ;
  __halt        = MakePrimitive(__prim_halt) ;
  __display     = MakePrimitive(__prim_display) ;
  __numEqual    = MakePrimitive(__prim_numEqual) ;
  (tmp_736 = MakeClosure(__lambda_2,MakeEnv(__alloc_env2())),tmp_736.clo.lam(MakeEnv(tmp_736.clo.env),(tmp_7310 = __sum,tmp_7310.clo.lam(MakeEnv(tmp_7310.clo.env),MakeInt(41), MakeInt(1))))) ;
  return 0;
 }

