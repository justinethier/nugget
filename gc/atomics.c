/*
 For GCC 4.6, need to use __sync functions, eg:

#define ATOMIC_INC(ptr) __sync_fetch_and_add((ptr),1)
#define ATOMIC_DEC(ptr) __sync_fetch_and_sub((ptr),1)
#define ATOMIC_GET(ptr) __sync_fetch_and_add((ptr),0)

and __sync_val_compare_and_swap() for storing

can use __atomic built-ins starting with gcc 4.7

TBD what would be used in other compilers such as LLVM
*/
