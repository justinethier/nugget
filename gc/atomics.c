/*
 For GCC 4.6, need to use __sync functions, eg:

#define ATOMIC_INC(ptr) __sync_fetch_and_add((ptr),1)
#define ATOMIC_DEC(ptr) __sync_fetch_and_sub((ptr),1)
#define ATOMIC_GET(ptr) __sync_fetch_and_add((ptr),0)

and __sync_val_compare_and_swap() for storing

can use __atomic built-ins starting with gcc 4.7

TBD what would be used in other compilers such as LLVM
*/
#include <stdio.h>

// TODO: how to differentiate between gcc 4.6 and 4.7+??
#define ATOMIC_INC(ptr) __sync_fetch_and_add((ptr),1)
#define ATOMIC_DEC(ptr) __sync_fetch_and_sub((ptr),1)
#define ATOMIC_GET(ptr) __sync_fetch_and_add((ptr),0)
#define ATOMIC_SET_IF_EQ(ptr, oldv, newv) __sync_val_compare_and_swap(ptr, oldv, newv)

int main(int argc, char **argv)
{
  int i = 0;
  printf("i = %d\n", i);
  ATOMIC_INC(&i);
  printf("i = %d\n", i);
  ATOMIC_DEC(&i);
  printf("i = %d\n", ATOMIC_GET(&i));
  ATOMIC_SET_IF_EQ(&i, 0, 10);
  printf("i = %d\n", i);
  return 0;
}
