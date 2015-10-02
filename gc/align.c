#include <stdio.h>
#define gc_align(n, bits) (((n)+(1<<(bits))-1)&(((unsigned int)-1)-((1<<(bits))-1)))
// 64-bit is 3, 32-bit is 2
#define gc_word_align(n) gc_align((n), 2)
#define gc_heap_align(n) gc_align(n, 5)
int main(){
  printf("%d\n", gc_word_align(1));
  printf("%d\n", gc_word_align(2));
  printf("%d\n", gc_word_align(3));

  printf("%d\n", gc_align(1, 4));
  printf("%d\n", gc_align(1, 5));
  return 0;
}
