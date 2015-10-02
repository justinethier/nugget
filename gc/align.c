#include <stdio.h>
#define sexp_align(n, bits) (((n)+(1<<(bits))-1)&(((unsigned int)-1)-((1<<(bits))-1)))
#define sexp_word_align(n) sexp_align((n), 2)
int main(){
  printf("%d\n", sexp_word_align(1));
  printf("%d\n", sexp_word_align(2));
  printf("%d\n", sexp_word_align(3));
  return 0;
}
