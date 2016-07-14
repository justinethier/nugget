#include <stdio.h>
#include <stdlib.h>

void lib2_init();
void lib1_init();
int lib1_get_sum();
int main(int argc, char **argv)
{
  lib2_init();
  lib1_init();
  printf("%d\n", lib1_get_sum());
  return 0;
}
