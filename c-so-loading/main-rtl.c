#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

void lib2_init();
void lib1_init();
int lib1_get_sum();
int main(int argc, char **argv)
{
  void *handle1, *handle2;
  void (*lib1_init)(void);
  int (*lib1_get_sum)(void);
  void (*lib2_init)(void);

  handle1 = dlopen("./lib1.so", RTLD_LAZY);
  if (!handle1) {
    fputs (dlerror(), stderr);
    exit(1);
  }
  handle2 = dlopen("./lib2.so", RTLD_LAZY);
  if (!handle2) {
    fputs (dlerror(), stderr);
    exit(1);
  }

  lib1_init = dlsym(handle1, "lib1_init");
  if (!handle1) {
    fputs (dlerror(), stderr);
    exit(1);
  }
  lib1_get_sum = dlsym(handle1, "lib1_get_sum");
  if (!handle1) {
    fputs (dlerror(), stderr);
    exit(1);
  }
  lib2_init = dlsym(handle2, "lib2_init");
  if (!handle2) {
    fputs (dlerror(), stderr);
    exit(1);
  }
  lib2_init();
  lib1_init();
  printf("%d\n", lib1_get_sum());
  return 0;
}
