int lib1 = 0;
extern int lib2;

void lib1_init()
{
  lib1 = 1;
}

int lib1_get_sum()
{
  return lib1 + lib2;
}
