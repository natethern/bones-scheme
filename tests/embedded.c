/* example for embedding */


#include <stdio.h>
#include <assert.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <dlfcn.h>
#endif

#include "bones.h"


extern BONES_X my_bones(BONES_X);


BONES_X (*my_other_bones)(BONES_X);


int main(int argc, char *argv[])
{
#ifdef _WIN32
  HMODULE h = LoadLibrary("tmp/embedded2.dll");
  my_other_bones = ((BONES_X (*)(BONES_X))GetProcAddress(h, "my_other_bones");
#else
  void *h = dlopen("tmp/embedded2.so", RTLD_NOW);
  my_other_bones = dlsym(h, "my_other_bones");
#endif
  BONES_X x = my_bones(NULL);
  assert(BONES_fix2int(x) == 123);
  x = my_other_bones(NULL);
  assert(BONES_fix2int(x) == 123);
  x = my_bones(BONES_int2fix(42));
  assert(x == BONES_int2fix(43));
  x = my_other_bones(BONES_int2fix(42));
  assert(x == BONES_int2fix(43));
  return 0;
}
