/* example for embedding */


#include <stdio.h>
#include <assert.h>
#include "bones.h"


extern BONES_X my_bones(BONES_X);
extern BONES_X my_other_bones(BONES_X);


int main(int argc, char *argv[])
{
  BONES_X x = my_bones(NULL);
  assert(BONES_fix2int(x) == 123);
  x = my_other_bones(NULL);
  assert(BONES_fix2int(x) == 123);
  x = my_bones(BONES_int2fix(42));
  assert(x == BONES_int2fix(43));
  x = my_other_bones(BONES_int2fix(42));
  assert(x == BONES_int2fix(43));
}
