#include <stdio.h>
#include <stdlib.h>
#include "test.h"

void XGet1 (void * X) {
  struct myStruct * val = (struct myStruct *) X;
  
  val->F = 199;
}
