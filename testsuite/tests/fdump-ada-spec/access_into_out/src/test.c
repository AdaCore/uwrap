#include <stdio.h>
#include "test.h"

void XGet1 (struct myStruct * X, struct myStruct * Y, struct myStruct * Z) {
  X->F = 101;  
  Y->F = 102;
  Z->F = 103;
}

void XGet2 (struct myStruct A, struct myStruct * X) {
  printf ("Xget2: %d\n", A.F);
  X->F = 104;
}

void XGet3 (struct myStruct X) {
    printf ("Xget3: %d\n", X.F);
}

void XSet (struct myStruct * X) {
    X->F = 105;
  }
