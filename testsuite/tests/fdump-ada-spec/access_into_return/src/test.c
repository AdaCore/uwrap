#include <stdio.h>
#include <stdlib.h>
#include "test.h"

void XGet1 (struct myStruct * X) {
  X->F = 199;
}

void XGet2 (struct myStruct A, struct myStruct * X) {
  printf ("%d %d\n", A.F, X->F);
}

void XGet3 (struct myStruct X) {
    printf ("%d\n", X.F);  
}

void XSet (struct myStruct * X) {
      printf ("%d\n", X->F); 
}
