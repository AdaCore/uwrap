#include "test.h"

int someFunction (int r, struct myStruct * X) {
  X->F = 99;

  return r;
}

int someOtherFunction (struct myStruct * X, int r) {
  X->F = 888;

  return r;
}
