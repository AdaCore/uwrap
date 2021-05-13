#include <stdio.h>
#include "test.h"

void Read (int ** A, int size) {
  for (int i = 0; i < size; ++i) {
    printf ("A[%d]=%d\n", i, *(A[i]));
  }
}
