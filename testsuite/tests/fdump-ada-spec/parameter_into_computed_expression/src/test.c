#include <stdio.h>
#include <stdlib.h>

void changeArray (int * arr, int length) {
  int i;

  printf ("LENGTH = %d\n", length);

  for (i = 0; i < length; ++i) {
    printf ("VALUE C = %d\n", arr [i]);
    arr [i] = arr [i] + 1;
  }
}
