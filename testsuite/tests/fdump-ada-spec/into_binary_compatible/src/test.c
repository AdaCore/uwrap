#include <stdio.h>
#include <stdlib.h>

void * addr (void * x) {
  int v;

  if (x == 0) {
    printf ("INPUT NULL\n");
  } else {
    printf ("GOT %d\n", (*((int *) x)));
  }

  int * res = malloc (sizeof (int));
  *res = 99;

  return res;
}
