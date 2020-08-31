#include<stdio.h>

void s1 (char * p1, char * p2) {
    printf(p1);
    printf(p2);
}

char * g1 = "ABC";
char * g2 = "DEF";

char * s2 (void) {
    return g1;
}

char * s3 (char * p1, char * p2) {
    printf(p1);
    printf(p2);

    return g2;
}

void s4 (char * p1, char * leaveMeAlone) {
    printf(p1);
    printf(leaveMeAlone);
    printf("OVER AND OUT\n");
}

