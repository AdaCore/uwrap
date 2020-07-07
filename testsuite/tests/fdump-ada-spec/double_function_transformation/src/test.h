#define OK      0x00
#define e1Error 0x01
#define e2Error 0x02
#define e3Error 0x03

struct myStruct {
    int F;
} ;

int someFunction (int r, struct myStruct * X);

int someOtherFunction (struct myStruct * X, int r);

