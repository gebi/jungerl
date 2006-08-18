/*
 * Type declarations 
 */

typedef unsigned long u_long_t;

typedef signed int* ptr_t;

typedef void** handle_t;

typedef enum { a=10, b=11, c=12, d=13, e=14,f=15 } * hexp_t;

typedef struct { float x; float y; } vec_t;

typedef struct { unsigned x:3; unsigned y:3; unsigned :2; } xy_t;

/* constants  */

float gFloat = 3.14;

const int gInt = 314;

unsigned long long gLL = 0x1234567812345678L;

unsigned long long int* gLLptr = &gLongLong;

char jtab[256] = {1,2,3,4,5,6};

struct { int x; int y; } xy_tab[3] =
{ {1,2}, {1,3}, {1,4} };



