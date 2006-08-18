
#define X 1
#define Y 0x123
#define F(x,y)  ((x)+(y)*#x)
#define G(a)  a+1

#define C1 /
#define C2 *
#define BEGIN(x,y) x##y
#define END(x,y)   x##y

  #define K k_was_defined
#ifndef foo bar
FOO
#endif

#define K defined_again

BEGIN(/,*) hello END(*,/)

K

G(1)

#ifdef FOO
    F(1,2)-F(2,3)
#else
    F(2,2)+F(2,3)
#endif

#if X > 1 && Y < 2
#  define Z 2
#elif X == 0 || Y == 0
#  define Z 3
#  line 20 "gnurf.h"
#endif

 
