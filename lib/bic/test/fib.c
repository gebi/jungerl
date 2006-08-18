
typedef unsigned long atom_t;
typedef unsigned long term_t;
typedef unsigned long pid_t;
typedef unsigned long size_t;

extern pid_t spawn(atom_t module, atom_t function, ...);
extern term_t send(pid_t process, term_t message);

typedef struct { int x; int y; } vector_t;

typedef int (*pfun_t)(int, float);
typedef char name[12];

typedef vector_t matrix_t[];

typedef enum { on=1, off=2, none=3 } mode_t;


void qsort(void *base, size_t nmemb, size_t size,
                  int(*compar)(const void *, const void *));

int main1(a,b)
    int a; float b;
{
    return (int) (a+b);
}

char* main2(int a,float b)
{
    return "a+b";
}

float* main3(int a, float b)
    int a; float b;
{
    return &b+a;
}


int fib(int n)
{
    int f0 = 1, f1 = 1;
    int i;

    for (i = 1; i <= n; i++) {
	int tmp = f1;

	f1 = f1 + f0;
	f0 = tmp;
    }
    return f1;
}



    
