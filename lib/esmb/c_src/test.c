#include "global.h"
#include "md4.h"

#define MD 4

#define MD_CTX MD4_CTX
#define MDInit MD4Init
#define MDUpdate MD4Update
#define MDFinal MD4Final


static void MDPrint (digest)
    unsigned char digest[16];
{
  unsigned int i;

  for (i = 0; i < 16; i++)
    printf ("%02x", digest[i]);
}

static void MDString (char *string)
{
    MD4_CTX context;
    unsigned char digest[16];
    unsigned int len = strlen (string);
    
    MD4Init (&context);
    MD4Update (&context, string, len);
    MD4Final (digest, &context);
    
    printf ("MD%d (\"%s\") = ", MD, string);
    MDPrint (digest);
    printf ("\n");
}


int main(int argc, char *argv[])
{

    MDString("qwe123");

}
