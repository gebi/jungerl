#include <stdio.h>
#include <slang.h>

int main ()
{
    int abort_char = 7;  /* For MSDOS, use 34 as scan code */
    unsigned int ch;
    
    if (-1 == SLang_init_tty (abort_char, 0, 1))
	{
	    fprintf (stderr, "Unable to initialize the terminal.\n");
	    exit (-1);
	}
    SLang_set_abort_signal (NULL);

    fflush (stdout);
	    
    ch = SLang_getkey ();

    printf("<< %c >> %d ",ch, SLang_Error);

    SLang_reset_tty ();
    return 0;
}
 



