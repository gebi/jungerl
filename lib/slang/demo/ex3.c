#include <stdio.h>
#include <slang.h>


stuff()
{
    SLsmg_normal_video ();
    SLsmg_gotorc (5, 0);
    SLsmg_write_string ("Hello ");
    SLsmg_erase_eol ();
    SLsmg_refresh ();
}


int main ()
{
    SLtt_get_terminfo ();
    SLkp_init ();
    SLang_init_tty (-1, 0, 0);
    SLsmg_init_smg ();
    

    stuff();

    SLkp_getkey ();
    
    /* do stuff .... */
    
    SLsmg_reset_smg ();
    SLang_reset_tty ();
    return 0;
}








