/* File    : test.c
** Author  :  <tony@bit.hemma.se>
** Description : small intl test
** Created :  9 Sep 2003 by  <tony@bit.hemma.se>
*/
#include <locale.h>
#include <libintl.h>
#define _(X) gettext((X))
#define N_(X) (X)   /* gettext_noop */

main(argc, argv)
{
    char* Locale = "sv_SE";
    char* LocaleDir = "./locale";

    setlocale(LC_MESSAGES, Locale);
    bindtextdomain("test", LocaleDir);
    textdomain("test");
    bind_textdomain_codeset("test", "ISO-8859-1");
    t1();
    t2();
    t3();
}

t1() 
{
    printf(_("Hello world\n"));
    printf(_("Bend over\n"));
    printf(N_("This is translated later\n"));
}


t2() 
{
    printf(gettext("You got mail\n"));
    printf(ngettext("You got one mail\n", "You got mail\n", 15));
    printf(dgettext("test", "You got virus\n"));
    printf(dngettext("test", "You got one virus\n", 
		     "You got plenty of viruses\n",
		     20));
    printf(dcgettext("test", "You got a %d coin\n",LC_MESSAGES),
	   40);
    printf(dcngettext("test", "You got a %d coin\n", 
		      "You got tons of %d\n", 30, LC_MESSAGES),
	   1000);
}

t3()
{
    printf(gettext("You got mail\n"));
    printf(dcgettext("test", "You got a %d coin\n",LC_MESSAGES), 40);
}

