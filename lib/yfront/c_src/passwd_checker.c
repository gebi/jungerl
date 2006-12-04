/*
  Created: 1 Dec 2006 by tobbe@tornkvist.org
  Desc.  : Minimal interface to cracklib to be used from
           Erlang via os:cmd/1. 

  On Gentoo download dictionaries and run like:
  (see e.g: http://www.cotse.com/tools/wordlists.htm)

     cp swedish /usr/share/dict/.
     create-cracklib-dict /usr/share/dict/*

  Also, see:

   http://gdub.wordpress.com/2006/08/26/using-cracklib-to-require-stronger-passwords/


  Compile: gcc -o passwd_checker passwd_checker.c -lcrack

  Run: ./passwd_checker tobbe /usr/lib/cracklib_dict

  Returns: "ok" | "<error string>"

 */

#include <stdio.h>
#include <crack.h>


/* const char *FascistCheck(const char *pw, const char *dictpath); */


main(int argc, char *argv[]) 
{
  const char *res;

  res = FascistCheck(argv[1], argv[2]);

  if (res == NULL) 
    printf("ok\n");
  else
    printf("%s\n", res);

}


