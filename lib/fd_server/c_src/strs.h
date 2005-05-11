
#ifndef WIN32
#ifndef STRS_H

#include <config.h>

#ifdef STDC_HEADERS
# include <string.h>
#else
# ifdef HAVE_STRINGS_H
#  include <strings.h>
# else 
#  error "Could not find string header file"
# endif
#endif

#define STRS_H 1
#endif

#else  /* win32 */

#include <string.h>

#endif


