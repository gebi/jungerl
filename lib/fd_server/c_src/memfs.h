
#ifndef MEMFS_H


#ifdef WIN32
#include <malloc.h>
#else

#ifdef STDC_HEADERS
# ifdef STDLIB_MALLOC
#  include <stdlib.h>
# else
#  ifdef HAVE_MALLOC_H
#   include <malloc.h>
#  else
#   error "Cannot find malloc"
#  endif
# endif
#endif

#define MEMFS_H 1
#endif

#endif /* not win32 */
