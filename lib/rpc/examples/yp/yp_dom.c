/* Copyright (c) 2000, 2001 Sendmail, Inc.  All rights reserved. */
#include <sys/systeminfo.h>

main()
{
   char buf[1024];

   sysinfo(SI_SRPC_DOMAIN, buf, 1024);

   printf("%s\n", buf);
   exit(0);
}
