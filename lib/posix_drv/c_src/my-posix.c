/*
** Copyright (c) 2003, Scott Lystig Fritchie.  All rights reserved.
** See the file "../LICENSE" for license details.
*/

#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
/* BSD-specific? (for NGROUPS_MAX) #include <sys/syslimits.h> */
#include <sys/stat.h>

#ifndef NGROUPS_MAX
#define NGROUPS_MAX     64      /* Big, but we'll be safe */
#endif  /* !NGROUPS_MAX */

#include <erl_driver.h>
#include <erl_driver_tk.h>
#include <posix_drv.h>
#include <my-posix.h>

int
my_getgroups(gid_t *gidset)
{
    return getgroups(NGROUPS_MAX, gidset);
}

int
make_getgroups_list(struct descriptor *desc, callstate_t *c,
		    ErlDrvTermData *msg, int *members, int *msgcount)
{
    int	i;

    for (i = 0; i < c->o.ret_int; i++) {
	*msgcount = LOAD_INT(msg, *msgcount, c->o.gidset[i]);
    }
    *msgcount = LOAD_NIL(msg, *msgcount);
    *msgcount = LOAD_LIST(msg, *msgcount, c->o.ret_int + 1); /* include the NIL! */

    *members = 1;
    return 1;
}
