/*
** Copyright (c) 2003, Scott Lystig Fritchie.  All rights reserved.
** See the file "../LICENSE" for license details.
*/

#include <stdio.h>
#include <string.h>
#include <grp.h>
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

int
make_groups_list(struct descriptor *desc, callstate_t *c,
		 ErlDrvTermData *msg, int *members, int *msgcount)
{
    int		i;
    char	**g;
    int		len;
    char	*gcopy;

    g = c->o.ret_grptr->gr_mem;
    for (i = 0; g != NULL && *g != NULL; g++, i++) {
	len = strlen(*g);
	if ((gcopy = edtk_driver_alloc_wrapper(len + 1)) == NULL) {
	    return 0;
	}
	strcpy(gcopy, *g);
	*msgcount = LOAD_BINARY(msg, *msgcount,
				edtk_alloced_ptr2ErlDrvBinary(gcopy), 0, len);
    }
    *msgcount = LOAD_NIL(msg, *msgcount);
    *msgcount = LOAD_LIST(msg, *msgcount, i + 1); /* include the NIL! */

    *members = 1;
    return 1;
}

