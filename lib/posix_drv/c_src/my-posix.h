/*
** Copyright (c) 2003, Scott Lystig Fritchie.  All rights reserved.
** See the file "../LICENSE" for license details.
*/

#ifndef	NGROUPS_MAX
#define	NGROUPS_MAX	64	/* Big, but we'll be safe */
#endif	/* !NGROUPS_MAX */

int my_getgroups(gid_t *);
int make_getgroups_list(struct descriptor *desc, callstate_t *c, ErlDrvTermData *msg, int *members, int *msgcount);
