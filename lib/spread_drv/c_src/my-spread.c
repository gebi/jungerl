
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#include <sp.h>

#include <erl_driver.h>
#include <spread_drv.h>
#include <erl_driver_tk.h>

#include <my-spread.h>

#ifdef  DRIVER_USING_PTHREADS
#include <pthread.h>
static pthread_once_t _once = PTHREAD_ONCE_INIT;
#endif  /* DRIVER_USING_PTHREADS */

static int _initialized = 0;
static ErlDrvTermData am_active;
static ErlDrvTermData am_error;
static ErlDrvTermData am_ok;

static void do_initializing_stuff(void);
static void init_my_atoms(void);
static int my_reply_xtra_xret_receive(descriptor_t *desc, callstate_t *c);

void
my_ready_input(ErlDrvData drv_data, ErlDrvEvent event)
{
    descriptor_t	*desc = (descriptor_t *) drv_data;
    callstate_t		c;
    int			ret;
    char		*groups = NULL, *mess = NULL;
    int			num_groups;
    int			mess_len;
    
    /*
    ** We don't know if the fd is ready because there's data to read or
    ** because the socket was closed on us.  QQQ Is there a better way
    ** to do this?
    */
    if (read(desc->mbox, &ret/*ignored*/, 0) != 0) {
	fprintf(stderr, "\r\nXXX %s: read test failed, errno = %d\r\n", __FUNCTION__, errno);
	driver_select(desc->port, (ErlDrvEvent) desc->mbox, DO_READ, 0);
	desc->active_p = 0;
	SP_disconnect(desc->mbox);
	desc->mbox_init_p = 0;
    }
    while ((ret = SP_poll(desc->mbox)) > 0) {
	/* fprintf(stderr, "\r\nXXX %s: got %d bytes waiting\r\n", __FUNCTION__, ret); */
	mess_len = ret;		/* Hopefully we're not too wasteful */
#ifdef	QQQ_COMMENT
	/*
	** 80 bytes seems to be the overhead for a single message.  If we're
	** wrong, we'll re-allocate and try again.
	*/
	if (mess_len > 80) {
	    mess_len -= 80;
	}
#endif	/* QQQ_COMMENT */
	num_groups = MAX_MULTIGROUPS;	/* guess, if wrong we'll retry */
	c.i.mbox = desc->mbox;
	c.i.sender = &desc->sender[0];
    retry:
	if (groups == NULL &&
	    (groups = sys_alloc(num_groups * MAX_GROUP_NAME)) == NULL) {
	    return;
	}
	if (mess == NULL &&
	    (mess = edtk_driver_alloc_wrapper(mess_len)) == NULL) {
	    free(groups);
	    return;
	}
	c.i.max_groups = num_groups;
	c.i.groups = groups;
	c.i.max_mess_len = mess_len;
	c.i.mess = mess;
	ret = SP_receive(c.i.mbox, &c.o.service_type, c.i.sender,
			 c.i.max_groups, &c.o.num_groups,
			 (char (*)[MAX_GROUP_NAME]) c.i.groups,
			 &c.o.mess_type, &c.o.endian_mismatch,
			 c.i.max_mess_len, c.i.mess);
	if (ret >= 0) {
	    /* fprintf(stderr, "\r\nQQQ %s: got %d bytes\r\n", __FUNCTION__, ret); */
	    c.o.__expect = 1;
	    c.o.ret_int = ret;
	    my_reply_xtra_xret_receive(desc, &c);
	    sys_free(groups);
	    /* mess has already been freed by my_reply_xtra_xret_receive() */
	    groups = mess = NULL;
	} else if (ret == GROUPS_TOO_SHORT) {
	    fprintf(stderr, "\r\nQQQ %s: GROUPS_TOO_SHORT\r\n", __FUNCTION__);
	    sys_free(groups);
	    groups = NULL;
	    num_groups = -1 * c.o.num_groups;
	    goto retry;
	} else if (ret == BUFFER_TOO_SHORT) {
	    /* XXX fprintf(stderr, "\r\nQQQ %s: BUFFER_TOO_SHORT: mess_len = %d, need = %d\r\n", __FUNCTION__, mess_len, c.o.endian_mismatch); */
	    edtk_driver_free_wrapper(mess);
	    mess = NULL;
	    mess_len = -1 * c.o.endian_mismatch;
	    goto retry;
	} else {
	    fprintf(stderr, "\r\nQQQ %s: Unknown problem: ret = %d\r\n", __FUNCTION__, ret);
	    sys_free(groups);
	    edtk_driver_free_wrapper(mess);
	    return;
	}
    }
    if (ret < 0) {
	fprintf(stderr, "\r\nXXX %s: ret ERROR = %d\r\n", __FUNCTION__, ret);
    }
}

int
my_set_active(ErlDrvData drv_data, int value)
{
    descriptor_t	*desc = (descriptor_t *) drv_data;

    value = !(!value);		/* Normalize */
    if (value == desc->active_p) {
	return value;
    }
    driver_select(desc->port, (ErlDrvEvent) desc->mbox, DO_READ, value);
    desc->active_p = value;
    return value;
}

int
my_multigroup_multicast(mailbox mbox, service service_type, char *groups,
			int16 mess_type, int mess_len, char *mess)
{
    char	grps[MAX_MULTIGROUPS][MAX_GROUP_NAME];
    int		i, len;

    i = 0;
    while (*groups != '\0') {
	strcpy(grps[i], groups);
	len = strlen(groups);
	if (i == MAX_MULTIGROUPS) {
	    break;
	}
	groups += len + 1;
	i++;
    }
    return SP_multigroup_multicast(mbox, service_type, i,
				   (const char (*)[MAX_GROUP_NAME]) grps,
				   mess_type, mess_len, mess);
}

int
make_xret_group_list(struct descriptor *desc, callstate_t *c, ErlDrvTermData *msg,
		     int *members, int *msgcount)
{
    int         mycount = 0;
    int		i;
    char	*g, *bufp;
    int		g_len;
    ErlDrvBinary *tmpbin = NULL;

    do_initializing_stuff();

    g = c->i.groups;
    for (i = 0; i < c->o.num_groups; i++, g += MAX_GROUP_NAME) {
	g_len = strlen(g);
	if ((bufp = edtk_driver_alloc_wrapper(g_len)) == NULL) {
	    /*
	    ** QQQ We're just screwed here, given the stuff we've already
	    ** pushed onto the stack.  I think our options are to:
	    ** 1. abort, or 2. return prematurely.  If we do #2, then
	    ** driver_output_term() will puke, then the Erlang receiver
	    ** will hang forever waiting for a reply ... but at least the
	    ** rest of the VM will have a chance to try to continue.
	    */
	    return 0;
	}
	memcpy(bufp, g, g_len);
	tmpbin = edtk_alloced_ptr2ErlDrvBinary(bufp);
	*msgcount = LOAD_BINARY(msg, *msgcount,	tmpbin, 0, g_len);
	desc->tofree[desc->num_tofree++] = tmpbin;
	mycount++;
    }

    *msgcount = LOAD_NIL(msg, *msgcount);
    mycount++;

    *members = mycount;
    return 1;
}

void
do_initializing_stuff(void)
{
    if (! _initialized) {
#ifdef  DRIVER_USING_PTHREADS
        pthread_once(&_once, init_my_atoms);
#else   /* DRIVER_USING_PTHREADS */
        init_my_atoms();
#endif  /* DRIVER_USING_PTHREADS */

    }
}

static void
init_my_atoms(void)
{
    am_active = driver_mk_atom("active");
    am_error = driver_mk_atom("error");
    am_ok = driver_mk_atom("ok");
    _initialized = 1;
}

/*
** XXX This is an _almost_ cut-and-paste of reply_xtra_xret_receive()
**     from spread_drv.c.
*/

static int
my_reply_xtra_xret_receive(descriptor_t *desc, callstate_t *c)
{
    ErlDrvTermData      msg[64];
    int                 msgcount = 0;
    int                 res;
    int                 members = 0;
    char                *tmp = NULL;
    int                 i;
    ErlDrvBinary        *tmpbin = NULL;

    do_initializing_stuff();

    tmp = tmp; tmpbin = tmpbin;
    desc->num_tofree = 0;
    msgcount = LOAD_PORT(msg, msgcount, driver_mk_port(desc->port));
    members = 2;        /* Rather, members will be 2 very shortly */
    if (c->o.__expect) {
        msgcount = LOAD_ATOM(msg, msgcount, am_active);	/* XXX MODIFIED! */
        {
            int members = 0;
             msgcount = LOAD_INT(msg, msgcount, c->o.service_type);
             members++;
             if ((tmp = edtk_driver_alloc_wrapper(strlen(c->i.sender))) == NULL) {
		 return -1;	/* XXX MODIFIED! */
             }
             memcpy(tmp,  c->i.sender, strlen(c->i.sender));
             tmpbin = edtk_alloced_ptr2ErlDrvBinary(tmp);
             msgcount = LOAD_BINARY(msg, msgcount, tmpbin, 0, strlen(c->i.sender));
             /* driver_output_term() incrs refc, and we're done, so decr refc LATER */

             desc->tofree[desc->num_tofree++] = tmpbin;
             members++;
             {
                 int members = 0;
                 
                 make_xret_group_list(desc, c, msg, &members, &msgcount);
                 msgcount = LOAD_LIST(msg, msgcount, members);
             }
             members++;
             msgcount = LOAD_INT(msg, msgcount, c->o.mess_type);
             members++;
             msgcount = LOAD_INT(msg, msgcount, c->o.endian_mismatch);
             members++;
             tmpbin = edtk_alloced_ptr2ErlDrvBinary(c->i.mess);
             msgcount = LOAD_BINARY(msg, msgcount, tmpbin, 0, c->o.ret_int);
             /* driver_output_term() incrs refc, and we're done, so decr refc LATER */
             desc->tofree[desc->num_tofree++] = tmpbin;
             members++;
            msgcount = LOAD_TUPLE(msg, msgcount, members);
        }
    } else {
        msgcount = LOAD_ATOM(msg, msgcount, am_error);
        {
            int members = 0;
             msgcount = LOAD_INT(msg, msgcount, c->o.ret_int);
             members++;
             msgcount = LOAD_INT(msg, msgcount, c->o.num_groups);
             members++;
             msgcount = LOAD_INT(msg, msgcount, c->o.endian_mismatch);
             members++;
            msgcount = LOAD_TUPLE(msg, msgcount, members);
        }
    }
    msgcount = LOAD_TUPLE(msg, msgcount, 3);
    edtk_debug("%s: i = %d", __FUNCTION__, msgcount);
    res = driver_output_term(desc->port, msg, msgcount);
    edtk_debug("%s: res = %d", __FUNCTION__, res);
    if (res < 0) {
        fprintf(stderr, "\r\n\r\n%s: driver_output_term() failed!  This should never happen!\r\n\r\n", __FUNCTION__);
    }

    for (i = 0; i < desc->num_tofree; i++) {
        driver_free_binary(desc->tofree[i]);
    }

    return res;
}

