/*
** exp1_drv.c: A first experiment in using R7B asynchronous/Pthreads-based
** drivers.
**
** The BEAM interpreter must be compiled to be able to utilize pthreads.
** The R7B-1 configure script doesn't work correctly, so, after running
** "./configure" at the top of the source tree, edit
** erts/emulator/{platform}/Makefile and add the following to CFLAGS:
**
**	-DUSE_THREADS -D_REENTRANT -DPOSIX_THREADS -D_THREAD_SAFE
**
** ... and add the following to LIBS:
**
**	-lpthread
**
** Use "-lc_r" for FreeBSD.
*/

/*
** I dimly recall the reasons for choosing gethostbyname() as a suitable
** experiment for writing a thread-aware async driver.  My goals were:
**
** 1. Try to figure out how thread-aware async drivers worked.
**    Choosing something "easy" and "simple" to start with was good.
**
** 2. Choose something that I could cause _real_ long timeouts,
**    such as putting in a UDP filter on port 53, while having the
**    VM doing something else, just to prove that that something
**    else was still busy executing, unfazed by this driver's wait 
**    for a DNS reply.
**
** NOTE: If there are multiple A records associated with the
**       hostname, we only return the first one.  That's pretty
**       silly, but it's easily remedied by expending time and
**       keystrokes....
*/

#define	EXP1_REQ_GETHOSTBYNAME		1

#define	EXP1_RESP_OK			0
#define	EXP1_RESP_ERROR			1
#define	EXP1_RESP_BYTES			2

#include	<stdio.h>
#include	<netdb.h>
#include	<errno.h>
#include	<assert.h>
#include	<string.h>
#include	<ctype.h>
#include	<unistd.h>
#include	<pthread.h>

#include "driver.h"

#define	PRIVATESTUFF		/* XXX set to "static" for real use */
#define	DEBUGIO		stderr
#define	KEY		NULL

#define put_int32(i, s) {((char*)(s))[0] = (char)((i) >> 24) & 0xff; \
                        ((char*)(s))[1] = (char)((i) >> 16) & 0xff; \
                        ((char*)(s))[2] = (char)((i) >> 8)  & 0xff; \
                        ((char*)(s))[3] = (char)((i)        & 0xff);}
#define get_int32(s) ((((unsigned char*) (s))[0] << 24) | \
                      (((unsigned char*) (s))[1] << 16) | \
                      (((unsigned char*) (s))[2] << 8)  | \
                      (((unsigned char*) (s))[3]))

typedef struct {
    int fd;
    unsigned int port;
    unsigned flags;             /* Original flags. */
} descriptor;

#define		RESBUFSIZE	128
struct t_data {			/* transaction (?) data */
    int		cmd;
    long	id;
    int		status;
    char	*name;
    char	resbuf[RESBUFSIZE];
    int		resbuflen;
};

/* Function prototypes */

struct driver_entry *driver_init(void *);
PRIVATESTUFF long exp1_start(int, char *);
PRIVATESTUFF int exp1_init(void);
PRIVATESTUFF int exp1_stop(descriptor *);
PRIVATESTUFF int exp1_read(descriptor *, char *, int);
PRIVATESTUFF int exp1_async_ready(descriptor *, void *);
PRIVATESTUFF void invoke_gethostbyname(void *);
PRIVATESTUFF void free_t_data(void *);
PRIVATESTUFF int reply_bytes(descriptor *, long, char *, int);
PRIVATESTUFF int reply_error(descriptor *, long, int);

/* XXX where do I find prototypes for these? */
void *sys_alloc(size_t);
void *sys_realloc(void *, size_t);
void sys_free(void *);
#if	!defined(linux)
struct hostent *gethostbyname_r(const char *, struct hostent *, char *,
				int, int *);
#endif	/* !linux */

struct driver_entry exp1_driver_entry = {
    exp1_init,
    exp1_start,
    exp1_stop,
    exp1_read,
    null_func,
    null_func,
    "exp1_drv",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    exp1_async_ready
};

struct driver_entry *
driver_init(void *handle)
{
    exp1_driver_entry.handle = handle;
    return &exp1_driver_entry;
}

PRIVATESTUFF int
exp1_init(void)
{
    return 0;
}

PRIVATESTUFF long
exp1_start(int port, char *args)
{
    descriptor	*d;

    fprintf(DEBUGIO, "exp1_start: starting, args = 0x%lx, %s\r\n", (unsigned long) args, args);

    if ((d = (descriptor *) sys_alloc(sizeof(descriptor))) == NULL)
	return -1;
    d->fd = -1;
    d->port = port;
    d->flags = 0;
    return (long) d;
}

PRIVATESTUFF int
exp1_stop(descriptor *d)
{
    sys_free(d);
    return 0;
}

PRIVATESTUFF void
free_t_data(void *p)
{
    struct t_data	*td = (struct t_data *) p;

    if (td->name != NULL)
	free(td->name);		/* XXX Um? */
    sys_free(td);
}

PRIVATESTUFF int
exp1_read(descriptor *d, char *buf, int buflen)
{
    char		cmd;
    struct t_data	*td = sys_alloc(sizeof(struct t_data));

    assert(td != NULL);
    assert(buflen >= 4);
    cmd = *buf++;
    td->cmd = cmd;
    td->id = get_int32(buf);
    buf += 4;
    td->name = NULL;

    fprintf(DEBUGIO, "exp1_read: cmd = %d, id = %ld\r\n", cmd, td->id);
    switch (cmd) {
    case EXP1_REQ_GETHOSTBYNAME:
	/* buf = null-terminated hostname */
	/* XXX use something other than strdup()? */
	td->name = strdup(buf);
	driver_async(d->port, KEY, invoke_gethostbyname, (void *) td,
		     free_t_data);
	break;
    }
    /*
    ** XXX I guess we'll ignore everything else, let the caller hang.
    */
    fprintf(DEBUGIO, "exp1_read: done\r\n");
    return 0;
}

PRIVATESTUFF int
exp1_async_ready(descriptor *d, void *p)
{
    struct t_data	*td = (struct t_data *) p;

    fprintf(DEBUGIO, "exp1_async_ready: cmd = %d, id = %ld\r\n", td->cmd, td->id);
    switch (td->cmd) {
    case EXP1_REQ_GETHOSTBYNAME:
	if (td->status != 0)
	    reply_error(d, td->id, td->status);
	else
	    reply_bytes(d, td->id, td->resbuf, td->resbuflen);
	break;
    default:
	abort();
    }
    fprintf(DEBUGIO, "exp1_async_ready: done\r\n");
    return 0;
}

PRIVATESTUFF void
invoke_gethostbyname(void *p)
{
    struct t_data	*td = (struct t_data *) p;
    struct hostent	hp, *h;
    char		buf[1000];
    int			h_errno;
    u_long		sip;	/* IP address in network byte order */

    fprintf(DEBUGIO, "invoke_gethostbyname: name = %s\r\n", td->name);
    fprintf(DEBUGIO, "invoke_gethostbyname: 0x%lx sleeping for 2 seconds\r\n", (unsigned long) pthread_self());
    sleep(2);
    fprintf(DEBUGIO, "invoke_gethostbyname: 0x%lx done sleeping\r\n", (unsigned long) pthread_self());
#if	defined(linux)
    gethostbyname_r(td->name, &hp, buf, sizeof(buf), &h, &h_errno);
#else	/* linux */
    h = gethostbyname_r(td->name, &hp, buf, sizeof(buf), &h_errno);
#endif	/* linux */
    if (h == NULL) {
	td->status = h_errno;
	fprintf(DEBUGIO, "invoke_gethostbyname: error = %d\r\n", td->status);
    } else {
	td->status = 0;
	fprintf(DEBUGIO, "invoke_gethostbyname: success = %d\r\n", td->status);
	sip = *((u_long *) hp.h_addr_list[0]);
	memcpy(td->resbuf, &sip, sizeof(sip));
	td->resbuflen = sizeof(sip);
    }
    fprintf(DEBUGIO, "invoke_gethostbyname: done\r\n");
}

/*
** NOTE: Do not use for large replies!
*/

PRIVATESTUFF int
reply_bytes(descriptor *d, long id, char *b, int blen)
{
#define		TMPSIZE	(RESBUFSIZE+5)
    char	tmp[TMPSIZE];

    assert(blen < TMPSIZE);

    /*
     * Contents of buffer sent back:
     *
     * +------------------------------+
     * | FILE_RESP_ERROR | ID | Bytes |
     * |      1 byte     | 4  | {var} |
     * +------------------------------+
     */

    fprintf(DEBUGIO, "reply_bytes: gonna send %d total bytes\r\n", blen+1);
    *tmp = EXP1_RESP_BYTES;
    put_int32(id, tmp + 1);
    memcpy(tmp + 5, b, blen);
    driver_output2(d->port, tmp, blen + 5, NULL, 0);
    fprintf(DEBUGIO, "reply_bytes: done\r\n");
    return 0;
}

PRIVATESTUFF int
reply_error(descriptor *d, long id, int err)
{
    char response[TMPSIZE];         /* Response buffer. */
    char* s;
    char* t;
    
    /*
     * Contents of buffer sent back:
     *
     * +----------------------------------------------+
     * | FILE_RESP_ERROR | ID | Posix error id string |
     * |        1 byte   | 4  |       {var}           |
     * +----------------------------------------------+
     */

    response[0] = EXP1_RESP_ERROR;
    put_int32(id, response + 1);
    for (s = erl_errno_id(err), t = response+5; *s; s++, t++)
        *t = tolower(*s);
    driver_output2(d->port, response, t-response, NULL, 0);
    return 0;
}
