/*
** An experimental UNIX domain socket driver.
**
** NOTE: Borrows heavily, at times, from the inet_drv driver.
*/

/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 * Portions Copyright (c) 2001 Scott Lystig Fritchie, <fritchie@mr.net>
 *
 *     $Id$
 */

#include	<stdio.h>
#include	<unistd.h>
#include	<assert.h>
#include	<sys/types.h>
#include	<sys/uio.h>
#include	<sys/socket.h>
#include	<sys/un.h>
#include	<sys/stat.h>
#include	<sys/stat.h>
#include	<fcntl.h>
#include	<string.h>
#include	<ctype.h>
#include	<errno.h>
#include	<stdarg.h>

#include	"driver.h"

#if     defined(__sun)
#define PF_LOCAL        AF_UNIX
#endif  /* __sun */

/* Begin driver<->emulator communication codes (xref with unixdom.hrl) */

#define	UNIXDOM_REQ_ENDIAN	1
#define	UNIXDOM_REQ_KNUTHHASH	2
#define	UNIXDOM_REQ_OPEN	3
#define	UNIXDOM_REQ_CONNECT	4
#define	UNIXDOM_REQ_CLOSE	5
#define	UNIXDOM_REQ_WRITE	6
#define	UNIXDOM_REQ_RECV	7
#define UNIXDOM_REQ_SETOPTS	8
#define	UNIXDOM_REQ_BIND	9
#define	UNIXDOM_REQ_ACCEPT	10
#define	UNIXDOM_REQ_GETIX	11
#define	UNIXDOM_REQ_GETOPTS	12

#define	UNIXDOM_REP_OK		1
#define	UNIXDOM_REP_ERROR	2
#define	UNIXDOM_REP_WOULDBLOCK	3

#define	UNIXDOM_REPBODY_LSB	254
#define	UNIXDOM_REPBODY_MSB	255

#define	UNIXDOM_OPT_ENDOFLIST	0
#define	UNIXDOM_OPT_IGNORE	1
#define	UNIXDOM_OPT_ACTIVE	2
#define	UNIXDOM_OPT_BACKLOG	3

#define	UNIXDOM_OPT_ACTIVE_FALSE	0
#define	UNIXDOM_OPT_ACTIVE_TRUE		1
#define	UNIXDOM_OPT_ACTIVE_ONCE		2

/* End driver<->emulator communication codes */

/* Begin stuff from erts/emulator/beam/sys.h */

/* This is really a mess... We used to use fcntl O_NDELAY, but that seems
   to only work on SunOS 4 - in particular, on SysV-based systems
   (including Solaris 2), it does set non-blocking mode, but causes
   read() to return 0!!  fcntl O_NONBLOCK is specified by POSIX, and
   seems to work on most systems, with the notable exception of AIX,
   where the old ioctl FIONBIO is the *only* one that will set a *socket*
   in non-blocking mode - and ioctl FIONBIO on AIX *doesn't* work for
   pipes or ttys (O_NONBLOCK does)!!! For now, we'll use FIONBIO for AIX. */
#ifdef NB_FIONBIO               /* Old BSD */
#include <sys/ioctl.h>
static const int zero_value = 0, one_value = 1;
#define SET_BLOCKING(fd)        ioctl((fd), FIONBIO, &zero_value)
#define SET_NONBLOCKING(fd)     ioctl((fd), FIONBIO, &one_value)
#define ERRNO_BLOCK EWOULDBLOCK
#else /* !NB_FIONBIO */
#include <fcntl.h>
#ifdef NB_O_NDELAY              /* Nothing needs this? */
#   define NB_FLAG O_NDELAY
#   ifndef ERRNO_BLOCK          /* allow override (e.g. EAGAIN) via Makefile */
#      define ERRNO_BLOCK EWOULDBLOCK
#   endif
#else  /* !NB_O_NDELAY */       /* The True Way - POSIX!:-) */
#   define NB_FLAG O_NONBLOCK
#   define ERRNO_BLOCK EAGAIN
#endif /* !NB_O_NDELAY */
#define SET_BLOCKING(fd)        fcntl((fd), F_SETFL, \
                                      fcntl((fd), F_GETFL, 0) & ~NB_FLAG)
#define SET_NONBLOCKING(fd)     fcntl((fd), F_SETFL, \
                                      fcntl((fd), F_GETFL, 0) | NB_FLAG)
#endif /* !NB_FIONBIO */

/* End stuff from erts/emulator/beam/sys.h */

#define	PRIVATESTUFF	/* XXX static */
#ifndef MAX_DESCRIPTOR
int sys_max_files(void);
#define MAX_DESCRIPTOR sys_max_files()
#endif

#define TYPE_STREAM    1
#define TYPE_DGRAM     2

#define MODE_LIST      0
#define MODE_BINARY    1

#define INVALID_SOCKET -1
#define INVALID_EVENT  -1
#define SOCKET_ERROR   -1

#define M_PASSIVE	0
#define M_ACTIVE	1
#define M_ONCE		2  /* active once then passive */
/*
** The default active mode should be _passive_.  If it's active, then
** you can run into weird race conditions while the ownership of the
** port is being transferred, e.g. to the new descriptor process on
** an accept.  Do not change M_ACTIVE_DEFAULT without causing extra work!
*/
#define	M_ACTIVE_DEFAULT M_PASSIVE	/* XXX should be active by default? */

#define F_OPEN         0x0001
#define F_BOUND        0x0002
#define F_ACTIVE       0x0004
#define F_LISTEN       0x0008
#define F_CON          0x0010
#define F_ACC          0x0020
#define F_LST          0x0040
#define F_BUSY         0x0080 

#define STATE_CLOSED    0
#define STATE_OPEN      (F_OPEN)
#define STATE_BOUND     (STATE_OPEN | F_BOUND)
#define STATE_CONNECTED (STATE_BOUND | F_ACTIVE)
#define STATE_CONNECTING (STATE_BOUND | F_CON)
#define STATE_ACCEPTING  (F_ACC)
/*
** STATE_LISTEN = ready to call accept()
** STATE_LISTENING = a call to accept() blocked, so we've registered the
** descriptor with the driver manager to tell us when an accept is
** possible.
*/
#define STATE_LISTEN     (STATE_BOUND | F_LISTEN)
#define STATE_LISTENING  (STATE_LISTEN | F_LST)
#define IS_OPEN(d) \
 (((d)->state & F_OPEN) == F_OPEN)
#define IS_BOUND(d) \
 (((d)->state & F_BOUND) == F_BOUND)
#define IS_CONNECTED(d) \
  (((d)->state & STATE_CONNECTED) == STATE_CONNECTED)
#define IS_CONNECTING(d) \
  (((d)->state & F_CON) == F_CON)
#define IS_BUSY(d) \
  (((d)->state & F_BUSY) == F_BUSY)
#define IS_LISTENING(d) \
  ((d)->state == STATE_LISTENING)

/*
** General comments on the high/low watermarks and the port "busy" status.
**
** The port I/O driver code, in cahoots with the descriptor select/poll
** manager, has a feature to avoid sending or buffering arbitrary amounts
** of data to a slow port.  If a write fails due to EWOULDBLOCK, this
** driver will queue the data for sending by unixdom_ready_output() when
** the socket becomes writable again.  If the amount of data queued
** exceeds HIGH_WATERMARK, then the port is marked as "busy":
**
**	* the descriptor state is marked with F_BUSY
**	* the descriptor "busy_on_send" flag is set
**	* the port is tagged as busy by set_busy_port().
**
** If another attempt to write to the port is made, that process will
** be blocked until the port is unbusy'ed via set_busy_port().  That
** is done when unixdom_ready_output() has drained the port's write
** queue so that its size is below LOW_WATERMARK.  When the port is
** unbusy'ed, the Erlang process trying to do "Port ! {command, Data}"
** will be rescheduled
**
** NOTE: I don't fully understand if the same holds true if you use
**       ctl_write(), but since ctl_write() is deprecated, you get
**       what you deserve if you use it.
*/

#define DEF_BUFSIZ     (1024*4)    /* Default buffer size */
#define MIN_BUFSIZ     1           /* Internal min buffer size */
#define MAX_BUFSIZ     (1024*64)   /* Internal max buffer max */

/*
** Note: HIGH_WATERMARK MUST be less than 2*MAX_BUFSIZ
*/
#define HIGH_WATERMARK (1024*8) /* 8k pending high => busy  */
/*
** Note: LOW_WATERMARK MUST be less than MAX_BUFSIZ and
** less than HIGH_WATERMARK
*/
#define LOW_WATERMARK  (1024*4) /* 4k pending => allow more */

#define INFINITY  0xffffffff  /* infinity value */

#define MAX_ASYNC 1           /* max number of async queue ops */

#define PTYPE_RAW	0
#define PTYPE_1		1
#define PTYPE_2		2
#define PTYPE_4		3
#define PTYPE_RM	5

#define EXBADPORT "exbadport"
#define EXBADSEQ  "exbadseq"

#define ALLOC(X)	sys_alloc(X)
#define REALLOC(X,Y)	sys_realloc((X),(Y))
#define FREE(X)		sys_free(X)

typedef int SOCKET;
typedef int HANDLE;
#define FD_READ    	DO_READ
#define FD_WRITE   	DO_WRITE
#define FD_CLOSE   	0
#define FD_CONNECT 	DO_WRITE
#define FD_ACCEPT  	DO_READ

#define sock_open(af, type, proto)  socket((af), (type), (proto))
#define sock_close(s)               close((s))
#define sock_accept(s, addr, len)   accept((s), (addr), (len))
#define sock_connect(s, addr, len)  connect((s), (addr), (len))
#define sock_listen(s, b)           listen((s), (b))
#define sock_bind(s, addr, len)     bind((s), (addr), (len))
#define sock_getopt(s,t,n,v,l)      getsockopt((s),(t),(n),(v),(l))
#define sock_setopt(s,t,n,v,l)      setsockopt((s),(t),(n),(v),(l))
#define sock_peer(s, addr, len)     getpeername((s), (addr), (len))
#define sock_send(s,buf,len,flag)   send((s),(buf),(len),(flag))
#define sock_sendv(s, vec, size, np, flag) \
                (*(np) = writev((s), (struct iovec*)(vec), (size)))
#define sock_recv(s,buf,len,flag)   recv((s),(buf),(len),(flag))
#define sock_recvfrom(s,buf,blen,flag,addr,alen) \
                recvfrom((s),(buf),(blen),(flag),(addr),(alen))
#define sock_sendto(s,buf,blen,flag,addr,alen) \
                sendto((s),(buf),(blen),(flag),(addr),(alen))
#define sock_errno()                errno
#define sock_create_event(d)        (d)->s   /* return file descriptor */
#define sock_close_event(e)                  /* do nothing */
#define sock_select(d, flags, onoff) do { \
	int res = -666; \
        (d)->event_mask = (onoff) ? \
                 ((d)->event_mask | (flags)) : \
                 ((d)->event_mask & ~(flags)); \
        res = driver_select((d)->port, (d)->event, (flags), (onoff)); \
	Debug("QQQ driver_select(0x%lx, %d, %d, %d) = %d at line %d\r\n", (d)->port, (d)->event, (flags), (onoff), res, __LINE__); \
   } while(0)

/* Borrowed from beam/sys.h */
#define get_int32(s) ((((unsigned char*) (s))[0] << 24) | \
                      (((unsigned char*) (s))[1] << 16) | \
                      (((unsigned char*) (s))[2] << 8)  | \
                      (((unsigned char*) (s))[3]))
#define put_int32(i, s) {((char*)(s))[0] = (char)((i) >> 24) & 0xff; \
                        ((char*)(s))[1] = (char)((i) >> 16) & 0xff; \
                        ((char*)(s))[2] = (char)((i) >> 8)  & 0xff; \
                        ((char*)(s))[3] = (char)((i)        & 0xff);}

#define MAX_VSIZE 16            /* Max number of entries allowed in an I/O
                                 * vector sock_sendv().
                                 */

PRIVATESTUFF struct driver_entry unixdom_drv_entry;
PRIVATESTUFF int erlang_port = -1;

typedef struct {
    SOCKET s;                   /* the socket or INVALID_SOCKET if not open */
    HANDLE event;               /* Event handle (same as s in unix) */
    long  event_mask;           /* current FD events */
    long  port;                 /* the port identifier */
    DriverTermData dport;       /* the port identifier as DriverTermData */
    int   state;                /* status */
    int   prebound;             /* only set when opened with inet_fdopen XXX impl later? */
    int   mode;                 /* BINARY | LIST
                                   (affect how to interpret hsz) */
    int   exitf;                /* exit port on close or not */

    int   active;               /* 0 = passive, 1 = active, 2 = active once */
    int   stype;                /* socket type SOCK_STREAM/SOCK_DGRAM */
    int   htype;                /* header type (tcp only?) */
    int   ix;                   /* descriptor index */
    int	  backlog;		/* listen backlog */

    int   bufsz;                /* minimum buffer constraint */
    DriverBinary *i_buf;	/* DriverBinary memory buffer */
    int   i_bufsz;		/* Minimum size of i_buf binary */
    char  *i_bufp;		/* Pointer to start of i_buf's buffer */
    int   low;			/* low water mark */
    int   high;			/* high water mark */
    int   send_timeout;		/* timeout to use in send */
    int	  busy_on_send;		/* Flag: state is F_BUSY & timer set */
    int   i_ix;                 /* accept descriptor index / read indicator */

    /* statistics */
    unsigned long recv_oct[2];  /* number of received octets >= 64 bits */
    unsigned long recv_cnt;     /* number of packets received */
    unsigned long recv_max;     /* maximum packet size received */
    double recv_avg;            /* average packet size received */
    double recv_dvi;            /* avarage deviation from avg_size */
    unsigned long send_oct[2];  /* number of octets sent >= 64 bits */
    unsigned long send_cnt;     /* number of packets sent */
    unsigned long send_max;     /* maximum packet send */
    double send_avg;            /* average packet size sent */
} descriptor;

PRIVATESTUFF descriptor** desc_table;
PRIVATESTUFF int desc_ix;
PRIVATESTUFF int desc_size;         /* number of descriptors */

PRIVATESTUFF DriverTermData am_ok;
PRIVATESTUFF DriverTermData am_unixdom;
PRIVATESTUFF DriverTermData am_error;
PRIVATESTUFF DriverTermData am_unixdom_reply;
PRIVATESTUFF DriverTermData am_timeout;
PRIVATESTUFF DriverTermData am_closed;
PRIVATESTUFF DriverTermData am_wouldblock;

PRIVATESTUFF int _verbose_driver = 0;	/* Set to zero to quiet things */

/* function prototypes */

struct driver_entry *driver_init(void *handle);
PRIVATESTUFF int unixdom_init(void);
PRIVATESTUFF long unixdom_start(int, char *);
PRIVATESTUFF int unixdom_stop(long);
PRIVATESTUFF int unixdom_output(long, char *, int);
PRIVATESTUFF int unixdom_ready_input(descriptor *, HANDLE);
PRIVATESTUFF int unixdom_ready_output(descriptor *, HANDLE);
PRIVATESTUFF int unixdom_finish(void *);
PRIVATESTUFF int unixdom_ctl(descriptor *, int, char *, int, char **, int);
PRIVATESTUFF int unixdom_timeout(descriptor *, HANDLE); /* XXXYYYZZZ correct? */
PRIVATESTUFF int unixdom_outputv(descriptor *, ErlIOVec *);
PRIVATESTUFF int do_outputv(descriptor *, ErlIOVec *);
PRIVATESTUFF int unixdom_recv(descriptor *, int);
PRIVATESTUFF int unixdom_recv_closed(descriptor *);
PRIVATESTUFF int unixdom_recv_error(descriptor *, int);

PRIVATESTUFF int ctl_endian(char **, int);
PRIVATESTUFF int ctl_knuthhash(char *, int, char **, int);
PRIVATESTUFF int ctl_open(descriptor *, char **, int);
PRIVATESTUFF int ctl_connect(descriptor *, char *, int, char **, int);
PRIVATESTUFF int ctl_close(descriptor *, char **, int);
PRIVATESTUFF int ctl_write(descriptor *, char *, int, char **, int);
PRIVATESTUFF int ctl_recv(descriptor *, unsigned long, unsigned long, char **, int);
PRIVATESTUFF int ctl_setopts(descriptor *, char *, int, char **, int);
PRIVATESTUFF int ctl_bind(descriptor *, char *, int, char **, int);
PRIVATESTUFF int ctl_accept(descriptor *, char *, int, char **, int);
PRIVATESTUFF int ctl_getopts(descriptor *, char *, int, char **, int);

PRIVATESTUFF int ctl_reply(int, char *, int, char **, int);
PRIVATESTUFF int ctl_error(int, char **, int);
PRIVATESTUFF int ctl_xerror(char*, char **, int);

PRIVATESTUFF int reply_ok(descriptor *);
PRIVATESTUFF int reply_error(descriptor *, int);
PRIVATESTUFF int reply_closed(descriptor *);
PRIVATESTUFF int reply_error_am(descriptor *, DriverTermData);
PRIVATESTUFF int reply_data(descriptor *, int);
PRIVATESTUFF void desc_close(descriptor *);
PRIVATESTUFF void input_count(descriptor *, int);
PRIVATESTUFF void output_count(descriptor *, int);
PRIVATESTUFF void clear_input(descriptor *);
PRIVATESTUFF void clear_output(descriptor *);
PRIVATESTUFF int unixdom_close(descriptor *);
PRIVATESTUFF DriverTermData error_atom(int);
PRIVATESTUFF void clear_listener(descriptor *);
PRIVATESTUFF int Debug(char *, ...);

/* XXX where do I find prototypes for these? */
void *sys_alloc(size_t);
void *sys_realloc(void *, size_t);
void sys_free(void *);

#ifdef	NEED_EXTRA_FUNCS
void * sys_memzero(void *, size_t);
#endif	/* NEED_EXTRA_FUNCS */

#define LOAD_ATOM(vec, i, atom) \
  (((vec)[(i)] = ERL_DRV_ATOM), \
  ((vec)[(i)+1] = (atom)), \
  (i+2))

#define LOAD_INT(vec, i, val) \
  (((vec)[(i)] = ERL_DRV_INT), \
  ((vec)[(i)+1] = (DriverTermData)(val)), \
  (i+2))

#define LOAD_PORT(vec, i, port) \
  (((vec)[(i)] = ERL_DRV_PORT), \
  ((vec)[(i)+1] = (port)), \
  (i+2))

#define LOAD_PID(vec, i, pid) \
  (((vec)[(i)] = ERL_DRV_PID), \
  ((vec)[(i)+1] = (pid)), \
  (i+2))

#define LOAD_BINARY(vec, i, bin, offs, len) \
  (((vec)[(i)] = ERL_DRV_BINARY), \
  ((vec)[(i)+1] = (DriverTermData)(bin)), \
  ((vec)[(i)+2] = (len)), \
  ((vec)[(i)+3] = (offs)), \
  (i+4))

#define LOAD_STRING(vec, i, str, len) \
  (((vec)[(i)] = ERL_DRV_STRING), \
  ((vec)[(i)+1] = (DriverTermData)(str)), \
  ((vec)[(i)+2] = (len)), \
  (i+3))

#define LOAD_STRING_CONS(vec, i, str, len) \
  (((vec)[(i)] = ERL_DRV_STRING_CONS), \
  ((vec)[(i)+1] = (DriverTermData)(str)), \
  ((vec)[(i)+2] = (len)), \
  (i+3))

#define LOAD_TUPLE(vec, i, size) \
  (((vec)[(i)] = ERL_DRV_TUPLE), \
  ((vec)[(i)+1] = (size)), \
  (i+2))

#define LOAD_LIST(vec, i, size) \
  (((vec)[(i)] = ERL_DRV_LIST), \
  ((vec)[(i)+1] = (size)), \
  (i+2))

/* begin code */

PRIVATESTUFF int
unixdom_init(void)
{
    Debug("unixdom_init: running\r\n");
    return 0;			/* Well, this is what tcp_inet_init() does */
}

/*
** Initialize & return a (static) driver entry struct.
*/

struct driver_entry *
driver_init(void *handle)
{
    int	bytes;

    Debug("unixdom driver_init: running\r\n");
    /* Create our atoms */
    am_ok = driver_mk_atom("ok");
    am_unixdom = driver_mk_atom("unixdom");
    am_error = driver_mk_atom("error");
    am_unixdom_reply = driver_mk_atom("unixdom_reply");
    am_timeout = driver_mk_atom("timeout");
    am_closed = driver_mk_atom("closed");
    am_wouldblock = driver_mk_atom("wouldblock");

    /* Allocate the descriptor table */
    desc_size = MAX_DESCRIPTOR;
    bytes = sizeof(descriptor *) * desc_size;
    if ((desc_table = (descriptor **) ALLOC(bytes)) == NULL)
	goto error;
    sys_memzero((void *) desc_table, (size_t) bytes);
    desc_ix = 0;

    /* Last, create a driver_entry structure */
    unixdom_drv_entry.init = unixdom_init;
    unixdom_drv_entry.start = unixdom_start;
    unixdom_drv_entry.stop = unixdom_stop;
    unixdom_drv_entry.output = unixdom_output;
    unixdom_drv_entry.ready_input = unixdom_ready_input;
    unixdom_drv_entry.ready_output = unixdom_ready_output;
    unixdom_drv_entry.driver_name = "unixdom_drv";
#ifdef	___AVOID_DOUBLE_FREE
    unixdom_drv_entry.finish = unixdom_finish;
#endif	/* ___AVOID_DOUBLE_FREE */
    unixdom_drv_entry.handle = handle;
    unixdom_drv_entry.control = unixdom_ctl;
    unixdom_drv_entry.timeout = unixdom_timeout;
    unixdom_drv_entry.outputv = unixdom_outputv;
    unixdom_drv_entry.ready_async = null_func;

    Debug("unixdom driver_init: done\r\n");
    return &unixdom_drv_entry;
 error:
    return NULL;
}

PRIVATESTUFF long
unixdom_start(int port, char *args)
{
    descriptor*	d;
    int		ix = desc_ix;
    int		save_ix = ix;

    Debug("unixdom_start: starting, args = 0x%lx, %s\r\n", (unsigned long) args, args);
    do {
        if (desc_table[ix] == NULL)
            break;
        ix = (ix + 1) % desc_size;
    } while(ix != save_ix);

    if (desc_table[ix] != NULL) {
        Debug("unixdom_start: ran out of unixdom descriptors, max = %d\r\r\n", desc_size);
        errno = EMFILE;
        return (long) NULL;
    }
    desc_ix = (ix + 1) % desc_size;
    Debug("unixdom_start: found ix = %d\r\n", ix);

    if ((d = (descriptor *) ALLOC(sizeof(descriptor))) == NULL)
        return (long) NULL;

    d->s = INVALID_SOCKET;
    d->event = INVALID_EVENT;
    d->event_mask = 0;
    d->port = port;
    d->dport = driver_mk_port(port);
    d->state = STATE_CLOSED;
    d->prebound = 0;
    d->mode = MODE_BINARY;
    d->exitf = 1;
    d->active = M_ACTIVE_DEFAULT;
    d->stype = -1;
    d->htype = PTYPE_RAW;
    d->ix = ix;
    d->backlog = -1;		/* Will be set later if we wish */

    d->bufsz = DEF_BUFSIZ;
    d->i_buf = NULL;
    d->i_bufp = NULL;
    d->low = LOW_WATERMARK;
    d->high = HIGH_WATERMARK;
    d->send_timeout = 5000;	/* XXXYYYZZZ INFINITY!  Use 5000 for debugging */
    d->busy_on_send = 0;
    d->i_ix = -1;

    d->recv_oct[0] = d->recv_oct[1] = 0;
    d->recv_cnt = 0;
    d->recv_max = 0;    
    d->recv_avg = 0.0;
    d->recv_dvi = 0.0;
    d->send_oct[0] = d->send_oct[1] = 0;
    d->send_cnt = 0;
    d->send_max = 0;
    d->send_avg = 0.0;

    desc_table[ix] = d;
    return (long) d;
}

PRIVATESTUFF int
unixdom_stop(long desc)
{
    descriptor	*d = (descriptor *) desc, *a_desc;

    /* XXX unfinished ... should free I/O buffers, etc. */
    Debug("unixdom_stop: freeing 0x%lx\r\n", (unsigned long) d);
    if (d->i_buf != NULL) {
	FREE(d->i_buf);
	d->i_buf = NULL;
    }

    /* Loosely from tcp_close_check() */
    if ((d->state == STATE_LISTENING) && (d->i_ix != -1)) {
	a_desc = desc_table[d->i_ix];
	if ((a_desc != NULL) && (a_desc->state == STATE_ACCEPTING)) {
	    /*
	    ** Some poor schmuck, a_desc, is waiting for a connection to 
	    ** come in on listening d.  But d is being closed, so we
	    ** tell a_desc to stop waiting.
	    */
	    Debug("XXXYYYXXX unixdom_stop: d is listening, a_desc is blocked, sending a_desc EINVAL\r\n");
	    reply_error(a_desc, EINVAL);
	}
    } else if (d->state == STATE_ACCEPTING) {
	/*
	** If we're waiting for an accept to finish, and we're now being
	** closed, we need to reset the state of the listening socket
	** with clear_listener().
	*/
	clear_listener(d);
	reply_error_am(d, am_closed);
    } else if (d->state == STATE_CONNECTING) {
	Debug("XXXYYYXXX unixdom_stop: UNIMPLEMENTED 3, d->state = %d\r\n", d->state);
	/* XXX async_error_am(INETP(d), am_closed); */
    } else if (d->state == STATE_CONNECTED) {
	reply_error_am(d, am_closed);
    }

    /* Loosely from inet_close() */
    if (d->state & F_OPEN) {
	desc_close(d);
	d->state = STATE_CLOSED;
    }
    desc_table[d->ix] = NULL;
    FREE(d);

    return 0;
}

PRIVATESTUFF int unixdom_output(long desc, char *foo, int bar)
{
    /*
    ** I don't think we need to implement this function.  If the driver's
    ** "commandv" method is defined, I think the function is never called.
    */
    Debug("XXXYYYZZZ unixdom_output: HEY, not implemented!  3rd arg = %d\n", bar);
    return 0;
}

PRIVATESTUFF int
unixdom_ready_input(descriptor *d, HANDLE handle)
{
    descriptor		*a_desc;
    int			n;
    int			ix;
#ifdef	XXX_WHY_DOES_THIS_FAIL_WITH_EFAULT_UNDER_SOLARIS_7_X86
    struct sockaddr	sin;
    socklen_t		sinlen;
#endif	/* XXX_WHY_DOES_THIS_FAIL_WITH_EFAULT_UNDER_SOLARIS_7_X86 */
    SOCKET		s;

    Debug("XXXYYYXXX unixdom_ready_input: top\r\n");
    if (d->state == STATE_LISTENING) {
	sock_select(d, FD_ACCEPT, 0);
	ix = d->i_ix;
	d->state = STATE_LISTEN; /* restore state */
	d->i_ix = -1;

	/* Has the accepting port been closed or otherwise changed state? */
	if ((a_desc = desc_table[ix]) == NULL ||
	    a_desc->state != STATE_ACCEPTING)
	    return 0;
						   
#ifdef	XXX_WHY_DOES_THIS_FAIL_WITH_EFAULT_UNDER_SOLARIS_7_X86
	/* See my comments in unixdom_echoserv.c regarding accept() */
	s = sock_accept(d->s, &sin, &sinlen);
#else	/* XXX_WHY_DOES_THIS_FAIL_WITH_EFAULT_UNDER_SOLARIS_7_X86 */
	s = sock_accept(d->s, NULL, NULL);
#endif	/* XXX_WHY_DOES_THIS_FAIL_WITH_EFAULT_UNDER_SOLARIS_7_X86 */
	Debug("XXXYYYXXX unixdom_ready_input: sock_accept(%d) = %d\r\n", d->s, s);
	/* XXX timer stuff here */
	if (s == INVALID_SOCKET) {
	    reply_error(a_desc, sock_errno());
	    desc_close(a_desc);
	} else {
	    a_desc->s = s;
	    if ((a_desc->event = sock_create_event(a_desc)) == INVALID_EVENT) {
		sock_close(s);
		reply_error(a_desc, sock_errno());
		desc_close(a_desc);
		return 0;
	    }
	    SET_NONBLOCKING(a_desc->s);
	    a_desc->state = STATE_CONNECTED;
	    sock_select(a_desc, FD_CONNECT, 0);
	    if (a_desc->active)
		sock_select(a_desc, FD_READ|FD_CLOSE, 1);
	    else
		sock_select(a_desc, FD_READ|FD_CLOSE, 0);
	    return reply_ok(d);	/* Yes, we really want to reply to d! */
	}
    } else if (IS_CONNECTED(d)) {
	n = unixdom_recv(d, 0);
	if (n > 0) {
	    Debug("XXXYYYXXX unixdom_ready_input: read %d bytes, calling reply_data\r\n", n);
	    reply_data(d, n);
	} else {
	    Debug("XXXYYYXXX unixdom_ready_input: read 0 bytes, socket closed\r\n");
	    /* unixdom_recv() already closed socket & sent closed reply */
	}
	if (!d->active) {
	    Debug("XXXYYYXXX unixdom_ready_input: d is not active, unselecting read&close\r\n");
	    sock_select(d, FD_READ|FD_CLOSE, 0);
	}
	return n;
    } else {
	/* XXX maybe a close op from connection attempt?? */
	sock_select(d, FD_ACCEPT, 0);
	return 0;
    }
    Debug("XXXYYYXXX unixdom_ready_input: fell through!\r\n");
    return -1;
}

/* socket has ouput:
** 1. STATE_CONNECTING => non block connect ?
** 2. STATE_CONNECTED  => pending write output (queued via driver_*q mechanism)
**
** XXX NOTE: "handle" arg is unused, whatever the heck it is.
*/
PRIVATESTUFF int
unixdom_ready_output(descriptor *d, HANDLE handle)
{
    int remaining, todo;
    int ix = d->port;

    if (d->state == STATE_CONNECTING) {
	Debug("unixdom_ready_output: XXXYYYZZZ async connect ready\r\n");
	d->state = STATE_CONNECTED;
	return reply_ok(d);
    } else if (IS_CONNECTED(d)) {
	todo = driver_deq(ix, 0);
	Debug("unixdom_ready_output: top of for loop, got %d bytes in the send queue\r\n", todo);
        for (;;) {
            int vsize;
            int n;
            SysIOVec* iov;

            if ((iov = driver_peekq(ix, &vsize)) == NULL) {
                sock_select(d, FD_WRITE, 0);
		Debug("unixdom_ready_output: all done!\r\n");
                return 0;
            }
            vsize = vsize > MAX_VSIZE ? MAX_VSIZE : vsize;
            Debug("unixdom_ready_output(%ld): s=%d, About to send %d items\r\n", d->port, d->s, vsize);
            if (sock_sendv(d->s, iov, vsize, &n, 0) == SOCKET_ERROR) {
                if ((sock_errno() != ERRNO_BLOCK) && (sock_errno() != EINTR)) {
                    Debug("unixdom_ready_output(%ld): sock_sendv errno = %d\r\n", d->port, sock_errno());
		    /* XXX Is this safe since unixdom_ready_output is acting asynchronously? */
                    return reply_error(d, sock_errno());
                }
                return 0;
            }
            if ((remaining = driver_deq(ix, n)) <= d->low) {
		Debug("unixdom_ready_output: we're under low watermark\r\n");
                if (IS_BUSY(d)) {
                    d->state &= ~F_BUSY;
                    set_busy_port(d->port, 0);
                    /* if we have a timer then cancel and send ok to client */
                    if (d->busy_on_send) {
                        driver_cancel_timer(d->port);
                        d->busy_on_send = 0;
			/* XXX Is this safe since unixdom_ready_output is acting asynchronously? */
                        reply_ok(d);
                    }
                } else {
		    Debug("unixdom_ready_output: we are NOT busy\r\n");
		}
            } else {
		Debug("unixdom_ready_output: %d bytes remain in port's send queue\r\n", remaining);
	    }
        }
    }
    return 0;
}

#ifdef	___AVOID_DOUBLE_FREE

/*
** This function is probably not necessary.  It's #ifdef'ed out of the
** inet driver for TCP.  If it isn't commented out, you get a double
** free as shown by:
**
** (foo@bb)1> {ok, S1} = unixdom:start_link().
** unixdom driver_init: running
** unixdom driver_init: done
** unixdom_init: running
** unixdom_start: starting, args = 0x8102000, unixdom_drv
** unixdom_start: found ix = 0
** {ok,<0.34.0>}
** (foo@bb)2> Narf.
** unixdom_stop: freeing 0x8112200
** unixdom_finish: starting
** unixdom_stop: freeing 0x8112200
** beam in free(): warning: chunk is already free.
** ** exited: {{unbound,'Narf'},[{erl_eval,expr,3}]} **
*/

PRIVATESTUFF int
unixdom_finish(void *arg)
{
    int		i;
    descriptor*	desc;

    Debug("unixdom_finish: starting\r\n");
    for (i = 0; i < desc_size; i++) {
        if ((desc = desc_table[i]) != NULL) {
	    unixdom_stop((long) desc);
        }
    }
    sys_free(desc_table);
    return 0;
}

#endif	/* ___AVOID_DOUBLE_FREE */

PRIVATESTUFF int
unixdom_ctl(descriptor *d, int cmd, char *buf, int buflen, char **rbuf,
	    int rbuflen)
{
    unsigned long	size, timeout;
    char		tbuf4[4];

    Debug("unixdom_ctl: top, cmd = %d\r\n", cmd);
    switch (cmd) {
    case UNIXDOM_REQ_ENDIAN:
	return ctl_endian(rbuf, rbuflen);
    case UNIXDOM_REQ_KNUTHHASH:
	return ctl_knuthhash(buf, buflen, rbuf, rbuflen);
    case UNIXDOM_REQ_OPEN:
	return ctl_open(d, rbuf, rbuflen);
    case UNIXDOM_REQ_CONNECT:
	return ctl_connect(d, buf, buflen, rbuf, rbuflen);
    case UNIXDOM_REQ_CLOSE:
	return ctl_close(d, rbuf, rbuflen);
    case UNIXDOM_REQ_WRITE:
	return ctl_write(d, buf, buflen, rbuf, rbuflen);
    case UNIXDOM_REQ_RECV:
	size = get_int32(buf);
	timeout = get_int32(buf);
	Debug("unixdom_ctl: buflen = %d, RECV length = %lu, timeout %lu\r\n", buflen, size, timeout);
	return ctl_recv(d, size, timeout, rbuf, rbuflen);
    case UNIXDOM_REQ_SETOPTS:
	return ctl_setopts(d, buf, buflen, rbuf, rbuflen);
    case UNIXDOM_REQ_BIND:
	return ctl_bind(d, buf, buflen, rbuf, rbuflen);
    case UNIXDOM_REQ_ACCEPT:
	return ctl_accept(d, buf, buflen, rbuf, rbuflen);
    case UNIXDOM_REQ_GETIX:
	Debug("unixdom_ctl: GETIX = %d\r\n", d->ix);
	put_int32(d->ix, tbuf4);
	return ctl_reply(UNIXDOM_REP_OK, tbuf4, 4, rbuf, rbuflen);
    case UNIXDOM_REQ_GETOPTS:
	return ctl_getopts(d, buf, buflen, rbuf, rbuflen);
    default:
	Debug("unixdom_ctl: got unknown command %d\r\n", cmd);
	return ctl_xerror(EXBADPORT, rbuf, rbuflen);
    }
}

PRIVATESTUFF int
unixdom_timeout(descriptor *d, HANDLE handle)
{
    int state = d->state;

    if ((state & STATE_CONNECTED) == STATE_CONNECTED) {
	/*
	** XXXYYYZZZ I disagree (and/or am confused by) the example set by
	** tcp_inet_timeout().  That function will only do something in
	** this case if d->busy_on_send is true.
	**
	** Well, here's what happens with the R7B-1 TCP driver if I send
	** a bunch of stuff to an "echo" server but the socket is not
	** in active mode, so we don't read stuff from the socket until its
	** congestion window gets completely full.
	**
	** 1> {ok, E1} = gen_tcp:connect({127,0,0,1}, 6666, [binary, {active, false}, {send_timeout, 6000}]).
	** {ok,#Port<0.7>}
	** 2> Big1 = list_to_binary(lists:duplicate(4096, $a)).
	** <<97,97,97,97,97,97,97,97,97,97,97,97,97,97,97,97,97,97,97,97,97,97,97,97,97,97,97,97,97,...>>
	** 3> gen_tcp:send(E1, Big1).
	** ok
	** 4> gen_tcp:send(E1, Big1).
	** ok
	** [...]
	** 32> gen_tcp:send(E1, Big1).
	** {error,timeout}
	** 33> gen_tcp:send(E1, Big1).
	** +++ blocks forever here +++
	**
	*/
        if (d->busy_on_send) {
            d->busy_on_send = 0;
            return reply_error_am(d, am_timeout);
        } else {
            /* assume recv timeout */
            assert(!d->active);
            sock_select(d, (FD_READ|FD_CLOSE), 0);
            /* XXX d->i_remain = 0; */
            return reply_error_am(d, am_timeout);
        }
    }
    else if ((state & STATE_CONNECTING) == STATE_CONNECTING) {
	/* assume connect timeout */
	/* close the socket since it's not usable (see man pages) */
	Debug("XXXYYYZZZ unixdom_timeout: async connect timeout\r\n");
	unixdom_close(d);
	return reply_error_am(d, am_timeout);
    }
    else if ((state & STATE_ACCEPTING) == STATE_ACCEPTING) {
	/* timer is set on accepting port */
	Debug("XXXYYYZZZ unixdom_timeout: accepting socket timeout\r\n");
	clear_listener(d);
	return reply_error_am(d, am_timeout);
    }
    return 0;
}

/*
** The driver's "outputv" method is called when an Erlang process uses
** "Port ! {self(), {command, Data}}".  The port driver glue has already
** converted Data to an ErlIOVec in a (hopefully) efficient manner.
*/

PRIVATESTUFF int
unixdom_outputv(descriptor *d, ErlIOVec *ev)
{
    Debug("unixdom_outputv: top\r\n");
    if (!IS_CONNECTED(d))
	return reply_error(d, ENOTCONN);
    if (do_outputv(d, ev) == 0)
	return reply_ok(d);
    /*
    ** If do_outputv() above didn't block, then we immediately return
    ** an 'ok'.  When the "ready_output" method has flushed the data,
    ** the 'ok' (or error status) will be sent to the caller then.
    ** So, if we're this far, don't send any reply now.
    */
    /*
    ** XXXYYYXXX Will we have a problem if multiple send() calls
    ** end up blocking, and then when the send queue drains, we only
    ** make one reply instead of several?  {shrug}
    */
    return 0;
}

PRIVATESTUFF int
do_outputv(descriptor *desc, ErlIOVec *ev)
{
    descriptor	*d = (descriptor *) desc;
    int sz;
    char buf[4];
    int h_len;
    int n;
    int ix = d->port;
    int len = ev->size;
    int vsize;

    Debug("do_outputv: top\r\n");
    /* XXX TCP driver does PB_1, PB_2, and PB_4 checking here */
    if (len == 0)
	return 0;
    h_len = 0;

    output_count(d, len+h_len);

    if (h_len > 0) {
        ev->iov[0].iov_base = buf;
        ev->iov[0].iov_len = h_len;
        ev->size += h_len;
    }

    if ((sz = driver_sizeq(ix)) > 0) {
        Debug("unixdom_outputv(%ld): s=%d, driver has queue size = %d, so we'll queue this request\r\n", d->port, d->s, sz);
        driver_enqv(ix, ev, 0);
        if (sz+ev->size >= d->high) {
	    Debug("unixdom_outputv(%ld): s=%d, driver has queue size = %d >= high = %d\r\n", d->port, d->s, sz, d->high);
            d->state |= F_BUSY;  /* mark for low-watermark */
            set_busy_port(d->port, 1);
            if (d->send_timeout != INFINITY) {
		Debug("unixdom_outputv(%ld): s=%d, driver has queue size = %d >= high = %d, setting timer for %dms\r\n", d->port, d->s, sz, d->high, d->send_timeout);
                d->busy_on_send = 1;
                driver_set_timer(d->port, d->send_timeout);
                return 1;
            }
        }
    }
    else {
        vsize = (ev->vsize > MAX_VSIZE) ? MAX_VSIZE : ev->vsize;
        Debug("unixdom_outputv(%ld): s=%d, about to send %d,%d bytes\r\n", d->port, d->s, h_len, len);
        
        if (sock_sendv(d->s, ev->iov, vsize, &n, 0) == SOCKET_ERROR) {
            if ((sock_errno() != ERRNO_BLOCK) && (sock_errno() != EINTR)) {
                int err = sock_errno();
                Debug("unixdom_outputv(%ld): s=%d,sock_sendv(size=2) errno = %d\r\n", d->port, d->s, err);
                return reply_error(d, err);
            }
            n = 0;
        }
        else if (n == ev->size)
            return 0;

        Debug("unixdom_outputv(%ld): s=%d, Send failed (errno %d), queueing n=%d bytes\r\n", d->port, d->s, sock_errno(), ev->size - n);
        driver_enqv(ix, ev, n); 
        sock_select(d, (FD_WRITE|FD_CLOSE), 1);
    }
    return 0;
}

PRIVATESTUFF int
unixdom_recv(descriptor *d, int request_len)
{
    int	sz, n, err;

    sz = (request_len > 0) ? request_len : d->bufsz;
    if (d->i_buf == NULL) {	/* Gotta allocate a read buffer? */
	if ((d->i_buf = driver_alloc_binary(sz)) == NULL)
	    return -1;
	d->i_bufsz = sz;
	d->i_bufp = d->i_buf->orig_bytes;
    }
    Debug("unixdom_recv: about to read %d bytes on sock %d\r\n", sz, d->s);
    n = sock_recv(d->s, d->i_bufp, sz, 0);
    if (n < 0) {
	err = sock_errno();
	if (err == ECONNRESET || err == EPIPE) {
	    Debug("unixdom_recv: detected close on sock %d\r\n", d->s);
	    return unixdom_recv_closed(d);
	} else if (err == EWOULDBLOCK) {
	    Debug("unixdom_recv: would block on sock %d\r\n", d->s);
	    return 0;
	} else {
	    Debug("unixdom_recv: errno %d sock %d\r\n", err, d->s);
	    return unixdom_recv_error(d, err);
	}
    } else if (n == 0) {
	Debug("unixdom_recv: (2) detected close on sock %d\r\n", d->s);
	return unixdom_recv_closed(d);
    }
    Debug("unixdom_recv: just read %d bytes from socket %d\r\n", n, d->s);
    input_count(d, n);

    return n;
}

PRIVATESTUFF int
unixdom_recv_closed(descriptor *d)
{
    if (!d->active) {
	Debug("unixdom_recv_closed: closing passive socket %d\r\n", d->s);
        /* We must cancel any timer here ! */
        driver_cancel_timer(d->port);
        /* passive mode do not terminate port ! */
        clear_input(d);
        desc_close(d);
        /* next time EXBADSEQ will be XappXed  */
	reply_error_am(d, am_closed);
    }
    else {
	Debug("unixdom_recv_closed: closing active socket %d\r\n", d->s);
        /* A send is blocked */
        if (IS_BUSY(d)) {
            clear_output(d);
            driver_cancel_timer(d->port);
            d->busy_on_send = 0;
            d->state &= ~F_BUSY;
            set_busy_port(d->port, 0);
            reply_error_am(d, am_closed); /* Yes, two error messages are kosher, I guess */
        }
        clear_input(d);
	sock_select(d, FD_WRITE|FD_READ, 0);
        
        if (d->exitf)
            driver_exit(d->port, 0);
        else
            desc_close(d);
    }
    return -2;
}

PRIVATESTUFF int
unixdom_recv_error(descriptor *d, int err)
{
    if (err != EWOULDBLOCK) {
        if (!d->active) {
            /* We must cancel any timer here ! */
            driver_cancel_timer(d->port);
            clear_input(d);
            reply_error_am(d, err);
        }
        else {
            /* A send is blocked */ /* XXX izzat correct? */
            if (IS_BUSY(d)) {
                clear_output(d);
                driver_cancel_timer(d->port);
                d->busy_on_send = 0;
                d->state &= ~F_BUSY;
                set_busy_port(d->port, 0);
                reply_error_am(d, am_closed);
            }
            clear_input(d);
            reply_error(d, err); /* first error */
            reply_closed(d);     /* then closed */
            if (d->exitf)
                driver_exit(d->port, err);
            else
                desc_close(d);
        }
        return -1;
    }
    return 0;
}

/* general control reply function */
PRIVATESTUFF int
ctl_reply(int rep, char* buf, int len, char **rbuf, int rbuflen)
{
    char* ptr;

    if ((len+1) > rbuflen) {
        ptr = ALLOC(len+1);
        *rbuf = ptr;
    }
    else
        ptr = *rbuf;
    *ptr++ = rep;
    memcpy(ptr, buf, len);
    return len+1;
}

/* general control error reply function */
PRIVATESTUFF int
ctl_error(int err, char **rbuf, int rbuflen)
{
    char response[256];         /* Response buffer. */
    char* s;
    char* t;

    for (s = erl_errno_id(err), t = response; *s; s++, t++)
        *t = tolower(*s);
    return ctl_reply(UNIXDOM_REP_ERROR, response, t-response, rbuf, rbuflen);
}

PRIVATESTUFF int
ctl_xerror(char* xerr, char **rbuf, int rbuflen)
{
    int n = strlen(xerr);
    return ctl_reply(UNIXDOM_REP_ERROR, xerr, n, rbuf, rbuflen);
}

PRIVATESTUFF int
reply_ok(descriptor *d)
{
    DriverTermData spec[8];
    int i = 0;

Debug("reply_ok: top\r\n");
    i = LOAD_ATOM(spec, i, am_unixdom_reply);
    i = LOAD_PORT(spec, i, d->dport);
    i = LOAD_ATOM(spec, i, am_ok);
    i = LOAD_TUPLE(spec, i, 3);
    assert(i == 8);

    return driver_output_term(d->port, spec, i);    
}

PRIVATESTUFF int
reply_wouldblock(descriptor *d)
{
    DriverTermData spec[8];
    int i = 0;

Debug("reply_ok: top\r\n");
    i = LOAD_ATOM(spec, i, am_unixdom_reply);
    i = LOAD_PORT(spec, i, d->dport);
    i = LOAD_ATOM(spec, i, am_wouldblock);
    i = LOAD_TUPLE(spec, i, 3);
    assert(i == 8);

    return driver_output_term(d->port, spec, i);    
}

PRIVATESTUFF int
reply_error(descriptor *d, int err)
{
    struct sockaddr_un	other;
    socklen_t sz = sizeof(other);
    int code;

    Debug("reply_error: top, d = 0x%lx\r\n", (unsigned long) d);
    /*
    ** XXX The busy state clearing stuff below is weird.  I don't
    ** understand why the TCP driver code does it here.  In all but one
    ** case, in the context where reply_error() could be called, we already
    ** know that the port cannot be busy, so there's no busy status to
    ** clear.
    ** The remaining case is when we're called by
    ** unixdom_ready_output()/tcp_inet_output(), inside the for (;;) loop.
    ** In that case, we know there's at least some data queued for the
    ** socket.  Do we really want to clear the busy status on the port
    ** in this case?  Assuming that the socket is still kosher & connected,
    ** another attempt to send data will end up queueing that data
    ** and (probably) putting the port back into busy status.  Right?
    */
    if (IS_BUSY(d)) {
	Debug("reply_error: d = 0x%lx is busy\r\n", (unsigned long) d);
        if (d->busy_on_send) {
            driver_cancel_timer(d->port);
            d->busy_on_send = 0;
        }
        d->state &= ~F_BUSY;
        set_busy_port(d->port, 0);
    }

    Debug("reply_error: d = 0x%lx before sock_peer()\r\n", (unsigned long) d);
    code = sock_peer(d->s,(struct sockaddr *) &other, &sz);
    if ((code == SOCKET_ERROR) && (sock_errno() == ENOTCONN ||
                                   sock_errno() == EPIPE)) {
        Debug("driver_failure_eof(%ld) in %s, line %d, errno %d\r\n",
                (unsigned long) d->port, __FILE__, __LINE__, sock_errno());
        if (d->active) {
	    /*
	    ** XXX I'm not certain if this is correct behavior.  Since this
	    ** is cut-and-pasted from the tcp driver (tcp_send_error()),
	    ** perhaps it is?  If the socket is active, then we send two
	    ** replies.  The first is the {closed, <port#>} tuple that
	    ** you'd expect from an active socket.  The second is the
	    ** {inet_reply, <port#>, {error, closed}} tuple, presumably
	    ** because the caller is/may be blocking for an inet_reply
	    ** tuple?  Should we really be sending both?
	    */
	    Debug("XXXYYYZZZ reply_error: Should we be sending two replies here??\r\n");
            reply_closed(d);
            reply_error_am(d, am_closed);
            if (d->exitf)
                driver_exit(d->port, 0);
            else
                desc_close(d);
        }
        else {
	    Debug("reply_error: d = 0x%lx 2\r\n", (unsigned long) d);
            clear_output(d);
            unixdom_close(d);
            reply_error_am(d, am_closed);
        }
    }
    else  {
	Debug("reply_error: d = 0x%lx 3\r\n", (unsigned long) d);
        reply_error_am(d, error_atom(sock_errno()));
    }
    return -1;
}

PRIVATESTUFF int
reply_closed(descriptor *d)
{
    DriverTermData spec[6];
    int i = 0;

    Debug("reply_closed(%ld):\r\n", d->port);

    i = LOAD_ATOM(spec, i, am_closed);
    i = LOAD_PORT(spec, i, d->dport);
    i = LOAD_TUPLE(spec, i, 2);
    assert(i <= 6);
    return driver_output_term(d->port, spec, i);
}

/* send:
**   {inet_reply, S, {error, Reason}} 
*/
PRIVATESTUFF int
reply_error_am(descriptor *d, DriverTermData reason)
{
    DriverTermData spec[12];
    int i = 0;
    int	r = 0;

    i = LOAD_ATOM(spec, i, am_unixdom_reply);
    i = LOAD_PORT(spec, i, d->dport);
    i = LOAD_ATOM(spec, i, am_error);
    i = LOAD_ATOM(spec, i, reason);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 3);
    assert(i == 12);

    Debug("reply_error_am: d->port = %ld\r\n", (unsigned long) d->port);
    r = driver_output_term(d->port, spec, i);
    Debug("reply_error_am: returning\r\n");
    return r;
}

PRIVATESTUFF int
reply_data(descriptor *d, int nbytes)
{
    DriverTermData spec[20];
    int i = 0;

    Debug("reply_data: top: d->port = 0x%lx\r\n", d->port);

    i = LOAD_ATOM(spec, i, am_unixdom);
    i = LOAD_PORT(spec, i, d->dport);
    i = LOAD_BINARY(spec, i, d->i_buf, 0, nbytes);
    i = LOAD_TUPLE(spec, i, 3);
    assert(i <= 20);    
    driver_output_term(d->port, spec, i);
    driver_free_binary(d->i_buf);
    d->i_buf = NULL;
    return nbytes;
}

/*
** This is a holdover from some of Scott's earliest driver experimentation
** days.  The basis for this function actually came from Tobbe
** T&oslash;rnkvist's "byteorder" driver.  It was a big help in figuring
** out some of the basics for dynamic driver handling.
*/

PRIVATESTUFF int
ctl_endian(char **rbuf, int rbuflen)
{
    int			endian = 1;
    unsigned char	tmpbuf;

    if (*(char *) &endian)
	tmpbuf = UNIXDOM_REPBODY_LSB;
    else
	tmpbuf = UNIXDOM_REPBODY_MSB;
    return ctl_reply(UNIXDOM_REP_OK, (char *) &tmpbuf, 1, rbuf, rbuflen);
}

/*
** Another early driver experimentation holdover.  Implementing this in
** Erlang is actually painful because of Erlang's bignum support.
** It's much faster and more useful in C due to integer overflows.
*/

PRIVATESTUFF int
ctl_knuthhash(char *path, int pathlen, char **rbuf, int rbuflen)
{
    enum {
	X = 3,
	Y = 5,
	PRIME = 23
    };
    unsigned char *pt;
    unsigned int n;

    n = 0;
    pt = (unsigned char *) path;
    while (pathlen > 0) {
	n = ((n << X) ^ (n >> Y)) ^ *pt;
	++pt;
	pathlen--;
    }
    return ctl_reply(UNIXDOM_REP_OK, (char *) &n, sizeof(n), rbuf, rbuflen);
}

PRIVATESTUFF int
ctl_open(descriptor *d, char **rbuf, int rbuflen)
{
    Debug("ctl_open: starting\r\n");
    assert(d != NULL);
    if (d->state != STATE_CLOSED)
	return ctl_xerror(EXBADSEQ, rbuf, rbuflen);
    if ((d->s = sock_open(PF_LOCAL, SOCK_STREAM, 0)) == INVALID_SOCKET)
	return ctl_error(sock_errno(), rbuf, rbuflen);
    if ((d->event = sock_create_event(d)) == INVALID_EVENT)
        return ctl_error(sock_errno(), rbuf, rbuflen);
#ifndef	SET_NONBLOCKING
    YOU_LOSE;
#endif
    SET_NONBLOCKING(d->s);
    d->state = STATE_OPEN;
    d->stype = SOCK_STREAM;	/* XXX hardcoded */
    Debug("ctl_open: done\r\n");
    return ctl_reply(UNIXDOM_REP_OK, NULL, 0, rbuf, rbuflen);
}

PRIVATESTUFF int
ctl_connect(descriptor *d, char *p, int plen, char **rbuf, int rbuflen)
{
    char		path[BUFSIZ];
    struct sockaddr_un  sin;
    int			res;
    unsigned long	timeout;

    Debug("ctl_connect: starting\r\n");
    if (!IS_OPEN(d))
	return ctl_xerror(EXBADPORT, rbuf, rbuflen);
    if (IS_CONNECTED(d))
	return ctl_error(EISCONN, rbuf, rbuflen);
    if (IS_CONNECTING(d))
	return ctl_error(EINVAL, rbuf, rbuflen);

    timeout = get_int32(p);
    Debug("ctl_connect: timeout = %ld\r\n");
    p += 4;
    plen -= 4;
    strncpy(path, p, plen);
    path[plen] = '\0';
    sin.sun_family = AF_UNIX;
    strcpy(sin.sun_path, path);
#if	defined(SUN_LEN) && (! (defined(solaris) || defined(linux)))
    sin.sun_len = SUN_LEN(&sin);
#endif	/* SUN_LEN */
    sock_select(d, FD_CONNECT, 1);
    res = sock_connect(d->s, (struct sockaddr *) &sin, sizeof(sin));
    if (res == SOCKET_ERROR) {
	if (sock_errno() == EINPROGRESS) {
	    Debug("XXXYYYZZZ ctl_connect: blocking 1, async not impl!\r\n");
	    d->state = STATE_CONNECTING;
	    if (timeout != INFINITY)
		driver_set_timer(d->port, timeout);
	    return ctl_error(EINVAL, rbuf, rbuflen);
	}
	Debug("XXXYYYZZZ ctl_connect: blocking 0, async not impl!\r\n");
	return ctl_error(sock_errno(), rbuf, rbuflen);
    }
    sock_select(d, FD_CONNECT, 0);
    d->state = STATE_CONNECTED;
    if (d->active)
	sock_select(d, (FD_READ|FD_CLOSE), 1);
    
    Debug("ctl_connect: bound to %s\r\n", path);
    return ctl_reply(UNIXDOM_REP_OK, NULL, 0, rbuf, rbuflen);
}

PRIVATESTUFF int
ctl_close(descriptor *d, char **rbuf, int rbuflen)
{
    if (d->state == STATE_CLOSED)
	ctl_error(EINVAL, rbuf, rbuflen);
    clear_input(d);		/* XXX TCP drvr doesn't do this, good idea? */
    desc_close(d);
    d->state = STATE_CLOSED;
    return ctl_reply(UNIXDOM_REP_OK, NULL, 0, rbuf, rbuflen);
}

/* socket has output:
** 1. STATE_CONNECTING => non block connect ? (XXXYYYZZZ not impl)
** 2. STATE_CONNECTED  => write output
**
** NOTE: Use of ctl_write() is deprecated, due to its lower efficiency than
**       sending data via "Port ! {self(), {command, Data}}", which ends
**       up calling the driver's outputv method, if defined.  I'll leave
**       ctl_write() in here for completeness.
*/

PRIVATESTUFF int
ctl_write(descriptor *d, char *buf, int count, char **rbuf, int rbuflen)
{
    ssize_t	bytes;
  
    Debug("ctl_write: desc = 0x%lx, count = %d\r\n", (unsigned long) d, count);

    if (IS_CONNECTING(d)) {
	Debug("XXXYYYZZZ ctl_write: async connect NOT IMPLEMENTED\r\n");
	return ctl_error(EINVAL, rbuf, rbuflen);
    } else if (IS_CONNECTED(d)) {
	Debug("ctl_write: writing ... ");
	if ((bytes = write(d->s, buf, count)) < 0) {
	    return ctl_error(sock_errno(), rbuf, rbuflen);
	}
	if (bytes != count) {
	    return ctl_error(EOVERFLOW, rbuf, rbuflen);
	}
	Debug("successful\r\n"); /* XXX */
	return ctl_reply(UNIXDOM_REP_OK, (char *) &count, sizeof(count), rbuf, rbuflen);
    } else {
	sock_select(d, FD_CONNECT, 0); /* XXX? */
	return ctl_error(EINVAL, rbuf, rbuflen);
    }
    /* NOTREACHED */
}

PRIVATESTUFF int
ctl_recv(descriptor *d, unsigned long bytes, unsigned long timeout,
	 char **rbuf, int rbuflen)
{
    int	n;

    /*
    ** unixdom_recv() will perform the read *and* will send a reply
    ** to the calling Erlang process.
    */
    if (!IS_CONNECTED(d)) {
	Debug("XXXYYYXXX ctl_recv: recv attempt on unconnected socket\r\n");
	return unixdom_recv_error(d, EBADF);
    }
    n = unixdom_recv(d, bytes);
    if (n > 0) {
	Debug("XXXYYYXXX ctl_recv: read %d bytes, calling reply_data\r\n", n);
	reply_data(d, n);
	return ctl_reply(UNIXDOM_REP_OK, NULL, 0, rbuf, rbuflen);
    } else if (n == 0) {
	Debug("XXXYYYXXX ctl_recv: unixdom_recv returned %d\r\n", n);
	/*
	** Apparently, we *must* return some kind of reply.  Silence is
	** not permitted (the driver returns {error, internal} if we
	** return 0 here).  So, we return a message which tells our Erlang
	** caller that we would've blocked and that it needs to wait for
	** another message when some actual data arrives.
	*/
	/*
	** reply_wouldblock() will clobber d->caller, which will make
	** returning an async error message (e.g. socket closed) quite
	** difficult.  XXXYYYZZZ What I don't understand is that there
	** are two different functions for sending terms back to an
	** Erlang process: driver_send_term(port, caller, spec, specparts),
	** and driver_output_term(port, spec, specparts).  The former is
	** apparently more specific.  Does driver_output_term() send the
	** term to all Erlang processes linked to the port?  Er, no,
	** it doesn't.  According to erts/beam/io.c, driver_output_term()
	** sends the message to the port's owner only.  That's awfully
	** convenient, since then we don't have to maintain that state.
	**
	** This brings up the question of, "Why does the R7B INET driver
	** maintain desc->driver state, and why does it use
	** driver_send_term()?"  It's as if you could have several
	** processes sending & receiving data from an INET socket at
	** once.  However, R7B has the same concept as R6B does of
	** having a single "controlling process" (or owner) for the
	** port itself.  ...
	**
	** Hrm.  It looks like driver_output_term() is only used by the
	** INET driver for sockets in active mode.  I'm guessing, then,
	** that the R7B driver designers envisioned a world where a
	** non-active INET socket could be used by several processes
	** simultaneously.  Hrmhrm.  I could see situations where this
	** might be helpful if multiple processes want to send data to
	** a single socket.  However, it would have to be situations where
	** the ordering of who sends what when on the socket doesn't
	** matter.  As for multiple processes reading from the socket?
	** I guess it's a matter of first come/requested, first served.
	**
	** As a practical matter, though, I don't like that model.  I
	** believe the R6B restriction of single controlling process
	** being the only process that can send & receive data on a
	** socket is a reasonable restriction.  That single process can
	** take care of making certain things are read & written in the
	** proper order.
	*/
	if (timeout == 0) {
	    /* Immediately send the 2nd message with timeout error */
	    reply_error_am(d, am_timeout);
	    return ctl_reply(UNIXDOM_REP_OK, NULL, 0, rbuf, rbuflen);
	} else {
	    if (timeout != INFINITY)
		driver_set_timer(d->port, timeout);
	    sock_select(d, FD_READ|FD_CLOSE, 1);
	    Debug("XXXYYYXXX ctl_recv: sock %d registered for read&close\r\n", d->s);
	    return ctl_reply(UNIXDOM_REP_OK, NULL, 0, rbuf, rbuflen);
	    /* XXX I *think* this is bogus: return reply_wouldblock(d); */
	}
    } else if (n == -1) {
	Debug("XXXYYYXXX ctl_recv: unixdom_recv returned %d, errno = %d\r\n", n, sock_errno());
	return ctl_error(sock_errno(), rbuf, rbuflen);
    } else if (n == -2) {	/* -2 = closed */ 
	Debug("XXXYYYXXX ctl_recv: unixdom_recv returned %d, i.e. closed\r\n", n);
	return ctl_xerror("closed", rbuf, rbuflen);
    }
    /* NOTREACHED, but shut up the compiler */
    return -1;
}

PRIVATESTUFF void
desc_close(descriptor *d)
{
    if (d->s != INVALID_SOCKET) {
        sock_select(d, FD_READ | FD_WRITE | FD_CLOSE, 0);
        sock_close(d->s);
        d->s = INVALID_SOCKET;
        sock_close_event(d->event);
        d->event = INVALID_EVENT;
        d->event_mask = 0;
    }
}

/* update statistics on input packets */
PRIVATESTUFF void
input_count(descriptor *d, int len)
{
    unsigned long n = d->recv_cnt + 1;
    unsigned long t = d->recv_oct[0] + len;
    int c = (t < d->recv_oct[0]);
    double avg = d->recv_avg;
    double dvi;

    /* at least 64 bit octet count */
    d->recv_oct[0] = t;
    d->recv_oct[1] += c;

    if (n == 0) /* WRAP */
        n = 1;

    /* average packet length */
    avg = avg + (len - avg) / n;
    d->recv_avg = avg;

    if (len > d->recv_max)
        d->recv_max = len;

    /* average deviation from average packet length */
    dvi = d->recv_dvi;
    d->recv_dvi = dvi + ((len - avg) - dvi) / n;
    d->recv_cnt = n;
}

/* update statistics on output packets */
PRIVATESTUFF void
output_count(descriptor *d, int len)
{
    unsigned long n = d->send_cnt + 1;
    unsigned long t = d->send_oct[0] + len;
    int c = (t < d->send_oct[0]);
    double avg = d->send_avg;

    /* at least 64 bit octet count */
    d->send_oct[0] = t;
    d->send_oct[1] += c;

    if (n == 0) /* WRAP, use old avg as input to a new sequence */
        n = 1;
    d->send_avg += (len - avg) / n;
    if (len > d->send_max)
        d->send_max = len;
    d->send_cnt = n;
}

/* clear input buffers */
PRIVATESTUFF void
clear_input(descriptor *d)
{
    Debug("XXXYYYXXX clear_input called\r\n");
    if (d->i_buf != NULL)
	driver_free_binary(d->i_buf);
    d->i_buf = NULL;
    d->i_bufp = NULL;
    d->i_bufsz = 0;
}

/* clear QUEUED output */
PRIVATESTUFF void
clear_output(descriptor *d)
{
    int ix  = d->port;
    int qsz = driver_sizeq(ix);

    driver_deq(ix, qsz);
}

PRIVATESTUFF int
unixdom_close(descriptor *d)
{
    if ((d->prebound == 0) && (d->state & F_OPEN)) {
        desc_close(d);
        d->state = STATE_CLOSED;
    }
    return 0;
}

PRIVATESTUFF DriverTermData
error_atom(int err)
{
    char errstr[256];
    char* s;
    char* t;

    Debug("XXX error_atom: %d -> %s\r\n", err, erl_errno_id(err));
    for (s = erl_errno_id(err), t = errstr; *s; s++, t++)
        *t = tolower(*s);
    *t = '\0';
    return driver_mk_atom(errstr);
}

#ifdef	NEED_EXTRA_FUNCS

void *
sys_memzero(void *b, size_t len)
{
    memset(b, 0, len);
    return b;
}

#endif	/* NEED_EXTRA_FUNCS */

PRIVATESTUFF int
ctl_setopts(descriptor *d, char *buf, int buflen, char **rbuf, int rbuflen)
{
    char	*bufp = buf;
    long	l;
    char	*err = EXBADPORT;

    Debug("XXXYYYXXX ctl_setopts: top\r\n");
    while (*bufp != UNIXDOM_OPT_ENDOFLIST) {
	switch (*bufp) {
	case UNIXDOM_OPT_IGNORE:
	    /* do nothing */
	    Debug("XXXYYYXXX ctl_setopts: IGNORE\r\n");
	    break;
	case UNIXDOM_OPT_ACTIVE:
	    d->active = *(bufp + 1);
	    if (d->active)
		sock_select(d, (FD_READ|FD_CLOSE), 1);
	    else
		sock_select(d, (FD_READ|FD_CLOSE), 0);
	    Debug("XXXYYYXXX ctl_setopts: ACTIVE: %d\r\n", d->active);
	    bufp += 1;		/* Skip past active status byte */
	    break;
	case UNIXDOM_OPT_BACKLOG:
	    if (!IS_OPEN(d))
		goto error;
	    if (!IS_BOUND(d)) {
		err = EXBADSEQ;
		goto error;
	    }
	    l = get_int32(bufp + 1);
	    if (sock_listen(d->s, l) == SOCKET_ERROR) {
		goto error;
	    }
	    d->backlog = l;
	    Debug("XXXYYYXXX ctl_setopts: BACKLOG: %ld\r\n", l);
	    bufp += 4;		/* Skip past backlog size */
	    break;
	default:
	    Debug("XXXYYYXXX ctl_setopts: default\r\n");
	    goto error;
	    break;
	}
	bufp++;			/* Advance to next command */
    }
    Debug("XXXYYYXXX ctl_setopts: bottom ok\r\n");
    return ctl_reply(UNIXDOM_REP_OK, NULL, 0, rbuf, rbuflen);
    /* XXXYYYZZZ icky stuff */
    reply_ok(d);
    return 1;

  error:
    /* XXXYYYZZZ is reply_error_am() appropriate here? */
    Debug("XXXYYYXXX ctl_setopts: bottom ERROR\r\n");
    return ctl_xerror(err, rbuf, rbuflen);
}

PRIVATESTUFF int
ctl_bind(descriptor *d, char *buf, int buflen, char **rbuf, int rbuflen)
{
    char		path[BUFSIZ];
    struct sockaddr_un  sin;
    int			res;
    int			backlog = -1;

    Debug("ctl_bind: starting\r\n");
    if (!IS_OPEN(d))
	return ctl_xerror(EXBADPORT, rbuf, rbuflen);
    if (IS_CONNECTED(d))
	return ctl_error(EISCONN, rbuf, rbuflen);
    if (IS_CONNECTING(d))
	return ctl_error(EINVAL, rbuf, rbuflen);

    /* Get backlog size, 4 bytes */
    backlog = get_int32(buf);
    assert(backlog >= 0);
    buf += 4;
    buflen -= 4;
    /* Get path, variable length */
    strncpy(path, buf, buflen);
    path[buflen] = '\0';
    sin.sun_family = AF_UNIX;
    strcpy(sin.sun_path, path);
#if	defined(SUN_LEN) && (! (defined(solaris) || defined(linux)))
    sin.sun_len = SUN_LEN(&sin);
#endif	/* SUN_LEN */

    res = sock_bind(d->s, (struct sockaddr *) &sin, sizeof(sin));
    if (res == SOCKET_ERROR) {
	if (sock_errno() == EINPROGRESS) {
	    Debug("XXXYYYZZZ ctl_bind: blocking 1, async not impl!\r\n");
	    /* XXXYYYZZZ this is where R6B driver sets TCP_STATE_CONNECTING */
	    /* XXXYYYZZZ unimplemented: timeout (see R6B TCP driver) */
	    return ctl_error(EINVAL, rbuf, rbuflen);
	}
	Debug("XXXYYYZZZ ctl_bind: blocking 0, async not impl!\r\n");
	return ctl_error(sock_errno(), rbuf, rbuflen);
    }
    res = sock_listen(d->s, backlog);
    if (res == SOCKET_ERROR) {
	Debug("XXXYYYZZZ ctl_bind: listen error = %d\r\n", sock_errno());
	return ctl_error(sock_errno(), rbuf, rbuflen);
    }
    /* XXXYYYXXX Listening sockets are not active. */
    d->active = 0;
    sock_select(d, FD_ACCEPT, 0); /* Just in case */
    d->state = STATE_LISTEN;
    
    Debug("ctl_bind: bound to %s\r\n", path);
    return ctl_reply(UNIXDOM_REP_OK, NULL, 0, rbuf, rbuflen);
}

PRIVATESTUFF int
ctl_accept(descriptor *d, char *buf, int buflen, char **rbuf, int rbuflen)
{
    int			ix;
    unsigned long	timeout;
    descriptor		*l_desc;
#ifdef	XXX_WHY_DOES_THIS_FAIL_WITH_EFAULT_UNDER_SOLARIS_7_X86
    struct sockaddr	sin;
    socklen_t		sinlen;
#endif	/* XXX_WHY_DOES_THIS_FAIL_WITH_EFAULT_UNDER_SOLARIS_7_X86 */
    SOCKET		s;

    Debug("XXXYYYZZZ ctl_accept: d->state = %d\r\n", d->state);
    if (d->state != STATE_CLOSED)
	return ctl_xerror(EXBADPORT, rbuf, rbuflen);

    /* Get ix, 4 bytes */
    ix = get_int32(buf);
    buf += 4;
    buflen -= 4;
    /* Get timeout, 4 bytes */
    timeout = get_int32(buf);
    buf += 4;
    buflen -= 4;
    Debug("XXXYYYZZZ ctl_accept: ix = %d, timeout = %ld\r\n", ix, timeout);

    if ((ix >= desc_size) ||
	((l_desc = desc_table[ix]) == NULL)) {
	Debug("XXXYYYZZZ ctl_accept: BOGUS ix = %d\r\n", ix);
	return ctl_error(EINVAL, rbuf, rbuflen);
    }
    if (l_desc->state != STATE_LISTEN) {
	Debug("XXXYYYZZZ ctl_accept: l_desc not STATE_LISTEN\r\n");
	return ctl_error(EINVAL, rbuf, rbuflen);
    }
    /* Some flags must be inherited at this point, others set */
    d->mode    = l_desc->mode;
    d->exitf   = l_desc->exitf;
    d->active  = M_ACTIVE_DEFAULT;
    d->stype   = l_desc->stype; 
    d->htype   = l_desc->htype; 
    d->bufsz   = l_desc->bufsz;
    d->i_bufsz = l_desc->i_bufsz;
    assert(d->i_buf == NULL);
    assert(d->i_bufp == NULL);
    d->high    = l_desc->high;
    d->low     = l_desc->low;
    d->send_timeout = l_desc->send_timeout;

#ifdef	XXX_WHY_DOES_THIS_FAIL_WITH_EFAULT_UNDER_SOLARIS_7_X86
    s = sock_accept(l_desc->s, &sin, &sinlen);
#else	/* XXX_WHY_DOES_THIS_FAIL_WITH_EFAULT_UNDER_SOLARIS_7_X86 */
    s = sock_accept(l_desc->s, NULL, NULL);
#endif	/* XXX_WHY_DOES_THIS_FAIL_WITH_EFAULT_UNDER_SOLARIS_7_X86 */
    if (s == INVALID_SOCKET) {
	if (sock_errno() == ERRNO_BLOCK) {
	    /*
	    ** Here's the point where the TCP driver sets the state of
	    ** l_desc to STATE_LISTENING and stores d's ix (the interested
	    ** party) into l_desc->i_ix so that the calling party
	    ** can be informed when an accept is finally done.
	    */
	    Debug("XXXYYYXXX ctl_accept: accept would block, moving to ACCEPTING/LISTENING states\r\n");
	    d->state = STATE_ACCEPTING;
	    l_desc->state = STATE_LISTENING;
	    l_desc->i_ix = d->ix;
	    sock_select(l_desc, FD_ACCEPT, 1);
	    if (timeout != INFINITY)
		driver_set_timer(d->port, timeout);
	    return ctl_reply(UNIXDOM_REP_WOULDBLOCK, NULL, 0, rbuf, rbuflen);
	}  else {
	    return ctl_error(sock_errno(), rbuf, rbuflen);
	}
    } else {
	d->s = s;
	if ((d->event = sock_create_event(d)) == INVALID_EVENT)
	    return ctl_error(sock_errno(), rbuf, rbuflen);
	SET_NONBLOCKING(d->s);
	d->state = STATE_CONNECTED;
	sock_select(d, FD_CONNECT, 0);
	if (d->active)
	    sock_select(d, FD_READ|FD_CLOSE, 1);
	else
	    sock_select(d, FD_READ|FD_CLOSE, 0);
	return ctl_reply(UNIXDOM_REP_OK, NULL, 0, rbuf, rbuflen);
    }

    Debug("XXXYYYZZZ ctl_accept: FALL THROUGH!\r\n");
    return -1;
}

PRIVATESTUFF int
ctl_getopts(descriptor *d, char *buf, int buflen, char **rbuf, int rbuflen)
{
    char	*dst, *dststart;
    int		dstlen = buflen*9 + 1; /* XXX magic number 9 here, max len of reply */
    char	cmd;

    Debug("XXX: ctl_getopts: top\r\n");
    if (dstlen > MAX_BUFSIZ)
	return ctl_error(EINVAL, rbuf, rbuflen);
    if (dstlen > rbuflen) {
	if ((dst = ALLOC(dstlen)) == NULL)
	    return ctl_error(ENOMEM, rbuf, rbuflen);
	*rbuf = dst;
    } else
	dst = *rbuf;
    dststart = dst;
    *dst++ = UNIXDOM_REP_OK;
    while (buflen--) {
	cmd = *buf++;
	switch (cmd) {
	case UNIXDOM_OPT_ENDOFLIST:
	    Debug("XXX: ctl_getopts: endoflist\r\n");
	    *dst++ = cmd;
	    break;
	case UNIXDOM_OPT_IGNORE:
	    Debug("XXX: ctl_getopts: ignore\r\n");
	    *dst++ = cmd;
	    break;
	case UNIXDOM_OPT_ACTIVE:
	    Debug("XXX: ctl_getopts: active\r\n");
	    *dst++ = cmd;
	    *dst++ = (d->active);
	    break;
	case UNIXDOM_OPT_BACKLOG:
	    Debug("XXX: ctl_getopts: backlog\r\n");
	    *dst++ = cmd;
	    put_int32(d->backlog, dst);
	    dst += 4;
	    break;
	default:
	    Debug("XXXYYYZZZ: getopts: unknown cmd: %d\r\n", cmd);
	    break;
	}
    }
    /* dest already has UNIXDOM_OPT_ENDOFLIST, if input was well-formed */
    return dst - dststart;
}

/*
** Given a accepting port, restore the state on the listener port
*/
PRIVATESTUFF void
clear_listener(descriptor *a_desc)
{
    int i;
    int ix = a_desc->ix;
    descriptor* l_desc;

    assert(a_desc->state == STATE_ACCEPTING);

    for (i = 0; i < desc_size; i++) {
	descriptor* inp = desc_table[i];
	if ((inp != NULL) && (inp->stype == SOCK_STREAM)) {
	    l_desc = (descriptor*) inp;
	    if (l_desc->i_ix == ix) {
		if (l_desc->state == STATE_LISTENING) {
		    l_desc->state = STATE_LISTEN;
		    sock_select(l_desc, FD_ACCEPT, 0);
		}
		l_desc->i_ix = -1;
		return;
	    }
	}
    }
}

PRIVATESTUFF int
Debug(char *fmt, ...)
{
    va_list	ap;
    int		ret;

    if (_verbose_driver) {
	va_start(ap, fmt);
	ret = vfprintf(stderr, fmt, ap);
	va_end(ap);
	return ret;
    } else
	return 0;
}
