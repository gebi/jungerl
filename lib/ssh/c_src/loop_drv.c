/*
 *  Loop driver (emulate the inet_drv!)
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <ctype.h>
#include <sys/types.h>
#include <errno.h>

/* use http processing */
#define USE_HTTP 
/* All platforms fail on malloc errors. */
#define FATAL_MALLOC

#include "erl_driver.h"

#include <sys/time.h>
#ifdef NETDB_H_NEEDS_IN_H
#include <netinet/in.h>
#endif
#include <netdb.h>

#include <sys/socket.h>
#include <netinet/in.h>

#ifdef DEF_INADDR_LOOPBACK_IN_RPC_TYPES_H
#include <rpc/types.h>
#endif

#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <sys/param.h>
#ifdef HAVE_ARPA_NAMESER_H
#include <arpa/nameser.h>
#endif

#ifdef HAVE_SYS_SOCKIO_H
#include <sys/sockio.h>
#endif

#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif

/* #define INET_DRV_DEBUG 1 */
#ifdef INET_DRV_DEBUG
#define DEBUG 1
#undef DEBUGF
#define DEBUGF(X) printf X
#else
#define DEBUGF(X)
#endif

#ifdef DEBUG
#  define ASSERT(e) \
  if (e) { \
     ; \
  } else { \
     erl_assert_error(#e, __FILE__, __LINE__); \
  }
#  define ASSERT_EXPR(e) \
    ((void) ((e) ? 1 : (erl_assert_error(#e, __FILE__, __LINE__), 0)))
void erl_assert_error(char* expr, char* file, int line);
#else
#  define ASSERT(e)
#  define ASSERT_EXPR(e) ((void) 1)
#endif

#define sys_memzero(mem, len) memset((mem), 0, (len))
#define sys_memcpy(dst, src, len) memcpy((dst), (src), (len))
#define sys_memmove(dst, src, len) memmove((dst), (src), (len))
#define sys_memcmp(src1, src2, len) memcmp((src1), (src2), (len))

#if !defined(__WIN32__) && !defined(HAVE_STRNCASECMP)
#define STRNCASECMP my_strncasecmp

static int my_strncasecmp(const char *s1, const char *s2, size_t n)
{
    int i;

    for (i=0;i<n-1 && s1[i] && s2[i] && toupper(s1[i]) == toupper(s2[i]);++i)
	;
    return (toupper(s1[i]) - toupper(s2[i]));
}
	
#else
#define  STRNCASECMP strncasecmp
#endif

/* Standard set of integer macros  .. */

#define get_int64(s) ((((unsigned char*) (s))[0] << 56) | \
                      (((unsigned char*) (s))[1] << 48) | \
                      (((unsigned char*) (s))[2] << 40) | \
                      (((unsigned char*) (s))[3] << 32) | \
                      (((unsigned char*) (s))[4] << 24) | \
                      (((unsigned char*) (s))[5] << 16) | \
                      (((unsigned char*) (s))[6] << 8)  | \
                      (((unsigned char*) (s))[7]))

#define put_int64(i, s) do {                                           \
                            Uint j = (i);                              \
                            ((char*)(s))[7] = (char)((j)&0xff), j>>=8; \
                            ((char*)(s))[6] = (char)((j)&0xff), j>>=8; \
                            ((char*)(s))[5] = (char)((j)&0xff), j>>=8; \
                            ((char*)(s))[4] = (char)((j)&0xff), j>>=8; \
                            ((char*)(s))[3] = (char)((j)&0xff), j>>=8; \
                            ((char*)(s))[2] = (char)((j)&0xff), j>>=8; \
                            ((char*)(s))[1] = (char)((j)&0xff), j>>=8; \
                            ((char*)(s))[0] = (char)((j)&0xff);        \
                        } while (0)

#define get_int32(s) ((((unsigned char*) (s))[0] << 24) | \
                      (((unsigned char*) (s))[1] << 16) | \
                      (((unsigned char*) (s))[2] << 8)  | \
                      (((unsigned char*) (s))[3]))

#define put_int32(i, s) {((char*)(s))[0] = (char)((i) >> 24) & 0xff; \
                         ((char*)(s))[1] = (char)((i) >> 16) & 0xff; \
                         ((char*)(s))[2] = (char)((i) >> 8)  & 0xff; \
                         ((char*)(s))[3] = (char)((i)        & 0xff);}

#define get_int16(s) ((((unsigned char*)  (s))[0] << 8) | \
                      (((unsigned char*)  (s))[1]))


#define put_int16(i, s) {((unsigned char*)(s))[0] = ((i) >> 8) & 0xff; \
                         ((unsigned char*)(s))[1] = (i)        & 0xff;}

#define get_int8(s) ((((unsigned char*)  (s))[0] ))


#define put_int8(i, s) { ((unsigned char*)(s))[0] = (i) & 0xff;}


#define get_int24(s) ((((unsigned char*) (s))[0] << 16) | \
                      (((unsigned char*) (s))[1] << 8)  | \
                      (((unsigned char*) (s))[2]))

#define get_little_int32(s) ((((unsigned char*) (s))[3] << 24) | \
			     (((unsigned char*) (s))[2] << 16)  | \
			     (((unsigned char*) (s))[1] << 8) | \
			     (((unsigned char*) (s))[0]))

#define INET_AF_INET        1
#define INET_AF_INET6       2

#define INET_TYPE_STREAM    1
#define INET_TYPE_DGRAM     2

#define INET_MODE_LIST      0
#define INET_MODE_BINARY    1

#define INET_DELIVER_PORT   0
#define INET_DELIVER_TERM   1

#define INET_PASSIVE        0
#define INET_ACTIVE         1
#define INET_ONCE           2  /* active once then passive */

#define INET_F_OPEN         0x0001
#define INET_F_BOUND        0x0002
#define INET_F_ACTIVE       0x0004
#define INET_F_LISTEN       0x0008
#define INET_F_CON          0x0010
#define INET_F_ACC          0x0020
#define INET_F_LST          0x0040
#define INET_F_BUSY         0x0080 

#define INET_STATE_CLOSED    0
#define INET_STATE_OPEN      (INET_F_OPEN)
#define INET_STATE_BOUND     (INET_STATE_OPEN | INET_F_BOUND)
#define INET_STATE_CONNECTED (INET_STATE_BOUND | INET_F_ACTIVE)

#define IS_OPEN(d) \
 (((d)->state & INET_F_OPEN) == INET_F_OPEN)

#define IS_BOUND(d) \
 (((d)->state & INET_F_BOUND) == INET_F_BOUND)

#define IS_CONNECTED(d) \
  (((d)->state & INET_STATE_CONNECTED) == INET_STATE_CONNECTED)

#define IS_CONNECTING(d) \
  (((d)->state & INET_F_CON) == INET_F_CON)

#define IS_BUSY(d) \
  (((d)->state & INET_F_BUSY) == INET_F_BUSY)

#define INET_REQ_OPEN          1
#define INET_REQ_CLOSE         2
#define INET_REQ_CONNECT       3
#define INET_REQ_PEER          4
#define INET_REQ_NAME          5
#define INET_REQ_BIND          6
#define INET_REQ_SETOPTS       7
#define INET_REQ_GETOPTS       8
#define INET_REQ_GETIX         9
/* #define INET_REQ_GETIF         10 REPLACE BY NEW STUFF */
#define INET_REQ_GETSTAT       11
#define INET_REQ_GETHOSTNAME   12
#define INET_REQ_FDOPEN        13
#define INET_REQ_GETFD         14
#define INET_REQ_GETTYPE       15
#define INET_REQ_GETSTATUS     16
#define INET_REQ_GETSERVBYNAME 17
#define INET_REQ_GETSERVBYPORT 18
#define INET_REQ_SETNAME       19
#define INET_REQ_SETPEER       20
#define INET_REQ_GETIFLIST     21
#define INET_REQ_IFGET         22
#define INET_REQ_IFSET         23
#define INET_REQ_SUBSCRIBE     24

#define INET_SUBS_EMPTY_OUT_Q  1

#define INET_REP_ERROR       0
#define INET_REP_OK          1
#define INET_REP_DATA        100

#define INET_OPT_REUSEADDR  0   /* enable/disable local address reuse */
#define INET_OPT_KEEPALIVE  1   /* enable/disable keep connections alive */
#define INET_OPT_DONTROUTE  2   /* enable/disable routing for messages */
#define INET_OPT_LINGER     3   /* linger on close if data is present */
#define INET_OPT_BROADCAST  4   /* enable/disable transmission of broadcase */
#define INET_OPT_OOBINLINE  5   /* enable/disable out-of-band data in band */
#define INET_OPT_SNDBUF     6   /* set send buffer size */
#define INET_OPT_RCVBUF     7   /* set receive buffer size */
#define TCP_OPT_NODELAY     10  /* don't delay send to coalesce packets */
#define UDP_OPT_MULTICAST_IF 11  /* set/get IP multicast interface */
#define UDP_OPT_MULTICAST_TTL 12 /* set/get IP multicast timetolive */
#define UDP_OPT_MULTICAST_LOOP 13 /* set/get IP multicast loopback */
#define UDP_OPT_ADD_MEMBERSHIP 14 /* add an IP group membership */
#define UDP_OPT_DROP_MEMBERSHIP 15 /* drop an IP group membership */
/* LOPT is local options */
#define INET_LOPT_BUFFER      20  /* min buffer size hint */
#define INET_LOPT_HEADER      21  /* list header size */
#define INET_LOPT_ACTIVE      22  /* enable/disable active receive */
#define INET_LOPT_PACKET      23  /* packet header type (TCP) */
#define INET_LOPT_MODE        24  /* list or binary mode */
#define INET_LOPT_DELIVER     25  /* port or term delivery */
#define INET_LOPT_EXITONCLOSE 26  /* exit port on active close or not ! */
#define INET_LOPT_TCP_HIWTRMRK     27  /* set local high watermark */
#define INET_LOPT_TCP_LOWTRMRK     28  /* set local low watermark */
#define INET_LOPT_BIT8             29  /* set 8 bit detection */
#define INET_LOPT_TCP_SEND_TIMEOUT 30      /* set send timeout */

#define INET_IFOPT_ADDR       1
#define INET_IFOPT_BROADADDR  2
#define INET_IFOPT_DSTADDR    3
#define INET_IFOPT_MTU        4
#define INET_IFOPT_NETMASK    5
#define INET_IFOPT_FLAGS      6
#define INET_IFOPT_HWADDR     7

#define INET_BIT8_CLEAR 0
#define INET_BIT8_SET   1
#define INET_BIT8_ON    2
#define INET_BIT8_OFF   3

/* Enumerate the statistics ops */
#define INET_STAT_RECV_CNT   1
#define INET_STAT_RECV_MAX   2
#define INET_STAT_RECV_AVG   3
#define INET_STAT_RECV_DVI   4
#define INET_STAT_SEND_CNT   5
#define INET_STAT_SEND_MAX   6
#define INET_STAT_SEND_AVG   7
#define INET_STAT_SEND_PND   8
#define INET_STAT_RECV_OCT   9      /* received octets */ 
#define INET_STAT_SEND_OCT   10     /* sent octets */


#define INET_DEF_BUFFER     1024        /* default buffer size */
#define INET_MIN_BUFFER     1           /* internal min buffer */
#define INET_MAX_BUFFER     (1024*64)   /* internal max buffer */

/* Note: INET_HIGH_WATERMARK MUST be less than 2*INET_MAX_BUFFER */
#define INET_HIGH_WATERMARK (1024*8) /* 8k pending high => busy  */
/* Note: INET_LOW_WATERMARK MUST be less than INET_MAX_BUFFER and
** less than INET_HIGH_WATERMARK
*/
#define INET_LOW_WATERMARK  (1024*4) /* 4k pending => allow more */

#define INET_INFINITY  0xffffffff  /* infinity value */

#define INET_MAX_ASYNC 1           /* max number of async queueu ops */

/* INET_UDP_POLL could be an option !! */
#define INET_UDP_POLL   5        /* maximum number of packets to poll */

/* Max interface name */
#define INET_IFNAMSIZ          16

/* Erlang version of flags */
#define INET_IFF_UP            0x0001
#define INET_IFF_BROADCAST     0x0002
#define INET_IFF_LOOPBACK      0x0004
#define INET_IFF_POINTTOPOINT  0x0008
#define INET_IFF_RUNNING       0x0010
#define INET_IFF_MULTICAST     0x0020

/* Complement flags for turning them off */
#define INET_IFF_DOWN            0x0100
#define INET_IFF_NBROADCAST      0x0200
/* #define INET_IFF_NLOOPBACK    0x0400 */
#define INET_IFF_NPOINTTOPOINT   0x0800
/* #define INET_IFF_NRUNNING     0x1000 */
/* #define INET_IFF_NMULTICAST   0x2000 */


#define BIN_REALLOC_LIMIT(x)  (((x)*3)/4)  /* 75% */

/* The general purpose sockaddr */
typedef union {
    struct sockaddr sa;
    struct sockaddr_in sai;
#ifdef HAVE_IN6
    struct sockaddr_in6 sai6;
#endif
} inet_address;


/* for AF_INET & AF_INET6 */
#define inet_address_port(x) ((x)->sai.sin_port)

#if defined(HAVE_IN6) && defined(AF_INET6)
#define addrlen(family) \
   ((family == AF_INET) ? sizeof(struct in_addr) : \
    ((family == AF_INET6) ? sizeof(struct in6_addr) : 0))
#else
#define addrlen(family) \
   ((family == AF_INET) ? sizeof(struct in_addr) : 0)
#endif

typedef struct {
    int            id;      /* id used to identify reply */
    ErlDrvTermData caller;  /* recipient of async reply */
    int            req;     /* Request id (CONNECT/ACCEPT/RECV) */
    unsigned       timeout; /* Request timeout (since op issued,not started) */
} inet_async_op;

typedef struct subs_list_ {
  ErlDrvTermData subscriber;
  struct subs_list_ *next;
} subs_list;

#define NO_PROCESS 0
#define NO_SUBSCRIBERS(SLP) ((SLP)->subscriber == NO_PROCESS)
static void send_to_subscribers(ErlDrvPort, subs_list *, int,
				ErlDrvTermData [], int);
static void free_subscribers(subs_list*);
static int save_subscriber(subs_list *, ErlDrvTermData);

typedef struct {
    long  event_mask;           /* current FD events */
    ErlDrvPort  port;           /* the port identifier */
    ErlDrvTermData dport;       /* the port identifier as DriverTermData */
    int   state;                /* status */
    int   mode;                 /* BINARY | LIST
				   (affect how to interpret hsz) */
    int   exitf;                /* exit port on close or not */
    int   bit8f;                /* check if data has bit number 7 set */
    int   deliver;              /* Delivery mode, TERM or PORT */

    ErlDrvTermData caller;      /* recipient of sync reply */

    inet_async_op* oph;          /* queue head or NULL */
    inet_async_op* opt;          /* queue tail or NULL */
    inet_async_op  op_queue[INET_MAX_ASYNC];  /* call queue */

    int   active;               /* 0 = passive, 1 = active, 2 = active once */
    int   stype;                /* socket type SOCK_STREAM/SOCK_DGRAM */
    int   sfamily;              /* address family */
    int   htype;                /* header type (tcp only?) */
    int   bit8;                 /* set if bit8f==true and data some data
				   seen had the 7th bit set */
    inet_address remote;        /* remote address for connected sockets */
    inet_address peer_addr;     /* fake peer address */
    inet_address name_addr;     /* fake local address */

    inet_address* peer_ptr;    /* fake peername or NULL */
    inet_address* name_ptr;    /* fake sockname or NULL */

    int   bufsz;                /* minimum buffer constraint */
    unsigned int hsz;           /* the list header size, -1 is large !!! */
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

    subs_list empty_out_q_subs; /* Empty out queue subscribers */
} inet_descriptor;

#define INETP(x) ((inet_descriptor*) (x))


#define TCP_REQ_ACCEPT    30
#define TCP_REQ_LISTEN    31
#define TCP_REQ_RECV      32
#define TCP_REQ_UNRECV    33
#define TCP_REQ_SHUTDOWN  34


#define TCP_STATE_CLOSED     INET_STATE_CLOSED
#define TCP_STATE_OPEN       (INET_F_OPEN)
#define TCP_STATE_BOUND      (TCP_STATE_OPEN | INET_F_BOUND)
#define TCP_STATE_CONNECTED  (TCP_STATE_BOUND | INET_F_ACTIVE)
#define TCP_STATE_LISTEN     (TCP_STATE_BOUND | INET_F_LISTEN)
#define TCP_STATE_LISTENING  (TCP_STATE_LISTEN | INET_F_LST)
#define TCP_STATE_CONNECTING (TCP_STATE_BOUND | INET_F_CON)
#define TCP_STATE_ACCEPTING  (INET_F_ACC)

#define TCP_PB_RAW     0
#define TCP_PB_1       1
#define TCP_PB_2       2
#define TCP_PB_4       3
#define TCP_PB_ASN1    4
#define TCP_PB_RM      5
#define TCP_PB_CDR     6
#define TCP_PB_FCGI    7
#define TCP_PB_LINE_LF 8
#define TCP_PB_TPKT    9
#define TCP_PB_HTTP    10
#define TCP_PB_HTTPH   11


#define TCP_MAX_PACKET_SIZE 0x1000000  /* 16 M */

#define MAX_VSIZE 16		/* Max number of entries allowed in an I/O
				 * vector sock_sendv().
				 */

static int loop_drv_init(void);
static void loop_drv_stop(ErlDrvData);
static void loop_drv_command(ErlDrvData, char*, int);
static void loop_drv_commandv(ErlDrvData, ErlIOVec*);
static void loop_drv_input(ErlDrvData, ErlDrvEvent);
static void loop_drv_output(ErlDrvData data, ErlDrvEvent event);
static ErlDrvData loop_drv_start(ErlDrvPort, char* command);
static int loop_drv_ctl(ErlDrvData, unsigned int, char*, int, char**, int);
static void  loop_drv_timeout(ErlDrvData);


static ErlDrvEntry loop_driver_entry = 
{
    loop_drv_init,
    loop_drv_start, 
    loop_drv_stop, 
    loop_drv_command,
    loop_drv_input,
    loop_drv_output,
    "loop_drv",
    NULL,
    NULL,
    loop_drv_ctl,
    loop_drv_timeout,
    loop_drv_commandv
};


typedef struct {
    inet_descriptor inet;       /* common data structure (DON'T MOVE) */
    int   high;                 /* high watermark */
    int   low;                  /* low watermark */
    int   send_timeout;         /* timeout to use in send */
    int   busy_on_send;         /* busy on send with timeout! */
    int   i_bufsz;              /* current input buffer size (<= bufsz) */
    ErlDrvBinary* i_buf;        /* current binary buffer */
    char*         i_ptr;        /* current pos in buf */
    char*         i_ptr_start;  /* packet start pos in buf */
    int           i_remain;     /* remaining chars to read */
#ifdef USE_HTTP
    int           http_state;   /* 0 = response|request  1=headers fields */
#endif
} loop_descriptor;

/* send function */
static int loop_send(loop_descriptor* desc, char* ptr, int len);
static int loop_sendv(loop_descriptor* desc, ErlIOVec* ev);
static int loop_recv(loop_descriptor* desc, int request_len);
static int loop_deliver(loop_descriptor* desc, int len);

static int async_ref = 0;          /* async reference id generator */
#define NEW_ASYNC_ID() ((async_ref++) & 0xffff)

static ErlDrvTermData am_ok;
static ErlDrvTermData am_tcp;
static ErlDrvTermData am_error;
static ErlDrvTermData am_inet_async;
static ErlDrvTermData am_inet_reply;
static ErlDrvTermData am_timeout;
static ErlDrvTermData am_closed;
static ErlDrvTermData am_tcp_closed;
static ErlDrvTermData am_tcp_error;
static ErlDrvTermData am_empty_out_q;

/* speical errors for bad ports and sequences */
#define EXBADPORT "exbadport"
#define EXBADSEQ  "exbadseq"

static int ctl_reply(int, char*, int, char**, int);

/*
 * Malloc wrapper,
 * we would like to change the behaviour for different 
 * systems here.
 */

#define ALLOC(X) driver_alloc((X))
#define REALLOC(X,Y) driver_realloc((X), (Y))
#define FREE(P) driver_free((P))


#define LOAD_ATOM(vec, i, atom) \
  (((vec)[(i)] = ERL_DRV_ATOM), \
  ((vec)[(i)+1] = (atom)), \
  (i+2))

#define LOAD_INT(vec, i, val) \
  (((vec)[(i)] = ERL_DRV_INT), \
  ((vec)[(i)+1] = (ErlDrvTermData)(val)), \
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
  ((vec)[(i)+1] = (ErlDrvTermData)(bin)), \
  ((vec)[(i)+2] = (len)), \
  ((vec)[(i)+3] = (offs)), \
  (i+4))

#define LOAD_STRING(vec, i, str, len) \
  (((vec)[(i)] = ERL_DRV_STRING), \
  ((vec)[(i)+1] = (ErlDrvTermData)(str)), \
  ((vec)[(i)+2] = (len)), \
  (i+3))

#define LOAD_STRING_CONS(vec, i, str, len) \
  (((vec)[(i)] = ERL_DRV_STRING_CONS), \
  ((vec)[(i)+1] = (ErlDrvTermData)(str)), \
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


static int load_ip_port(ErlDrvTermData* spec, int i, char* buf)
{
    spec[i++] = ERL_DRV_INT;
    spec[i++] = (ErlDrvTermData) get_int16(buf);
    return i;
}

static int load_ip_address(ErlDrvTermData* spec, int i, int family, char* buf)
{
    if (family == AF_INET) {
	spec[i++] = ERL_DRV_INT;
	spec[i++] = (ErlDrvTermData) ((unsigned char)buf[0]);
	spec[i++] = ERL_DRV_INT;
	spec[i++] = (ErlDrvTermData) ((unsigned char)buf[1]);
	spec[i++] = ERL_DRV_INT;
	spec[i++] = (ErlDrvTermData) ((unsigned char)buf[2]);
	spec[i++] = ERL_DRV_INT;
	spec[i++] = (ErlDrvTermData) ((unsigned char)buf[3]);
	spec[i++] = ERL_DRV_TUPLE;
	spec[i++] = 4;
    }
#if defined(HAVE_IN6) && defined(AF_INET6)
    else if (family == AF_INET6) {
	spec[i++] = ERL_DRV_INT;
	spec[i++] = (ErlDrvTermData) get_int16(buf);
	spec[i++] = ERL_DRV_INT;
	spec[i++] = (ErlDrvTermData) get_int16(buf+2);
	spec[i++] = ERL_DRV_INT;
	spec[i++] = (ErlDrvTermData) get_int16(buf+4);
	spec[i++] = ERL_DRV_INT;
	spec[i++] = (ErlDrvTermData) get_int16(buf+6);
	spec[i++] = ERL_DRV_INT;
	spec[i++] = (ErlDrvTermData) get_int16(buf+8);
	spec[i++] = ERL_DRV_INT;
	spec[i++] = (ErlDrvTermData) get_int16(buf+10);
	spec[i++] = ERL_DRV_INT;
	spec[i++] = (ErlDrvTermData) get_int16(buf+12);
	spec[i++] = ERL_DRV_INT;
	spec[i++] = (ErlDrvTermData) get_int16(buf+14);
	spec[i++] = ERL_DRV_TUPLE;
	spec[i++] = 8;
    }
#endif
    else {
	spec[i++] = ERL_DRV_TUPLE;
	spec[i++] = 0;
    }
    return i;
}


/*
** Binary Buffer Managment
** We keep a stack of usable buffers 
*/
#define BUFFER_STACK_SIZE 16

static ErlDrvBinary* buffer_stack[BUFFER_STACK_SIZE];
static int buffer_stack_pos = 0;

#ifdef DEBUG
static int tot_buf_allocated = 0;  /* memory in use for i_buf */
static int tot_buf_stacked = 0;   /* memory on stack */
static int max_buf_allocated = 0; /* max allocated */

#define COUNT_BUF_ALLOC(sz) do { \
  tot_buf_allocated += (sz); \
  if (tot_buf_allocated > max_buf_allocated) \
    max_buf_allocated = tot_buf_allocated; \
} while(0)

#define COUNT_BUF_FREE(sz) do { tot_buf_allocated -= (sz); } while(0)

#define COUNT_BUF_STACK(sz) do { tot_buf_stacked += (sz); } while(0)

#else

#define COUNT_BUF_ALLOC(sz)
#define COUNT_BUF_FREE(sz)
#define COUNT_BUF_STACK(sz)

#endif

static ErlDrvBinary* alloc_buffer(long minsz)
{
    ErlDrvBinary* buf = NULL;

    DEBUGF(("alloc_buffer: sz = %d, tot = %d, max = %d\r\n", 
	    minsz, tot_buf_allocated, max_buf_allocated));

    if (buffer_stack_pos > 0) {
	int origsz;

	buf = buffer_stack[--buffer_stack_pos];
	origsz = buf->orig_size;
	COUNT_BUF_STACK(-origsz);
	if (origsz < minsz) {
	    if ((buf = driver_realloc_binary(buf, minsz)) == NULL)
		return NULL;
	    COUNT_BUF_ALLOC(buf->orig_size - origsz);
	}
    }
    else {
	if ((buf = driver_alloc_binary(minsz)) == NULL)
	    return NULL;
	COUNT_BUF_ALLOC(buf->orig_size);
    }
    return buf;
}

/*
** Max buffer memory "cached" BUFFER_STACK_SIZE * INET_MAX_BUFFER
** (16 * 64k ~ 1M)
*/
/*#define CHECK_DOUBLE_RELEASE 1*/
static void release_buffer(ErlDrvBinary* buf)
{
    DEBUGF(("release_buffer: %ld\r\n", (buf==NULL) ? 0 : buf->orig_size));
    if (buf == NULL)
	return;
    if ((buf->orig_size > INET_MAX_BUFFER) || 
	(buffer_stack_pos >= BUFFER_STACK_SIZE)) {
	COUNT_BUF_FREE(buf->orig_size);
	driver_free_binary(buf);
    }
    else {
#ifdef CHECK_DOUBLE_RELEASE
#warning CHECK_DOUBLE_RELEASE is enabled, this is a custom build emulator
	int i;
	for (i = 0; i < buffer_stack_pos; ++i) {
	    if (buffer_stack[i] == buf) {
		erl_exit(1,"Multiple buffer release in inet_drv, this is a "
			 "bug, save the core and send it to "
			 "support@erlang.ericsson.se!");
	    }
	}
#endif
	buffer_stack[buffer_stack_pos++] = buf;
	COUNT_BUF_STACK(buf->orig_size);
    }
}

static ErlDrvBinary* realloc_buffer(ErlDrvBinary* buf, long newsz)
{
    ErlDrvBinary* bin;
#ifdef DEBUG
    long orig_size =  buf->orig_size;
#endif

    if ((bin = driver_realloc_binary(buf,newsz)) != NULL) {
	COUNT_BUF_ALLOC(newsz - orig_size);
	;
    }
    return bin;
}

/* use a TRICK, access the refc field to see if any one else has
 * a ref to this buffer then call driver_free_binary else 
 * release_buffer instead
 */
static void free_buffer(ErlDrvBinary* buf)
{
    DEBUGF(("free_buffer: %ld\r\n", (buf==NULL) ? 0 : buf->orig_size));

    if (buf != NULL) {
	if (buf->refc == 1)
	    release_buffer(buf);
	else {
	    COUNT_BUF_FREE(buf->orig_size);
	    driver_free_binary(buf);
	}
    }
}

/* general control reply function */
static int ctl_reply(int rep, char* buf, int len, char** rbuf, int rsize)
{
    char* ptr;

    if ((len+1) > rsize) {
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
static int ctl_error(int err, char** rbuf, int rsize)
{
    char response[256];		/* Response buffer. */
    char* s;
    char* t;

    for (s = erl_errno_id(err), t = response; *s; s++, t++)
	*t = tolower(*s);
    return ctl_reply(INET_REP_ERROR, response, t-response, rbuf, rsize);
}

static int ctl_xerror(char* xerr, char** rbuf, int rsize)
{
    int n = strlen(xerr);
    return ctl_reply(INET_REP_ERROR, xerr, n, rbuf, rsize);
}


static ErlDrvTermData error_atom(int err)
{
    char errstr[256];
    char* s;
    char* t;

    for (s = erl_errno_id(err), t = errstr; *s; s++, t++)
	*t = tolower(*s);
    *t = '\0';
    return driver_mk_atom(errstr);
}

/* setup a new async id + caller (format async_id into buf) */
static int enq_async(inet_descriptor* desc, char* buf, int req)
{
    int id = NEW_ASYNC_ID();
    inet_async_op* opp;

    if ((opp = desc->oph) == NULL)            /* queue empty */
	opp = desc->oph = desc->opt = desc->op_queue;
    else if (desc->oph == desc->opt) { /* queue full */ 
	DEBUGF(("enq(%ld): queue full\r\n", (long)desc->port));
	return -1;
    }

    opp->id = id;
    opp->caller = driver_caller(desc->port);
    opp->req = req;

    DEBUGF(("enq(%ld): %d %ld %d\r\n", 
	    (long) desc->port, opp->id, opp->caller, opp->req));

    opp++;
    if (opp >= desc->op_queue + INET_MAX_ASYNC)
	desc->oph = desc->op_queue;
    else
	desc->oph = opp;

    if (buf != NULL)
	put_int16(id, buf);
    return 0;
}

static int deq_async(inet_descriptor* desc, int* ap, ErlDrvTermData* cp, int* rp)
{
    inet_async_op* opp;

    if ((opp = desc->opt) == NULL) {  /* queue empty */
	DEBUGF(("deq(%ld): queue empty\r\n", (long)desc->port));
	return -1;
    }
    *ap = opp->id;
    *cp = opp->caller;
    *rp = opp->req;
    
    DEBUGF(("deq(%ld): %d %ld %d\r\n", 
	    (long)desc->port, opp->id, opp->caller, opp->req));
    
    opp++;
    if (opp >= desc->op_queue + INET_MAX_ASYNC)
	desc->opt = desc->op_queue;
    else
	desc->opt = opp;

    if (desc->opt == desc->oph)
	desc->opt = desc->oph = NULL;
    return 0;
}


/* send message:
**     {inet_async, Port, Ref, ok} 
*/
static int 
send_async_ok(ErlDrvPort port, ErlDrvTermData sender, int aid, ErlDrvTermData recipient)
{
    ErlDrvTermData spec[10];
    int i = 0;

    i = LOAD_ATOM(spec, i, am_inet_async);
    i = LOAD_PORT(spec, i, sender);
    i = LOAD_INT(spec, i, aid);
    i = LOAD_ATOM(spec, i, am_ok);
    i = LOAD_TUPLE(spec, i, 4);

    ASSERT(i == 10);

    return driver_send_term(port, recipient, spec, i);
}

/* send message:
**      {inet_async, Port, Ref, {error,Reason}}
*/
static int
send_async_error(ErlDrvPort port, ErlDrvTermData Port, int Ref,
		 ErlDrvTermData recipient, ErlDrvTermData Reason)
{
    ErlDrvTermData spec[14];
    int i = 0;

    i = 0;
    i = LOAD_ATOM(spec, i, am_inet_async);
    i = LOAD_PORT(spec, i, Port);
    i = LOAD_INT(spec, i, Ref);
    {
	i = LOAD_ATOM(spec, i, am_error);
	i = LOAD_ATOM(spec, i, Reason);
	i = LOAD_TUPLE(spec, i, 2);
    }
    i = LOAD_TUPLE(spec, i, 4);
    ASSERT(i == 14);
    return driver_send_term(port, recipient, spec, i);
}


static int async_ok(inet_descriptor* desc)
{
    int req;
    int aid;
    ErlDrvTermData caller;

    if (deq_async(desc, &aid, &caller, &req) < 0)
	return -1;
    return send_async_ok(desc->port, desc->dport, aid, caller);
}


static int async_error_am(inet_descriptor* desc, ErlDrvTermData reason)
{
    int req;
    int aid;
    ErlDrvTermData caller;

    if (deq_async(desc, &aid, &caller, &req) < 0)
	return -1;
    return send_async_error(desc->port, desc->dport, aid, caller,
			    reason);
}

/* dequeue all operations */
static int async_error_am_all(inet_descriptor* desc, ErlDrvTermData reason)
{
    int req;
    int aid;
    ErlDrvTermData caller;

    while (deq_async(desc, &aid, &caller, &req) == 0) {
	send_async_error(desc->port, desc->dport, aid, caller,
			 reason);
    }
    return 0;
}


static int async_error(inet_descriptor* desc, int err)
{
    return async_error_am(desc, error_atom(err));
}

/* send:
**   {inet_reply, S, ok} 
*/

static int inet_reply_ok(inet_descriptor* desc)
{
    ErlDrvTermData spec[8];
    ErlDrvTermData caller = desc->caller;
    int i = 0;

    i = LOAD_ATOM(spec, i, am_inet_reply);
    i = LOAD_PORT(spec, i, desc->dport);
    i = LOAD_ATOM(spec, i, am_ok);
    i = LOAD_TUPLE(spec, i, 3);
    ASSERT(i == 8);

    desc->caller = 0;
    return driver_send_term(desc->port, caller, spec, i);    
}

/* send:
**   {inet_reply, S, {error, Reason}} 
*/
static int inet_reply_error_am(inet_descriptor* desc, ErlDrvTermData reason)
{
    ErlDrvTermData spec[12];
    ErlDrvTermData caller = desc->caller;
    int i = 0;

    i = LOAD_ATOM(spec, i, am_inet_reply);
    i = LOAD_PORT(spec, i, desc->dport);
    i = LOAD_ATOM(spec, i, am_error);
    i = LOAD_ATOM(spec, i, reason);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 3);
    ASSERT(i == 12);
    desc->caller = 0;

    return driver_send_term(desc->port, caller, spec, i);
}

/* send:
**   {inet_reply, S, {error, Reason}} 
*/
static int inet_reply_error(inet_descriptor* desc, int err)
{
    return inet_reply_error_am(desc, error_atom(err));
}

/* 
** Deliver port data from buffer 
*/
static int inet_port_data(inet_descriptor* desc, char* buf, int len)
{
    unsigned int hsz = desc->hsz;

    DEBUGF(("inet_port_data(%ld): len = %d\r\n", (long)desc->port, len));

    if ((desc->mode == INET_MODE_LIST) || (hsz > len))
	return driver_output2(desc->port, buf, len, NULL, 0);
    else if (hsz > 0)
	return driver_output2(desc->port, buf, hsz, buf+hsz, len-hsz);
    else
	return driver_output(desc->port, buf, len);
}

/* 
** Deliver port data from binary
*/
static int
inet_port_binary_data(inet_descriptor* desc, ErlDrvBinary* bin, int offs, int len)
{
    unsigned int hsz = desc->hsz;

    DEBUGF(("inet_port_binary_data(%ld): offs=%d, len = %d\r\n", 
	    (long)desc->port, offs, len));

    if ((desc->mode == INET_MODE_LIST) || (hsz > len)) 
	return driver_output2(desc->port, bin->orig_bytes+offs, len, NULL, 0);
    else 
	return driver_output_binary(desc->port, bin->orig_bytes+offs, hsz,
				    bin, offs+hsz, len-hsz);
}

#ifdef USE_HTTP

#define HTTP_HDR_HASH_SIZE  53
#define HTTP_METH_HASH_SIZE 13

static char tspecial[128];

static char* http_hdr_strings[] = {
  "Cache-Control",
  "Connection",
  "Date",
  "Pragma",
  "Transfer-Encoding",
  "Upgrade",
  "Via",
  "Accept",
  "Accept-Charset",
  "Accept-Encoding",
  "Accept-Language",
  "Authorization",
  "From",
  "Host",
  "If-Modified-Since",
  "If-Match",
  "If-None-Match",
  "If-Range",
  "If-Unmodified-Since",
  "Max-Forwards",
  "Proxy-Authorization",
  "Range",
  "Referer",
  "User-Agent",
  "Age",
  "Location",
  "Proxy-Authenticate",
  "Public",
  "Retry-After",
  "Server",
  "Vary",
  "Warning",
  "Www-Authenticate",
  "Allow",
  "Content-Base",
  "Content-Encoding",
  "Content-Language",
  "Content-Length",
  "Content-Location",
  "Content-Md5",
  "Content-Range",
  "Content-Type",
  "Etag",
  "Expires",
  "Last-Modified",
  "Accept-Ranges",
  "Set-Cookie",
  "Set-Cookie2",
  "X-Forwarded-For",
  "Cookie",
  "Keep-Alive",
  "Proxy-Connection",
    NULL
};


static char* http_meth_strings[] = {
  "OPTIONS",
  "GET",
  "HEAD",
  "POST",
  "PUT",
  "DELETE",
  "TRACE",
    NULL
};

typedef struct http_atom {
  struct http_atom* next;   /* next in bucket */
  unsigned long h;          /* stored hash value */
  char* name;
  int   len;
  int index;                /* index in table + bit-pos */
  ErlDrvTermData atom;      /* erlang atom rep */
} http_atom_t;

static http_atom_t http_hdr_table[sizeof(http_hdr_strings)/sizeof(char*)];
static http_atom_t http_meth_table[sizeof(http_meth_strings)/sizeof(char*)];

static http_atom_t* http_hdr_hash[HTTP_HDR_HASH_SIZE];
static http_atom_t* http_meth_hash[HTTP_METH_HASH_SIZE];

static ErlDrvTermData am_http_eoh;
static ErlDrvTermData am_http_header;
static ErlDrvTermData am_http_request;
static ErlDrvTermData am_http_response;
static ErlDrvTermData am_http_error;
static ErlDrvTermData am_abs_path;
static ErlDrvTermData am_absoluteURI;
static ErlDrvTermData am_star;
static ErlDrvTermData am_undefined;
static ErlDrvTermData am_http;
static ErlDrvTermData am_https;
static ErlDrvTermData am_scheme;



#define CRNL(ptr) (((ptr)[0] == '\r') && ((ptr)[1] == '\n'))
#define NL(ptr)   ((ptr)[0] == '\n')
#define SP(ptr)   (((ptr)[0] == ' ') || ((ptr)[0] == '\t'))
#define is_tspecial(x) ((((x) > 32) && ((x) < 128)) ? tspecial[(x)] : 1)

#define hash_update(h,c) do { \
    unsigned long __g; \
    (h) = ((h) << 4) + (c); \
    if ((__g = (h) & 0xf0000000)) { \
       (h) ^= (__g >> 24); \
       (h) ^= __g; \
    } \
 } while(0)

static void http_hash(char* name, http_atom_t* entry,
		      http_atom_t** hash, int hsize)
{
  unsigned long h = 0;
  unsigned char* ptr = (unsigned char*) name;
  int ix;
  int len = 0;

  while(*ptr != '\0') {
    hash_update(h, *ptr);
    ptr++;
    len++;
  }
  ix = h % hsize;

  entry->next = hash[ix];
  entry->h    = h;
  entry->name = name;
  entry->len  = len;
  entry->atom = driver_mk_atom(name);
    
  hash[ix] = entry;
}

static http_atom_t* http_hash_lookup(unsigned char* name, int len,
				     unsigned long h,
				     http_atom_t** hash, int hsize)
{
  int ix = h % hsize;
  http_atom_t* ap = hash[ix];

  while (ap != NULL) {
    if ((ap->h == h) && (ap->len == len) && 
	(strncmp(ap->name, name, len) == 0))
      return ap;
    ap = ap->next;
  }
  return NULL;
}
     


static int http_init(void)
{
  int i;
  unsigned char* ptr;

  for (i = 0; i < 33; i++)
    tspecial[i] = 1;
  for (i = 33; i < 127; i++)
    tspecial[i] = 0;
  for (ptr = "()<>@,;:\\\"/[]?={} \t"; *ptr != '\0'; ptr++)
    tspecial[*ptr] = 1;

  am_http_eoh      = driver_mk_atom("http_eoh");
  am_http_header   = driver_mk_atom("http_header");
  am_http_request  = driver_mk_atom("http_request");
  am_http_response = driver_mk_atom("http_response");
  am_http_error    = driver_mk_atom("http_error");
  am_star          = driver_mk_atom("*");
  am_undefined     = driver_mk_atom("undefined");
  am_abs_path      = driver_mk_atom("abs_path");
  am_absoluteURI   = driver_mk_atom("absoluteURI");
  am_http          = driver_mk_atom("http");
  am_https         = driver_mk_atom("https");
  am_scheme        = driver_mk_atom("scheme");

  for (i = 0; i < HTTP_HDR_HASH_SIZE; i++)
    http_hdr_hash[i] = NULL;
  for (i = 0; http_hdr_strings[i] != NULL; i++) {
    http_hdr_table[i].index = i;
    http_hash(http_hdr_strings[i], 
	      &http_hdr_table[i], 
	      http_hdr_hash, HTTP_HDR_HASH_SIZE);
  }

  for (i = 0; i < HTTP_METH_HASH_SIZE; i++)
    http_meth_hash[i] = NULL;
  for (i = 0; http_meth_strings[i] != NULL; i++) {
    http_meth_table[i].index = i;
    http_hash(http_meth_strings[i],
	      &http_meth_table[i], 
	      http_meth_hash, HTTP_METH_HASH_SIZE);
  }
  return 0;
}

static int
http_response_message(loop_descriptor* desc, int major, int minor, int status,
		      char* phrase, int phrase_len)
{
  int i = 0;
  ErlDrvTermData spec[27];

  if (desc->inet.active == INET_PASSIVE) {
    /* {inet_async,S,Ref,{ok,{http_response,Version,Status,Phrase}}} */
    int req;
    int aid;
    ErlDrvTermData caller;

    if (deq_async(INETP(desc), &aid, &caller, &req) < 0)
      return -1;
    i = LOAD_ATOM(spec, i,  am_inet_async);
    i = LOAD_PORT(spec, i,  desc->inet.dport);
    i = LOAD_INT(spec, i,   aid);
    i = LOAD_ATOM(spec, i,  am_ok);
    i = LOAD_ATOM(spec, i,  am_http_response);
    i = LOAD_INT(spec, i, major);
    i = LOAD_INT(spec, i, minor);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_INT(spec, i, status);
    i = LOAD_STRING(spec, i, phrase, phrase_len);
    i = LOAD_TUPLE(spec, i, 4);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 4);
    return driver_send_term(desc->inet.port, caller, spec, i);
  }
  else {
    /* {http_response, S, Version, Status, Phrase} */
    i = LOAD_ATOM(spec, i,  am_http_response);
    i = LOAD_PORT(spec, i,  desc->inet.dport);
    i = LOAD_INT(spec, i, major);
    i = LOAD_INT(spec, i, minor);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_INT(spec, i, status);
    i = LOAD_STRING(spec, i, phrase, phrase_len);
    i = LOAD_TUPLE(spec, i, 5);
    return driver_output_term(desc->inet.port, spec, i);
  }
}

/*
** Handle URI syntax:
**
**  Request-URI    = "*" | absoluteURI | abs_path
**  absoluteURI    = scheme ":" *( uchar | reserved )
**  net_path       = "//" net_loc [ abs_path ]
**  abs_path       = "/" rel_path
**  rel_path       = [ path ] [ ";" params ] [ "?" query ]
**  path           = fsegment *( "/" segment )
**  fsegment       = 1*pchar
**  segment        = *pchar
**  params         = param *( ";" param )
**  param          = *( pchar | "/" )
**  query          = *( uchar | reserved )
**
**  http_URL       = "http:" "//" host [ ":" port ] [ abs_path ]
**
**  host           = <A legal Internet host domain name
**                   or IP address (in dotted-decimal form),
**                   as defined by Section 2.1 of RFC 1123>
**  port           = *DIGIT
**
**  {absoluteURI, <scheme>, <host>, <port>, <path+params+query>}
**       when <scheme> = http | https
**  {scheme, <scheme>, <chars>}
**       wheb <scheme> is something else then http or https
**  {abs_path,  <path>}
**
**  <string>  (unknown form)
**
*/

/* host [ ":" port ] [ abs_path ] */
static int
http_load_absoluteURI(ErlDrvTermData* spec, int i, ErlDrvTermData scheme,
		      char* uri_ptr, int uri_len)
{
  char* p;
  char* abs_path_ptr;
  int   abs_path_len;

  if ((p = memchr(uri_ptr, '/', uri_len)) == NULL) {
    /* host [":" port] */
    abs_path_ptr = "/";
    abs_path_len = 1;
  }
  else {
    int n = (p - uri_ptr);

    abs_path_ptr = p;
    abs_path_len = uri_len - n;
    uri_len = n;
  }
  i = LOAD_ATOM(spec, i, am_absoluteURI);
  i = LOAD_ATOM(spec, i, scheme);

  /* host[:port]  */
  if ((p = memchr(uri_ptr, ':', uri_len)) == NULL) {
    i = LOAD_STRING(spec, i, uri_ptr, uri_len);
    i = LOAD_ATOM(spec, i, am_undefined);
  }
  else {
    int n = (p - uri_ptr);
    int port = 0;

    i = LOAD_STRING(spec, i, uri_ptr, n);
    n = uri_len - (n+1);
    p++;
    while(n && isdigit((int) *p)) {
      port = port*10 + (*p - '0');
      n--;
      p++;
    }
    if ((n != 0) || (port == 0))
      i = LOAD_ATOM(spec, i, am_undefined);
    else
      i = LOAD_INT(spec, i, port);
  }
  i = LOAD_STRING(spec, i, abs_path_ptr, abs_path_len);
  i = LOAD_TUPLE(spec, i, 5);
  return i;
}

static int http_load_uri(ErlDrvTermData* spec, int i, char* uri_ptr, int uri_len)
{
  if ((uri_len == 1) && (uri_ptr[0] == '*'))
    i = LOAD_ATOM(spec, i, am_star);
  else if ((uri_len <= 1) || (uri_ptr[0] == '/')) {
    i = LOAD_ATOM(spec, i, am_abs_path);
    i = LOAD_STRING(spec, i, uri_ptr, uri_len);
    i = LOAD_TUPLE(spec, i, 2);
  }
  else {
    if ((uri_len>=7) && (STRNCASECMP(uri_ptr, "http://", 7) == 0)) {
      uri_len -= 7;
      uri_ptr += 7;
      return http_load_absoluteURI(spec, i, am_http, uri_ptr, uri_len);
    }
    else if ((uri_len>=8) && (STRNCASECMP(uri_ptr, "https://", 8) == 0)) {
      uri_len -= 8;
      uri_ptr += 8;    
      return http_load_absoluteURI(spec, i, am_https, uri_ptr,uri_len);
    }
    else {
      char* ptr;
      if ((ptr = memchr(uri_ptr, ':', uri_len)) == NULL)
	i = LOAD_STRING(spec, i, uri_ptr, uri_len);
      else {
	int slen = ptr - uri_ptr;
	i = LOAD_ATOM(spec, i, am_scheme);
	i = LOAD_STRING(spec, i, uri_ptr, slen);
	i = LOAD_STRING(spec, i, uri_ptr+(slen+1), uri_len-(slen+1));
	i = LOAD_TUPLE(spec, i, 3);
      }
    }
  }
  return i;
}

static int
http_request_message(loop_descriptor* desc, http_atom_t* meth, char* meth_ptr,
		     int meth_len, char* uri_ptr, int uri_len,
		     int major, int minor)
{
  int i = 0;
  ErlDrvTermData spec[43];

  if (desc->inet.active == INET_PASSIVE) {
    /* {inet_async, S, Ref, {ok,{http_request,Meth,Uri,Version}}} */
    int req;
    int aid;
    ErlDrvTermData caller;

    if (deq_async(INETP(desc), &aid, &caller, &req) < 0)
      return -1;
    i = LOAD_ATOM(spec, i,  am_inet_async);
    i = LOAD_PORT(spec, i,  desc->inet.dport);
    i = LOAD_INT(spec, i,   aid);
    i = LOAD_ATOM(spec, i,  am_ok);
    i = LOAD_ATOM(spec, i,  am_http_request);
    if (meth != NULL)
      i = LOAD_ATOM(spec, i, meth->atom);
    else
      i = LOAD_STRING(spec, i, meth_ptr, meth_len);
    i = http_load_uri(spec, i, uri_ptr, uri_len);
    i = LOAD_INT(spec, i, major);
    i = LOAD_INT(spec, i, minor);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 4);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 4);
    ASSERT(i <= 43);
    return driver_send_term(desc->inet.port, caller, spec, i);
  }
  else {
    /* {http_request, S, Meth, Uri, Version} */
    i = LOAD_ATOM(spec, i,  am_http_request);
    i = LOAD_PORT(spec, i,  desc->inet.dport);
    if (meth != NULL)
      i = LOAD_ATOM(spec, i, meth->atom);
    else
      i = LOAD_STRING(spec, i, meth_ptr, meth_len);
    i = http_load_uri(spec, i, uri_ptr, uri_len);
    i = LOAD_INT(spec, i, major);
    i = LOAD_INT(spec, i, minor);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 5);
    ASSERT(i <= 43);
    return driver_output_term(desc->inet.port, spec, i);
  }
}

static int
http_header_message(loop_descriptor* desc, http_atom_t* name, char* name_ptr,
		    int name_len, char* value_ptr, int value_len)
{
  int i = 0;
  ErlDrvTermData spec[26];

  if (desc->inet.active == INET_PASSIVE) {
    /* {inet_async,S,Ref,{ok,{http_header,Bit,Name,IValue,Value}} */
    int req;
    int aid;
    ErlDrvTermData caller;

    if (deq_async(INETP(desc), &aid, &caller, &req) < 0)
      return -1;
    i = LOAD_ATOM(spec, i,  am_inet_async);
    i = LOAD_PORT(spec, i,  desc->inet.dport);
    i = LOAD_INT(spec, i,   aid);
    i = LOAD_ATOM(spec, i,  am_ok);
    i = LOAD_ATOM(spec, i,  am_http_header);
    if (name != NULL) {
      i = LOAD_INT(spec, i,  name->index+1);
      i = LOAD_ATOM(spec, i, name->atom);
    }
    else {
      i = LOAD_INT(spec, i,  0);
      i = LOAD_STRING(spec, i, name_ptr, name_len);
    }
    i = LOAD_ATOM(spec, i, am_undefined);
    i = LOAD_STRING(spec, i, value_ptr, value_len);
    i = LOAD_TUPLE(spec, i, 5);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 4);
    ASSERT(i <= 26);
    return driver_send_term(desc->inet.port, caller, spec, i);
  }
  else {
    /* {http_header,S,Bit,Name,Code,Value} */
    i = LOAD_ATOM(spec, i,  am_http_header);
    i = LOAD_PORT(spec, i,  desc->inet.dport);
    if (name != NULL) {
      i = LOAD_INT(spec, i,  name->index+1);
      i = LOAD_ATOM(spec, i, name->atom);
    }
    else {
      i = LOAD_INT(spec, i,  0);
      i = LOAD_STRING(spec, i, name_ptr, name_len);
    }
    i = LOAD_ATOM(spec, i, am_undefined);
    i = LOAD_STRING(spec, i, value_ptr, value_len);
    i = LOAD_TUPLE(spec, i, 6);
    ASSERT(i <= 26);
    return driver_output_term(desc->inet.port, spec, i);
  }
}

static int http_eoh_message(loop_descriptor* desc)
{
  int i = 0;
  ErlDrvTermData spec[14];

  if (desc->inet.active == INET_PASSIVE) {
    /* {inet_async,S,Ref,{ok,http_eoh}} */
    int req;
    int aid;
    ErlDrvTermData caller;

    if (deq_async(INETP(desc), &aid, &caller, &req) < 0)
      return -1;
    i = LOAD_ATOM(spec, i,  am_inet_async);
    i = LOAD_PORT(spec, i,  desc->inet.dport);
    i = LOAD_INT(spec, i,   aid);
    i = LOAD_ATOM(spec, i,  am_ok);
    i = LOAD_ATOM(spec, i,  am_http_eoh);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 4);
    ASSERT(i <= 14);
    return driver_send_term(desc->inet.port, caller, spec, i);
  }
  else {
    /* {http_eoh,S} */
    i = LOAD_ATOM(spec, i,  am_http_eoh);
    i = LOAD_PORT(spec, i,  desc->inet.dport);
    i = LOAD_TUPLE(spec, i, 2);
    ASSERT(i <= 14);
    return driver_output_term(desc->inet.port, spec, i);
  }
}

static int http_error_message(loop_descriptor* desc, char* buf, int len)
{
  int i = 0;
  ErlDrvTermData spec[19];

  if (desc->inet.active == INET_PASSIVE) {
    /* {inet_async,S,Ref,{error,{http_error,Line}}} */
    int req;
    int aid;
    ErlDrvTermData caller;

    if (deq_async(INETP(desc), &aid, &caller, &req) < 0)
      return -1;
    i = LOAD_ATOM(spec, i,  am_inet_async);
    i = LOAD_PORT(spec, i,  desc->inet.dport);
    i = LOAD_INT(spec, i,   aid);
    i = LOAD_ATOM(spec, i,  am_error);
    i = LOAD_ATOM(spec, i,  am_http_error);
    i = LOAD_STRING(spec, i, buf, len);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 4);
    ASSERT(i <= 19);
    return driver_send_term(desc->inet.port, caller, spec, i);
  }
  else {
    /* {http_error,S,Line} */
    i = LOAD_ATOM(spec, i,  am_http_error);
    i = LOAD_PORT(spec, i,  desc->inet.dport);
    i = LOAD_STRING(spec, i, buf, len);
    i = LOAD_TUPLE(spec, i, 3);
    ASSERT(i <= 19);
    return driver_output_term(desc->inet.port, spec, i);
  }
}

/*
** load http message:
**  {http_eoh, S}                          - end of headers
**  {http_header,   S, Key, Value}         - Key = atom() | string()
**  {http_request,  S, Method,Url,Version}
**  {http_response, S, Version, Status, Message}
**  {http_error,    S, Error-Line}
*/
static int http_message(loop_descriptor* desc, char* buf, int len)
{
  char* ptr = buf;
  int n = len;

  /* remove trailing CRNL (accept NL as well) */
  if ((n >= 2) && (buf[n-2] == '\r'))
    n -= 2;
  else if ((n >= 1) && (buf[n-1] == '\n'))
    n -= 1;

  if (desc->http_state == 0) {
    unsigned long h;
    http_atom_t* meth;
    char* meth_ptr;
    int   meth_len;
    int c;
    /* start-line = Request-Line | Status-Line */
    if (n == 0)
      return 0;
    h = 0;
    meth_ptr = ptr;
    while (n && !is_tspecial((unsigned char)*ptr)) {
      c = *ptr;
      hash_update(h, c);
      ptr++;
      n--;
    }
    if ((meth_len = (ptr - meth_ptr)) == 0)
      return -1;
    meth = http_hash_lookup(meth_ptr, meth_len, h,
			    http_meth_hash, HTTP_METH_HASH_SIZE);
    if (n) {
      if ((*ptr == '/') && (strncmp(buf, "HTTP", 4) == 0)) {
	int major  = 0;
	int minor  = 0;
	int status = 0;
	/* Status-Line = HTTP-Version SP 
	 *              Status-Code SP Reason-Phrase 
	 *              CRNL
	 * HTTP-Version   = "HTTP" "/" 1*DIGIT "." 1*DIGIT
	 */
	ptr++;
	n--;
	if (!n || !isdigit((int) *ptr)) return -1;
	while(n && isdigit((int) *ptr)) {
	  major = 10*major + (*ptr - '0');
	  ptr++;
	  n--;
	}
	if (!n || (*ptr != '.'))
	  return -1;
	ptr++;
	n--;
	if (!n || !isdigit((int) *ptr)) return -1;
	while(n && isdigit((int) *ptr)) {
	  minor = 10*minor + (*ptr - '0');
	  ptr++;
	  n--;
	}
	if (!n || !SP(ptr))
	  return -1;

	while(n && SP(ptr)) { ptr++; n--; }

	while(n && isdigit((int) *ptr)) {
	  status = 10*status + (*ptr - '0');
	  ptr++;
	  n--;
	}
	if (!n || !SP(ptr))
	  return -1;

	while(n && SP(ptr)) { ptr++; n--; }

	/* NOTE: the syntax allows empty reason phrases */
	desc->http_state++;

	return http_response_message(desc, major, minor, status,
				     (char*)ptr, n);
      }
      else if (SP(ptr)) {
	/* Request-Line = Method SP Request-URI SP HTTP-Version CRLF */
	char* uri_ptr;
	int   uri_len;
	int major  = 0;
	int minor  = 0;
	
	while(n && SP(ptr)) { ptr++; n--; }
	uri_ptr = ptr;
	while(n && !SP(ptr)) { ptr++; n--; }
	if ((uri_len = (ptr - uri_ptr)) == 0)
	  return -1;
	while(n && SP(ptr)) { ptr++; n--; }
	if (n == 0) {
	  desc->http_state++;
	  return http_request_message(desc, meth,
				      meth_ptr, meth_len,
				      uri_ptr, uri_len,
				      0, 9);
	}
	if (n < 8)
	  return -1;
	if (strncmp(ptr, "HTTP/", 5) != 0)
	  return -1;
	ptr += 5;
	n   -= 5;

	if (!n || !isdigit((int) *ptr)) return -1;
	while(n && isdigit((int) *ptr)) {
	  major = 10*major + (*ptr - '0');
	  ptr++;
	  n--;
	}

	if (!n || (*ptr != '.'))
	  return -1;
	ptr++;
	n--;

	if (!n || !isdigit((int) *ptr)) return -1;
	while(n && isdigit((int) *ptr)) {
	  minor = 10*minor + (*ptr - '0');
	  ptr++;
	  n--;
	}
	desc->http_state++;
	return http_request_message(desc, meth,
				    meth_ptr, meth_len,
				    uri_ptr, uri_len,
				    major, minor);
      }
    }
    return -1;
  }
  else {
    int up = 1;      /* make next char uppercase */
    http_atom_t* name;
    char* name_ptr;
    int   name_len;
    unsigned long h;

    if (n == 0) {
      /* end of headers */
      desc->http_state = 0;  /* reset state (for next request) */
      return http_eoh_message(desc);
    }
    h = 0;
    while(n && !is_tspecial((unsigned char)*ptr)) {
      int c = *ptr;
      if (up) {
	if (islower(c)) {
	  c = toupper(c);
	}
	up = 0;
      }
      else {
	if (isupper(c))
	  c = tolower(c);
	else if (c == '-')
	  up = 1;
      }
      *ptr = c;
      hash_update(h, c);
      ptr++;
      n--;
    }
    if (*ptr != ':') {
      /* Error case */
      return -1;
    }
    name_ptr = buf;
    name_len = (ptr - buf);
    name = http_hash_lookup(name_ptr, name_len, h,
			    http_hdr_hash, HTTP_HDR_HASH_SIZE);
    ptr++;
    n--;
    /* Skip white space */
    while(n && SP(ptr)) { ptr++; n--; }

    return http_header_message(desc, name, name_ptr, name_len,
			       ptr, n);
  }
}
#endif

/* 
** passive mode reply:
**        {inet_async, S, Ref, {ok,[H1,...Hsz | Data]}}
*/
static int inet_async_data(inet_descriptor* desc, char* buf, int len)
{
    unsigned int hsz = desc->hsz;
    ErlDrvTermData spec[20];
    ErlDrvTermData caller;
    int req;
    int aid;
    int i = 0;

    DEBUGF(("inet_async_data(%ld): len = %d\r\n", (long)desc->port, len));

    if (deq_async(desc, &aid, &caller, &req) < 0)
	return -1;

    i = LOAD_ATOM(spec, i, am_inet_async);
    i = LOAD_PORT(spec, i, desc->dport);
    i = LOAD_INT(spec, i, aid);

    i = LOAD_ATOM(spec, i, am_ok);
    if ((desc->mode == INET_MODE_LIST) || (hsz > len)) {
	i = LOAD_STRING(spec, i, buf, len); /* => [H1,H2,...Hn] */ 
	i = LOAD_TUPLE(spec, i, 2);
	i = LOAD_TUPLE(spec, i, 4);
	ASSERT(i == 15);
	desc->caller = 0;
	return driver_send_term(desc->port, caller, spec, i);
    }
    else {
	/* INET_MODE_BINARY => [H1,H2,...HSz | Binary] */
	ErlDrvBinary* bin;
	int sz = len - hsz;
	int code;

	if ((bin = driver_alloc_binary(sz)) == NULL)
	    return async_error(desc, ENOMEM);
	memcpy(bin->orig_bytes, buf+hsz, sz);
	i = LOAD_BINARY(spec, i, bin, 0, sz);
	if (hsz > 0)
	    i = LOAD_STRING_CONS(spec, i, buf, hsz);
	i = LOAD_TUPLE(spec, i, 2);
	i = LOAD_TUPLE(spec, i, 4);
	ASSERT(i <= 20);
	desc->caller = 0;
	code = driver_send_term(desc->port, caller, spec, i);
	driver_free_binary(bin);  /* must release binary */
	return code;
    }
}

/* 
** passive mode reply:
**        {inet_async, S, Ref, {ok,[H1,...Hsz | Data]}}
*/
static int 
inet_async_binary_data(inet_descriptor* desc, unsigned int phsz,
		       ErlDrvBinary* bin, int offs, int len)
{
    unsigned int hsz = desc->hsz + phsz;
    ErlDrvTermData spec[20];
    ErlDrvTermData caller = desc->caller;
    int aid;
    int req;
    int i = 0;

    DEBUGF(("inet_async_binary_data(%ld): offs=%d, len = %d\r\n", 
	    (long)desc->port, offs, len));

    if (deq_async(desc, &aid, &caller, &req) < 0)
	return -1;

    i = LOAD_ATOM(spec, i, am_inet_async);
    i = LOAD_PORT(spec, i, desc->dport);
    i = LOAD_INT(spec, i,  aid);

    i = LOAD_ATOM(spec, i, am_ok);

    if ((desc->mode == INET_MODE_LIST) || (hsz > len)) {
	/* INET_MODE_LIST => [H1,H2,...Hn] */
	i = LOAD_STRING(spec, i, bin->orig_bytes+offs, len);
    }
    else {
	/* INET_MODE_BINARY => [H1,H2,...HSz | Binary] */
	int sz = len - hsz;
	i = LOAD_BINARY(spec, i, bin, offs+hsz, sz);
	if (hsz > 0)
	    i = LOAD_STRING_CONS(spec, i, bin->orig_bytes+offs, hsz);
    }
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 4);
    ASSERT(i <= 20);    
    desc->caller = 0;
    return driver_send_term(desc->port, caller, spec, i);
}

/* 
** active mode message:
**        {tcp, S, [H1,...Hsz | Data]}
*/
static int loop_message(inet_descriptor* desc, char* buf, int len)
{
    unsigned int hsz = desc->hsz;
    ErlDrvTermData spec[20];
    int i = 0;

    DEBUGF(("loop_message(%ld): len = %d\r\n", (long)desc->port, len));    

    i = LOAD_ATOM(spec, i, am_tcp);
    i = LOAD_PORT(spec, i, desc->dport);

    if ((desc->mode == INET_MODE_LIST) || (hsz > len)) {
	i = LOAD_STRING(spec, i, buf, len); /* => [H1,H2,...Hn] */ 
	i = LOAD_TUPLE(spec, i, 3);
	ASSERT(i <= 20);
	return driver_output_term(desc->port, spec, i);
    }
    else {
	/* INET_MODE_BINARY => [H1,H2,...HSz | Binary] */
	ErlDrvBinary* bin;
	int sz = len - hsz;
	int code;

	if ((bin = driver_alloc_binary(sz)) == NULL)
	    return async_error(desc, ENOMEM);
	memcpy(bin->orig_bytes, buf+hsz, sz);
	i = LOAD_BINARY(spec, i, bin, 0, sz);
	if (hsz > 0)
	    i = LOAD_STRING_CONS(spec, i, buf, hsz);
	i = LOAD_TUPLE(spec, i, 3);
	ASSERT(i <= 20);
	code = driver_output_term(desc->port, spec, i);
	driver_free_binary(bin);  /* must release binary */
	return code;
    }
}

/* 
** active mode message:
**        {tcp, S, [H1,...Hsz | Data]}
*/
static int
loop_binary_message(inet_descriptor* desc, ErlDrvBinary* bin, int offs, int len)
{
    unsigned int hsz = desc->hsz;
    ErlDrvTermData spec[20];
    int i = 0;

    DEBUGF(("loop_binary_message(%ld): len = %d\r\n", (long)desc->port, len)); 

    i = LOAD_ATOM(spec, i, am_tcp);
    i = LOAD_PORT(spec, i, desc->dport);

    if ((desc->mode == INET_MODE_LIST) || (hsz > len)) {
	/* INET_MODE_LIST => [H1,H2,...Hn] */
	i = LOAD_STRING(spec, i, bin->orig_bytes+offs, len);
    }
    else {
	/* INET_MODE_BINARY => [H1,H2,...HSz | Binary] */
	int sz = len - hsz;

	i = LOAD_BINARY(spec, i, bin, offs+hsz, sz);
	if (hsz > 0)
	    i = LOAD_STRING_CONS(spec, i, bin->orig_bytes+offs, hsz);
    }
    i = LOAD_TUPLE(spec, i, 3);
    ASSERT(i <= 20);
    return driver_output_term(desc->port, spec, i);
}

/*
** send:  active mode  {tcp_closed, S}
*/
static int loop_closed_message(loop_descriptor* desc)
{
    ErlDrvTermData spec[6];
    int i = 0;

    DEBUGF(("loop_closed_message(%ld):\r\n", (long)desc->inet.port)); 

    i = LOAD_ATOM(spec, i, am_tcp_closed);
    i = LOAD_PORT(spec, i, desc->inet.dport);
    i = LOAD_TUPLE(spec, i, 2);
    ASSERT(i <= 6);
    return driver_output_term(desc->inet.port, spec, i);
}

/*
** send active message {tcp_error, S, Error}
*/
static int loop_error_message(loop_descriptor* desc, int err)
{
    ErlDrvTermData spec[8];
    ErlDrvTermData am_err = error_atom(err);
    int i = 0;

    DEBUGF(("loop_error_message(%ld): %d\r\n", (long)desc->inet.port, err)); 

    i = LOAD_ATOM(spec, i, am_tcp_error);
    i = LOAD_PORT(spec, i, desc->inet.dport);
    i = LOAD_ATOM(spec, i, am_err);
    i = LOAD_TUPLE(spec, i, 3);
    ASSERT(i <= 8);
    return driver_output_term(desc->inet.port, spec, i);
}

/*
** The fcgi header is 8 bytes. After that comes the data and
** possibly some padding.
** return length of the header (and total length int plen)
** return -1 when not enough bytes
** return -2 when error
*/
#define FCGI_VERSION_1		1

struct fcgi_head {
  unsigned char version;
  unsigned char type;
  unsigned char requestIdB1;
  unsigned char requestIdB0;
  unsigned char contentLengthB1;
  unsigned char contentLengthB0;
  unsigned char paddingLength;
  unsigned char reserved;
};


#define CDR_MAGIC "GIOP"

struct cdr_head {
    unsigned char magic[4];        /* 4 bytes must be 'GIOP' */
    unsigned char major;           /* major version */ 
    unsigned char minor;           /* minor version */
    unsigned char flags;           /* bit 0: 0 == big endian, 1 == little endian 
				      bit 1: 1 == more fragments follow
				   */
    unsigned char message_type;    /* message type ... */
    unsigned char message_size[4]; /* size in (flags bit 0 byte order) */
};


#define TPKT_VRSN 3

struct tpkt_head {
    unsigned char vrsn;             /* contains TPKT_VRSN */
    unsigned char reserved;
    unsigned char packet_length[2]; /* size incl header, big-endian (?) */
};


/* scan buffer for bit 7 */
static void scanbit8(inet_descriptor* desc, char* buf, int len)
{
    int c;

    if (!desc->bit8f || desc->bit8) return;
    c = 0;
    while(len--) c |= *buf++;
    desc->bit8 = ((c & 0x80) != 0);
}

/* 
** active=TRUE:
**  (NOTE! distribution MUST use active=TRUE, deliver=PORT)
**       deliver=PORT  {S, {data, [H1,..Hsz | Data]}}
**       deliver=TERM  {tcp, S, [H1..Hsz | Data]}
**
** active=FALSE:
**       {async, S, Ref, {ok,[H1,...Hsz | Data]}}
*/
static int loop_reply_data(loop_descriptor* desc, char* buf, int len)
{
    int code;

    /* adjust according to packet type */
    switch(desc->inet.htype) {
    case TCP_PB_1:  buf += 1; len -= 1; break;
    case TCP_PB_2:  buf += 2; len -= 2; break;
    case TCP_PB_4:  buf += 4; len -= 4; break;
    case TCP_PB_FCGI:
	len -= ((struct fcgi_head*)buf)->paddingLength;
	break;
    }

    scanbit8(INETP(desc), buf, len);

    if (desc->inet.deliver == INET_DELIVER_PORT)
        code = inet_port_data(INETP(desc), buf, len);
#ifdef USE_HTTP
    else if ((desc->inet.htype == TCP_PB_HTTP) ||
	     (desc->inet.htype == TCP_PB_HTTPH)) {
        if ((code = http_message(desc, buf, len)) < 0)
	    http_error_message(desc, buf, len);
	code = 0;
    }
#endif    
    else if (desc->inet.active == INET_PASSIVE)
        return inet_async_data(INETP(desc), buf, len);
    else
        code = loop_message(INETP(desc), buf, len);

    if (code < 0)
	return code;
    if (desc->inet.active == INET_ONCE)
	desc->inet.active = INET_PASSIVE;
    return code;
}

static int
loop_reply_binary_data(loop_descriptor* desc, ErlDrvBinary* bin, int offs, int len)
{
    int code;

    /* adjust according to packet type */
    switch(desc->inet.htype) {
    case TCP_PB_1:  offs += 1; len -= 1; break;
    case TCP_PB_2:  offs += 2; len -= 2; break;
    case TCP_PB_4:  offs += 4; len -= 4; break;
    case TCP_PB_FCGI:
	len -= ((struct fcgi_head*)(bin->orig_bytes+offs))->paddingLength;
	break;
    }

    scanbit8(INETP(desc), bin->orig_bytes+offs, len);

    if (desc->inet.deliver == INET_DELIVER_PORT)
        code = inet_port_binary_data(INETP(desc), bin, offs, len);
#ifdef USE_HTTP
    else if ((desc->inet.htype == TCP_PB_HTTP) ||
	     (desc->inet.htype == TCP_PB_HTTPH)) {
        if ((code = http_message(desc, bin->orig_bytes+offs, len)) < 0)
	    http_error_message(desc, bin->orig_bytes+offs, len);
	code = 0;
    }
#endif
    else if (desc->inet.active == INET_PASSIVE)
	return inet_async_binary_data(INETP(desc), 0, bin, offs, len);
    else
	code = loop_binary_message(INETP(desc), bin, offs, len);
    if (code < 0)
	return code;
    if (desc->inet.active == INET_ONCE)
	desc->inet.active = INET_PASSIVE;
    return code;
}



/* ----------------------------------------------------------------------------

   INET

---------------------------------------------------------------------------- */


/*
** Set a inaddr structure:
**  src = [P1,P0,X1,X2,.....]
**  dst points to a structure large enugh to keep any kind
**  of inaddr.
** *len is set to length of src on call
** and is set to actual length of dst on return
** return NULL on error and ptr after port address on success
*/
static char* inet_set_address(int family, inet_address* dst, char* src, int* len)
{
    short port;

    if ((family == AF_INET) && (*len >= 6)) {
	sys_memzero((char*)dst, sizeof(struct sockaddr_in));
	port = get_int16(src);
	dst->sai.sin_family = family;
	dst->sai.sin_port   = htons(port);
	sys_memcpy(&dst->sai.sin_addr, src+2, sizeof(struct in_addr));
	*len = sizeof(struct sockaddr_in);
	return src + 6;
    }
#if defined(HAVE_IN6) && defined(AF_INET6)
    else if ((family == AF_INET6) && (*len >= 18)) {
	sys_memzero((char*)dst, sizeof(struct sockaddr_in6));
	port = get_int16(src);
	dst->sai6.sin6_family = family;
	dst->sai6.sin6_port   = htons(port);
	dst->sai6.sin6_flowinfo = 0;   /* XXX this may be set as well ?? */
	sys_memcpy(&dst->sai6.sin6_addr, src+2, sizeof(struct in6_addr));
	*len = sizeof(struct sockaddr_in6); 
	return src + 18;
    }
#endif
    return NULL;
}


/* Get a inaddr structure
** src = inaddr structure
** *len is the lenght of structure
** dst is filled with [F,P1,P0,X1,....] 
** where F is the family code (coded)
** and *len is the length of dst on return 
** (suitable to deliver to erlang)
*/
static int inet_get_address(int family, char* dst, inet_address* src, int* len)
{
    short port;

    if ((family == AF_INET) && (*len >= sizeof(struct sockaddr_in))) {
	dst[0] = INET_AF_INET;
	port = ntohs(src->sai.sin_port);
	put_int16(port, dst+1);
	sys_memcpy(dst+3, (char*)&src->sai.sin_addr, sizeof(struct in_addr));
	*len = 3 + sizeof(struct in_addr);
	return 0;
    }
#if defined(HAVE_IN6) && defined(AF_INET6)
    else if ((family == AF_INET6) && (*len >= sizeof(struct sockaddr_in6))) {
	dst[0] = INET_AF_INET6;
	port = ntohs(src->sai6.sin6_port);
	put_int16(port, dst+1);
	sys_memcpy(dst+3, (char*)&src->sai6.sin6_addr,sizeof(struct in6_addr));
	*len = 3 + sizeof(struct in6_addr);
	return 0;
    }
#endif
    return -1;
}




static int erl_inet_close(inet_descriptor* desc)
{
    free_subscribers(&desc->empty_out_q_subs);
    desc->state = INET_STATE_CLOSED;
    return 0;
}


static int inet_ctl_open(inet_descriptor* desc, int domain, int type,
			 char** rbuf, int rsize)
{
    if (desc->state != INET_STATE_CLOSED)
	return ctl_xerror(EXBADSEQ, rbuf, rsize);
    desc->state = INET_STATE_OPEN;
    desc->stype = type;
    desc->sfamily = domain;
    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
}

/*
**  store interface info as: (bytes)
**  [Len] Name(Len) Flags(1) addr(4) baddr(4) mask(4) bw(4)
*/
struct addr_if {
    char name[INET_IFNAMSIZ];
    long           flags;        /* coded flags */
    struct in_addr addr;         /* interface address */
    struct in_addr baddr;        /* broadcast address */
    struct in_addr mask;         /* netmask */
};


#ifndef SIOCGIFNETMASK
static struct in_addr net_mask(in)
struct in_addr in;
{
    register u_long i = ntohl(in.s_addr);

    if (IN_CLASSA(i))
	in.s_addr = htonl(IN_CLASSA_NET);
    else if (IN_CLASSB(i))
	in.s_addr = htonl(IN_CLASSB_NET);
    else
	in.s_addr = htonl(IN_CLASSC_NET);
    return in;
}
#endif

#if defined(__WIN32__) && defined(SIO_GET_INTERFACE_LIST)

/* format address in dot notation */
static char* fmt_addr(unsigned long x, char* ptr)
{
    int i;
    for (i = 0; i < 4; i++) {
	int nb[3];
	int y = (x >> 24) & 0xff;
	x <<= 8;
	nb[2] = y % 10; y /= 10;
	nb[1] = y % 10; y /= 10;
	nb[0] = y % 10; y /= 10;
	switch((nb[2]>0 ? 3 : (nb[1]>0 ? 2 : 1))) {
	case 3:  *ptr++ = nb[2] + '0';
	case 2:  *ptr++ = nb[1] + '0';
	case 1:  *ptr++ = nb[0] + '0';
	}
	*ptr++ = '.';
    }
    *(ptr-1) = '\0';
    return ptr;
}

static int parse_addr(char* ptr, int n, long* x)
{
    long addr = 0;
    int  dots = 0;
    int  digs = 0;

    while(n--) {
	switch(*ptr) {
	case '0': case '1': case '2':case '3':case '4':case '5':
	case '6': case '7': case '8':case '9':
	    n = n*10 + *ptr++ - '0';
	    digs++;
	    break;
	case '.':
	    if ((dots>2) || (digs==0) || (digs > 3) || (n > 0xff)) return -1;
	    dots++;
	    digs = 0;
	    addr = (addr << 8) | n;
	    n = 0;
	    ptr++;
	    break;
	default:
	    return -1;
	}
    }
    if ((dots != 3) || (digs==0) || (digs > 3) || (n > 0xff)) return -1;
    addr = (addr << 8) | n;
    *x = addr;
    return 0;
}

#endif

#define buf_check(ptr, end, n) \
do { if ((end)-(ptr) < (n)) goto error; } while(0)

static char* sockaddr_to_buf(struct sockaddr* addr, char* ptr, char* end)
{
    if (addr->sa_family == AF_INET || addr->sa_family == 0) {
	struct in_addr a;
	buf_check(ptr,end,sizeof(struct in_addr));
	a = ((struct sockaddr_in*) addr)->sin_addr;
	sys_memcpy(ptr, (char*)&a, sizeof(struct in_addr));
	return ptr + sizeof(struct in_addr);
    }
#if defined(HAVE_IN6) && defined(AF_INET6)
    else if (addr->sa_family == AF_INET6) {
	struct in6_addr a;
	buf_check(ptr,end,sizeof(struct in6_addr));
	a = ((struct sockaddr_in6*) addr)->sin6_addr;
	sys_memcpy(ptr, (char*)&a, sizeof(struct in6_addr));
	return ptr + sizeof(struct in6_addr);
    }
#endif
 error:
    return NULL;

}

static char* buf_to_sockaddr(char* ptr, char* end, struct sockaddr* addr)
{
    buf_check(ptr,end,sizeof(struct in_addr));
    sys_memcpy((char*) &((struct sockaddr_in*)addr)->sin_addr, ptr,
	       sizeof(struct in_addr));
    addr->sa_family = AF_INET;
    return ptr +  sizeof(struct in_addr);

 error:
    return NULL;
}

/* set socket options:
** return -1 on error
**         0 if ok
**         1 if ok force deliver of queued data
*/
static int inet_set_opts(inet_descriptor* desc, char* ptr, int len)
{
    int type;
    int proto;
    int opt;
    struct linger li_val;
#ifdef HAVE_MULTICAST_SUPPORT
    struct ip_mreq mreq_val;
#endif
    int ival;
    char* arg_ptr;
    int arg_sz;
    int old_htype = desc->htype;
    int old_active = desc->active;

    while(len >= 5) {
	opt = *ptr++;
	ival = get_int32(ptr);
	ptr += 4;
	len -= 5;
	arg_ptr = (char*) &ival;
	arg_sz = sizeof(ival);
	proto = SOL_SOCKET;

	switch(opt) {
	case INET_LOPT_HEADER:
	    DEBUGF(("inet_set_opts(%ld): s=%d, HEADER=%d\r\n",
		    (long)desc->port, desc->s,ival));
	    desc->hsz = ival;
	    continue;

	case INET_LOPT_MODE:
	    DEBUGF(("inet_set_opts(%ld): s=%d, MODE=%d\r\n",
		    (long)desc->port, desc->s, ival));
	    desc->mode = ival;
	    continue;

	case INET_LOPT_DELIVER:
	    DEBUGF(("inet_set_opts(%ld): s=%d, DELIVER=%d\r\n",
		    (long)desc->port, desc->s, ival));
	    desc->deliver = ival;
	    continue;
	    
	case INET_LOPT_BUFFER:
	    DEBUGF(("inet_set_opts(%ld): s=%d, BUFFER=%d\r\n",
		    (long)desc->port, desc->s, ival));
	    if (ival > INET_MAX_BUFFER)  ival = INET_MAX_BUFFER;
	    else if (ival < INET_MIN_BUFFER) ival = INET_MIN_BUFFER;
	    desc->bufsz = ival;
	    continue;

	case INET_LOPT_ACTIVE:
	    DEBUGF(("inet_set_opts(%ld): s=%d, ACTIVE=%d\r\n",
		    (long)desc->port, desc->s,ival));
	    desc->active = ival;
	    continue;

	case INET_LOPT_PACKET:
	    DEBUGF(("inet_set_opts(%ld): s=%d, PACKET=%d\r\n",
		    (long)desc->port, desc->s, ival));
	    desc->htype = ival;
	    continue;

	case INET_LOPT_EXITONCLOSE:
	    DEBUGF(("inet_set_opts(%ld): s=%d, EXITONCLOSE=%d\r\n",
		    (long)desc->port, desc->s, ival));
	    desc->exitf = ival;
	    continue;

	case INET_LOPT_BIT8:
	    DEBUGF(("inet_set_opts(%ld): s=%d, BIT8=%d\r\n",
		    (long)desc->port, desc->s, ival));
	    switch(ival) {
	    case INET_BIT8_ON:
		desc->bit8f = 1;
		desc->bit8  = 0;
		break;
	    case INET_BIT8_OFF:
		desc->bit8f = 0;
		desc->bit8  = 0;
		break;
	    case INET_BIT8_CLEAR:
		desc->bit8f = 1;
		desc->bit8  = 0;
		break;
	    case INET_BIT8_SET:
		desc->bit8f = 1;
		desc->bit8  = 1;
		break;
	    }
	    continue;

	case INET_LOPT_TCP_HIWTRMRK:
	    if (desc->stype == SOCK_STREAM) {
		loop_descriptor* tdesc = (loop_descriptor*) desc;
		if (ival < 0) ival = 0;
		else if (ival > INET_MAX_BUFFER*2) ival = INET_MAX_BUFFER*2;
		if (tdesc->low > ival)
		    tdesc->low = ival;
		tdesc->high = ival;
	    }
	    continue;

	case INET_LOPT_TCP_LOWTRMRK:
	    if (desc->stype == SOCK_STREAM) {
		loop_descriptor* tdesc = (loop_descriptor*) desc;
		if (ival < 0) ival = 0;
		else if (ival > INET_MAX_BUFFER) ival = INET_MAX_BUFFER;
		if (tdesc->high < ival)
		    tdesc->high = ival;
		tdesc->high = ival;
	    }
	    continue;

	case INET_LOPT_TCP_SEND_TIMEOUT:
	    if (desc->stype == SOCK_STREAM) {
		loop_descriptor* tdesc = (loop_descriptor*) desc;
		tdesc->send_timeout = ival;
	    }
	    continue;

	case INET_OPT_REUSEADDR: 
#ifdef __WIN32__
	    continue;  /* Bjorn says */
#else
	    type = SO_REUSEADDR;
	    DEBUGF(("inet_set_opts(%ld): s=%d, SO_REUSEADDR=%d\r\n",
		    (long)desc->port, desc->s,ival));
	    break;
#endif
	case INET_OPT_KEEPALIVE: type = SO_KEEPALIVE;
	    DEBUGF(("inet_set_opts(%ld): s=%d, SO_KEEPALIVE=%d\r\n",
		    (long)desc->port, desc->s, ival));
	    break;
	case INET_OPT_DONTROUTE: type = SO_DONTROUTE;
	    DEBUGF(("inet_set_opts(%ld): s=%d, SO_DONTROUTE=%d\r\n",
		    (long)desc->port, desc->s, ival));
	    break;
	case INET_OPT_BROADCAST: type = SO_BROADCAST;
	    DEBUGF(("inet_set_opts(%ld): s=%d, SO_BROADCAST=%d\r\n",
		    (long)desc->port, desc->s,ival));
	    break;
	case INET_OPT_OOBINLINE: type = SO_OOBINLINE; 
	    DEBUGF(("inet_set_opts(%ld): s=%d, SO_OOBINLINE=%d\r\n",
		    (long)desc->port, desc->s, ival));
	    break;
	case INET_OPT_SNDBUF:    type = SO_SNDBUF; 
	    DEBUGF(("inet_set_opts(%ld): s=%d, SO_SNDBUF=%d\r\n",
		    (long)desc->port, desc->s, ival));
	    /* 
	     * Setting buffer sizes in VxWorks gives unexpected results
	     * our workaround is to leave it at default.
	     */
	    break;

	case INET_OPT_RCVBUF:    type = SO_RCVBUF; 
	    DEBUGF(("inet_set_opts(%ld): s=%d, SO_RCVBUF=%d\r\n",
		    (long)desc->port, desc->s, ival));
	    break;

	case INET_OPT_LINGER:    type = SO_LINGER; 
	    if (len < 4)
		return -1;
	    li_val.l_onoff = ival;
	    li_val.l_linger = get_int32(ptr);
	    ptr += 4;
	    len -= 4;
	    arg_ptr = (char*) &li_val;
	    arg_sz = sizeof(li_val);
	    DEBUGF(("inet_set_opts(%ld): s=%d, SO_LINGER=%d,%d",
		    (long)desc->port, desc->s, li_val.l_onoff,li_val.l_linger));
	    break;
	case TCP_OPT_NODELAY:
	    proto = IPPROTO_TCP; 
	    type = TCP_NODELAY; 
	    DEBUGF(("inet_set_opts(%ld): s=%d, TCP_NODELAY=%d\r\n",
		    (long)desc->port, desc->s, ival));
	    break;

	default:
	    return -1;
	}

	if (type == SO_RCVBUF) {
	    /* make sure we have desc->bufsz >= SO_RCVBUF */
	    if (ival > desc->bufsz)
		desc->bufsz = ival;
	}
    }

    if ( ((desc->stype == SOCK_STREAM) && IS_CONNECTED(desc)) ||
	((desc->stype == SOCK_DGRAM) && IS_OPEN(desc))) {

	if ((desc->stype==SOCK_STREAM) && desc->active) {
	    if (!old_active) return 1;  /* passive => active change */
	    if (desc->htype != old_htype) return 2;  /* header type change */
	    return 0;
	}
    }
    return 0;
}

/* load all option values into the buf and reply 
** return total length of reply filled into ptr
** ptr should point to a buffer with 9*len +1 to be safe!!
*/

static int inet_fill_opts(inet_descriptor* desc,
			  char* buf, int len, char* ptr)
{
    int type;
    int proto;
    int opt;
    struct linger li_val;
    int ival;
    char* arg_ptr;
    int arg_sz;
    char* save_ptr = ptr;

    *ptr++ = INET_REP_OK;

    while(len--) {
	opt = *buf++;
	proto = SOL_SOCKET;
	arg_sz = sizeof(ival);
	arg_ptr = (char*) &ival;

	switch(opt) {
	case INET_LOPT_BUFFER:
	    *ptr++ = opt;
	    put_int32(desc->bufsz, ptr);
	    ptr += 4;
	    continue;
	case INET_LOPT_HEADER:
	    *ptr++ = opt;
	    put_int32(desc->hsz, ptr);
	    ptr += 4;
	    continue;
	case INET_LOPT_MODE:
	    *ptr++ = opt;
	    put_int32(desc->mode, ptr);
	    ptr += 4;
	    continue;
	case INET_LOPT_DELIVER:
	    *ptr++ = opt;
	    put_int32(desc->deliver, ptr);
	    ptr += 4;
	    continue;
	case INET_LOPT_ACTIVE:
	    *ptr++ = opt;
	    put_int32(desc->active, ptr);
	    ptr += 4;
	    continue;
	case INET_LOPT_PACKET:
	    *ptr++ = opt;
	    put_int32(desc->htype, ptr);
	    ptr += 4;
	    continue;
	case INET_LOPT_EXITONCLOSE:
	    *ptr++ = opt;
	    put_int32(desc->exitf, ptr);
	    ptr += 4;
	    continue;

	case INET_LOPT_BIT8:
	    *ptr++ = opt;
	    if (desc->bit8f) {
		put_int32(desc->bit8, ptr);
	    }
	    else {
		put_int32(INET_BIT8_OFF, ptr);
	    }
	    ptr += 4;
	    continue;

	case INET_LOPT_TCP_HIWTRMRK:
	    if (desc->stype == SOCK_STREAM) {
		*ptr++ = opt;
		ival = ((loop_descriptor*)desc)->high;
		put_int32(ival, ptr);
		ptr += 4;
	    }
	    continue;

	case INET_LOPT_TCP_LOWTRMRK:
	    if (desc->stype == SOCK_STREAM) {
		*ptr++ = opt;
		ival = ((loop_descriptor*)desc)->low;
		put_int32(ival, ptr);
		ptr += 4;
	    }
	    continue;

	case INET_LOPT_TCP_SEND_TIMEOUT:
	    if (desc->stype == SOCK_STREAM) {
		*ptr++ = opt;
		ival = ((loop_descriptor*)desc)->send_timeout;
		put_int32(ival, ptr);
		ptr += 4;
	    }
	    continue;

	case INET_OPT_REUSEADDR: 
	    type = SO_REUSEADDR; 
	    break;
	case INET_OPT_KEEPALIVE: 
	    type = SO_KEEPALIVE; 
	    break;
	case INET_OPT_DONTROUTE: 
	    type = SO_DONTROUTE; 
	    break;
	case INET_OPT_BROADCAST: 
	    type = SO_BROADCAST;
	    break;
	case INET_OPT_OOBINLINE: 
	    type = SO_OOBINLINE; 
	    break;
	case INET_OPT_SNDBUF:    
	    type = SO_SNDBUF; 
	    break;
	case INET_OPT_RCVBUF:    
	    type = SO_RCVBUF; 
	    break;
	case TCP_OPT_NODELAY:
	    proto = IPPROTO_TCP;
	    type = TCP_NODELAY;
	    break;

	default:
	    return -1;
	}
	*ptr++ = opt;
	if (arg_ptr == (char*)&ival) {
	    put_int32(ival, ptr);
	    ptr += 4; 
	}
	else {
	    put_int32(li_val.l_onoff, ptr);
	    ptr += 4; 
	    put_int32(li_val.l_linger, ptr);
	    ptr += 4; 
	}
    }
    return (ptr - save_ptr);
}


/* fill statistics reply, op codes from src and result in dest
** dst area must be a least 5*len + 1 bytes
*/
static int inet_fill_stat(inet_descriptor* desc, char* src, int len, char* dst)
{
    unsigned long val;
    int op;
    char* dst_start = dst;

    *dst++ = INET_REP_OK;     /* put reply code */
    while (len--) {
	op = *src++;
	*dst++ = op;  /* copy op code */
	switch(op) {
	case INET_STAT_RECV_CNT:  
	    val = desc->recv_cnt;    
	    break;
	case INET_STAT_RECV_MAX:  
	    val = (unsigned long) desc->recv_max;    
	    break;
	case INET_STAT_RECV_AVG:  
	    val = (unsigned long) desc->recv_avg;    
	    break;
	case INET_STAT_RECV_DVI:  
	    val = (unsigned long) fabs(desc->recv_dvi); 
	    break;
	case INET_STAT_SEND_CNT:  
	    val = desc->send_cnt; 
	    break;
	case INET_STAT_SEND_MAX:  
	    val = desc->send_max; 
	    break;
	case INET_STAT_SEND_AVG: 
	    val = (unsigned long) desc->send_avg;
	    break;
	case INET_STAT_SEND_PND:  
	    val = driver_sizeq(desc->port); 
	    break;
	case INET_STAT_RECV_OCT:
	    put_int32(desc->recv_oct[1], dst);   /* write high 32bit */
	    put_int32(desc->recv_oct[0], dst+4); /* write low 32bit */
	    dst += 8;
	    continue;
	case INET_STAT_SEND_OCT:
	    put_int32(desc->send_oct[1], dst);   /* write high 32bit */
	    put_int32(desc->send_oct[0], dst+4); /* write low 32bit */
	    dst += 8;
	    continue;
	default: return -1; /* invalid argument */
	}
	put_int32(val, dst);  /* write 32bit value */
	dst += 4;
    }
    return dst - dst_start;  /* actual length */
}

static void
send_empty_out_q_msgs(inet_descriptor* desc)
{
  ErlDrvTermData msg[6];
  int msg_len = 0;

  if(NO_SUBSCRIBERS(&desc->empty_out_q_subs))
    return;

  msg_len = LOAD_ATOM(msg, msg_len, am_empty_out_q);
  msg_len = LOAD_PORT(msg, msg_len, desc->dport);
  msg_len = LOAD_TUPLE(msg, msg_len, 2);

  ASSERT(msg_len == 6);

  send_to_subscribers(desc->port,
		      &desc->empty_out_q_subs,
		      1,
		      msg,
		      msg_len);
}

/* subscribe and fill subscription reply, op codes from src and
** result in dest dst area must be a least 5*len + 1 bytes
*/
static int inet_subscribe(inet_descriptor* desc, char* src, int len, char* dst)
{
    unsigned long val;
    int op;
    char* dst_start = dst;

    *dst++ = INET_REP_OK;     /* put reply code */
    while (len--) {
	op = *src++;
	*dst++ = op;  /* copy op code */
	switch(op) {
	case INET_SUBS_EMPTY_OUT_Q:  
	  val = driver_sizeq(desc->port);
	  if(val > 0)
	    if(!save_subscriber(&desc->empty_out_q_subs,
				driver_caller(desc->port)))
	      return 0;
	  break;
	default: return -1; /* invalid argument */
	}
	put_int32(val, dst);  /* write 32bit value */
	dst += 4;
    }
    return dst - dst_start;  /* actual length */
}

/* Terminate socket */
static void inet_stop(inet_descriptor* desc)
{
    erl_inet_close(desc);
    FREE(desc);
}


/* Allocate descriptor */
static ErlDrvData inet_start(ErlDrvPort port, int size)
{
    inet_descriptor* desc;

    if ((desc = (inet_descriptor*) ALLOC(size)) == NULL)
	return NULL;

    desc->port = port;
    desc->dport = driver_mk_port(port);
    desc->state = INET_STATE_CLOSED;
    desc->bufsz = INET_DEF_BUFFER; 
    desc->hsz = 0;                     /* list header size */
    desc->htype = TCP_PB_RAW;          /* default packet type */
    desc->stype = -1;                  /* bad stype */
    desc->sfamily = -1;
    desc->mode    = INET_MODE_LIST;    /* list mode */
    desc->exitf   = 1;                 /* exit port when close on active 
					  socket */
    desc->bit8f   = 0;
    desc->bit8    = 0;
    desc->deliver = INET_DELIVER_TERM; /* standard term format */
    desc->active  = INET_PASSIVE;      /* start passive */
    desc->oph = NULL;
    desc->opt = NULL;

    desc->peer_ptr = NULL;
    desc->name_ptr = NULL;

    desc->recv_oct[0] = desc->recv_oct[1] = 0;
    desc->recv_cnt = 0;
    desc->recv_max = 0;    
    desc->recv_avg = 0.0;
    desc->recv_dvi = 0.0;
    desc->send_oct[0] = desc->send_oct[1] = 0;
    desc->send_cnt = 0;
    desc->send_max = 0;
    desc->send_avg = 0.0;
    desc->empty_out_q_subs.subscriber = NO_PROCESS;
    desc->empty_out_q_subs.next = NULL;

    sys_memzero((char *)&desc->remote,sizeof(desc->remote));

    return (ErlDrvData)desc;
}


#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 256
#endif

/*
** common tcp/udp control command
*/
static int inet_ctl(inet_descriptor* desc, int cmd, char* buf, int len,
		    char** rbuf, int rsize)
{
    switch (cmd) {

    case INET_REQ_GETSTAT: {
	  char* dst;
	  int i;
	  int dstlen = 1;  /* Reply code */

	  for (i = 0; i < len; i++) {
	      switch(buf[i]) {
	      case INET_STAT_SEND_OCT: dstlen += 9; break;
	      case INET_STAT_RECV_OCT: dstlen += 9; break;
	      default: dstlen += 5; break;
	      }
	  }
	  DEBUGF(("inet_ctl(%ld): GETSTAT\r\n", (long) desc->port)); 
	  if (dstlen > INET_MAX_BUFFER) /* sanity check */
	      return 0;
	  if (dstlen > rsize) {
	      if ((dst = (char*) ALLOC(dstlen)) == NULL)
		  return 0;
	      *rbuf = dst;  /* call will free this buffer */
	  }
	  else
	      dst = *rbuf;  /* ok we fit in buffer given */
	  return inet_fill_stat(desc, buf, len, dst);
      }

    case INET_REQ_SUBSCRIBE: {
	  char* dst;
	  int dstlen = 1 /* Reply code */ + len*5;
	  DEBUGF(("inet_ctl(%ld): INET_REQ_SUBSCRIBE\r\n", (long) desc->port)); 
	  if (dstlen > INET_MAX_BUFFER) /* sanity check */
	      return 0;
	  if (dstlen > rsize) {
	      if ((dst = (char*) ALLOC(dstlen)) == NULL)
		  return 0;
	      *rbuf = dst;  /* call will free this buffer */
	  }
	  else
	      dst = *rbuf;  /* ok we fit in buffer given */
	  return inet_subscribe(desc, buf, len, dst);
      }

    case INET_REQ_GETOPTS: {    /* get options */
	char* dst;
	int dstlen = len*9 + 1;  /* max length of reply */
	int replen;

	DEBUGF(("inet_ctl(%ld): GETOPTS\r\n", (long)desc->port)); 
	if (dstlen > INET_MAX_BUFFER)
	    return 0;
	if (dstlen > rsize) {
	    if ((dst = (char*) ALLOC(dstlen)) == NULL)
		return 0;
	    *rbuf = dst;  /* call will free this buffer */	    
	}
	else
	    dst = *rbuf;
	if ((replen = inet_fill_opts(desc, buf, len, dst)) < 0)
	    return ctl_error(EINVAL, rbuf, rsize);
	return replen;
    }

    case INET_REQ_GETIFLIST: {
	DEBUGF(("inet_ctl(%ld): GETIFLIST\r\n", (long)desc->port)); 
	return ctl_error(EINVAL, rbuf, rsize);
    }

    case INET_REQ_IFGET: {
	DEBUGF(("inet_ctl(%ld): IFGET\r\n", (long)desc->port)); 	
	return ctl_error(EINVAL, rbuf, rsize);
    }

    case INET_REQ_IFSET: {
	DEBUGF(("inet_ctl(%ld): IFSET\r\n", (long)desc->port));
	return ctl_error(EINVAL, rbuf, rsize);
    }

    case INET_REQ_SETOPTS:  {   /* set options */
	DEBUGF(("inet_ctl(%ld): SETOPTS\r\n", (long)desc->port)); 
	switch(inet_set_opts(desc, buf, len)) {
	case -1: 
	    return ctl_error(EINVAL, rbuf, rsize);
	case 0: 
	    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
	default:  /* active/passive change!! */
	    /*
	     * Let's hope that the descriptor really is a loop_descriptor here.
	     */
	    loop_deliver((loop_descriptor *) desc, 0);
	    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
	}
    }

    case INET_REQ_GETIX: {          /* get internal index (listen/accept) */
	char tbuf[4];

	DEBUGF(("inet_ctl(%ld): GETIX\r\n", (long)desc->port)); 
	put_int32(0, tbuf);
	return ctl_reply(INET_REP_OK, tbuf, 4, rbuf, rsize);
    }

    case INET_REQ_GETSTATUS: {
	char tbuf[4];

	DEBUGF(("inet_ctl(%ld): GETSTATUS\r\n", (long)desc->port)); 
	put_int32(desc->state, tbuf);
	return ctl_reply(INET_REP_OK, tbuf, 4, rbuf, rsize);
    }

    case INET_REQ_GETTYPE: {
	char tbuf[8];

	DEBUGF(("inet_ctl(%ld): GETTYPE\r\n", (long)desc->port)); 
	if (desc->sfamily == AF_INET) {
	    put_int32(INET_AF_INET, &tbuf[0]);
	}
#if defined(HAVE_IN6) && defined(AF_INET6)
        else if (desc->sfamily == AF_INET6) {
	    put_int32(INET_AF_INET6, &tbuf[0]);
	}
#endif
	else
	    return ctl_error(EINVAL, rbuf, rsize);

	if (desc->stype == SOCK_STREAM) {
	    put_int32(INET_TYPE_STREAM, &tbuf[4]);
	}
	else if (desc->stype == SOCK_DGRAM) {
	    put_int32(INET_TYPE_DGRAM, &tbuf[4]);
	}
	else
	    return ctl_error(EINVAL, rbuf, rsize);
	return ctl_reply(INET_REP_OK, tbuf, 8, rbuf, rsize);
    }


    case INET_REQ_GETFD: {
	char tbuf[4];

	DEBUGF(("inet_ctl(%ld): GETFD\r\n", (long)desc->port)); 
	if (!IS_OPEN(desc))
	    return ctl_error(EINVAL, rbuf, rsize);
	put_int32(0, tbuf);
	return ctl_reply(INET_REP_OK, tbuf, 4, rbuf, rsize);
    }
	
    case INET_REQ_GETHOSTNAME: { /* get host name */
	return ctl_error(EINVAL, rbuf, rsize);
    }

    case INET_REQ_PEER:  {      /* get peername */
	char tbuf[sizeof(inet_address)];
	inet_address peer;
	inet_address* ptr;
	int sz = sizeof(peer);

	DEBUGF(("inet_ctl(%ld): PEER\r\n", (long)desc->port)); 

	if (!(desc->state & INET_F_ACTIVE))
	    return ctl_error(ENOTCONN, rbuf, rsize);
	if ((ptr = desc->peer_ptr) == NULL) 
	    return ctl_error(EINVAL, rbuf, rsize);
	if (inet_get_address(desc->sfamily, tbuf, ptr, &sz) < 0)
	    return ctl_error(EINVAL, rbuf, rsize);
	return ctl_reply(INET_REP_OK, tbuf, sz, rbuf, rsize);
    }

    case INET_REQ_SETPEER: { /* set fake peername Port Address */
	if (len == 0) {
	    desc->peer_ptr = NULL;
	    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
	}
	else if (len < 2)
	    return ctl_error(EINVAL, rbuf, rsize);	    
	else if (inet_set_address(desc->sfamily, &desc->peer_addr,
				  buf, &len) == NULL)
	    return ctl_error(EINVAL, rbuf, rsize);
	else {
	    desc->peer_ptr = &desc->peer_addr;
	    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);	    
	}
    }

    case INET_REQ_NAME:  {      /* get sockname */
	char tbuf[sizeof(inet_address)];
	inet_address name;
	inet_address* ptr;
	int sz = sizeof(name);

	DEBUGF(("inet_ctl(%ld): NAME\r\n", (long)desc->port)); 

	if (!IS_BOUND(desc))
	    return ctl_error(EINVAL, rbuf, rsize); /* address is not valid */

	if ((ptr = desc->name_ptr) == NULL)
	    return ctl_error(EINVAL, rbuf, rsize);
	if (inet_get_address(desc->sfamily, tbuf, ptr, &sz) < 0)
	    return ctl_error(EINVAL, rbuf, rsize);
	return ctl_reply(INET_REP_OK, tbuf, sz, rbuf, rsize);
    }

    case INET_REQ_SETNAME: { /* set fake peername Port Address */
	if (len == 0) {
	    desc->name_ptr = NULL;
	    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
	}
	else if (len < 2)
	    return ctl_error(EINVAL, rbuf, rsize);	    
	else if (inet_set_address(desc->sfamily, &desc->name_addr,
				  buf, &len) == NULL)
	    return ctl_error(EINVAL, rbuf, rsize);
	else {
	    desc->name_ptr = &desc->name_addr;
	    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);	    
	}
    }

    case INET_REQ_BIND:  {      /* bind socket */
	char tbuf[2];
	inet_address local;
	short port;

	DEBUGF(("inet_ctl(%ld): BIND\r\n", (long)desc->port)); 

	if (len < 2)
	    return ctl_error(EINVAL, rbuf, rsize);
	if (desc->state != INET_STATE_OPEN)
	    return ctl_xerror(EXBADPORT, rbuf, rsize);

	if (inet_set_address(desc->sfamily, &local, buf, &len) == NULL)
	    return ctl_error(EINVAL, rbuf, rsize);

	desc->state = INET_STATE_BOUND;

	port = inet_address_port(&local);
	port = ntohs(port);
	put_int16(port, tbuf);
	return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
    }

#ifndef VXWORKS

    case INET_REQ_GETSERVBYNAME: { /* L1 Name-String L2 Proto-String */
	return ctl_error(EINVAL, rbuf, rsize);
    }

    case INET_REQ_GETSERVBYPORT: { /* P1 P0 L1 Proto-String */
	return ctl_error(EINVAL, rbuf, rsize);
    }
	
#endif /* !VXWORKS */	

    default:
	return ctl_xerror(EXBADPORT, rbuf, rsize);
    }
}

/* update statistics on output packets */
static void inet_output_count(inet_descriptor* desc, int len)
{
    unsigned long n = desc->send_cnt + 1;
    unsigned long t = desc->send_oct[0] + len;
    int c = (t < desc->send_oct[0]);
    double avg = desc->send_avg;

    /* at least 64 bit octet count */
    desc->send_oct[0] = t;
    desc->send_oct[1] += c;

    if (n == 0) /* WRAP, use old avg as input to a new sequence */
	n = 1;
    desc->send_avg += (len - avg) / n;
    if (len > desc->send_max)
	desc->send_max = len;
    desc->send_cnt = n;
}

/* update statistics on input packets */
static void inet_input_count(inet_descriptor* desc, int len)
{
    unsigned long n = desc->recv_cnt + 1;
    unsigned long t = desc->recv_oct[0] + len;
    int c = (t < desc->recv_oct[0]);
    double avg = desc->recv_avg;
    double dvi;

    /* at least 64 bit octet count */
    desc->recv_oct[0] = t;
    desc->recv_oct[1] += c;

    if (n == 0) /* WRAP */
	n = 1;

    /* average packet length */
    avg = avg + (len - avg) / n;
    desc->recv_avg = avg;

    if (len > desc->recv_max)
	desc->recv_max = len;

    /* average deviation from average packet length */
    dvi = desc->recv_dvi;
    desc->recv_dvi = dvi + ((len - avg) - dvi) / n;
    desc->recv_cnt = n;
}

/*----------------------------------------------------------------------------

   LOOP

-----------------------------------------------------------------------------*/

/*
** Set new size on buffer, used when packet size is determined
** and the buffer is to small.
** buffer must have a size of at least len bytes (counting from ptr_start!)
*/
static int loop_expand_buffer(loop_descriptor* desc, int len)
{
    ErlDrvBinary* bin;
    int offs1;
    int offs2;
    int used = desc->i_ptr_start - desc->i_buf->orig_bytes;
    int ulen = used + len;

    if (desc->i_bufsz >= ulen) /* packet will fit */
	return 0;
    else if (desc->i_buf->orig_size >= ulen) { /* buffer is large enough */
	desc->i_bufsz = ulen;  /* set "virtual" size */
	return 0;
    }

    DEBUGF(("loop_expand_buffer(%ld): s=%d, from %ld to %d\r\n",
	    (long)desc->inet.port, desc->inet.s, desc->i_buf->orig_size, ulen));

    offs1 = desc->i_ptr_start - desc->i_buf->orig_bytes;
    offs2 = desc->i_ptr - desc->i_ptr_start;

    if ((bin = driver_realloc_binary(desc->i_buf, ulen)) == NULL)
	return -1;

    desc->i_buf = bin;
    desc->i_ptr_start = bin->orig_bytes + offs1;
    desc->i_ptr       = desc->i_ptr_start + offs2;
    desc->i_bufsz     = ulen;
    return 0;
}

/* push data into i_buf  */
static int loop_push_buffer(loop_descriptor* desc, char* buf, int len)
{
    ErlDrvBinary* bin;

    if (desc->i_buf == NULL) {
	bin = alloc_buffer(len);
	sys_memcpy(bin->orig_bytes, buf, len);
	desc->i_buf = bin;
	desc->i_bufsz = len;
	desc->i_ptr_start = desc->i_buf->orig_bytes;
	desc->i_ptr = desc->i_ptr_start + len;
    }
    else {
	char* start =  desc->i_buf->orig_bytes;
	int sz_before = desc->i_ptr_start - start;
	int sz_filled = desc->i_ptr - desc->i_ptr_start;
	
	if (len <= sz_before) {
	    sys_memcpy(desc->i_ptr_start - len, buf, len);
	    desc->i_ptr_start -= len;
	}
	else {
	    bin = alloc_buffer(desc->i_bufsz+len);
	    sys_memcpy(bin->orig_bytes, buf, len);
	    sys_memcpy(bin->orig_bytes+len, desc->i_ptr_start, sz_filled);
	    free_buffer(desc->i_buf);
	    desc->i_bufsz += len;
	    desc->i_buf = bin;
	    desc->i_ptr_start = bin->orig_bytes;
	    desc->i_ptr = desc->i_ptr_start + sz_filled + len;
	}
    }
    desc->i_remain = 0;	
    return 0;
}

/* clear CURRENT input buffer */
static void loop_clear_input(loop_descriptor* desc)
{
    if (desc->i_buf != NULL)
	free_buffer(desc->i_buf);
    desc->i_buf = NULL;
    desc->i_remain    = 0;
    desc->i_ptr       = NULL;
    desc->i_ptr_start = NULL;
    desc->i_bufsz     = 0;
}

/* clear QUEUED output */
static void loop_clear_output(loop_descriptor* desc)
{
    ErlDrvPort ix  = desc->inet.port;
    int qsz = driver_sizeq(ix);

    driver_deq(ix, qsz);
    send_empty_out_q_msgs(INETP(desc));
}


/* Move data so that ptr_start point at buf->orig_bytes */
static void loop_restart_input(loop_descriptor* desc)
{
    if (desc->i_ptr_start != desc->i_buf->orig_bytes) {
	int n = desc->i_ptr - desc->i_ptr_start;

	DEBUGF(("loop_restart_input: move %d bytes\r\n", n));
	sys_memmove(desc->i_buf->orig_bytes, desc->i_ptr_start, n);
	desc->i_ptr_start = desc->i_buf->orig_bytes;
	desc->i_ptr = desc->i_ptr_start + n;
    }
}

DRIVER_INIT(loop_drv) 
{
  return &loop_driver_entry;
}


static int loop_drv_init(void)
{
    buffer_stack_pos = 0;

#ifdef DEBUG
    tot_buf_allocated = 0;
    max_buf_allocated = 0;
    tot_buf_stacked = 0;
#endif
    am_ok           = driver_mk_atom("ok");
    am_tcp          = driver_mk_atom("tcp");
    am_tcp_closed   = driver_mk_atom("tcp_closed");
    am_tcp_error    = driver_mk_atom("tcp_error");
    am_error        = driver_mk_atom("error");
    am_inet_async   = driver_mk_atom("inet_async");
    am_inet_reply   = driver_mk_atom("inet_reply");
    am_timeout      = driver_mk_atom("timeout");
    am_closed       = driver_mk_atom("closed");
    am_empty_out_q  = driver_mk_atom("empty_out_q");

    http_init();
    return 0;
}

/* initialize the tcp descriptor */

static ErlDrvData loop_drv_start(ErlDrvPort port, char* args)
{
    loop_descriptor* desc;

    desc = (loop_descriptor*)inet_start(port, sizeof(loop_descriptor));
    if (desc == NULL)
	return ERL_DRV_ERROR_ERRNO;
    desc->high = INET_HIGH_WATERMARK;
    desc->low  = INET_LOW_WATERMARK;
    desc->send_timeout = INET_INFINITY;
    desc->busy_on_send = 0;
    desc->i_buf = NULL;
    desc->i_ptr = NULL;
    desc->i_ptr_start = NULL;
    desc->i_remain = 0;
    desc->i_bufsz = 0;
#ifdef USE_HTTP
    desc->http_state = 0;
#endif
    return (ErlDrvData) desc;
}

/*
** Check Special cases:
** 1. we are a listener doing nb accept -> report error on accept !
** 2. we are doing accept -> restore listener state
*/
static void loop_close_check(loop_descriptor* desc)
{
    if (desc->inet.state == TCP_STATE_CONNECTED)
	async_error_am_all(INETP(desc), am_closed);
}

/*
** Cleanup & Free
*/
static void loop_drv_stop(ErlDrvData e)
{
    loop_descriptor* desc = (loop_descriptor*)e;
    loop_close_check(desc);
    /* free input buffer & output buffer */
    if (desc->i_buf != NULL)
	release_buffer(desc->i_buf);
    desc->i_buf = NULL; /* net_mess2 may call this function recursively when 
			   faulty messages arrive on dist ports*/
    inet_stop(INETP(desc));
}


/* tcp requests from Erlang */
static int loop_drv_ctl(ErlDrvData e, unsigned int cmd, char* buf, int len,
			char** rbuf, int rsize)
{
    loop_descriptor* desc = (loop_descriptor*)e;
    switch(cmd) {
    case INET_REQ_OPEN:   /* open socket and return internal index */
	DEBUGF(("loop_drv_ctl(%ld): OPEN\r\n", (long)desc->inet.port));
	if ((len == 1) && (buf[0] == INET_AF_INET))
	    return inet_ctl_open(INETP(desc),AF_INET,SOCK_STREAM,rbuf,rsize);
#if defined(HAVE_IN6) && defined(AF_INET6)
        else if ((len == 1) && (buf[0] == INET_AF_INET6))
	    return inet_ctl_open(INETP(desc),AF_INET6,SOCK_STREAM,rbuf,rsize);
#endif
	else
	    return ctl_error(EINVAL, rbuf, rsize);

    case TCP_REQ_LISTEN: { /* argument backlog */
	return ctl_error(EINVAL, rbuf, rsize);
    }


    case INET_REQ_CONNECT: {   /* do async connect */
	int code;
	char tbuf[2];
	unsigned timeout;

	DEBUGF(("loop_drv_ctl(%ld): CONNECT\r\n", (long)desc->inet.port)); 
	/* INPUT: Timeout(4), Port(2), Address(N) */

	if (!IS_OPEN(INETP(desc)))
	    return ctl_xerror(EXBADPORT, rbuf, rsize);
	if (IS_CONNECTED(INETP(desc)))
	    return ctl_error(EISCONN, rbuf, rsize);
	if (!IS_BOUND(INETP(desc)))
	    return ctl_xerror(EXBADSEQ, rbuf, rsize);
	if (len < 6)
	    return ctl_error(EINVAL, rbuf, rsize);
	timeout = get_int32(buf);
	buf += 4;
	len -= 4;

	if (inet_set_address(desc->inet.sfamily, &desc->inet.peer_addr,
			     buf, &len) == NULL)
	    return ctl_error(EINVAL, rbuf, rsize);

	desc->inet.state = TCP_STATE_CONNECTED;
	enq_async(INETP(desc), tbuf, INET_REQ_CONNECT);
	async_ok(INETP(desc));
	return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
    }

    case TCP_REQ_ACCEPT: {  /* do async accept */
	return ctl_error(EINVAL, rbuf, rsize);
    }

    case INET_REQ_CLOSE:
	DEBUGF(("loop_drv_ctl(%ld): CLOSE\r\n", (long)desc->inet.port)); 
	loop_close_check(desc);
	erl_inet_close(INETP(desc));
	return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);


    case TCP_REQ_RECV: {
	unsigned timeout;
	char tbuf[2];
	int n;

	DEBUGF(("loop_drv_ctl(%ld): RECV\r\n", (long)desc->inet.port)); 
	/* INPUT: Timeout(4),  Length(4) */
	if (!IS_CONNECTED(INETP(desc)))
	    return ctl_error(ENOTCONN, rbuf, rsize);
	if (desc->inet.active || (len != 8))
	    return ctl_error(EINVAL, rbuf, rsize);
	timeout = get_int32(buf);
	buf += 4;
	n = get_int32(buf);
	DEBUGF(("loop_drv_ctl(%ld) timeout = %d, n = %d\r\n",
		(long)desc->inet.port,timeout,n));
	if ((desc->inet.htype != TCP_PB_RAW) && (n != 0))
	    return ctl_error(EINVAL, rbuf, rsize);
	if (n > TCP_MAX_PACKET_SIZE)
	    return ctl_error(ENOMEM, rbuf, rsize);
	if (enq_async(INETP(desc), tbuf, TCP_REQ_RECV) < 0)
	    return ctl_error(EALREADY, rbuf, rsize);

	if (loop_recv(desc, n) == 0) {
	    if (timeout == 0)
		async_error_am(INETP(desc), am_timeout);
	    else {
		if (timeout != INET_INFINITY)
		    driver_set_timer(desc->inet.port, timeout); 
	    }
	}
	return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
    }

    case TCP_REQ_UNRECV: {
	if (!IS_CONNECTED(INETP(desc)))
	    return ctl_error(ENOTCONN, rbuf, rsize);
	loop_push_buffer(desc, buf, len);
	if (desc->inet.active)
	    loop_deliver(desc, 0);
	return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
    }
#ifndef _OSE_
    case TCP_REQ_SHUTDOWN: {
	int how;
	if (!IS_CONNECTED(INETP(desc))) {
	    return ctl_error(ENOTCONN, rbuf, rsize);
	}
	if (len != 1) {
	    return ctl_error(EINVAL, rbuf, rsize);
	}
	how = buf[0];
	return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
    }
#endif
    default:
	return inet_ctl(INETP(desc), cmd, buf, len, rbuf, rsize);
    }

}

/*
** loop_drv_timeout:
** called when timer expire:
** tcp socket may be:
**
** a)  receiving   -- deselect
** b)  connecting  -- close socket
** c)  accepting   -- reset listener
**
*/

static void loop_drv_timeout(ErlDrvData e)
{
    loop_descriptor* desc = (loop_descriptor*)e;
    int state = desc->inet.state;

    if ((state & TCP_STATE_CONNECTED) == TCP_STATE_CONNECTED) {
	if (desc->busy_on_send) {
	    desc->busy_on_send = 0;
	    inet_reply_error_am(INETP(desc), am_timeout);
	}
	else {
	    /* assume recv timeout */
	    ASSERT(!desc->inet.active);
	    desc->i_remain = 0;
	    async_error_am(INETP(desc), am_timeout);
	}
    }
}

/*
** command:
**   output on a socket only !
**   a reply code will be sent to connected (caller later)
**   {inet_reply, S, Status}
** NOTE! normal sockets use the the loop_drv_commandv
** but distribution still uses the loop_drv_command!!
*/

static void loop_drv_command(ErlDrvData e, char *buf, int len)
{
    loop_descriptor* desc = (loop_descriptor*)e;
    desc->inet.caller = driver_caller(desc->inet.port);

    if (!IS_CONNECTED(INETP(desc)))
	inet_reply_error(INETP(desc), ENOTCONN);
    else if (loop_send(desc, buf, len) == 0)
	inet_reply_ok(INETP(desc));
}


static void loop_drv_commandv(ErlDrvData e, ErlIOVec* ev)
{
    loop_descriptor* desc = (loop_descriptor*)e;
    desc->inet.caller = driver_caller(desc->inet.port);

    if (!IS_CONNECTED(INETP(desc)))
	inet_reply_error(INETP(desc), ENOTCONN);
    else if (loop_sendv(desc, ev) == 0)
	inet_reply_ok(INETP(desc));
}


/* The socket has closed, cleanup and send event */
static int loop_recv_closed(loop_descriptor* desc)
{
    if (!desc->inet.active) {
	/* We must cancel any timer here ! */
	driver_cancel_timer(desc->inet.port);
	/* passive mode do not terminate port ! */
	loop_clear_input(desc);
	async_error_am_all(INETP(desc), am_closed);
	/* next time EXBADSEQ will be delivered  */
    }
    else {
	DEBUGF(("loop_recv_close(%ld): s=%d, in %s, line %d\r\n",
		(long)desc->inet.port, desc->inet.s, __FILE__, __LINE__));
	/* A send is blocked */
	if (IS_BUSY(INETP(desc))) {
	    loop_clear_output(desc);
	    driver_cancel_timer(desc->inet.port);
	    desc->busy_on_send = 0;
	    desc->inet.state &= ~INET_F_BUSY;
	    set_busy_port(desc->inet.port, 0);
	    inet_reply_error_am(INETP(desc), am_closed);
	}

	loop_clear_input(desc);
	loop_closed_message(desc);
	
	if (desc->inet.exitf) {
	    driver_exit(desc->inet.port, 0);
	}
    }
    return -1;
}


/* We have a read error determine the action */
static int loop_recv_error(loop_descriptor* desc, int err)
{
    if (!desc->inet.active) {
	/* We must cancel any timer here ! */
	driver_cancel_timer(desc->inet.port);
	loop_clear_input(desc);
	async_error(INETP(desc), err);  /* emit error string */
    }
    else {
	/* A send is blocked */
	if (IS_BUSY(INETP(desc))) {
	    loop_clear_output(desc);
	    driver_cancel_timer(desc->inet.port);
	    desc->busy_on_send = 0;
	    desc->inet.state &= ~INET_F_BUSY;
	    set_busy_port(desc->inet.port, 0);
	    inet_reply_error_am(INETP(desc), am_closed);
	}
	loop_clear_input(desc);
	loop_error_message(desc, err); /* first error */
	loop_closed_message(desc);     /* then closed */
	if (desc->inet.exitf)
	    driver_exit(desc->inet.port, err);
    }
    return -1;
}



/*
** Calculate number of bytes that remain to read before deliver
** Assume buf, ptr_start, ptr has been setup
**
** return  > 0 if more to read
**         = 0 if holding complete packet
**         < 0 on error
**
** if return value == 0 then *len will hold the length of the first packet
**    return value > 0 then if *len == 0 then value means upperbound
**                             *len > 0  then value means exact
**
*/
static int loop_remain(loop_descriptor* desc, int* len)
{
    char* ptr = desc->i_ptr_start;
    int nfill = (desc->i_ptr - desc->i_buf->orig_bytes); /* filled */
    int nsz   = desc->i_bufsz - nfill;                   /* remain */
    int n = desc->i_ptr - ptr;  /* number of bytes read */
    int plen;
    int hlen;

    DEBUGF(("loop_remain(%ld): s=%d, n=%d, nfill=%d nsz=%d\r\n", 
	    (long)desc->inet.port, desc->inet.s, n, nfill, nsz));

    switch(desc->inet.htype) {
    case TCP_PB_RAW:
	if (n == 0) goto more;
	else {
	    *len = n;
	    DEBUGF((" => nothing remain packet=%d\r\n", n));	    
	    return 0;  /* deliver */
	}

    case TCP_PB_1:
	/* TCP_PB_1:    [L0 | Data] */
	hlen = 1;
	if (n < hlen) goto more;
	plen = get_int8(ptr);
	goto remain;

    case TCP_PB_2:
	/* TCP_PB_2:    [L1,L0 | Data] */
	hlen = 2;
	if (n < hlen) goto more;
	plen = get_int16(ptr);
	goto remain;

    case TCP_PB_4:
	/* TCP_PB_4:    [L3,L2,L1,L0 | Data] */
	hlen = 4;
	if (n < hlen) goto more;
	plen = get_int32(ptr);
	goto remain;

    case TCP_PB_RM:
	/* TCP_PB_RM:    [L3,L2,L1,L0 | Data] 
	 ** where MSB (bit) is used to signal end of record
	 */
	hlen = 4;
	if (n < hlen) goto more;
	plen = get_int32(ptr) & 0x7fffffff;
	goto remain;

    case TCP_PB_LINE_LF: {
	/* TCP_PB_LINE_LF:  [Data ... \n]  */
	char* ptr2;
	if  ((ptr2 = memchr(ptr, '\n', n)) == NULL) {
	    if ((nsz == 0) && (nfill == n)) { /* buffer full */
		*len = n;
		DEBUGF((" => line buffer full (no NL)=%d\r\n", n));
		return 0;
	    }
	    goto more;
	}
	else {
	    *len = (ptr2 - ptr) + 1;  /* include newline */
	    DEBUGF((" => nothing remain packet=%d\r\n", *len));
	    return 0;
	}
    }

    case TCP_PB_ASN1: {
	/* TCP_PB_ASN1: handles long (4 bytes) or short length format */
	char* tptr = ptr;
	int length;
	int nn = n;

	if (n < 2) goto more;
	nn--;
	if ((*tptr++ & 0x1f) == 0x1f) { /* Long tag format */
	    while(nn && ((*tptr & 0x80) == 0x80)) {
		tptr++;
		nn--;
	    }
	    if (nn < 2) goto more;
	    tptr++;
	    nn--;
	}

	/* tptr now point to length field and n characters remain */
	length = *tptr & 0x7f;
	if ((*tptr & 0x80) == 0x80) {   /* Long length format */
	    tptr++;
	    nn--;
	    if (nn < length) goto more;
	    switch(length) {
	    case 1: plen = get_int8(tptr);  tptr += 1; break;
	    case 2: plen = get_int16(tptr); tptr += 2; break;
	    case 3: plen = get_int24(tptr); tptr += 3; break;
	    case 4: plen = get_int32(tptr); tptr += 4; break;
	    default: goto error; /* error */
	    }
	}
	else {
	    tptr++;
	    plen = length;
	}
	hlen = (tptr-ptr);
	goto remain;
    }


    case TCP_PB_CDR: {
	struct cdr_head* hp;
	hlen = sizeof(struct cdr_head);
	if (n < hlen) goto more;
	hp = (struct cdr_head*) ptr;
	if (sys_memcmp(hp->magic, CDR_MAGIC, 4) != 0)
	    goto error;
	if (hp->flags & 0x01) /* Byte ordering flag */
	    plen = get_little_int32(hp->message_size);
	else
	    plen = get_int32(hp->message_size);
	goto remain;
    }

    case TCP_PB_FCGI: {
	struct fcgi_head* hp;
	hlen = sizeof(struct fcgi_head);
	if (n < hlen) goto more;
	hp = (struct fcgi_head*) ptr;
	if (hp->version != FCGI_VERSION_1)
	    goto error;			/* ERROR, unknown header version */
	plen = ((hp->contentLengthB1 << 8) | hp->contentLengthB0)
	    + hp->paddingLength;
	goto remain;
    }
#ifdef USE_HTTP
    case TCP_PB_HTTPH:
	desc->http_state = 1;
    case TCP_PB_HTTP: {
        /* TCP_PB_HTTP:  data \r\n(SP data\r\n)*  */
        plen = n;
	if (((plen == 1) && NL(ptr)) || ((plen == 2) && CRNL(ptr)))
	    goto done;
	else {
	    char* ptr1 = ptr;
	    int   len = plen;

	    while(1) {
	      char* ptr2 = memchr(ptr1, '\n', len);

	      if  (ptr2 == NULL) {
		  if ((nsz == 0) && (nfill == n)) { /* buffer full */
		      plen = n;
		      goto done;
		  }
		  goto more;
	      }
	      else {
  		  plen = (ptr2 - ptr) + 1;

		  if (desc->http_state == 0) 
		      goto done;
	        
		  if (plen < n) {
		      if (SP(ptr2+1)) {
			  ptr1 = ptr2+1;
			  len -= plen;
		      }
		      else
			  goto done;
		  }
		  else
		      goto more;
	      }
	    }
	}
    }
#endif
    case TCP_PB_TPKT: {
	struct tpkt_head* hp;
	hlen = sizeof(struct tpkt_head);
	if (n < hlen) 
	    goto more;
	hp = (struct tpkt_head*) ptr;
	if (hp->vrsn == TPKT_VRSN) {
	    plen = get_int16(hp->packet_length) - hlen;
	    if (plen < 0)
		goto error;
	} else
	    goto error;
	goto remain;
    }

    default:  /* this can not occure (make compiler happy) */
	DEBUGF((" => case error\r\n"));
	return -1;
    }

 done: {
      *len = plen;
      DEBUGF((" => nothing remain packet=%d\r\n", plen));
      return 0;
    }

 remain: {
     int remain = (plen+hlen) - n;
     if (remain <= 0) {
	 *len = plen + hlen;
	 DEBUGF((" => nothing remain packet=%d\r\n", plen+hlen));
	 return 0;
     }
     else {
	 if (loop_expand_buffer(desc, plen+hlen) < 0)
	     return -1;
	 DEBUGF((" => remain=%d\r\n", remain));
	 *len = remain;
	 return remain;
     }
 }

 more:
    *len = 0;
    if (nsz == 0) {
	if (nfill == n)
	    goto error;
	DEBUGF((" => restart more=%d\r\n", nfill - n));
	return nfill - n;
    }
    else {
	DEBUGF((" => more=%d \r\n", nsz));
	return nsz;
    }

 error:
    DEBUGF((" => packet error\r\n"));
    return -1;
}

/*
** Deliver all packets ready 
** if len == 0 then check start with a check for ready packet
*/
static int loop_deliver(loop_descriptor* desc, int len)
{
    int count = 0;
    int n;

    /* Poll for ready packet */
    if (len == 0) {
	/* empty buffer or waiting for more input */
	if ((desc->i_buf == NULL) || (desc->i_remain > 0))
	    return count;
	if ((n = loop_remain(desc, &len)) != 0) {
	    if (n < 0) /* packet error */
		return n;
	    if (len > 0)  /* more data pending */
		desc->i_remain = len;
	    return count;
	}
    }

    while (len > 0) {
	int code = 0;

	inet_input_count(INETP(desc), len);

	/* deliver binary? */
	if (len*4 >= desc->i_buf->orig_size*3) { /* >=75% */
	    /* something after? */
	    if (desc->i_ptr_start + len == desc->i_ptr) { /* no */
		code = loop_reply_binary_data(desc, desc->i_buf,
					      (desc->i_ptr_start -
					       desc->i_buf->orig_bytes),
					      len);
		loop_clear_input(desc);
	    }
	    else { /* move trail to beginning of a new buffer */
		ErlDrvBinary* bin;
		char* ptr_end = desc->i_ptr_start + len;
		int sz = desc->i_ptr - ptr_end;

		bin = alloc_buffer(desc->i_bufsz);
		memcpy(bin->orig_bytes, ptr_end, sz);

		code = loop_reply_binary_data(desc, desc->i_buf,
					      (desc->i_ptr_start-
					       desc->i_buf->orig_bytes),
					      len);
		free_buffer(desc->i_buf);
		desc->i_buf = bin;
		desc->i_ptr_start = desc->i_buf->orig_bytes;
		desc->i_ptr = desc->i_ptr_start + sz;
		desc->i_remain = 0;
	    }
	}
	else {
	    code = loop_reply_data(desc, desc->i_ptr_start, len);
	    desc->i_ptr_start += len;
	    if (desc->i_ptr_start == desc->i_ptr)
		loop_clear_input(desc);
	    else
		desc->i_remain = 0;
	}

	if (code < 0)
	    return code;

	count++;
	len = 0;

	if (!desc->inet.active) {
	    driver_cancel_timer(desc->inet.port);
	    if (desc->i_buf != NULL)
		loop_restart_input(desc);
	}
	else if (desc->i_buf != NULL) {
	    if ((n = loop_remain(desc, &len)) != 0) {
		if (n < 0) /* packet error */
		    return n;
		loop_restart_input(desc);
		if (len > 0)
		    desc->i_remain = len;
		len = 0;
	    }
	}
    }
    return count;
}


static int loop_recv(loop_descriptor* desc, int request_len)
{
    int n;
    int len;
    int nread;
    int evsz;
    ErlIOVec ev;

    if (desc->i_buf == NULL) {  /* allocte a read buffer */
	int sz = (request_len > 0) ? request_len : desc->inet.bufsz;

	if ((desc->i_buf = alloc_buffer(sz)) == NULL)
	    return -1;
	/* XXX: changing bufsz during recv SHOULD/MAY? affect 
	 * ongoing operation but is not now 
	 */
	desc->i_bufsz = sz; /* use i_bufsz not i_buf->orig_size ! */
	desc->i_ptr_start = desc->i_buf->orig_bytes;
	desc->i_ptr = desc->i_ptr_start;
	nread = sz;
	if (request_len > 0)
	    desc->i_remain = request_len;
	else
	    desc->i_remain = 0;
    }
    else if (request_len > 0) { /* we have a data in buffer and a request */
	n = desc->i_ptr - desc->i_ptr_start;
	if (n >= request_len)
	    return loop_deliver(desc, request_len);
	else if (loop_expand_buffer(desc, request_len) < 0)
	    return loop_recv_error(desc, ENOMEM);
	else
	    desc->i_remain = nread = request_len - n;
    }
    else if (desc->i_remain == 0) {  /* poll remain from buffer data */
	if ((nread = loop_remain(desc, &len)) < 0)
	    return loop_recv_error(desc, EMSGSIZE);
	else if (nread == 0)
	    return loop_deliver(desc, len);
	else if (len > 0)
	    desc->i_remain = len;  /* set remain */
    }
    else  /* remain already set use it */
	nread = desc->i_remain;
    
    DEBUGF(("loop_recv(%ld): s=%d about to read %d bytes...\r\n",  
	    (long)desc->inet.port, desc->inet.s, nread));

    if ((evsz = driver_peekqv(desc->inet.port, &ev)) == 0)
	return 0;

    n = driver_vec_to_buf(&ev, desc->i_ptr, nread);

    driver_deq(desc->inet.port, n);
	
    DEBUGF((" => got %d bytes\r\n", n));
    desc->i_ptr += n;
    if (desc->i_remain > 0) {
	desc->i_remain -= n;
	if (desc->i_remain == 0)
	    return loop_deliver(desc, desc->i_ptr - desc->i_ptr_start);
    }
    else {
	if ((nread = loop_remain(desc, &len)) < 0)
	    return loop_recv_error(desc, EMSGSIZE);
	else if (nread == 0)
	    return loop_deliver(desc, len);
	else if (len > 0)
	    desc->i_remain = len;  /* set remain */
    }
    return 0;
}

/*
** should never be called
*/
static void loop_drv_input(ErlDrvData port, ErlDrvEvent ev)
{
}


/*
** Send non-blocking vector data
*/
static int loop_sendv(loop_descriptor* desc, ErlIOVec* ev)
{
    int sz;
    char buf[4];
    int h_len;
    int n;
    ErlDrvPort ix = desc->inet.port;
    int len = ev->size;

    switch(desc->inet.htype) {
    case TCP_PB_1: 
	put_int8(len, buf);
	h_len = 1;
	break;
    case TCP_PB_2: 
	put_int16(len, buf);
	h_len = 2; 
	break;
    case TCP_PB_4: 
	put_int32(len, buf);
	h_len = 4; 
	break;
    default:
	if (len == 0)
	    return 0;
	h_len = 0;
	break;
    }

    inet_output_count(INETP(desc), len+h_len);

    if (h_len > 0) {
	ev->iov[0].iov_base = buf;
	ev->iov[0].iov_len = h_len;
	ev->size += h_len;
    }

    driver_enqv(ix, ev, 0);

    loop_recv(desc, 0);

    return 0;
}

/*
** Send non blocking data
*/
static int loop_send(loop_descriptor* desc, char* ptr, int len)
{
    int sz;
    char buf[4];
    int h_len;
    int n;
    ErlDrvPort ix = desc->inet.port;
    SysIOVec iov[2];

    switch(desc->inet.htype) {
    case TCP_PB_1: 
	put_int8(len, buf);
	h_len = 1;
	break;
    case TCP_PB_2: 
	put_int16(len, buf);
	h_len = 2; 
	break;
    case TCP_PB_4: 
	put_int32(len, buf);
	h_len = 4; 
	break;
    default:
	if (len == 0)
	    return 0;
	h_len = 0;
	break;
    }

    inet_output_count(INETP(desc), len+h_len);

    driver_enq(ix, buf, h_len);
    driver_enq(ix, ptr, len);

    loop_recv(desc, 0);

    return 0;
}

/*
 * never called
*/
static void loop_drv_output(ErlDrvData data, ErlDrvEvent event)
{
}

/*-----------------------------------------------------------------------------

   Subscription

-----------------------------------------------------------------------------*/

static int
save_subscriber(subs, subs_pid)
subs_list *subs; ErlDrvTermData subs_pid;
{
  subs_list *tmp;

  if(NO_SUBSCRIBERS(subs)) {
    subs->subscriber = subs_pid;
    subs->next = NULL;
  }
  else {
    tmp = subs->next;
    subs->next = ALLOC(sizeof(subs_list));
    if(subs->next == NULL) {
      subs->next = tmp;
      return 0;
    }
    subs->next->subscriber = subs_pid;
    subs->next->next = tmp;
  }
  return 1;
}

static void
free_subscribers(subs)
subs_list *subs;
{
  subs_list *this;
  subs_list *next;

  this = subs->next;
  while(this) {
    next = this->next;
    FREE((void *) this);
    this = next;
  }

  subs->subscriber = NO_PROCESS;
  subs->next = NULL;
}

static void
send_to_subscribers(port, subs, free_subs, msg, msg_len)
ErlDrvPort port;
subs_list *subs;
int free_subs;
ErlDrvTermData msg[];
int msg_len;
{
  subs_list *this;
  subs_list *next;
  int first = 1;

  if(NO_SUBSCRIBERS(subs))
    return;

  this = subs;
  while(this) {
    
    (void) driver_send_term(port, this->subscriber, msg, msg_len);

    if(free_subs && !first) {
      next = this->next;
      FREE((void *) this);
      this = next;
    }
    else
      this = this->next;
    first = 0;
  }

  if(free_subs) {
    subs->subscriber = NO_PROCESS;
    subs->next = NULL;
  }

}
