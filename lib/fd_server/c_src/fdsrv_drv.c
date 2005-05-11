/*
 * Created: Sebastian Strollo <seb@erix.ericsson.se>, 1998-04-03
 * Purpose: Program that passes open file descriptors between
 *          processes using AF_UNIX stream sockets, as described in
 *          Stevens UNIX Network programming. This is an erlang driver
 *          which receives the file descriptors created by fdsrv. (It
 *          can also be compiled with -DTEST, in which case it will be
 *          a standalone test program.) 
 * Modified:
 */

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include <sys/un.h>
#include <sys/param.h>

#include "erl_driver.h"
#ifndef ERL_DRV_NIL
#include "erl_driver_compat.h"
#endif

#include "fdlib.h"

/* forward declarations */
static ErlDrvData start(ErlDrvPort port, char *buf);
static void stop(ErlDrvData drv_data);
static void fd_is_ready(ErlDrvData drv_data, ErlDrvEvent event);
static void recv_fd(ErlDrvPort port, int* fd, ErlDrvEvent sock_fd);

/* driver global variables */
static ErlDrvPort  instance =  (ErlDrvPort) -1;
static ErlDrvEvent fdsrv;



/* driver structure entry */
#ifdef DYNAMIC_DRIVER
static
#endif
ErlDrvEntry fdsrv_drv_entry;

#ifdef DYNAMIC_DRIVER
DRIVER_INIT(fdsrv_drv)
{
    fdsrv_drv_entry.driver_name = "fdsrv_drv";
    fdsrv_drv_entry.init        = NULL;
    fdsrv_drv_entry.start       = start;
    fdsrv_drv_entry.stop        = stop;
    fdsrv_drv_entry.ready_input = fd_is_ready;
    fdsrv_drv_entry.finish      = NULL;
    return &fdsrv_drv_entry;
}
#endif

/*
 * Called when erlang side does "open_port()".
 */
static ErlDrvData start(ErlDrvPort port, char *buf)
{
    int len;
    char *path;
    struct sockaddr_un addr;
    int s;

    if (instance != (ErlDrvPort) -1)   /* only allow one copy at the time */
	return (ErlDrvData) -1;
    if (strlen(buf) > (sizeof(addr.sun_path) + 1))
	return (ErlDrvData) -1;
    instance = port;

    /* Figure out the path to the named socket */
    if ((path = strrchr(buf, (int)' ')) == NULL)
	path = buf;
    else
	path++;

    s = socket(AF_UNIX, SOCK_STREAM, 0);
    bzero((char *)&addr, sizeof(addr));
    addr.sun_family = AF_UNIX;
    strcpy(addr.sun_path, path);
    len = strlen(path) + sizeof(addr.sun_family) + 1;

    if (connect(s, (struct sockaddr *)&addr, len) < 0) {
	perror("connect");
	instance = (ErlDrvPort) -1;
	return (ErlDrvData) -1;
    }
    fdsrv = (ErlDrvEvent) s;
    /* Have Erlang select on fdsrv */
    driver_select(port, fdsrv, DO_READ, 1);

    return (ErlDrvData) port;
}

	    

/*
 * Called when select triggers. Which is when there is an incomming fd.
 */
static void fd_is_ready(ErlDrvData drv_data, ErlDrvEvent event)
{
    ErlDrvPort port = (ErlDrvPort) drv_data;
    int received_fd;
	
    if (port != instance)
	return;
    if (event != fdsrv)
	return;

    /* receive the file descriptor */
    recv_fd(port, &received_fd, fdsrv);
    
    if (received_fd < 0) {
	close((int)fdsrv);
	driver_select(port, fdsrv, DO_READ, 0);
	reply_err(port);
    }
    else
	reply_int(port, received_fd);
}
    


/*
 * Called when the Erlang port is closed.
 */
static void stop(ErlDrvData drv_data)
{
    /* make sure we stop Erlang from selecting on fdsrv */
    driver_select(instance, fdsrv, DO_READ, 0);

    /* cleanup */
    close((int) fdsrv);
    instance = (ErlDrvPort) -1;
}


/*
 * The code that actually receives a file descriptor over a AF_UNIX
 * stream socket. More or less directly from Stevens.
 */

#ifdef HAVE_MSGHDR_MSG_CONTROL
/* We need the newer CMSG_LEN() and CMSG_SPACE() macros, but few
   implementations support them today.  These two macros really need
   an ALIGN() macro, but each implementation does this differently. */
#ifndef CMSG_LEN
#define CMSG_LEN(size)          (sizeof(struct cmsghdr) + (size))
#endif
#ifndef CMSG_SPACE
#define CMSG_SPACE(size)        (sizeof(struct cmsghdr) + (size))
#endif

/* These two macros are really needed as well */
#ifndef CMSG_FIRSTHDR
#define CMSG_FIRSTHDR(mhdr) \
  ((size_t) (mhdr)->msg_controllen >= sizeof (struct cmsghdr)		      \
   ? (struct cmsghdr *) (mhdr)->msg_control : (struct cmsghdr *) NULL)
#endif
#ifndef CMSG_DATA
#define CMSG_DATA(cmsg) ((unsigned char *) ((struct cmsghdr *) (cmsg) + 1))
#endif

#endif

static void recv_fd(ErlDrvPort port, int* fd, ErlDrvEvent sock_fd)
{
    struct iovec iov[1];
    struct msghdr msg;
    int res;
    char c;
#ifdef HAVE_MSGHDR_MSG_CONTROL
    union {
	struct cmsghdr cm;
	char control[CMSG_SPACE(sizeof(int))];
    } control_un;
    struct cmsghdr *cmptr;

    msg.msg_control = control_un.control;
    msg.msg_controllen = sizeof(control_un.control);
#else
    int newfd;

    msg.msg_accrights = (caddr_t) &newfd;
    msg.msg_accrightslen = sizeof(int);
#endif
    msg.msg_name = NULL;
    msg.msg_namelen = 0;

    iov[0].iov_base = &c;
    iov[0].iov_len = 1;
    msg.msg_iov = iov;
    msg.msg_iovlen = 1;

    if ((res = recvmsg((int)sock_fd, &msg, 0)) < 0) {
	reply_err_errno(port);
	goto error;
    }
    
    /* printf("recvmsg() -> %d (flags = %d)\n", res, msg.msg_flags); */
    if (res != 1) {
	reply_err_string(port, "recvmsg() unexpected return value");
	goto error;
    }
#ifdef HAVE_MSGHDR_MSG_CONTROL
    if ( (cmptr = CMSG_FIRSTHDR(&msg)) != NULL
#ifdef BROKEN_CMSG_FIELDS
	) {
#else
	  && cmptr->cmsg_len == CMSG_LEN(sizeof(int))) {
	if (cmptr->cmsg_level != SOL_SOCKET || cmptr->cmsg_type != SCM_RIGHTS){
	    reply_err_string(port, "level or controle type error!\n");
	    goto error;
	}
#endif
	*fd = *((int *) CMSG_DATA(cmptr));
    } else {
	reply_err_string(port, "error in received fd");
	goto error;
    }
#else
    if (msg.msg_accrightslen == sizeof(int)) {
	*fd = newfd;
    } else {
	reply_err_string(port, "error in received fd!\n");
	goto error;
    }
#endif
    return;
error:
    *fd = -1;
    return;
}


#ifdef TEST
/*
 * Define TEST to get a standalone program that can be used to test
 * the functionality.
 *
 */

/* needed by driver code above */
int null_func() { return 0; }

/* needed by driver code above */
int driver_select(i1, i2, i3, i4) { return 0; }

/*
 * Called by the driver with the received file descriptor as a string.
 * Put the descriptor in the static variable rfd.
 */
static int rfd;

int driver_output(port, buf, len)
    char *buf;
    int len;
{
    rfd = atoi(buf);
    return 0;
}

static void echo_test(fd, spec)
    int fd;
    char *spec;
{
    char tmpbuf[512];
    unsigned char c;
    int sfd, r;

    fprintf(stderr, "\nSpec is %s\n", spec);
    
    c = strlen(spec);
    write(fd, &c, 1);
    write(fd, spec, strlen(spec));

    fd_is_ready(instance, fdsrv);

    fprintf(stderr, "Received fd is %d\n", rfd);

    fprintf(stderr, "listen/"); fflush(stderr);
    if (listen(rfd, 2) < 0) {
	perror("listen");
	close(rfd);
	return;
    }
    
    fprintf(stderr, "accept/"); fflush(stderr);
    sfd = accept(rfd, (struct sockaddr *)NULL, (int *)0);
    if (sfd < 0) {
	perror("accept");
	close(rfd);
	return;
    }
    close(rfd);

    fprintf(stderr, "read/"); fflush(stderr);
    r = read(sfd, tmpbuf, 512);
    if (r < 0) {
	perror("read");
	close(sfd);
	return;
    }
    fprintf(stderr, "write\n"); fflush(stderr);
    write(sfd, tmpbuf, r);
    close(sfd);
    
    return;
}


int main(argc, argv)
    int argc;
    char *argv[];
{
    int i, portfd[2];
    pid_t pid;
    char path[64];

    /* make a filename for the named pipe */
    sprintf(path, "/tmp/fdsrv%d", (int)getpid());

    /*
     * Run fdsrv as Erlang does, i.e. at the other end of a pipe. (Well two
     * pipes, but we aren't interested in data from fdsrv so just let it
     * spill out on stdout.)
     */
    if (pipe(portfd) < 0) {
	perror("pipe");
	exit(1);
    }

    if ((pid = fork()) < 0) {
	perror("fork");
	exit(1);
    }
    if (pid == 0) {		/* child */
	close(portfd[1]);
	dup2(portfd[0], 0);
	close(portfd[0]);
	execl("./fdsrv", "fdsrv", path, NULL);
	perror("execl");
	exit(1);
    }
    /* parent */
    close(portfd[0]);
    sleep(1);
    fprintf(stderr, "\n");

#if 0
    {
	int i;
	for(i=0; i<59; i++)
	    if (open("/dev/null", 0, 0)<0) perror("open");
    }
#endif

    start(1l, path);

    /* Accept specs on command line */
    if (argc > 1) {
	for (i=1; i<argc; i++)
	    echo_test(portfd[1], argv[i]);
    } else {
	echo_test(portfd[1], ":9999");
	echo_test(portfd[1], ":9998");
    }

    stop();
    close(portfd[1]);
    exit(0);
}

#endif
