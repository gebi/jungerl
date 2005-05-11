/*
 * Created: Sebastian Strollo <seb@erix.ericsson.se>, 1998-04-03
 * Purpose: Program that passes open file descriptors between
 *          processes using AF_UNIX stream sockets, as described in
 *          Stevens UNIX Network programming. This is a server program
 *          which can be run as setuid root to give it access to
 *          priviliged ports. The program is meant to be run from
 *          erlang with 1 byte length packets. 
 * Modified:
 */


#include <config.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/param.h>

#include "erl_driver.h"
#ifndef ERL_DRV_NIL
#include "erl_driver_compat.h"
#endif

#include "fdlib.h"


/*
 * Create a named pipe with name "path". Accept connection on it and then
 * close it and unlink "path".
 */
int fd_establish_communication(ErlDrvPort port, char* path)
{
    int ret ,fd;
    char buf[4];

    if ((fd = fd_listen_path(port, path)) == -1)
	return -1;

    /* We need to synchronize the creation of the named pipe - so that the
     * client does not try to connect to it until it is created.
     * This is used as synchronization.
     */
    buf[0] = 2; buf[1] = 'o'; buf[2] = 'k';
    write(1, buf, 3);
    
    ret = fd_accept_path(port, fd, path);


    /* Communication established, no need to listen to the pipe anymore,
     * or to have it lying around in the filesystem.
     */
    close(fd);
    unlink(path);
    return ret;

}


int fd_listen_path(ErlDrvPort port, char* path)
{

    int s, len;
    struct sockaddr_un addr;

    s = socket(AF_UNIX, SOCK_STREAM, 0);
    bzero((char *)&addr, sizeof(addr));
    addr.sun_family = AF_UNIX;

    /* make sure we don't overrun */
    if (strlen(path) > (sizeof(addr.sun_path) + 1)) {
	fprintf(stderr, "path too long: %s\n", path);
	return -1;
    }
    strcpy(addr.sun_path, path);
    len = strlen(addr.sun_path) + sizeof(addr.sun_family) + 1;

    if (bind(s, (struct sockaddr*)&addr, len) < 0) {
	reply_err_errno(port);
	return -1;
    }

    /* Limit access to the pipe */
    chmod(path, S_IRUSR|S_IWUSR);


    /* An OS who shall remain unnamed creates files with owner set to euid
     * instead of uid. Undo the damage.
     */
    if (chown(path, getuid(), getgid()) < 0)
	perror("chown");

    listen(s, 1);


    return s;
}

int fd_accept_path(ErlDrvPort port, int s, char* path)
{
    int c;
    int len = 0;

    /* Need to make this non blocking .. */

    if ((c = accept(s, (struct sockaddr *)NULL, &len)) < 0) {
	reply_err_errno(port);
	return -1;
    }
    return c;
}


/*
 * Function to send a file descriptor, fd, over a stream socket, sock_fd.
 * This function is more or less directly from Stevens.
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

int send_fd(fd, sock_fd)
    int *fd;
    int sock_fd;
{
    struct msghdr msg;
    struct iovec iov[1];
#ifdef HAVE_MSGHDR_MSG_CONTROL
    union {
	struct cmsghdr cm;
	char control[CMSG_SPACE(sizeof(int))];
    } control_un;
    struct cmsghdr *cmptr;

    msg.msg_control = control_un.control;
    msg.msg_controllen = sizeof(control_un.control);

    cmptr = CMSG_FIRSTHDR(&msg);
    cmptr->cmsg_len = CMSG_LEN(sizeof(int));
    cmptr->cmsg_level = SOL_SOCKET;
    cmptr->cmsg_type = SCM_RIGHTS;
    *((int *) CMSG_DATA(cmptr)) = *fd;
#else
    msg.msg_accrights = (caddr_t) fd;
    msg.msg_accrightslen = sizeof(*fd);
#endif

    iov[0].iov_base = "";	/* send one byte */
    iov[0].iov_len = 1;
    msg.msg_iov = iov;
    msg.msg_iovlen = 1;
    msg.msg_name = NULL;
    msg.msg_namelen = 0;
    
    if (sendmsg(sock_fd, &msg, 0) < 0) {
	perror("sendmsg");
	return -1;
    }
    return 1;
}

/*
 * Parse an address specification and fill in a sockaddr_in accordingly.
 * The address should be on the form:
 *   [a.d.re.ss|hostname]:{portnumber|servicename}
 */
int fd_parse_addr(struct sockaddr_in *addr, char *str)
{
    int port = 0;
    char *cp;
    struct hostent *hp;
    struct servent *se;

    if ((cp = strrchr(str, (int)':')) != NULL)
        *cp++ = '\0';
    if (cp) {
        if (!isdigit((int)cp[0])) {
            if ((se = getservbyname(cp, "tcp")) != NULL) {
                port = ntohs(se->s_port);
	    } else {
		/* fprintf(stderr, "unknown port %s\n", cp); */
		return -1;
	    }
        } else {
            port = atoi(cp);
        }
    }
    if (port < 0 || port > 0xffff) {
	/* fprintf(stderr, "bad port number %d\n", port); */
        return -1;
    }
    
    bzero(addr, sizeof(*addr));
    addr->sin_family = AF_INET;
    addr->sin_port = htons(port);
    if (*str == '\000') {
	addr->sin_addr.s_addr = INADDR_ANY;
    } else {
	if ((addr->sin_addr.s_addr = inet_addr(str)) == INADDR_NONE) {
	    if ((hp = gethostbyname(str)) == NULL) {
		/*fprintf(stderr, "\"%s\" unknown host or address!\n", str);*/
		return -1;
	    } else {
		bcopy(hp->h_addr_list[0], &addr->sin_addr.s_addr,hp->h_length);
	    }
	}
    }
    return 0;
}



#ifdef DYNAMIC_DRIVER
void reply_int(ErlDrvPort port,  int i)
{
    char buf[5];
    buf[0] = 1;
    put_int32(i, buf+1);
    driver_output(port, buf, 5);
}

void reply_ok(ErlDrvPort port)
{
    char c = 1;
    driver_output(port, &c, 1);
}

void reply_err(ErlDrvPort port)
{
    char c = 0;
    driver_output(port, &c, 1);
}

void reply_err_string(ErlDrvPort port, char* s)
{
    char buf[BUFSIZ];
    buf[0] = 0;
    strncpy(buf+1, s, BUFSIZ-1);
    
    driver_output(port, buf, strlen(buf+1) + 1);
}

void reply_err_errno(ErlDrvPort port)
{
    char buf[BUFSIZ];
    buf[0] = 0;
    strncpy(buf+1, strerror(errno), BUFSIZ-1);
    driver_output(port, buf, strlen(buf+1) + 1);
}
#else
void reply_err_errno(ErlDrvPort port)
{
    perror("");
}

#endif

