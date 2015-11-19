
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/un.h>

#include <my-unixdom.h>

void
null(void)
{
    printf("Hello, world!  This is null()\r\n");
}

#define IS_SERVER   1
#define IS_ABSTRACT 2
#define IS_NULLTERM 4

int
my_open(char *path, int flags)
{
    struct stat		sb;
    struct sockaddr_un	sin;
    size_t		sin_len;
    int			s;
    int			res;
    int			is_server = !!(flags & IS_SERVER);
    int			is_abstract = !!(flags & IS_ABSTRACT);
    int			is_nullterm = !!(flags & IS_NULLTERM);
    
    /* Race condition exists, but is better than nothing */
    if (is_server && !is_abstract && stat(path, &sb) == 0) {
	errno = EEXIST;
	return -1;
    }
    if ((s = socket(PF_LOCAL, SOCK_STREAM, 0)) < 0) {
	return -1;
    }

    memset(&sin, 0, sizeof(sin));
    sin.sun_family = AF_UNIX;
    /* Path to abstract unix domain contains \0 in first position. */
    /* TODO abstract path doesn't need to be null terminated! */
    if (is_abstract) {
	sin.sun_path[0] = '\0';
	strncpy(sin.sun_path+1, path, sizeof(sin.sun_path)-1);
    } else {
	strncpy(sin.sun_path, path, sizeof(sin.sun_path));
    }

    if (is_nullterm) {
	if (is_abstract)
	    sin_len = sizeof(sin) - sizeof(sin.sun_path)
		+ strlen(sin.sun_path+1) + 1;
	else
	    sin_len = sizeof(sin) - sizeof(sin.sun_path)
		+ strlen(sin.sun_path);
    } else
	sin_len = sizeof(sin);

#if	defined(SUN_LEN) && (! (defined(solaris) || defined(linux)))
    sin.sun_len = SUN_LEN(&sin);
#endif	/* SUN_LEN */

    if (is_server) {
	res = bind(s, (struct sockaddr *) &sin, sin_len);
    } else {
	res = connect(s,  (struct sockaddr *) &sin, sin_len);
    }
    if (res < 0) {
	return -1;
    } else {
	return s;
    }
}

int
my_getfd(int fd)
{
    return fd;
}

int
my_sendfd(int fd, int wfd)
{
    if (writefd(fd, wfd) >= 0) {
	return 0;
    } else {
	return -1;
    }
}

int
my_receivefd(int fd)
{
    int		passed_fd = -42;

    if (readfd(fd, &passed_fd) < 0) {
	return -1;
    } else {
	return passed_fd;
    }
}

/*
** My interpretation of Steven's file descriptor passing code.
*/

#include	<stdio.h>
#include	<unistd.h>
#include	<assert.h>
#include	<sys/types.h>
#include	<sys/socket.h>
#include	<sys/un.h>
#include	<sys/stat.h>
#include	<signal.h>
#include	<errno.h>
#include	<stdlib.h>
#include	<netinet/in.h>			/* htonl() et al */
#ifndef		TCP_NODELAY
#include	<netinet/tcp.h>
#endif		/* TCP_NODELAY */
#include	<sys/uio.h>

ssize_t
writevfd(int fd, const struct iovec *iov, int iovcnt, int wfd)
{
    struct msghdr mh;
    char cmhbuf[sizeof(struct cmsghdr) + sizeof(int)];
    struct cmsghdr *const cmh = (struct cmsghdr *) cmhbuf;
    int *const fdp = (int *) (cmhbuf + sizeof(struct cmsghdr));

    *fdp = wfd;
    memset(&mh, 0, sizeof(mh));
    mh.msg_iov = (struct iovec *) iov;
    mh.msg_iovlen = iovcnt;

    mh.msg_control = (char *) cmh;
    mh.msg_controllen = cmh->cmsg_len = sizeof(cmhbuf) /* + sizeof(int) */;
    cmh->cmsg_level = SOL_SOCKET;
    cmh->cmsg_type = SCM_RIGHTS;

    return sendmsg(fd, &mh, 0);
}

ssize_t
writefd(int fd, int wfd)
{
    void *buf = "b";		/* Gotta send at least 1 byte along with msg */
    size_t len = 1;
    struct iovec iov[1];

    iov->iov_base = (void *) buf;
    iov->iov_len = len;
    return writevfd(fd, iov, 1, wfd);
}


ssize_t
readvfd(int fd, const struct iovec *iov, int iovcnt, int *rfdp)
{
    struct msghdr mh;
    char cmhbuf[sizeof(struct cmsghdr) + sizeof(int)];
    struct cmsghdr *const cmh = (struct cmsghdr *) cmhbuf;
    int *const fdp = (int *) (cmhbuf + sizeof(struct cmsghdr));
    int n;

    *fdp = -1;
    memset(&mh, 0, sizeof(mh));
    mh.msg_iov = (struct iovec *) iov;
    mh.msg_iovlen = iovcnt;

    mh.msg_control = (char *) cmh;
    mh.msg_controllen = cmh->cmsg_len = sizeof(cmhbuf) /* + sizeof(int) */;
    cmh->cmsg_level = SOL_SOCKET;
    cmh->cmsg_type = SCM_RIGHTS;

    n = recvmsg(fd, &mh, 0);
    *rfdp = *fdp;
    return n;
}

ssize_t
readfd(int fd, int *rfdp)
{
    char readbuf[128];
    size_t len = sizeof(readbuf);
    struct iovec iov[1];

    iov->iov_base = readbuf;
    iov->iov_len = len;
    return readvfd(fd, iov, 1, rfdp);
}
