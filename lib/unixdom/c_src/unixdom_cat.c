#include	<stdio.h>
#include	<assert.h>
#include	<sys/types.h>
#include	<sys/socket.h>
#include	<sys/un.h>
#include	<sys/stat.h>
#include	<fcntl.h>
#include	<errno.h>
#include	<unistd.h>

#if     defined(__sun)
#define PF_LOCAL        AF_UNIX
/* XXX not needed? typedef uint32_t socklen_t; */
#endif  /* __sun */

void do_cat(int);

int
main(int argc, char *argv[])
{
    char		*path = argv[1];
    struct stat		sb;
    int			s;
    struct sockaddr_un	sin;

    if (argc != 2) {
	fprintf(stderr, "usage: %s /path/to/unix-dom-socket\n", argv[0]);
	exit(1);
    }
    if ((s = socket(PF_LOCAL, SOCK_STREAM, 0)) < 0) {
	perror("socket(PF_LOCAL, SOCK_STREAM");
	exit(2);
    }
    if (stat(path, &sb) < 0) {
	fprintf(stderr, "Socket %s does not exist, exiting!\n", path);
	exit(4);
    }

    sin.sun_family = AF_UNIX;
    strcpy(sin.sun_path, path);
#if	defined(SUN_LEN) && (! (defined(solaris) || defined(linux)))
    sin.sun_len = SUN_LEN(&sin);
#endif	/* SUN_LEN */
    if (connect(s, (struct sockaddr *) &sin, sizeof(sin)) < 0) {
	perror("connect");
	exit(5);
    }

    do_cat(s);
    close(s);
    exit(0);
}

void
do_cat(s)
{
    char	buf[1024];
    int		bytes;
    int		flags;
    int		doit = 1;
    
    /* Set stdin nonblocking */
    if ((flags = fcntl(0, F_GETFL, 0)) < 0) {
	perror("fcntl 1");
	exit(20);
    }
    flags |= O_NONBLOCK;
    if (fcntl(0, F_SETFL, flags) < 0) {
	perror("fcntl 2");
	exit(21);
    }
    /* Set socket nonblocking */
    if ((flags = fcntl(s, F_GETFL, 0)) < 0) {
	perror("fcntl 1");
	exit(20);
    }
    flags |= O_NONBLOCK;
    if (fcntl(s, F_SETFL, flags) < 0) {
	perror("fcntl 2");
	exit(21);
    }

    while (doit) {
	if ((bytes = read(0, buf, 1024)) > 0) {
	    /* XXX Ja, incomplete writes are possible here */
	    write(s, buf, bytes);
	}
	if (bytes < 0 && errno != EWOULDBLOCK)
	    doit = 0;
	if ((bytes = read(s, buf, 1024)) > 0) {
	    /* XXX Ja, incomplete writes are possible here */
	    write(1, buf, bytes);
	}
	if (bytes < 0 && errno != EWOULDBLOCK)
	    doit = 0;
	usleep(100000);
    }
    fprintf(stderr, "XXX cat finished, closing socket\n");
}
