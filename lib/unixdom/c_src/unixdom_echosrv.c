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

int verbose = 0;
int read_only = 0;
int sleep_time = 0;
int backlog = 5;

#if	defined(__sun)
#define	PF_LOCAL	AF_UNIX
/* XXX not needed? typedef uint32_t socklen_t; */
#endif	/* __sun */

void echo_server(int, int);
void do_echo(int, int);

#define	USAGE	"usage: %s [-b listen_backlog] [-erv] [-s sleeptime] /path/to/unix-dom-socket\n"

int
main(int argc, char *argv[])
{
    char		*path, *argv0 = argv[0], ch;
    struct stat		sb;
    int			s;
    int			delay_echo = 0;
    struct sockaddr_un	sin;

    while ((ch = getopt(argc, argv, "b:ers:v")) != -1) {
	switch (ch) {
	case 'b':
	    backlog = atoi(optarg);
	    break;
	case 'e':
	    delay_echo = 1;
	    break;
	case 'r':
	    read_only = 1;
	    break;
	case 's':
	    sleep_time = atoi(optarg);
	    break;
	case 'v':
	    verbose = 1;
	    break;
	default:
	    fprintf(stderr, USAGE, argv0);
	    exit(1);
	}
    }
    argc -= optind;
    argv += optind;
    path = *argv;
    if (path == NULL) {
	fprintf(stderr, USAGE, argv0);
	exit(1);
    }
    if ((s = socket(PF_LOCAL, SOCK_STREAM, 0)) < 0) {
	perror("socket(PF_LOCAL, SOCK_STREAM");
	exit(2);
    }
    if (stat(path, &sb) == 0) {
	if (S_ISSOCK(sb.st_mode)) {
	    unlink(path);
	} else {
	    fprintf(stderr, "File %s exists and isn't a socket, exiting!\n", path);
	    exit(4);
	}
    }

    sin.sun_family = AF_UNIX;
    strcpy(sin.sun_path, path);
#if	defined(SUN_LEN) && (! (defined(solaris) || defined(linux)))
    sin.sun_len = SUN_LEN(&sin);
#endif	/* SUN_LEN */
    if (bind(s, (struct sockaddr *) &sin, sizeof(sin)) < 0) {
	perror("bind");
	exit(5);
    }

    signal(SIGPIPE, SIG_IGN);
    echo_server(s, delay_echo);
    /* NOTREACHED, in theory */
    exit(0);
}

void
echo_server(int ms, int delay_echo)
{
    int			s;
#ifdef	XXX_NOT_USED
    socklen_t		len;
    struct sockaddr	sin;
#endif	/* XXX_NOT_USED */

    if (listen(ms, backlog) < 0) {
	perror("listen");
	exit(20);
    }

#ifdef	XXX_NOT_USED
    /*
    ** This was originally written on a Solaris/SPARC platform,
    ** I think.  This pukes at runtime on my Solaris 7/x86
    ** box here at home, failing with EFAULT.  I don't understand,
    ** but passing a couple of NULL pointers appears to work.
    */
    while ((s = accept(ms, &sin, &len)) >= 0)
#endif	/* XXX_NOT_USED */
    while ((s = accept(ms, NULL, NULL)) >= 0) {
	fprintf(stderr, "Connection accepted\n");
	do_echo(s, delay_echo);
	if (sleep_time > 0) {
	    fprintf(stderr, "Sleeping for %d seconds...", sleep_time);
	    sleep(sleep_time);
	    fprintf(stderr, "done\n");
	}
    }
    fprintf(stderr, "We've exited the accept loop, returning!\n");
}

void
do_echo(int s, int delay_echo)
{
    char	buf[1024];
    int		bytes;
    char	*msg = "Echo: ";

    while ((bytes = read(s, buf, 1024)) > 0) {
	/* XXX Ja, incomplete writes are possible here */
	if (!read_only)
		write(s, buf, bytes);
	if (verbose) {
	    fprintf(stderr, "Echo server: got %d bytes: ", bytes);
	    fwrite(buf, 1, bytes, stderr);
	}
	if (delay_echo && !read_only) {
	    if (fork() == 0) {
		sleep(1);
		write(s, msg, strlen(msg));
		write(s, buf, bytes);
		exit(0);
	    }
	}
    }
    if (bytes < 0) {
	fprintf(stderr, "XXX read error, errno = %d\n", errno);
    }
    fprintf(stderr, "XXX echo instance finished, closing socket\n");
    close(s);
}
