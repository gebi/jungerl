#include	<stdio.h>
#include	<unistd.h>
#include	<assert.h>
#include	<sys/types.h>
#include	<sys/socket.h>
#include	<sys/un.h>
#include	<sys/stat.h>
#include	<signal.h>
#include	<errno.h>
#include	<netinet/in.h>
#include	<netinet/tcp.h>
#include	<netdb.h>
#include	<string.h>
#include	<stdlib.h>

int verbose = 0;
int read_only = 0;

void echo_server(int, int);
void do_echo(int, int);

#define	USAGE	"usage: %s [-erv] hostname-to-bind portnum-to-bind\n"

int
main(int argc, char *argv[])
{
    char		*hostp, *portp, *argv0 = argv[0], ch;
    int			s;
    int			delay_echo = 0;
    struct sockaddr_in	caddr;
    struct hostent	*ph;
    u_long		sip;	/* IP address in network byte order */
    int			one = 1;

    while ((ch = getopt(argc, argv, "erv")) != -1) {
	switch (ch) {
	case 'e':
	    delay_echo = 1;
	    break;
	case 'r':
	    read_only = 1;
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
    hostp = *argv;
    portp = *++argv;
    if (hostp == NULL || portp == NULL) {
	fprintf(stderr, USAGE, argv0);
	exit(1);
    }

    ph = gethostbyname(hostp);
    if (ph == NULL) {
	fprintf(stderr, "can't get address for hostname \"%s\": %s\n",
		hostp, strerror(errno));
	exit(1);
    }
    sip = *((u_long *) ph->h_addr_list[0]);

    /*
    ** Open TCP master side and wait for
    ** connection.
    */
    s = socket(PF_INET, SOCK_STREAM, 0);
    if (s == -1) {
	perror("can't open TCP listen socket");
	exit(1);
    }
    setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &one, sizeof (one));

    memset(&caddr, 0, sizeof (caddr));
    caddr.sin_family = AF_INET;
    caddr.sin_addr.s_addr = htonl(INADDR_ANY);
    caddr.sin_port = htons(atoi(portp));
    if (bind(s, (struct sockaddr *) &caddr, sizeof (caddr)) == -1) {
	perror("can't bind TCP listen socket");
	exit(1);
    }
    if (listen(s, 256) == -1) {
	perror("can't listen on TCP listen socket");
	exit(1);
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
    socklen_t		len;
    struct sockaddr	sin;

    if (listen(ms, 5) < 0) {
	perror("listen");
	exit(20);
    }
    while ((s = accept(ms, &sin, &len)) >= 0) {
	do_echo(s, delay_echo);
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
