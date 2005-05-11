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

#include "config.h"
#include "strs.h"
#include "memfs.h"



#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "erl_driver.h"
#ifndef ERL_DRV_NIL
#include "erl_driver_compat.h"
#endif

#include "fdlib.h"

static uid_t saved_euid;
static char *progname;

extern int errno;

/*
 * Send an error message to Erlang. (If msg is NULL, send errno error.)
 */

static void report_error(msg)
    char *msg;
{
    unsigned char c;
    
    if (!msg)
	msg = sys_errlist[errno];
    if (strlen(msg) > 255)
	c = 255;
    else
	c = (unsigned char)strlen(msg);
    write(1, &c, 1);
    write(1, msg, (int)c);
    return;
}

static void loop(fd)
    int fd;
{
    int i, r, s;
    char buf[256];
    struct sockaddr_in addr;

    for(;;) {
	/*
	 * Read address specs on stdin.
	 */
	if ((r = read(0, buf, 1)) == 0)
	    return;
	if (r != 1) {
	    perror("read");
	    exit(1);
	}
	i = (int)((unsigned int)buf[0]);
	if ((r = read(0, buf, i)) == 0)
	    return;
	if (r != i) {
	    perror("read");
	    exit(1);
	}
	buf[i] = 0;		/* null terminate string */

	/* buf[0] contains protocol opcode */
	if (fd_parse_addr(&addr, &buf[1]) < 0) {
	    report_error("Couldn't parse address");
	    continue;
	}

	/*
	 * Create the socket to be passed.
	 */
	if (buf[0] == 0) {
	    /* tcp */
	    if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
	        /* perror("socket"); */
	        report_error((char *)NULL);
		continue;
	    }
	    i = 1; setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *)&i,
			      sizeof(i));
	}
	else {
	    /* udp */
	    if ((s = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
	        /* perror("socket"); */
	        report_error((char *)NULL);
		continue;
	    }
	}

	/*
	 * Bind it according to spec.
	 */
	/*seteuid(saved_euid);*/  /* flip euid, XXX doesn't work on Solaris */
	r = bind(s, (struct sockaddr *)&addr, sizeof(addr));
	/* seteuid(getuid()); */

	if (r < 0) {
	    /* perror("bind"); */
	    report_error((char *)NULL);
	    close(s);
	    continue;
	}

	/*
	 * Send it away, and then close our copy.
	 */
	send_fd(&s, fd);
	close(s);
    }
}

int main(argc, argv)
    int argc;
    char *argv[];
{
    int fd;
    
    if (argc != 2)
	exit(1);

    saved_euid = geteuid();
    /* seteuid(getuid()); */

    if ((progname = strrchr(argv[0], (int)'/')) == NULL)
	progname = argv[0];
    else
	progname++;

    fd = fd_establish_communication(0, argv[1]);

    loop(fd);

    exit(0);
}
