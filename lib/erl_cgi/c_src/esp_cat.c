/*
 * esp_cat [ <port-number> ]
 *
 * 1. send environment
 * 2. proxy stdin/stdout to a socket in an erlang node
 *
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/poll.h>

extern char** environ;

#define DEFAULT_CGI_PORT 3344
#define ENV_DATA         1
#define ENV_END          0

int read_write(int rfd, int wfd)
{
    char buffer[2048];
    int n;

    if ((n = read(rfd, buffer, sizeof(buffer))) < 0) {
	perror("read");
	return -1;
    }
    if (n > 0) {
	if ((n = write(wfd, buffer, n)) < 0) {
	    perror("write");
	    return -1;
	}
    }
    return n;
}

int main(int argc, char** argv)
{
    int fd;
    int i;
    struct sockaddr_in laddr;
    struct sockaddr_in raddr;
    struct pollfd      ufds[3];
    char hbuf[5];

    laddr.sin_family = AF_INET;
    laddr.sin_port = 0;
    laddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);

    raddr.sin_family = AF_INET;
    raddr.sin_port   = htons(DEFAULT_CGI_PORT);
    raddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);

    if (argc == 2)
	raddr.sin_port = htons(atoi(argv[1]));

    /* setup socket */
    if ((fd = socket(PF_INET, SOCK_STREAM, 0)) < 0) {
	perror("socket");
	exit(1);
    }

    if (bind(fd, (struct sockaddr*) &laddr, sizeof(laddr)) < 0) {
	perror("bind");
	exit(1);
    }

    if (connect(fd, (struct sockaddr*) &raddr, sizeof(raddr)) < 0) {
	perror("connect");
	exit(1);
    }

    i = 0;
    while(environ[i] != NULL) {
	int len = strlen(environ[i]);
	int hlen = len + 1;
	hbuf[0] = (hlen>>24) & 0xff;
	hbuf[1] = (hlen>>16) & 0xff;
	hbuf[2] = (hlen>>8) & 0xff;
	hbuf[3] = (hlen) & 0xff;
	hbuf[4] = ENV_DATA;
	write(fd, hbuf, 5);
	write(fd, environ[i], len);
	i++;
    }
    hbuf[0] = 0;
    hbuf[1] = 0;
    hbuf[2] = 0;
    hbuf[3] = 1;
    hbuf[4] = ENV_END;
    write(fd, hbuf, 5);

    ufds[0].fd = 0;   /* stdin  */
    ufds[0].events = POLLIN;

    ufds[1].fd = fd;  /* socket */
    ufds[1].events = POLLIN;

    ufds[2].fd = 1;   /* stdout */
    ufds[2].events = POLLOUT;

    while(1) {
	int k = poll(ufds, 2, -1);
	int n;
	if (k < 0) {
	    perror("poll");
	    exit(1);
	}

	if (ufds[0].revents & POLLIN) {
	    if ((n = read_write(ufds[0].fd, ufds[1].fd)) < 0)
		exit(1);
	    if (n == 0) {
		shutdown(fd, SHUT_WR);
		ufds[0].events = 0;
	    }
	}

	if (ufds[1].revents & POLLIN) {
	    if ((n = read_write(ufds[1].fd, 1)) < 0) 
		exit(1);
	    if (n == 0) {
		close(fd);
		exit(0);
	    }
	}
    }
}

