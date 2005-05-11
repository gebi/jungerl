/*
 * Created: klacke@erix.ericsson.se 
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include <sys/un.h>

#include "erl_driver.h"
#ifndef ERL_DRV_NIL
#include "erl_driver_compat.h"
#endif

#include "fdlib.h"

/* forward declarations */
static ErlDrvData start(ErlDrvPort port, char *buf);
static void stop(ErlDrvData drv_data);
static void from_erlang(ErlDrvData drv_data, char* buf, int len);

/* driver global variables */
static int instance = -1;

static ErlDrvEntry fdsend_drv_entry;

#define MAXSLAVES 64

#define LISTEN 'L'
#define ACCEPT 'A'

struct slave {
    int  fd;
    char *path;
    char type;
} slaves[MAXSLAVES];


static int find_free_slave()
{
    int i;
    for(i=0; i<MAXSLAVES; i++) {
	if (slaves[i].fd == -1)
	    return i;
    }
    return -1;
}

static int find_slave(int fd)
{
    int i;
    for(i=0; i<MAXSLAVES; i++) {
	if (slaves[i].fd == fd)
	    return i;
    }
    return -1;
}

DRIVER_INIT(fdsend_drv)
{
    fdsend_drv_entry.init         = NULL;
    fdsend_drv_entry.start        = start;
    fdsend_drv_entry.stop         = stop;
    fdsend_drv_entry.output       = from_erlang;
    fdsend_drv_entry.ready_input  = NULL;
    fdsend_drv_entry.ready_output = NULL;
    fdsend_drv_entry.driver_name  = "fdsend_drv";
    fdsend_drv_entry.finish       = NULL;
    fdsend_drv_entry.outputv      = NULL;
    return &fdsend_drv_entry;
}


/*
 * Called when erlang side does "open_port()".
 */
static ErlDrvData start(ErlDrvPort port, char *buf)
{
    int i;
    if (instance != -1)		/* only allow one copy at the time */
	return (ErlDrvData) -1;
    for (i =0; i< MAXSLAVES; i++) {
	slaves[i].fd = -1;
	slaves[i].path = NULL;
    }
    return (ErlDrvData)port;
}
	    

static void from_erlang(ErlDrvData drv_data, char* buf, int n)
{
    ErlDrvPort port = (ErlDrvPort) drv_data;
    char *tp;
    int i, j;
    int slavefd, fd, afd;

    switch (*buf++) {
    case 'L':   /*  Bind and listen to new af_unix socket to path */
	fd = fd_listen_path(port, buf);

	if ((i = find_free_slave()) == -1) {
	    reply_err(port);
	    return;
	}
	
	if (fd > 0) {
	    if ((tp = (char*) driver_alloc(n)) != NULL) {
		slaves[i].type = LISTEN;
		slaves[i].path = tp;
		strcpy(slaves[i].path, buf);
		slaves[i].fd = fd;
		reply_int(port, fd);
		return;
	    }
	}
	reply_err(port);
	return;

    case 'A':  /* accept the af_unix path socket */
	fd = get_int32(buf);

	if ((i = find_slave(fd)) == -1) {
	    reply_err(port);
	    return;
	}
	if ((j = find_free_slave()) == -1) {
	    reply_err(port);
	    return;
	}
	tp = slaves[i].path;	
	if ((afd = fd_accept_path(port, fd, tp)) > 0) {

	    slaves[j].path = tp;
	    slaves[j].type = ACCEPT;
	    slaves[j].fd = afd;
	    reply_int(port, afd);
	}
	else {
	    reply_err(port);
	}
	return;
    case 'l':
    case 'S':   /* send a socket to slave */
	fd = get_int32(buf);
	slavefd = get_int32(buf+4);

	if (send_fd(&fd, slavefd) == -1) 
	    reply_err(port);
	else {
	    reply_ok(port);
	}
	return;
    case 'C':  /* close */
	fd = get_int32(buf);
	if ((i = find_slave(fd)) == -1) {
	    reply_err(port);
	    return;
	}
	if (slaves[i].path && slaves[i].type == LISTEN)
	    driver_free(slaves[i].path);
	slaves[i].path = NULL;
	close(fd);
	slaves[i].fd = -1;
	reply_ok(port);
	return;
    }
    return;
}



/*
 * Called when the Erlang port is closed.
 */
static void stop(ErlDrvData drv_data)
{
    int i;
    for (i=0; i< MAXSLAVES; i++) {
	if (slaves[i].fd > 0) {
	    if (slaves[i].type == LISTEN)
		driver_free(slaves[i].path);
	    slaves[i].path = NULL;
	    close(slaves[i].fd);
	}
    }
    instance = -1;
}

