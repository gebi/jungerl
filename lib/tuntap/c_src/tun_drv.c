/* Tunnel device linked-in driver.
   See /usr/src/linux/Documentation/networking/tuntap.txt

   Written by Luke Gorrie <luke@bluetail.com> in November 2001, and
   updated in February 2003 to support TAP interfaces and the new
   Erlang driver interface. */

#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include <net/if.h>
#include <sys/ioctl.h>
#include <assert.h>

#include <linux/if_tun.h>

#include "erl_driver.h"

#define REPLY_ERROR 0
#define REPLY_OK    1

#define REQUEST_GET_DEVICE 0
#define REQUEST_WRITE      1

/* Generous buffer size for reading single ethernet frames.

   FIXME: Do we need to worry about bigger packets, e.g. if we have a
   giant MTU in 'tap' or receive pre-defragmented IP packets in
   'tun'? */
#define TUNNEL_BUF_SIZE 2048

/*
 *
 * Helpers
 *
 */

/* parse driver argument string: "tunnel_drv <tun|tap> [device_name]" */
static int parse_args(char *str, int *mode, char *dev_name, int max_name_len)
{
    /* skip to start of first argument */
    for (; *str != ' '; str++) { if (*str == '\0') return 0; }
    for (; *str == ' '; str++) { if (*str == '\0') return 0; }
    
    /* looking at "tun" or "tap" - parse and move over */
    if (strncmp(str, "tun", 3) == 0) {
        *mode = IFF_TUN;
    } else if (strncmp(str, "tap", 3) == 0) {
        *mode = IFF_TAP;
    } else {
        return 0;
    }
    str += 3;
    /* optional device name - skip any whitespace, then copy */
    while (*str == ' ') str++;
    strncpy(dev_name, str, max_name_len);

    return 1;
}

/* make a tunnel interface */
static int make_if(int mode, char *dev) {
    struct ifreq ifr;
    int fd, err;
    
    if ((fd = open("/dev/net/tun", O_RDWR)) < 0) return -1;

    memset(&ifr, 0, sizeof(ifr));
    ifr.ifr_flags = mode | IFF_NO_PI;
    if( *dev )
        strncpy(ifr.ifr_name, dev, IFNAMSIZ);

    if( (err = ioctl(fd, TUNSETIFF, (void *) &ifr)) < 0 ){
        close(fd);
        return err;
    }
    strcpy(dev, ifr.ifr_name);
    return fd;
}


/*
 *
 * erl_driver interface
 *
 */

struct tun_state {
    ErlDrvPort port;
    int fd;
    char dev[IFNAMSIZ];
    char buf[TUNNEL_BUF_SIZE];
};

static int ctl_reply(int rep, char *buf, int len, char **rbuf, int rsize)
{
    char* ptr;

    if ((len+1) > rsize) {
	ptr = (char *)sys_alloc(len+1);
        assert(ptr);
	*rbuf = ptr;
    }
    else
	ptr = *rbuf;
    *ptr++ = rep;
    memcpy(ptr, buf, len);
    return len+1;
}

static int tun_ctl(ErlDrvData data,
                   unsigned int cmd,
                   char *buf,
                   int len,
                   char **rbuf,
                   int rsize)
{
    struct tun_state *state = (struct tun_state *)data;
    switch (cmd) {
    case REQUEST_GET_DEVICE:
        return ctl_reply(REPLY_OK, state->dev, strlen(state->dev), rbuf, rsize);
    case REQUEST_WRITE:
        /* sure hope this doesn't block.. */
        if (write(state->fd, buf, len) > 0) {
            return ctl_reply(REPLY_OK, "", 0, rbuf, rsize);
        } else {
            return ctl_reply(REPLY_ERROR, "", 0, rbuf, rsize);
        }
    default:
        return ctl_reply(REPLY_ERROR, "", 0, rbuf, rsize);
    }
}

static void tun_input(ErlDrvData data, ErlDrvEvent nil)
{
    struct tun_state *state = (struct tun_state *)data;
    int len;

    len = read(state->fd, state->buf, TUNNEL_BUF_SIZE);
    if (len > 0) {
        driver_output(state->port, state->buf, len);
    }
}

static void tun_stop(ErlDrvData data)
{
    struct tun_state *state = (struct tun_state *)data;
    driver_select(state->port, (ErlDrvEvent)state->fd, DO_READ, 0);
    close(state->fd);
    sys_free(state);
}

static ErlDrvData tun_start(ErlDrvPort port, char *args)
{
    struct tun_state *state;
    int fd;

    int mode;
    char dev_name[IFNAMSIZ];

    state = (struct tun_state*) sys_alloc(sizeof(struct tun_state));
    if (state == NULL) {
        errno = ENOMEM;         /* appropriate to set errno? */
        return ERL_DRV_ERROR_ERRNO;
    }

    if (!parse_args(args, &mode, state->dev, IFNAMSIZ - 1)) {
        return ERL_DRV_ERROR_BADARG;
    }

    fd = make_if(mode, state->dev);
    if (fd < 0) {
        return ERL_DRV_ERROR_GENERAL;
    }
    state->port = port;
    state->fd = fd;

    driver_select(port, (ErlDrvEvent)fd, DO_READ, 1);

    return (ErlDrvData)state;
}

static int tun_init(void)
{
    return 0;
}

ErlDrvEntry tun_driver_entry;

ErlDrvEntry *driver_init(void)
{
    memset(&tun_driver_entry, 0, sizeof(tun_driver_entry));
    tun_driver_entry.init = tun_init;
    tun_driver_entry.start = tun_start;
    tun_driver_entry.stop = tun_stop;
    tun_driver_entry.ready_input = tun_input;
    tun_driver_entry.control = tun_ctl;
    tun_driver_entry.driver_name = "tun_drv";
    return &tun_driver_entry;
}

