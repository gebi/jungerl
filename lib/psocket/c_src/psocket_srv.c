/** -*- mode: c; mode: outline-minor; outline-regexp: "/\\*\\*+" -*- */

/* PF_PACKET socket port program. */
/** Includes */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <features.h>    /* for the glibc version number */
#include <netinet/in.h>
#if __GLIBC__ >= 2 && __GLIBC_MINOR >= 1
#include <netpacket/packet.h>
#include <net/ethernet.h>     /* the L2 protocols */
#else
#include <asm/types.h>
#include <linux/if_packet.h>
#include <linux/if_ether.h>   /* The L2 protocols */
#endif
#include <linux/if.h>
#include <linux/if_ether.h>
#include <linux/if_arp.h>
#include <linux/sockios.h>

#define ETHH_SIZE 14
#define ETHADDR_SIZE 6          /* Ethernet addresses are 6 bytes (48 bits) */

#define BUFSIZE 2048

#define NIPQUAD(x) (x[0]), (x[1]), (x[2]), (x[3])

/* Command constants from Erlang */

#define CMD_WRITE 0
#define CMD_JOIN  1
#define CMD_DROP  2

#ifndef SIOCSPACKETPREBRIDGE
/* This ioctl is a special addition to our kernel. The constant is
   defined here so that you can build against a normal kernel. */
#define SIOCSPACKETPREBRIDGE 0x89D0  /* Make a PACKET socket run before the bridge */
#endif


/** Generic erlang-port-program helpers (mostly nicked off Klacke) */

#define get_int16(s) ((((unsigned char*)  (s))[0] << 8) |       \
                      (((unsigned char*)  (s))[1]))


#define put_int16(i, s) {((unsigned char*)(s))[0] = ((i) >> 8) & 0xff;  \
        ((unsigned char*)(s))[1] = (i)         & 0xff;}

/* #define DEBUG 1 */

/* the 'require' macro is like 'assert', but a require-failure doesn't
   mean a program bug, just that it can't continue. The technical
   difference is that 'require' exits silently unless debug-compiled. */
#ifdef DEBUG
#define require assert
#else
#define require(condition)  if (!(condition)) { exit(1); }
#endif

#define impossible require(0)

static int read_fill(int fd, char *buf, int len)
{
    int i, got = 0;

    do {
        if ((i = read(fd, buf+got, len-got)) <= 0) {
            require (i != 0);
            if (errno != EINTR)
                return got;
            i = 0;
        }
        got += i;
    } while (got < len);
    return (len);
}

static int write_fill(int fd, char *buf, int len)
{
    int i, done = 0; 
    
    do {
        if ((i = write(fd, buf+done, len-done)) < 0) {
            require (errno == EINTR);
            i = 0;
        }
        done += i;
    } while (done < len);
    return (len);
}


/** System call helpers */

/* Get the index of an interface by name. */
static int iface_get_id(int sock_fd, const char *device)
{
    struct ifreq ifr;

    memset(&ifr, 0, sizeof(ifr));
    strncpy(ifr.ifr_name, device, sizeof(ifr.ifr_name));

    if (ioctl(sock_fd, SIOCGIFINDEX, &ifr) == -1) {
        return -1;
    }

    return ifr.ifr_ifindex;
}

/* Return true if the device `device' running. */
static int device_running(int sock_fd, const char *device)
{
    struct ifreq ifr;

    memset(&ifr, 0, sizeof(ifr));
    strncpy(ifr.ifr_name, device, sizeof(ifr.ifr_name));

    if (ioctl(sock_fd, SIOCGIFFLAGS, &ifr) == -1) {
        return 0;
    }

    return ifr.ifr_flags & IFF_RUNNING;
}

static int read_from_erlang(char *buffer)
{
    int len;
    read_fill(0, buffer, 2);
    len = get_int16(buffer);
    require (len > 0);
    read_fill(0, buffer, len);
    return len;
}

static int read_from_socket(int sock_fd, char *buffer)
{
    int len = recv(sock_fd, buffer, BUFSIZE, 0);
    require (len > 0);
    return len;
}

static void write_to_erlang(char *buffer, int len)
{
    char lenbuf[2];
    put_int16(len, lenbuf);
    write_fill(1, lenbuf, 2);
    write_fill(1, buffer, len);
}

static void write_to_socket(int sock_fd, int ifindex, char *buffer, int len)
{
    struct sockaddr_ll lladdr;
    struct sockaddr *addr = (struct sockaddr *)&lladdr;
    lladdr.sll_family = PF_PACKET;
    lladdr.sll_ifindex = ifindex;
    while (sendto(sock_fd, buffer, len, 0, addr, sizeof(lladdr)) == -1)
        require (errno == EINTR);
}


/** Main loop */

static void select_loop(int sock_fd, int ifindex)
{
    fd_set rfds;
    char buffer[BUFSIZE];

    while (1) {
        FD_ZERO(&rfds);
        FD_SET(sock_fd, &rfds);
        FD_SET(0, &rfds);
        select(sock_fd + 1, &rfds, NULL, NULL, NULL);
        if (FD_ISSET(sock_fd, &rfds)) {
            int bytes = read_from_socket(sock_fd, buffer);
            write_to_erlang(buffer, bytes);
        }
        if (FD_ISSET(0, &rfds)) {
            int bytes = read_from_erlang(buffer);
            int control = buffer[0];
            char *data = &buffer[1];
            if (control == CMD_WRITE)
                write_to_socket(sock_fd, ifindex, data, bytes-1);
            else {
                struct packet_mreq m;
                int option;
                m.mr_ifindex = ifindex;
                m.mr_type = PACKET_MR_MULTICAST;
                m.mr_alen = ETHADDR_SIZE;
                memcpy(&m.mr_address, data, ETHADDR_SIZE);
                if (control == CMD_JOIN)      option = PACKET_ADD_MEMBERSHIP;
                else if (control == CMD_DROP) option = PACKET_DROP_MEMBERSHIP;
                else impossible;
                if (setsockopt(sock_fd, SOL_PACKET, option, &m, sizeof(m)) != 0) {
                    perror("multicast setsockopt");
                    exit(1);
                }
            }
        }
    }
}

void setup(int sock_fd)
{
    struct sockaddr_ll addr;
    int n;

    /* Bail out unless the socket is actually readable (interface has to be up) */
    require ((recv(sock_fd, NULL, 0, MSG_PEEK|MSG_DONTWAIT)) != -1 || errno == EAGAIN);

    /* Write MAC address. */
    n = sizeof(addr);
    require (getsockname(sock_fd, (struct sockaddr *)&addr, &n) == 0);
    write_to_erlang((char*)&addr.sll_addr, ETHADDR_SIZE);
}

int main(int argc, char** argv)
{
    int index;
    int sock_fd;
    struct sockaddr_ll from;
    int flags;

    if (argc != 2) {
        fprintf(stderr, "Usage: %s <interface>\n", argv[0]);
        exit(1);
    }

    sock_fd = socket(PF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
    if (sock_fd == -1) {
        perror("socket");
        exit(1);
    }

    index = iface_get_id(sock_fd, argv[1]);
    if (index == -1) {
        perror("get interface id");
        exit(1);
    }

    from.sll_family = PF_PACKET;
    from.sll_protocol = htons(ETH_P_ALL);
    from.sll_ifindex = index;
    if (bind(sock_fd, (struct sockaddr *)&from, sizeof(from)) != 0) {
        perror("bind");
        exit(1);
    }

    /* Make sure the interface is running */
    require (device_running(sock_fd, argv[1]));

    setup(sock_fd);
    select_loop(sock_fd, index);

    return 0;
}

