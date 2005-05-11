

/* should be, but isn't on all systems, defined in <netinet/in.h> */
#ifndef INADDR_NONE
#define INADDR_NONE 0xffffffff
#endif




int fd_parse_addr();
int  send_fd(int*, int);
int fd_establish_communication(ErlDrvPort, char*);
int fd_accept_path(ErlDrvPort, int, char*);
int fd_listen_path(ErlDrvPort, char*);


#define get_int32(s) ((((unsigned char*) (s))[0] << 24) | \
                      (((unsigned char*) (s))[1] << 16) | \
                      (((unsigned char*) (s))[2] << 8)  | \
                      (((unsigned char*) (s))[3]))

#define put_int32(i, s) {((char*)(s))[0] = (char)((i) >> 24) & 0xff; \
                        ((char*)(s))[1] = (char)((i) >> 16) & 0xff; \
                        ((char*)(s))[2] = (char)((i) >> 8)  & 0xff; \
                        ((char*)(s))[3] = (char)((i)        & 0xff);}

void reply_ok(ErlDrvPort);
void reply_err(ErlDrvPort);
void reply_err_string(ErlDrvPort, char*);
void reply_err_errno(ErlDrvPort);
void reply_int(ErlDrvPort, int);
