/*
** Copyright (c) 2003, Scott Lystig Fritchie.  All rights reserved.
** See the file "../LICENSE" for license details.
*/

#include <erl_driver.h>

/*
** ErlIOVec manipulation functions, stolen from efile_drv.c
*/

/* char *EV_CHAR_P(ErlIOVec *ev, int p, int q) */
#define EV_CHAR_P(ev, p, q) \
    (((char *)(ev)->iov[(q)].iov_base) + (p))

/* char *EV_UCHAR_P(ErlIOVec *ev, int p, int q) */
#define EV_UCHAR_P(ev, p, q) \
    (((unsigned char *)(ev)->iov[(q)].iov_base) + (p))

/* int EV_GET_CHAR(ErlIOVec *ev, char *p, int *pp, int *qp) */
#define EV_GET_CHAR(ev, p, pp, qp) \
    (*(pp)+1 <= (ev)->iov[*(qp)].iov_len \
     ? (*(p) = *EV_CHAR_P(ev, *(pp), *(qp)), \
        *(pp) = (*(pp)+1 < (ev)->iov[*(qp)].iov_len \
                 ? *(pp)+1 \
                 : ((*(qp))++, 0)), \
        !0) \
     : 0)

#define EV_GET_UINT8(ev, p, pp, qp)	EV_GET_CHAR(ev, p, pp, qp)

/* int EV_GET_UINT16(ErlIOVec *ev, Uint32 *p, int *pp, int *qp) */
#define EV_GET_UINT16(ev, p, pp, qp) \
    (*(pp)+2 <= (ev)->iov[*(qp)].iov_len \
     ? (*((unsigned short *) p) = (*EV_UCHAR_P(ev, *(pp), *(qp)) << 8)\
                | *EV_UCHAR_P(ev, *(pp)+1, *(qp)), \
        *(pp) = (*(pp)+2 < (ev)->iov[*(qp)].iov_len \
                 ? *(pp)+2 \
                 : ((*(qp))++, 0)), \
        !0) \
     : 0)

/* int EV_GET_UINT32(ErlIOVec *ev, Uint32 *p, int *pp, int *qp) */
#define EV_GET_UINT32(ev, p, pp, qp) \
    (*(pp)+4 <= (ev)->iov[*(qp)].iov_len \
     ? (*((unsigned long *) p) = (*EV_UCHAR_P(ev, *(pp), *(qp)) << 24) \
                | (*EV_UCHAR_P(ev, *(pp)+1, *(qp)) << 16) \
                | (*EV_UCHAR_P(ev, *(pp)+2, *(qp)) << 8)\
                | *EV_UCHAR_P(ev, *(pp)+3, *(qp)), \
        *(pp) = (*(pp)+4 < (ev)->iov[*(qp)].iov_len \
                 ? *(pp)+4 \
                 : ((*(qp))++, 0)), \
        !0) \
     : 0)

/* char * EV_GETPOS(ErlIOVec *ev, int p, int q) */
#define EV_GETPOS(ev, p, q) \
    ((q) < (ev)->vsize \
    ? ((ev)->iov[(q)].iov_base + p) \
    : NULL)

/*********************************************************************
 * Term manipulation functions, stolen from inet_drv.c
 */

#define LOAD_NIL(vec, i) \
  (((vec)[(i)] = ERL_DRV_NIL), \
  (i+1))

#define LOAD_ATOM(vec, i, atom) \
  (((vec)[(i)] = ERL_DRV_ATOM), \
  ((vec)[(i)+1] = (atom)), \
  (i+2))

#define LOAD_INT(vec, i, val) \
  (((vec)[(i)] = ERL_DRV_INT), \
  ((vec)[(i)+1] = (ErlDrvTermData)(val)), \
  (i+2))

#define LOAD_PORT(vec, i, port) \
  (((vec)[(i)] = ERL_DRV_PORT), \
  ((vec)[(i)+1] = (port)), \
  (i+2))

#define LOAD_PID(vec, i, pid) \
  (((vec)[(i)] = ERL_DRV_PID), \
  ((vec)[(i)+1] = (pid)), \
  (i+2))

#define LOAD_BINARY(vec, i, bin, offs, len) \
  (((vec)[(i)] = ERL_DRV_BINARY), \
  ((vec)[(i)+1] = (ErlDrvTermData)(bin)), \
  ((vec)[(i)+2] = (len)), \
  ((vec)[(i)+3] = (offs)), \
  (i+4))

#define LOAD_STRING(vec, i, str, len) \
  (((vec)[(i)] = ERL_DRV_STRING), \
  ((vec)[(i)+1] = (ErlDrvTermData)(str)), \
  ((vec)[(i)+2] = (len)), \
  (i+3))

#define LOAD_STRING_CONS(vec, i, str, len) \
  (((vec)[(i)] = ERL_DRV_STRING_CONS), \
  ((vec)[(i)+1] = (ErlDrvTermData)(str)), \
  ((vec)[(i)+2] = (len)), \
  (i+3))

#define LOAD_TUPLE(vec, i, size) \
  (((vec)[(i)] = ERL_DRV_TUPLE), \
  ((vec)[(i)+1] = (size)), \
  (i+2))

#define LOAD_LIST(vec, i, size) \
  (((vec)[(i)] = ERL_DRV_LIST), \
  ((vec)[(i)+1] = (size)), \
  (i+2))

#define get_int32(s) ((((unsigned char*) (s))[0] << 24) | \
                      (((unsigned char*) (s))[1] << 16)  | \
                      (((unsigned char*) (s))[2] << 8) | \
                      (((unsigned char*) (s))[3]))

#define DEBUGIO         stderr

#define	PIPE_DRIVER_TERM_NIL	0
#define	PIPE_DRIVER_TERM_ATOM	1
#define	PIPE_DRIVER_TERM_PORT	2
#define	PIPE_DRIVER_TERM_INT	3
#define	PIPE_DRIVER_TERM_TUPLE	4
#define	PIPE_DRIVER_TERM_BINARY	5
#define	PIPE_DRIVER_TERM_STRING	6
#define	PIPE_DRIVER_TERM_LIST	7

/* Forward declarations */
struct descriptor;
typedef struct descriptor descriptor_t;

extern int edtk_debug_flag;	/* For verbose debugging trace messages */

/* Wrapper functions for driver_{alloc,realloc,free}_binary */
void *edtk_driver_alloc_wrapper(size_t);
void *edtk_driver_realloc_wrapper(void *, size_t);
void edtk_driver_free_wrapper(void *);
ErlDrvBinary *edtk_alloced_ptr2ErlDrvBinary(void *);

/* Where are these from? */
void *sys_alloc(size_t);
void *sys_realloc(void *, size_t);
void sys_free(void *);

int edtk_debug(char *f, ...);
void edtk_debug_errcall(const char *, char *);
void edtk_free_data(void *data);

int edtk_ev_forward_N(ErlIOVec *ev, int n, int *pp, int *qp, int);

