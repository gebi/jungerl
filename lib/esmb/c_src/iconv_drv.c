/* Created     : 23 Mar 2004 by Tobbe <tobbe@bluetail.com>
 * Description : iconv driver - conversion between character sets
 * 
 * $Id$
 */
#include <stdio.h>
#include <string.h>
#include <iconv.h>

#include "erl_driver.h"
#ifndef ERL_DRV_NIL
#include "erl_driver_compat.h"
#endif

/* op codes */
#define IV_OPEN       'o'
#define IV_CONV       'v'
#define IV_CLOSE      'c'

/* convert buffers */
#define INBUF_SZ 512
#define OUTBUF_SZ INBUF_SZ*4
static char inbuf[INBUF_SZ];
static char outbuf[OUTBUF_SZ];


/* these should really be defined in driver.h */
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

static int driver_send_bin();

/* atoms which are sent to erlang */
static ErlDrvTermData am_ok;
static ErlDrvTermData am_value;
static ErlDrvTermData am_error;
static ErlDrvTermData am_enomem;
static ErlDrvTermData am_einval;
static ErlDrvTermData am_unknown;

static ErlDrvEntry iconvdrv_driver_entry;

typedef struct t_iconvdrv {
    ErlDrvPort     port;
    ErlDrvTermData dport;          /* the port identifier as ErlDrvTermData */
    unsigned char digest[16];
} t_iconvdrv;


static ErlDrvData iconvdrv_start(ErlDrvPort port, char *buf)
{
    t_iconvdrv *iconv = (t_iconvdrv*) driver_alloc(sizeof(t_iconvdrv));

    if (iconv == NULL) return (ErlDrvData) -1;
    iconv->port = port;
    iconv->dport = driver_mk_port(port);
    return (ErlDrvData) iconv;
}

static void iconvdrv_stop(ErlDrvData drv_data)
{
    t_iconvdrv *iv = (t_iconvdrv*) drv_data;
    driver_free(iv);
}


/* send {P, value, Bin} to caller */
static int driver_send_bin(t_iconvdrv *iv, ErlDrvBinary *bin, int len)
{
    int i = 0;
    ErlDrvTermData to, spec[10];

    to = driver_caller(iv->port);

    i = LOAD_PORT(spec, i, iv->dport);
    i = LOAD_ATOM(spec, i, am_value);
    i = LOAD_BINARY(spec, i, bin, 0, len);
    i = LOAD_TUPLE(spec, i, 3);

    return driver_send_term(iv->port, to, spec, i);
}

/* send {P, ok} to caller */
static int driver_send_ok(t_iconvdrv *iv)
{
    int i = 0;
    ErlDrvTermData to, spec[10];

    to = driver_caller(iv->port);

    i = LOAD_PORT(spec, i, iv->dport);
    i = LOAD_ATOM(spec, i, am_ok);
    i = LOAD_TUPLE(spec, i, 2);

    return driver_send_term(iv->port, to, spec, i);
}

/* send {P, error, Error} to caller */
static int driver_send_error(t_iconvdrv *iv, ErlDrvTermData *am)
{
    int i = 0;
    ErlDrvTermData to, spec[8];

    to = driver_caller(iv->port);

    i = LOAD_PORT(spec, i, iv->dport);
    i = LOAD_ATOM(spec, i, am_error);
    i = LOAD_ATOM(spec, i, *am);
    i = LOAD_TUPLE(spec, i, 3);

    return driver_send_term(iv->port, to, spec, i);
}

#define CODE_STR_SZ  64

#define get_int16(s) ((((unsigned char*)  (s))[0] << 8) | \
                      (((unsigned char*)  (s))[1]))


#define put_int16(i, s) {((unsigned char*)(s))[0] = ((i) >> 8) & 0xff; \
                        ((unsigned char*)(s))[1] = (i)         & 0xff;}

static void iv_open(t_iconvdrv *iv, char *tocode, char *fromcode)
{
    int len;
    iconv_t *cd;
    ErlDrvBinary *bin;

    if ((*cd = iconv_open(tocode, fromcode)) == (iconv_t) -1) {
	driver_send_error(iv, &am_einval);
    }
    else {
	len = sizeof(iconv_t);
	if (!(bin = driver_alloc_binary(len))) {
	    driver_send_error(iv, &am_enomem);
	}
	else {
	    memcpy(bin->orig_bytes, cd, len);
	    driver_send_bin(iv, bin, len);
	    driver_free_binary(bin);
	}
    }

    return;
}

static void iv_conv(t_iconvdrv *iv, iconv_t cd, char *ip, int ileft)
{
    int oleft=OUTBUF_SZ;
    char *op;
    int len;
    ErlDrvBinary *bin;

    op = &outbuf[0];

    /* Reset cd to initial state */
    iconv(cd, NULL, NULL, NULL, NULL);

    if (iconv(cd, &ip, &ileft, &op, &oleft) == (size_t) -1) {
	fprintf(stderr, "iconv failed \n");
    }
    else if (ileft == 0) {
	len = OUTBUF_SZ - oleft;
	if (!(bin = driver_alloc_binary(len))) {
	    driver_send_error(iv, &am_enomem);
	}
	else {
	    memcpy(bin->orig_bytes, &outbuf[0], len);
	    driver_send_bin(iv, bin, len);
	    driver_free_binary(bin);
	}
    }

    return;
}

static void iv_close(t_iconvdrv *iv, iconv_t cd)
{
    iconv_close(cd);
    driver_send_ok(iv);
    return;
}

static void iconvdrv_from_erlang(ErlDrvData drv_data, char *buf, int len)
{
    t_iconvdrv *iv = (t_iconvdrv *) drv_data;
    char tocode[CODE_STR_SZ], fromcode[CODE_STR_SZ];
    char *bp=buf;
    unsigned int i=0;
    iconv_t cd;

    i = bp[0];
    bp++;
    switch (i) {

    case IV_OPEN: {
	/*
	 * Format: <to-len:16><tocode><from-len:16><from-buf>
	 */
	i = get_int16(bp);
	bp += 2;
	memcpy(tocode, bp, i);
	tocode[i] = '\0';
	bp += i;
	i = get_int16(bp);
	bp += 2;
	memcpy(fromcode, bp, i);
	fromcode[i] = '\0';

	iv_open(iv, tocode, fromcode);
	break;
    }

    case IV_CONV: {
	/*
	 * Format: <cd-len:16><cd><buf-len:16><buf>
	 */
	i = get_int16(bp);
	bp += 2;
	memcpy(&cd, bp, i);
	bp += i;
	i = get_int16(bp);
	bp += 2;

	iv_conv(iv, cd, bp, i);
	break;
    }

    case IV_CLOSE: {
	/*
	 * Format: <cd-len:16><cd>
	 */
	i = get_int16(bp);
	bp += 2;
	memcpy(&cd, bp, i);

	iv_close(iv, cd);
	break;
    }

    } /* switch */

    return;
}
    

/*
 * Initialize and return a driver entry struct
 */

DRIVER_INIT(iconvdrv)
{
  am_ok           = driver_mk_atom("ok");
  am_value        = driver_mk_atom("value");
  am_error        = driver_mk_atom("error");
  am_enomem       = driver_mk_atom("enomem");
  am_einval       = driver_mk_atom("einval");
  am_unknown      = driver_mk_atom("unknown");

  iconvdrv_driver_entry.init         = NULL;   /* Not used */
  iconvdrv_driver_entry.start        = iconvdrv_start;
  iconvdrv_driver_entry.stop         = iconvdrv_stop;
  iconvdrv_driver_entry.output       = iconvdrv_from_erlang;
  iconvdrv_driver_entry.ready_input  = NULL;
  iconvdrv_driver_entry.ready_output = NULL;
  iconvdrv_driver_entry.driver_name  = "iconv_drv";
  iconvdrv_driver_entry.finish       = NULL;
  iconvdrv_driver_entry.outputv      = NULL;
  return &iconvdrv_driver_entry;
}
