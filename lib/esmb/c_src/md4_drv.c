/* Created     : 12 Mar 2004 by Tobbe <tobbe@bluetail.com>
 * Description : MD4 driver - API in md4.erl 
 * 
 * $Id$
 */
#include <stdio.h>
#include <string.h>

#include "erl_driver.h"
#ifndef ERL_DRV_NIL
#include "erl_driver_compat.h"
#endif

#include "global.h"
#include "md4.h"



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
static ErlDrvTermData am_value;
static ErlDrvTermData am_error;
static ErlDrvTermData am_enomem;
static ErlDrvTermData am_unknown;

static ErlDrvEntry md4drv_driver_entry;

typedef struct t_md4drv {
    ErlDrvPort     port;
    ErlDrvTermData dport;          /* the port identifier as ErlDrvTermData */
    MD4_CTX context;
    unsigned char digest[16];
} t_md4drv;


static ErlDrvData md4drv_start(ErlDrvPort port, char *buf)
{
    t_md4drv *md4 = (t_md4drv*) driver_alloc(sizeof(t_md4drv));

    if (md4 == NULL) return (ErlDrvData) -1;
    md4->port = port;
    md4->dport = driver_mk_port(port);
    return (ErlDrvData) md4;
}

static void md4drv_stop(ErlDrvData drv_data)
{
    t_md4drv *md4 = (t_md4drv*) drv_data;
    driver_free(md4);
}


/* send {P, value, Bin} to caller */
static int driver_send_bin(t_md4drv *md4, ErlDrvBinary *bin, int len)
{
    int i = 0;
    ErlDrvTermData to, spec[10];

    to = driver_caller(md4->port);

    i = LOAD_PORT(spec, i, md4->dport);
    i = LOAD_ATOM(spec, i, am_value);
    i = LOAD_BINARY(spec, i, bin, 0, len);
    i = LOAD_TUPLE(spec, i, 3);

    return driver_send_term(md4->port, to, spec, i);
}

/* send {P, error, Error} to caller */
static int driver_send_error(t_md4drv *md4, ErlDrvTermData *am)
{
    int i = 0;
    ErlDrvTermData to, spec[8];

    to = driver_caller(md4->port);

    i = LOAD_PORT(spec, i, md4->dport);
    i = LOAD_ATOM(spec, i, am_error);
    i = LOAD_ATOM(spec, i, *am);
    i = LOAD_TUPLE(spec, i, 3);

    return driver_send_term(md4->port, to, spec, i);
}

static void md4drv_from_erlang(ErlDrvData drv_data, char *buf, int len)
{
    MD4_CTX context;
    unsigned char digest[16];
    t_md4drv *md4 = (t_md4drv *) drv_data;
    ErlDrvBinary *bin = NULL;

    MD4Init(&context);
    MD4Update(&context, buf, len);
    MD4Final(digest, &context);

    if (!(bin = driver_alloc_binary(len))) {
	driver_send_error(md4, &am_enomem);
    }
    else {
	memcpy(bin->orig_bytes, digest, 16);
	driver_send_bin(md4, bin, 16);
	driver_free_binary(bin);
    }
    return;
}
    

/*
 * Initialize and return a driver entry struct
 */

DRIVER_INIT(md4drv)
{
  am_value        = driver_mk_atom("value");
  am_error        = driver_mk_atom("error");
  am_enomem       = driver_mk_atom("enomem");
  am_unknown      = driver_mk_atom("unknown");

  md4drv_driver_entry.init         = NULL;   /* Not used */
  md4drv_driver_entry.start        = md4drv_start;
  md4drv_driver_entry.stop         = md4drv_stop;
  md4drv_driver_entry.output       = md4drv_from_erlang;
  md4drv_driver_entry.ready_input  = NULL;
  md4drv_driver_entry.ready_output = NULL;
  md4drv_driver_entry.driver_name  = "md4_drv";
  md4drv_driver_entry.finish       = NULL;
  md4drv_driver_entry.outputv      = NULL;
  return &md4drv_driver_entry;
}
