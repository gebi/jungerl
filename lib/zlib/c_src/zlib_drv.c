/*
 * ZLib interface for erlang
 *
 */
#include <stdio.h>
#include <zlib.h>
#include <errno.h>

#include "erl_driver.h"


#define DEFLATE_INIT    1
#define DEFLATE_INIT2   2
#define DEFLATE_SETDICT 3
#define DEFLATE_RESET   4
#define DEFLATE_END     5
#define DEFLATE_PARAMS  6
#define DEFLATE         7

#define INFLATE_INIT    8
#define INFLATE_INIT2   9
#define INFLATE_SETDICT 10
#define INFLATE_SYNC    11
#define INFLATE_RESET   12
#define INFLATE_END     13
#define INFLATE         14

#define CRC32_0         15
#define CRC32_1         16
#define CRC32_2         17

#define SET_BUFSZ       18
#define GET_BUFSZ       19
#define GET_QSIZE       20

#define DEFAULT_BUFSZ   4000

#ifdef LOADABLE
static void zlib_finish(void);
#endif

static int zlib_init(void);
static ErlDrvData zlib_start(ErlDrvPort port, char* buf);
static void zlib_stop(ErlDrvData e);
static int zlib_ctl(ErlDrvData drv_data, unsigned int command, char *buf, 
		    int len, char **rbuf, int rlen);
static void zlib_outputv(ErlDrvData drv_data, ErlIOVec *ev);

ErlDrvEntry zlib_driver_entry = {
    zlib_init,
    zlib_start,
    zlib_stop,
    NULL,
    NULL,
    NULL,
    "zlib_drv",
    zlib_finish,
    NULL,
    zlib_ctl,
    NULL,
    zlib_outputv,
    NULL,
    NULL,
    NULL,
    NULL
};

typedef enum {
    ST_NONE    = 0,
    ST_DEFLATE = 1,
    ST_INFLATE = 2
} ZLibState;


typedef struct {
    z_stream s;
    ZLibState state;
    ErlDrvBinary* bin;
    int binsz;
    int binsz_need;
    uLong crc;
    int   want_crc;     /* 1 if crc is calculated on clear text */
    ErlDrvPort port;    /* the associcated port */
} ZLibData;


static inline int i32(char* buf)
{
    return (int) (
		  (((unsigned char*)buf)[0] << 24) |
		  (((unsigned char*)buf)[1] << 16) |
		  (((unsigned char*)buf)[2] << 8) |
		  (((unsigned char*)buf)[3] << 0));
}

static char* zlib_reason(int code, int* err)
{
    switch(code) {
    case Z_OK:
	*err = 0;
	return "ok";
    case Z_STREAM_END:
	*err = 0;
	return "stream_end"; 
    case Z_ERRNO:
	*err = 1;
	return erl_errno_id(errno);
    case Z_STREAM_ERROR:
	*err = 1;
	return "stream";
    case Z_DATA_ERROR:
	*err = 1;
	return "data";
    case Z_MEM_ERROR:
	*err = 1;
	return "mem";
    case Z_BUF_ERROR:
	*err = 1;
	return "buf";
    case Z_VERSION_ERROR:
	*err = 1;
	return "version";
    default:
	*err = 1;
	return "unknown";
    }
}


static int zlib_return(int code, char** rbuf, int rlen)
{
    int msg_code = 0; /* 0=ok, 1=error */
    char* dst = *rbuf;
    char* src;
    int len = 0;

    src = zlib_reason(code, &msg_code);
    *dst++ = msg_code;
    rlen--;
    len = 1;

    while((rlen > 0) && *src) {
	*dst++ = *src++;
	rlen--;
	len++;
    }
    return len;
}

static int zlib_value(int value, char** rbuf, int rlen)
{
    int msg_code = 2; /* 2 = integer */
    char* dst = *rbuf;
    char* src;
    int len = 0;

    if (rlen  < 5)
	return -1;
    *dst++ = msg_code;
    *dst++ = (value >> 24) & 0xff;
    *dst++ = (value >> 16) & 0xff;
    *dst++ = (value >> 8) & 0xff;
    *dst++ = (value >> 0) & 0xff;
    return 5;
}

static int zlib_failure(ZLibData* d, int code)
{
    int dummy;

    return driver_failure_atom(d->port, zlib_reason(code, &dummy));
}


static int zlib_output_init(ZLibData* d)
{
    if (d->bin != NULL)
	driver_free_binary(d->bin);
    if ((d->bin = driver_alloc_binary(d->binsz_need)) == NULL)
	return -1;
    d->binsz = d->binsz_need;
    d->s.next_out = d->bin->orig_bytes;
    d->s.avail_out = d->binsz;
    return 0;
}

/*
 * Send compressed or uncompressed data
 * and restart output procesing
 */
static int zlib_output(ZLibData* d)
{
    if (d->bin != NULL) {
	int len = d->binsz - d->s.avail_out;
	if (len > 0) {
	    if (driver_output_binary(d->port, NULL, 0, d->bin, 0, len) < 0) 
		return -1;
	}
	driver_free_binary(d->bin);
	d->bin = NULL;
	d->binsz = 0;
    }
    return zlib_output_init(d);
}

int zlib_inflate(ZLibData* d, int flush)
{
    int res = Z_OK;
    int sz;

    if ((d->bin == NULL) && (zlib_output_init(d) < 0)) {
	errno = ENOMEM;
	return Z_ERRNO;
    }

    while ((driver_sizeq(d->port) > 0) && (res != Z_STREAM_END)) {
	int vlen;
	SysIOVec* iov = driver_peekq(d->port, &vlen);
	int len;

	d->s.next_in = iov[0].iov_base;
	d->s.avail_in = iov[0].iov_len;

	while((d->s.avail_in > 0) && (res != Z_STREAM_END)) {
	    res = inflate(&d->s, Z_NO_FLUSH);
	    if (res < 0)
		return res;
	    if (d->s.avail_out == 0) {
		if (d->want_crc)
		    d->crc = crc32(d->crc, d->bin->orig_bytes,
				   d->binsz - d->s.avail_out);
		zlib_output(d);
	    }
	}
	len = iov[0].iov_len - d->s.avail_in;
	driver_deq(d->port, len);
    }

    if (flush != Z_NO_FLUSH) {
	if ((res = inflate(&d->s, flush)) < 0)
	    return res;
	while(d->s.avail_out < d->binsz) {
	    if (d->want_crc)
		d->crc = crc32(d->crc, d->bin->orig_bytes,
			       d->binsz - d->s.avail_out);
	    zlib_output(d);
	    if (res == Z_STREAM_END)
		break;
	    if ((res = inflate(&d->s, flush)) < 0)
		return res;
	}
    }
    return res;
}

int zlib_deflate(ZLibData* d, int flush)
{
    int res = Z_OK;
    int sz;

    if ((d->bin == NULL) && (zlib_output_init(d) < 0)) {
	errno = ENOMEM;
	return Z_ERRNO;
    }

    while ((driver_sizeq(d->port) > 0) && (res != Z_STREAM_END)) {
	int vlen;
	SysIOVec* iov = driver_peekq(d->port, &vlen);
	int len;

	d->s.next_in = iov[0].iov_base;
	d->s.avail_in = iov[0].iov_len;

	while((d->s.avail_in > 0) && (res != Z_STREAM_END)) {
	    if ((res = deflate(&d->s, Z_NO_FLUSH)) < 0)
		return res;
	    if (d->s.avail_out == 0)
		zlib_output(d);
	}
	len = iov[0].iov_len - d->s.avail_in;
	if (d->want_crc)
	    d->crc = crc32(d->crc, iov[0].iov_base, len);
	driver_deq(d->port, len);
    }

    if (flush != Z_NO_FLUSH) {
	if ((res = deflate(&d->s, flush)) < 0)
	    return res;
	while(d->s.avail_out < d->binsz) {
	    zlib_output(d);
	    if (res == Z_STREAM_END)
		break;
	    if ((res = deflate(&d->s, flush)) < 0)
		return res;
	}
    }
    return res;
}



static void* zlib_alloc(void* data, unsigned int items, unsigned int size)
{
    return (void*) driver_alloc(items*size);
}

static void zlib_free(void* data, void* addr)
{
    driver_free(addr);
}


#ifdef LOADABLE
static void zlib_finish(void)
{
}

DRIVER_INIT(zlib_drv)
{
    zlib_driver_entry.handle = NULL;
    zlib_driver_entry.driver_name = "zlib_drv";
    zlib_driver_entry.finish = zlib_finish;
    zlib_driver_entry.init = zlib_init;
    zlib_driver_entry.start = zlib_start;
    zlib_driver_entry.stop = zlib_stop;
    zlib_driver_entry.output = NULL;
    zlib_driver_entry.outputv = zlib_outputv;
    zlib_driver_entry.control= zlib_ctl;
    zlib_driver_entry.ready_input = NULL;
    zlib_driver_entry.ready_output = NULL;
    return &zlib_driver_entry;
}

#endif

static int zlib_init()
{
    return 0;
}

static ErlDrvData zlib_start(ErlDrvPort port, char* buf)
{
    ZLibData* d;

    if ((d = (ZLibData*) driver_alloc(sizeof(ZLibData))) == NULL)
        return ERL_DRV_ERROR_GENERAL;

    memset(&d->s, 0, sizeof(z_stream));

    d->s.zalloc = zlib_alloc;
    d->s.zfree  = zlib_free;
    d->s.opaque = d;
    d->s.data_type = Z_BINARY;

    d->port      = port;
    d->state     = ST_NONE;
    d->bin       = NULL;
    d->binsz     = 0;
    d->binsz_need = DEFAULT_BUFSZ;
    d->crc       = crc32(0L, Z_NULL, 0);
    d->want_crc  = 0;
    return (ErlDrvData)d;
}


static void zlib_stop(ErlDrvData e)
{
    ZLibData* d = (ZLibData*)e;

    if (d->state == ST_DEFLATE)
	deflateEnd(&d->s);
    else if (d->state == ST_INFLATE)
	inflateEnd(&d->s);

    if (d->bin != NULL)
	driver_free_binary(d->bin);

    driver_free(d);
}

static int zlib_ctl(ErlDrvData drv_data, unsigned int command, char *buf, 
		    int len, char **rbuf, int rlen)
{
    ZLibData* d = (ZLibData*)drv_data;
    int res;


    switch(command) {
    case DEFLATE_INIT:
	if (len != 4) goto badarg;
	if (d->state != ST_NONE) goto badarg;
	res = deflateInit(&d->s, i32(buf));
	if (res == Z_OK) {
	    d->state = ST_DEFLATE;
	    d->want_crc = 0;
	    d->crc = crc32(0L, Z_NULL, 0);
	}
	return zlib_return(res, rbuf, rlen);

    case DEFLATE_INIT2: {
	int wbits;

	if (len != 20) goto badarg;
	if (d->state != ST_NONE) goto badarg;
	wbits = i32(buf+8);
	res = deflateInit2(&d->s, i32(buf), i32(buf+4), wbits, 
			   i32(buf+12), i32(buf+16));
	if (res == Z_OK) {
	    d->state = ST_DEFLATE;
	    d->want_crc = (wbits < 0);
	    d->crc = crc32(0L, Z_NULL, 0);
	}
	return zlib_return(res, rbuf, rlen);
    }
	
    case DEFLATE_SETDICT:
	if (d->state != ST_DEFLATE) goto badarg;
	res = deflateSetDictionary(&d->s, buf, len);
	return zlib_return(res, rbuf, rlen);

    case DEFLATE_RESET:
	if (len != 0) goto badarg;
	if (d->state != ST_DEFLATE) goto badarg;
	driver_deq(d->port, driver_sizeq(d->port));
	res = deflateReset(&d->s);
	return zlib_return(res, rbuf, rlen);	
	
    case DEFLATE_END:
	if (len != 0) goto badarg;
	if (d->state != ST_DEFLATE) goto badarg;
	driver_deq(d->port, driver_sizeq(d->port));
	res = deflateEnd(&d->s);
	d->state = ST_NONE;
	return zlib_return(res, rbuf, rlen);

    case DEFLATE_PARAMS:
	if (len != 8) goto badarg;
	if (d->state != ST_DEFLATE) goto badarg;
	res = deflateParams(&d->s, i32(buf), i32(buf+4));
	return zlib_return(res, rbuf, rlen);

    case DEFLATE:
	if (d->state != ST_DEFLATE) goto badarg;
	if (len != 4) goto badarg;
	res = zlib_deflate(d, i32(buf));
	return zlib_return(res, rbuf, rlen);

    case INFLATE_INIT:
	if (len != 0) goto badarg;
	if (d->state != ST_NONE) goto badarg;
	res = inflateInit(&d->s);
	if (res == Z_OK) {
	    d->state = ST_INFLATE;
	    d->want_crc = 0;
	    d->crc = crc32(0L, Z_NULL, 0);
	}
	return zlib_return(res, rbuf, rlen);	
	
    case INFLATE_INIT2: {
	int wbits;

	if (len != 4) goto badarg;	
	if (d->state != ST_NONE) goto badarg;
	wbits = i32(buf);
	res = inflateInit2(&d->s, wbits);
	if (res == Z_OK) {
	    d->state = ST_INFLATE;
	    d->want_crc = (wbits < 0);
	    d->crc = crc32(0L, Z_NULL, 0);
	}
	return zlib_return(res, rbuf, rlen);
    }
	
    case INFLATE_SETDICT:
	if (d->state != ST_INFLATE) goto badarg;
	res = inflateSetDictionary(&d->s, buf, len);
	return zlib_return(res, rbuf, rlen);

    case INFLATE_SYNC:
	if (d->state != ST_INFLATE) goto badarg;
	if (len != 0) goto badarg;
	res = inflateSync(&d->s);
	return zlib_return(res, rbuf, rlen);

    case INFLATE_RESET:
	if (d->state != ST_INFLATE) goto badarg;
	if (len != 0) goto badarg;
	driver_deq(d->port, driver_sizeq(d->port));
	res = inflateReset(&d->s);
	return zlib_return(res, rbuf, rlen);

    case INFLATE_END:
	if (d->state != ST_INFLATE) goto badarg;
	if (len != 0) goto badarg;
	driver_deq(d->port, driver_sizeq(d->port));
	res = inflateEnd(&d->s);
	d->state = ST_NONE;
	return zlib_return(res, rbuf, rlen);

    case INFLATE:
	if (d->state != ST_INFLATE) goto badarg;
	if (len != 4) goto badarg;
	res = zlib_inflate(d, i32(buf));
	return zlib_return(res, rbuf, rlen);

    case GET_QSIZE:
	return zlib_value(driver_sizeq(d->port), rbuf, rlen);

    case GET_BUFSZ:
	return zlib_value(d->binsz_need, rbuf, rlen);

    case SET_BUFSZ: {
	int need;
	if (len != 4) goto badarg;
	need = i32(buf);
	if ((need < 16) || (need > 0x00ffffff))
	    goto badarg;
	if (d->binsz_need != need) {
	    d->binsz_need = need;
	    if (d->bin != NULL) {
		if (d->s.avail_out == d->binsz) {
		    driver_free_binary(d->bin);
		    d->bin = NULL;
		    d->binsz = 0;
		}
		else
		    zlib_output(d);
	    }
	}
	return zlib_return(Z_OK, rbuf, rlen);
    }

    case CRC32_0:
	return zlib_value(d->crc, rbuf, rlen);

    case CRC32_1: {
	uLong crc = crc32(0L, Z_NULL, 0);
	crc = crc32(crc, buf, len);
	return zlib_value(crc, rbuf, rlen);
    }
	
    case CRC32_2: {
	uLong crc;
	if (len < 4) goto badarg;
	crc = i32(buf);
	crc = crc32(crc, buf+4, len-4);
	return zlib_value(crc, rbuf, rlen);
    }
    }

 badarg:
    errno = EINVAL;
    return zlib_return(Z_ERRNO, rbuf, rlen);
}


static void zlib_outputv(ErlDrvData drv_data, ErlIOVec *ev)
{
    ZLibData* d = (ZLibData*) drv_data;

    driver_enqv(d->port, ev, 0);
}
