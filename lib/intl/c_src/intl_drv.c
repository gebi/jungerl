/*
 * intl_drv:
 *    interface to gettext and friends
 *
 */
#include <stdio.h>
#include <errno.h>

#include <locale.h>
#include <libintl.h>

#include "erl_driver.h"

#define INTL_GETTEXT                 1
#define INTL_DGETTEXT                2
#define INTL_DCGETTEXT               3
#define INTL_NGETTEXT                4
#define INTL_DNGETTEXT               5
#define INTL_DCNGETTEXT              6
#define INTL_TEXTDOMAIN              7
#define INTL_BINDTEXTDOMAIN          8
#define INTL_BIND_TEXTDOMAIN_CODESET 9
#define INTL_SETLOCALE               10

#define INTL_OK       0
#define INTL_ERROR    1
#define INTL_INTEGER  2
#define INTL_UINTEGER 3
#define INTL_STRING   4

#define INTL_LC_CTYPE           0
#define INTL_LC_NUMERIC         1
#define INTL_LC_TIME            2
#define INTL_LC_COLLATE         3
#define INTL_LC_MONETARY        4
#define INTL_LC_MESSAGES        5
#define	INTL_LC_ALL             6
#ifndef __sun__
#define INTL_LC_PAPER           7
#define INTL_LC_NAME            8
#define INTL_LC_ADDRESS         9
#define INTL_LC_TELEPHONE	10
#define INTL_LC_MEASUREMENT	11
#define INTL_LC_IDENTIFICATION  12
#endif

#ifdef LOADABLE
static void intl_finish(void);
#endif

static int intl_init(void);
static ErlDrvData intl_start(ErlDrvPort port, char* buf);
static void intl_stop(ErlDrvData e);
static int intl_ctl(ErlDrvData drv_data, unsigned int command, char *buf,
                    int len, char **rbuf, int rlen);
static void intl_outputv(ErlDrvData drv_data, ErlIOVec *ev);

ErlDrvEntry intl_driver_entry = {
    intl_init,
    intl_start,
    intl_stop,
    NULL,
    NULL,
    NULL,
    "intl_drv",
    intl_finish,
    NULL,
    intl_ctl,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};

static inline int i32(char* buf)
{
    return (int) (
                  (((unsigned char*)buf)[0] << 24) |
                  (((unsigned char*)buf)[1] << 16) |
                  (((unsigned char*)buf)[2] << 8) |
                  (((unsigned char*)buf)[3] << 0));
}

static inline unsigned int u32(char* buf)
{
    return (unsigned int) (
                  (((unsigned char*)buf)[0] << 24) |
                  (((unsigned char*)buf)[1] << 16) |
                  (((unsigned char*)buf)[2] << 8) |
                  (((unsigned char*)buf)[3] << 0));
}

static char* intl_reason(int code, int* err)
{
    if (code == 0) {
	*err = INTL_OK;
	return "ok";
    }
    else {
	*err = INTL_ERROR;
	return erl_errno_id(code);
    }
}

static int ret_code(int code, char** rbuf, int rlen)
{
    int msg_code = INTL_OK;
    char* dst = *rbuf;
    char* src;
    int len = 0;

    src = intl_reason(code, &msg_code);
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

static int ret_integer(int value, char** rbuf, int rlen)
{
    int msg_code = INTL_INTEGER; /* 2 = integer */
    char* dst = *rbuf;
    char* src;

    if (rlen  < 5)
	return -1;
    *dst++ = msg_code;
    *dst++ = (value >> 24) & 0xff;
    *dst++ = (value >> 16) & 0xff;
    *dst++ = (value >> 8) & 0xff;
    *dst++ = (value >> 0) & 0xff;
    return 5;
}

static int ret_string(char* value, char** rbuf, int rlen)
{
    int msg_code = INTL_STRING; /* 2 = integer */
    char* dst    = *rbuf;
    char* src = value;
    int len = 0;

    if (src == NULL)
	return ret_code(errno, rbuf, rlen);

    if (rlen  < strlen(value)+1) {
	int alen = strlen(value)+1;
	if ((*rbuf = driver_alloc(alen)) == NULL)
	    return -1;
	dst = *rbuf;
    }
    *dst++ = msg_code;
    len++;
    while(*src) {
	*dst++ = *src++;
	len++;
    }
    return len;
}

static int get_string(char** ptr, int* len, char** value)
{
    char* src = *ptr;
    char* nptr;
    int   nlen = *len;

    if (*src++ != INTL_STRING)
	return -1;
    nptr = src;
    while((nlen > 0) && (*nptr != '\0')) {
	nptr++;
	nlen--;
    }
    if (nlen == 0)
	return -1;
    *ptr = ++nptr;
    *len = nlen - 1;
    *value = src;
    return 0;
}

static int get_integer(char** ptr, int* len, int* value)
{
    char* src = *ptr;

    if ((*src != INTL_INTEGER) || (*len < 5))
	return -1;
    *value = i32(src+1);
    *ptr = src+5;
    *len = *len-5;
    return 0;
}

static int get_uinteger(char** ptr, int* len, unsigned int* value)
{
    char* src = *ptr;

    if ((*src != INTL_UINTEGER) || (*len < 5))
	return -1;
    *value = u32(src+1);
    *ptr = src+5;
    *len = *len-5;
    return 0;
}

static int get_category(int ecode)
{
    switch(ecode) {
    case INTL_LC_CTYPE: return LC_CTYPE;
    case INTL_LC_NUMERIC: return LC_NUMERIC;
    case INTL_LC_TIME: return LC_TIME;
    case INTL_LC_COLLATE: return LC_COLLATE;
    case INTL_LC_MONETARY: return LC_MONETARY;
    case INTL_LC_MESSAGES: return LC_MESSAGES;
    case INTL_LC_ALL: return LC_ALL;
#ifndef __sun__
    case INTL_LC_PAPER: return LC_PAPER;
    case INTL_LC_NAME: return LC_NAME;
    case INTL_LC_ADDRESS: return LC_ADDRESS;
    case INTL_LC_TELEPHONE: return LC_TELEPHONE;
    case INTL_LC_MEASUREMENT: return LC_MEASUREMENT;
    case INTL_LC_IDENTIFICATION: return LC_IDENTIFICATION;
#endif
    default: return -1;
    }
}
	

#ifdef LOADABLE
static void intl_finish(void)
{
}

DRIVER_INIT(intl_drv)
{
    intl_driver_entry.handle = NULL;
    intl_driver_entry.driver_name = "intl_drv";
    intl_driver_entry.finish = intl_finish;
    intl_driver_entry.init = intl_init;
    intl_driver_entry.start = intl_start;
    intl_driver_entry.stop = intl_stop;
    intl_driver_entry.output = NULL;
    intl_driver_entry.outputv = NULL;
    intl_driver_entry.control= intl_ctl;
    intl_driver_entry.ready_input = NULL;
    intl_driver_entry.ready_output = NULL;
    return &intl_driver_entry;
}

#endif

static int intl_init()
{
    return 0;
}

static ErlDrvData intl_start(ErlDrvPort port, char* buf)
{
    return (ErlDrvData)port;
}


static void intl_stop(ErlDrvData e)
{
}


static int intl_ctl(ErlDrvData drv_data, unsigned int command, char *buf,
                    int len, char **rbuf, int rlen)
{
    void* d = (void*)drv_data;
    char* aptr = buf;
    int   alen = len;
    char* str1;
    char* str2;
    char* str3;
    int   int1;
    int   int2;
    unsigned int uint1;

    switch(command) {
    case INTL_GETTEXT:
	/* arguments: string msgid */
	if (get_string(&aptr, &alen, &str1) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	return ret_string(gettext(str1), rbuf, rlen);

    case INTL_NGETTEXT:
	/* arguments: string msgid, string msgid_plural, int n */
	if (get_string(&aptr, &alen, &str1) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	if (get_string(&aptr, &alen, &str2) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	if (get_uinteger(&aptr, &alen, &uint1) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	return ret_string(ngettext(str1,str2,uint1), rbuf, rlen);

    case INTL_DGETTEXT:
	/* arguments: string domain, string msgid */
	if (get_string(&aptr, &alen, &str1) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	if (get_string(&aptr, &alen, &str2) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	return ret_string(dgettext(str1,str2), rbuf, rlen);

    case INTL_DNGETTEXT:
	/* arguments: string domain, string msgid, 
	   string msgid_plural, integer n */
	if (get_string(&aptr, &alen, &str1) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	if (get_string(&aptr, &alen, &str2) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	if (get_string(&aptr, &alen, &str3) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	if (get_uinteger(&aptr, &alen, &uint1) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	return ret_string(dngettext(str1,str2,str3,uint1), rbuf, rlen);
	
    case INTL_DCGETTEXT:
	/* arguments: string domain, string msgid, int category */
	if (get_string(&aptr, &alen, &str1) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	if (get_string(&aptr, &alen, &str2) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	if (get_integer(&aptr,&alen, &int1) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	if ((int1 = get_category(int1)) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	return ret_string(dcgettext(str1,str2,int1),rbuf,rlen);

    case INTL_DCNGETTEXT:
	/* arguments: string domain, string msgid, 
	   strinf msgid_plural, unsigned n, int category */
	if (get_string(&aptr, &alen, &str1) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	if (get_string(&aptr, &alen, &str2) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	if (get_string(&aptr, &alen, &str3) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	if (get_uinteger(&aptr,&alen, &uint1) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	if (get_integer(&aptr,&alen, &int1) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	if ((int1 = get_category(int1)) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	return ret_string(dcngettext(str1,str2,str3,uint1,int1),rbuf,rlen);

    case INTL_TEXTDOMAIN:
	/* arguments: string domainname */
	if (len == 0)
	    return ret_string(textdomain(NULL), rbuf, rlen);
	if (get_string(&aptr, &alen, &str1) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	return ret_string(textdomain(str1), rbuf, rlen);

    case INTL_BINDTEXTDOMAIN:
	/* arguments: string domainname, string dirname */
	if (len == 0)
	    return ret_string(textdomain(NULL), rbuf, rlen);
	if (get_string(&aptr, &alen, &str1) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	if (get_string(&aptr, &alen, &str2) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	return ret_string(bindtextdomain(str1,str2), rbuf, rlen);

    case INTL_BIND_TEXTDOMAIN_CODESET:
	/* arguments: string domainname, string dirname */
	if (len == 0)
	    return ret_string(textdomain(NULL), rbuf, rlen);
	if (get_string(&aptr, &alen, &str1) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	if (get_string(&aptr, &alen, &str2) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	return ret_string(bind_textdomain_codeset(str1,str2), rbuf, rlen);

    case INTL_SETLOCALE:
	if (get_integer(&aptr,&alen, &int1) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	if ((int1 = get_category(int1)) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	if (get_string(&aptr, &alen, &str1) < 0)
	    return ret_code(EINVAL, rbuf, rlen);
	return ret_string(setlocale(int1,str1), rbuf, rlen);
    }
    return -1;
}






