/*
** Copyright (c) 2003, Scott Lystig Fritchie.  All rights reserved.
** See the file "../LICENSE" for license details.
*/

#include <stdio.h>
#include <stdarg.h>

#include <erl_driver.h>
#include <erl_driver_tk.h>

int edtk_debug_flag = 1;

void *
edtk_driver_alloc_wrapper(size_t size)
{
    ErlDrvBinary	*eb;

    edtk_debug("%s: top", __FUNCTION__);
    if ((eb = driver_alloc_binary(size)) != NULL) {
	edtk_debug("%s: size %lu eb = 0x%lx orig_bytes = 0x%lx", __FUNCTION__, size, eb, eb->orig_bytes);
	return eb->orig_bytes;
    } else {
	return NULL;
    }
}

void *
edtk_driver_realloc_wrapper(void *p, size_t size)
{
    ErlDrvBinary	*eb, *neweb;

    edtk_debug("%s: top", __FUNCTION__);
    if ((eb = edtk_alloced_ptr2ErlDrvBinary(p)) != NULL) {
	if ((neweb = driver_realloc_binary(eb, size)) != NULL) {
	    return neweb->orig_bytes;
	} else {
	    return NULL;
	}
    } else {
	return NULL;
    }
}

void
edtk_driver_free_wrapper(void *p)
{
    ErlDrvBinary	*eb;

    edtk_debug("%s: top", __FUNCTION__);
    if ((eb = edtk_alloced_ptr2ErlDrvBinary(p)) != NULL) {
	edtk_debug("%s: eb = 0x%lx p = 0x%lx", __FUNCTION__, eb, p);
	driver_free_binary(eb);
    }
}

ErlDrvBinary *
edtk_alloced_ptr2ErlDrvBinary(void *p)
{
    ErlDrvBinary	stackeb;
    int			offset = stackeb.orig_bytes - (char *) &stackeb;

    if (p != NULL) {
	edtk_debug("%s: p = 0x%lx eb = 0x%lx", __FUNCTION__, p, (ErlDrvBinary *) ((char *) p - offset));
	return (ErlDrvBinary *) ((char *) p - offset);
    } else {
	return NULL;
    }
}

int
edtk_debug(char *f, ...)
{
    va_list	ap;
    int		res;

    va_start(ap, f);
    if (edtk_debug_flag) {
	res = vfprintf(DEBUGIO, f, ap);
	fprintf(DEBUGIO, "\r\n");
    }
    va_end(ap);
    return res;
}

void 
edtk_debug_errcall(const char *errpfx, char *msg)
{
    if (edtk_debug_flag)
	fprintf(stderr, "\t\t\t\t\t%s: %s: %s\r\n", __FUNCTION__, errpfx, msg);
}

void
edtk_free_data(void *data)
{
    if (data != NULL)
	sys_free(data);
}

/*
** Arg want_contiguous: if true, return error if the 'n' we're forwarding
** past are in a single contiguous buffer.
** 
** Return value:
**      -1 = error
**       0 = Success, there is no more data to be read
**       1 = Success
*/

int
edtk_ev_forward_N(ErlIOVec *ev, int n, int *pp, int *qp, int want_contiguous)
{
    int	forward_pos = *pp + n;

    if ((*qp) >= ev->vsize) {
	return -1;
    }
    if (forward_pos < ev->iov[*qp].iov_len) {
	*pp += n;
	return 1;
    } else if (forward_pos == ev->iov[*qp].iov_len) {
	(*qp)++;
	*pp = 0;
	if ((*qp) < ev->vsize) {
	    return 1;
	} else {
	    return 0;
	}
    } else {
	n -= (ev->iov[*qp].iov_len - *pp);
	(*qp)++;
	(*pp) = 0;
	if ((*qp) < ev->vsize && ! want_contiguous) {
	    return edtk_ev_forward_N(ev, n, pp, qp, want_contiguous);
	} else {
	    return -1;
	}
    }
}

