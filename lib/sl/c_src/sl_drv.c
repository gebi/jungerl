/*
 * sl_drv.c
 *
 *   Windows/Unix serial com driver
 *
 */

#ifdef __WIN32__

#include <stdio.h>
#include "windows.h"
#include "erl_driver.h"

typedef HANDLE  com_t;
#define INVALID INVALID_HANDLE_VALUE

#ifdef DEBUG
#define DEBUGF(fmt,args...)
#else
#define DEBUGF(fmt,args...)
#endif

#else

#include <stdio.h>
#include <termios.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "erl_driver.h"

typedef int     com_t;
#define INVALID -1
#define DWORD   int

#ifdef DEBUG
#define DEBUGF(fmt,args...) fprintf(stderr, fmt "\r\n", args)
#else
#define DEBUGF(fmt,args...)
#endif

#endif


/* Standard set of integer macros  .. */

#define get_int32(s) ((((unsigned char*) (s))[0] << 24) | \
                      (((unsigned char*) (s))[1] << 16) | \
                      (((unsigned char*) (s))[2] << 8)  | \
                      (((unsigned char*) (s))[3]))

#define put_int32(i, s) {((char*)(s))[0] = (char)((i) >> 24) & 0xff; \
                        ((char*)(s))[1] = (char)((i) >> 16) & 0xff; \
                        ((char*)(s))[2] = (char)((i) >> 8)  & 0xff; \
                        ((char*)(s))[3] = (char)((i)        & 0xff);}

#define get_int16(s) ((((unsigned char*)  (s))[0] << 8) | \
                      (((unsigned char*)  (s))[1]))


#define put_int16(i, s) {((unsigned char*)(s))[0] = ((i) >> 8) & 0xff; \
                        ((unsigned char*)(s))[1] = (i)         & 0xff;}

#define get_int8(s) ((((unsigned char*)  (s))[0] ))


#define put_int8(i, s) { ((unsigned char*)(s))[0] = (i)         & 0xff;}


static struct _rate {
    int baud;
    unsigned int speed;
} rtab[] = 

#ifdef __WIN32__
{
    {0,      0},
    {75,     BAUD_075},
    {110,    BAUD_110},
    {134,    BAUD_134_5},
    {150,    BAUD_150},
    {300,    BAUD_300},
    {600,    BAUD_600},
    {1200,   BAUD_1200},
    {1800,   BAUD_1800},
    {2400,   BAUD_2400},
    {4800,   BAUD_4800},
    {7200,   BAUD_7200},
    {9600,   BAUD_9600},
    {14400,  BAUD_14400},
    {19200,  BAUD_19200},
    {38400,  BAUD_38400},
    {56000,  BAUD_56K},
    {57600,  BAUD_57600},
    {115200, BAUD_115200},
    {128000, BAUD_128K},
    { -1,    0}
};

#else
{
    {0,       B0     },
    {50,      B50    },
    {75,      B75    },
    {110,     B110   },
    {134,     B134   },
    {150,     B150   },
    {200,     B200   },
    {300,     B300   },
    {600,     B600   },
    {1200,    B1200  },
    {1800,    B1800  },
    {2400,    B2400  },
    {4800,    B4800  },
    {9600,    B9600  },
#ifdef B19200
    {19200,   B19200 },
#elif defined(EXTA)
    {19200,   EXTA },
#endif
#ifdef B38400
    {38400,   B38400 },
#elif defined(EXTB)
    {38400,   EXTB },
#endif
#ifdef B57600
    {57600,   B57600 },
#endif
#ifdef B76800
    {76800,   B76800 },
#endif
#ifdef B115200
    {115200,  B115200 },
#endif
#ifdef B153600
    {153600,  B153600 }, 	
#endif
#ifdef B230400
    {230400,  B230400 }, 	
#endif
#ifdef B307200
    {307200,  B307200 }, 	
#endif
#ifdef B460800
    {460800,  B460800 }, 	
#endif
#ifdef B500000
    {500000,  B500000 },
#endif
#ifdef B576000
    {576000,  B576000 },
#endif
#ifdef B921600 
    {921600,  B921600 },
#endif
#ifdef B1000000
    {1000000, B1000000 },
#endif
#ifdef B1152000
    {1152000, B1152000 },
#endif
#ifdef B1500000
    {1500000, B1500000 },
#endif
#ifdef B2000000
    {2000000, B2000000 },
#endif
#ifdef B2500000
    {2500000, B2500000 },
#endif
#ifdef B3000000
    {3000000, B3000000 },
#endif
#ifdef B3500000
    {3500000, B3500000 },
#endif
#ifdef B4000000
    {4000000, B4000000 },
#endif
    { -1, B0 }
};
#endif


#define SL_CONNECT      1
#define SL_DISCONNECT   2
#define SL_OPEN         3
#define SL_CLOSE        4
#define SL_XOFF         5
#define SL_XON          6
#define SL_BREAK        7
#define SL_UPDATE       8
#define SL_GET_RATES    9
#define SL_REVERT       10

#define SL_SET_DEV      20
#define SL_SET_BAUD     22
#define SL_SET_CSIZE    26
#define SL_SET_BUFSZ    28
#define SL_SET_BUFTM    30
#define SL_SET_STOPB    32
#define SL_SET_PARITY   34
#define SL_SET_HWFLOW   36
#define SL_SET_SWFLOW   38
#define SL_SET_XONCHAR  40
#define SL_SET_XOFFCHAR 42
#define SL_SET_ECHO     44
#define SL_SET_MODE     46

#define SL_GET_DEV      21
#define SL_GET_BAUD     23
#define SL_GET_CSIZE    27
#define SL_GET_BUFSZ    29
#define SL_GET_BUFTM    31
#define SL_GET_STOPB    33
#define SL_GET_PARITY   35
#define SL_GET_HWFLOW   37
#define SL_GET_SWFLOW   39
#define SL_GET_XONCHAR  41
#define SL_GET_XOFFCHAR 43
#define SL_GET_ECHO     45
#define SL_GET_MODE     47

#define SL_OK     0
#define SL_ERROR  1

#define SL_INT    0   /* sent as 4 bytes */
#define SL_BOOL   1   /* send as 1 byte */
#define SL_STRING 2   /* sent as N bytes */

#define SL_ERR_OK      0
#define SL_ERR_BADARG  1
#define SL_ERR_NOTOPEN 2
#define SL_ERR_ACCESS  3
#define SL_ERR_NOMEM   4

#define SL_MODE_RAW    0
#define SL_MODE_LINE   1

#define FL_BAUD        0x0001
#define FL_CSIZE       0x0004
#define FL_STOPB       0x0008
#define FL_PARITY      0x0010
#define FL_BUFSZ       0x0020
#define FL_BUFTM       0x0040
#define FL_HWFLOW      0x0080
#define FL_SWFLOW      0x0100
#define FL_XONCHAR     0x0200
#define FL_XOFFCHAR    0x0400
#define FL_ECHO        0x0800
#define FL_MODE        0x8000  /* mode update */

typedef struct _sl_t
{
    ErlDrvPort port;
    com_t      com;       /* Connection handle */
    int        ilen;     /* length of input buffer */
    char*      ibuf;     /* Overlapped input buffer */
    int        ipending; /* Input is pending */
    int        olen;     /* length of output buffer */
    char*      obuf;     /* Overlapped output buffer */
    int        opending; /* Output is pending */
    int        cmode;    /* current mode */
#ifdef __WIN32__
    OVERLAPPED in;       /* Overlapped input  */
    OVERLAPPED out;      /* Overlapped output */
    DCB        tio;      /* Comm parameter block */

#else
    struct termios tio;   /* The termios structure */
#endif
    char*      dev;       /* device name */
    int        flags;     /* flags FL_ */
    int        baud;      /* baud rate  */
    int        csize;     /* 5,6,7,8 */
    int        stopb;     /* 1,2 */
    int        parity;    /* None=0,Odd=1,Even=2 */
    int        hwflow;    /* Hardware flow control */
    int        swflow;    /* Software flow control */
    int        echo;      /* Local echo */
    int        xonchar;   /* XON character (for swflow) */
    int        xoffchar;  /* XOFF character (for swflow) */
    int        bufsz;     /* Number of bytes buffer */
    int        buftm;     /* Buffer fill timeout */
    int        mode;      /* (pending mode RAW | LINE) */
} sl_t;

static ErlDrvEntry sl_entry;
static ErlDrvData sl_start(ErlDrvPort port, char *buf);
static void sl_stop(ErlDrvData drv_data);
static void sl_output(ErlDrvData drv_data,  char* buf, int len);
static void sl_ready_input(ErlDrvData drv_data, ErlDrvEvent event); 
static void sl_ready_output(ErlDrvData drv_data, ErlDrvEvent event);

static unsigned int to_speed(int baud)
{
    int i = 0;
    int speed = 0;

    while((rtab[i].baud != -1) && (baud > rtab[i].baud))
	i++;
    if (rtab[i].baud == -1)
	speed = rtab[i-1].speed;
    else 
	speed = rtab[i].speed;
    /* fprintf(stderr, "to_speed: baud=%d, speed=%d\n", baud, speed); */
    return speed;
}

static int from_speed(unsigned int speed)
{
    int i = 0;
    int baud;

    while((rtab[i].baud != -1) && (rtab[i].speed != speed))
	i++;
    baud = rtab[i].baud;
    /* fprintf(stderr, "from_speed: speed=%d, baud=%d\n", speed, baud);  */
    return baud;
}


static int get_com_state(sl_t* slp)
{
#ifdef __WIN32__
    if (!GetCommState(slp->com, &slp->tio)) {
	DEBUGF("GetCommState: error %d", GetLastError());
	return -1;
    }
    return 0;
#else
    int res;
    if ((res = tcgetattr(slp->com, &slp->tio)) < 0) {
	DEBUGF("tcgetattr: error %s", strerror(errno));
    }
    return res;
#endif
}

static int set_com_state(sl_t* slp)
{
#ifdef __WIN32__
    slp->tio.DCBlength = sizeof(DCB);
    if (!SetCommState(slp->com, &slp->tio)) {
	DEBUGF("SetCommState: error %d", GetLastError());
	return -1;
    }
    return 0;
#else
    int res;

    if ((res = tcsetattr(slp->com, TCSAFLUSH, &slp->tio)) < 0) {
	DEBUGF("tcsetattr: error %s", strerror(errno));
    }
    return res;
#endif
}

static void set_raw_mode(sl_t* slp)
{
#ifdef __WIN32__
    slp->tio.fBinary = TRUE;
#else
    slp->tio.c_iflag &= ~(ICRNL |    /* disable CR-to-NL mapping */
			  INLCR |    /* disable NL-to-CR mapping */
			  IGNCR |    /* disable ignore CR */
			  ISTRIP |   /* disable stripping of eighth bit */
			  BRKINT |   /* disable generate SIGINT on brk */
			  IGNPAR |
			  IGNBRK |
			  INPCK);    /* disable input parity detection */

    slp->tio.c_lflag &= ~(ECHOE |    /* disable visual erase */
			  ECHOK |    /* disable echo newline after kill */
			  ECHOKE |   /* disable visual kill with bs-sp-bs */
			  ECHONL |   /* disable echo nl when echo off */
			  ISIG |     /* disable tty-generated signals */
			  IEXTEN);   /* disable extended input processing */

    slp->tio.c_lflag &= ~ICANON;    /* enable non-canonical mode */

    slp->tio.c_oflag &= ~OPOST;      /* disable output processing */
    slp->tio.c_cflag |= CLOCAL;      /* ignore modem control lines??? */
    slp->tio.c_cc[VMIN]  = 1;
    slp->tio.c_cc[VTIME] = 0;
#endif
    slp->cmode = SL_MODE_RAW;
}

static void set_line_mode(sl_t* slp)
{
#ifdef __WIN32__
    slp->tio.fBinary = TRUE;
#else
    slp->tio.c_iflag &= ~(ICRNL |    /* disable CR-to-NL mapping */
			  INLCR |    /* disable NL-to-CR mapping */
			  IGNCR |    /* disable ignore CR */
			  ISTRIP |   /* disable stripping of eighth bit */
			  BRKINT |   /* disable generate SIGINT on brk */
			  IGNPAR |
			  IGNBRK |
			  INPCK);    /* disable input parity detection */

    slp->tio.c_lflag &= ~(ECHOE |    /* disable visual erase */
			  ECHOK |    /* disable echo newline after kill */
			  ECHOKE |   /* disable visual kill with bs-sp-bs */
			  ECHONL |   /* disable echo nl when echo off */
			  ISIG |     /* disable tty-generated signals */
			  IEXTEN);   /* disable extended input processing */

    slp->tio.c_lflag |= ICANON;      /* enable canonical mode */


    slp->tio.c_oflag &= ~OPOST;      /* disable output processing */
    slp->tio.c_cflag |= CLOCAL;      /* ignore modem control lines??? */
    slp->tio.c_cc[VINTR]    = 0;
    slp->tio.c_cc[VQUIT]    = 0;
    slp->tio.c_cc[VERASE]   = 0;
    slp->tio.c_cc[VKILL]    = 0;
    slp->tio.c_cc[VEOF]     = 0;
    slp->tio.c_cc[VSWTC]    = 0;
    slp->tio.c_cc[VSUSP]    = 0;
    slp->tio.c_cc[VEOL]     = 0;
#ifdef VDISCARD
    slp->tio.c_cc[VDISCARD] = 0;
#endif
    /* disabled by ~IEXTEN */
#ifdef VREPRINT
    slp->tio.c_cc[VREPRINT] = 0;
#endif
#ifdef VWERASE
    slp->tio.c_cc[VWERASE]  = 0;
#endif
#ifdef VLNEXT
    slp->tio.c_cc[VLNEXT]   = 0;
#endif
#ifdef VEOL2
    slp->tio.c_cc[VEOL2]    = 0;
#endif
#endif
    slp->cmode = SL_MODE_LINE;
}



static void set_baud(sl_t* slp, int baud)
{
#ifdef __WIN32__
    slp->tio.BaudRate = to_speed(baud);
#else
    /* fprintf(stderr, "set_baud: %d\n", baud); */
    cfsetispeed(&slp->tio, to_speed(baud));
    cfsetospeed(&slp->tio, to_speed(baud));
#endif
}

static int get_baud(sl_t* slp)
{
#ifdef __WIN32__
    return from_speed(slp->tio.BaudRate);
#else
    return from_speed(cfgetispeed(&slp->tio));
#endif
}


static void set_parity(sl_t* slp, int parity)
{
#ifdef __WIN32__
    switch(parity) {
    case 0: 
	slp->tio.fParity = FALSE;
	slp->tio.Parity = NOPARITY; 
	break;
    case 1: 
	slp->tio.fParity = TRUE;
	slp->tio.Parity = ODDPARITY; 
	break;
    case 2: 
	slp->tio.fParity = TRUE;
	slp->tio.Parity = EVENPARITY; 
	break;
    case 3: 
	slp->tio.fParity = TRUE;
	slp->tio.Parity = MARKPARITY; 
	break;
    }
#else
    switch(parity) {
    case 0: /* none */
	slp->tio.c_iflag &= ~PARMRK;
	slp->tio.c_cflag &= ~PARENB;
	break;
    case 1: /* odd */
	slp->tio.c_iflag &= ~PARMRK;
	slp->tio.c_cflag  |= PARODD;
	slp->tio.c_cflag |= PARENB;
	break;
    case 2: /* even */
	slp->tio.c_iflag &= ~PARMRK;
	slp->tio.c_cflag &= ~PARODD;
	slp->tio.c_cflag |= PARENB;
	break;
    case 3:  /* mark (FIXME) */
	slp->tio.c_iflag |= PARMRK;
	slp->tio.c_cflag |= PARENB;
	break;
    }
#endif
}


static int get_parity(sl_t* slp)
{
#ifdef __WIN32__
    if (slp->tio.fParity) {
	switch (slp->tio.Parity) {
	case NOPARITY: return 0;
	case ODDPARITY: return 1;
	case EVENPARITY: return 2;
	case MARKPARITY: return 3;
	}
    }
    return 0;
#else
    if (slp->tio.c_cflag & PARENB) {
	if (slp->tio.c_iflag & PARMRK)
	    return 3;
	else if (slp->tio.c_cflag & PARODD)
	    return 1;
	else
	    return 2;
    }
    return 0;
#endif
}

static void set_stopb(sl_t* slp, int stopb)
{
#ifdef __WIN32__
    if (stopb == 1)
	slp->tio.StopBits = ONESTOPBIT;
    else if (stopb == 2)
	slp->tio.StopBits = TWOSTOPBITS;
#else
    if (stopb == 1)
	slp->tio.c_cflag &= ~CSTOPB;
    else if (stopb == 2)
	slp->tio.c_cflag |= CSTOPB;
#endif
}

static int get_stopb(sl_t* slp)
{
#ifdef __WIN32__
    if (slp->tio.StopBits == ONESTOPBIT)
	return 1;
    else if (slp->tio.StopBits = TWOSTOPBITS)
	return 2;
    else
	return -1;
#else
    if (slp->tio.c_cflag & CSTOPB)
	return 2;
    else
	return 1;
#endif
}


static void set_csize(sl_t* slp, int csize)
{
#ifdef __WIN32__
    slp->tio.ByteSize = csize;
#else
    slp->tio.c_cflag &= ~CSIZE;
    switch(csize) {
    case 5: slp->tio.c_cflag |= CS5; break;
    case 6: slp->tio.c_cflag |= CS6; break;
    case 7: slp->tio.c_cflag |= CS7; break;
    case 8: slp->tio.c_cflag |= CS8; break;
    }
#endif
}

static int get_csize(sl_t* slp)
{
#ifdef __WIN32__
    return slp->tio.ByteSize;
#else
    switch(slp->tio.c_cflag & CSIZE) {
    case CS5: return 5;
    case CS6: return 6;
    case CS7: return 7;
    case CS8: return 8;
    default: return -1;
    }
#endif
}

/* set minimum buffer size */
static void set_bufsz(sl_t* slp, int bufsz)
{
#ifdef __WIN32__
    /* we have to emulate this */
#else
    slp->tio.c_cc[VMIN] = bufsz;
#endif
}    

static int get_bufsz(sl_t* slp)
{
#ifdef __WIN32__
    return slp->bufsz; /* we have to emulate this */
#else
    return slp->tio.c_cc[VMIN];
#endif
}

/* set read timeout value */
static void set_buftm(sl_t* slp, int buftm)
{
#ifdef __WIN32__
    /* we have to emulate this */
#else
    slp->tio.c_cc[VTIME] = buftm;
#endif
}

/* set read timeout value */
static int get_buftm(sl_t* slp)
{
#ifdef __WIN32__
    return slp->buftm;
#else
    return slp->tio.c_cc[VTIME];
#endif
}

static void set_xonchar(sl_t* slp, int xonchar)
{
#ifdef __WIN32__
    slp->tio.XonChar = xonchar;
#else
    slp->tio.c_cc[VSTART] = xonchar;
#endif
}

static int get_xonchar(sl_t* slp)
{
#ifdef __WIN32__
    return slp->tio.XonChar;
#else
    return slp->tio.c_cc[VSTART];
#endif
}

static void set_xoffchar(sl_t* slp, int xoffchar)
{
#ifdef __WIN32__
    slp->tio.XoffChar = xoffchar;
#else
    slp->tio.c_cc[VSTOP] = xoffchar;
#endif
}

static int get_xoffchar(sl_t* slp)
{
#ifdef __WIN32__
    return slp->tio.XoffChar;
#else
    return slp->tio.c_cc[VSTOP];
#endif
}

static void set_swflow(sl_t* slp, int on)
{
#ifdef __WIN32__
    slp->tio.fOutX = slp->tio.fInX = on;
#else
    if (on)
	slp->tio.c_cflag |= (IXON | IXOFF);
    else
	slp->tio.c_cflag &= ~(IXON | IXOFF);
#endif
}

static int get_swflow(sl_t* slp)
{
#ifdef __WIN32__
    return slp->tio.fOutX;
#else
    switch (slp->tio.c_cflag & (IXON|IXOFF)) {
    case 0: return 0;
    case (IXON|IXOFF): return 1;
    default: return -1;
    }
#endif
}

static void set_hwflow(sl_t* slp, int on)
{
#ifdef __WIN32__
    slp->tio.fOutxCtsFlow = on;
#else
    if (slp->hwflow)
	slp->tio.c_cflag |= CRTSCTS;
    else
	slp->tio.c_cflag &= ~CRTSCTS;
#endif
}

static int get_hwflow(sl_t* slp)
{
#ifdef __WIN32__
    return slp->tio.fOutxCtsFlow;
#else
    return (slp->tio.c_cflag & CRTSCTS) ? 1 : 0;
#endif
}


static void set_echo(sl_t* slp, int on)
{
#ifdef __WIN32__
    /* emulate this ??? */
#else
    if (on)
	slp->tio.c_lflag |= ECHO;
    else
	slp->tio.c_lflag &= ~ECHO;
#endif
}

static int get_echo(sl_t* slp)
{
#ifdef __WIN32__
    return 0; /* emulate this ??? */
#else
    return (slp->tio.c_lflag & ECHO) ? 1 : 0;
#endif
}

static void do_send_break(sl_t* slp)
{
    if (slp->com != INVALID) {
#ifdef __WIN32__
    /* FIXME */
#else
	tcsendbreak(slp->com, 0);    
#endif
    }
}

static void do_send_xon(sl_t* slp)
{
    if ((slp->com != INVALID) && (slp->swflow)) {
#ifdef __WIN32__
	/* FIXME */
#else
	tcflow(slp->com, TCION);
#endif
    }
}

static void do_send_xoff(sl_t* slp)
{
    if ((slp->com != INVALID) && (slp->swflow)) {
#ifdef __WIN32__
	/* FIXME */
#else
	tcflow(slp->com, TCIOFF);
#endif
    }
}


static void do_close(sl_t* slp)
{
    if (slp->com != INVALID) {
	driver_select(slp->port, (ErlDrvEvent)slp->com, DO_READ|DO_WRITE, 0);
#ifdef __WIN32__
	driver_select(slp->port, (ErlDrvEvent)slp->in.hEvent, DO_READ, 0);
	driver_select(slp->port, (ErlDrvEvent)slp->out.hEvent, DO_READ, 0);
	CloseHandle(slp->com);
#else
	close(slp->com);
#endif
	slp->com = INVALID;
    }
}

static void do_update(sl_t* slp)
{
    if ((slp->flags == 0) || (slp->com == INVALID))
	return;

    if (slp->flags & FL_MODE) {
	if (slp->mode == SL_MODE_RAW)
	    set_raw_mode(slp);
	else if (slp->mode == SL_MODE_LINE)
	    set_line_mode(slp);
    }

    if (slp->flags & FL_BAUD)
	set_baud(slp, slp->baud);

    if (slp->flags & FL_CSIZE)
	set_csize(slp, slp->csize);

    if (slp->flags & FL_STOPB)
	set_stopb(slp, slp->stopb);

    if (slp->flags & FL_PARITY)
	set_parity(slp, slp->parity);

    if (slp->flags & FL_BUFSZ)
	set_bufsz(slp, slp->bufsz);

    if (slp->flags & FL_BUFTM)
	set_buftm(slp, slp->buftm);
    
    if (slp->flags & FL_XONCHAR)
	set_xonchar(slp, slp->xonchar);

    if (slp->flags & FL_XOFFCHAR)
	set_xoffchar(slp, slp->xoffchar);

    if (slp->flags & FL_HWFLOW)
	set_hwflow(slp, slp->hwflow);

    if (slp->flags & FL_SWFLOW)
	set_swflow(slp, slp->swflow);

    if (slp->flags & FL_ECHO)
	set_echo(slp, slp->echo);

    set_com_state(slp);

    slp->flags = 0;
}

/* initiate a read operation */
static int do_read(sl_t* slp)
{
    DWORD n;

    if ((slp->com == INVALID) || slp->ipending)
	return -1;

    if ((slp->ibuf == NULL) || (slp->bufsz > slp->ilen)) {
	slp->ibuf = driver_realloc(slp->ibuf, slp->bufsz);
	slp->ilen = slp->bufsz;
    }    
#ifdef __WIN32__
    if (!ReadFile(slp->com, slp->ibuf, slp->bufsz, &n, &slp->in)) {
	if (GetLastError() == ERROR_IO_PENDING) {
	    slp->ipending = 1;
	    driver_select(slp->port,(ErlDrvEvent)slp->in.hEvent, DO_READ, 1);
	    return 0;
	}
	return -1;
    }
    driver_output(slp->com, slp->ibuf, n);
    return n;
#else
    if ((n = read(slp->com, slp->ibuf, slp->bufsz)) > 0) {
	driver_output(slp->port, slp->ibuf, n);
	return n;
    }
    else if ((n < 0) && (errno == EAGAIN)) {
	driver_select(slp->port, (ErlDrvEvent)slp->com, DO_READ, 1);
	return 0;
    }
    return n;
#endif
}


static int do_write_buf(sl_t*slp, char* buf, int len)
{
    DWORD n;
#ifdef __WIN32__
    if (!WriteFile(slp->com, buf, len, &n, &slp->out)) {
	if (GetLastError() == ERROR_IO_PENDING) {
	    slp->opending = 1;
	    driver_select(slp->port, (ErlDrvEvent)slp->out.hEvent,DO_READ,1);
	    return 0;
	}
	return -1;
    }
#else

    if (((n = write(slp->com, buf, len)) < 0) && (errno == EAGAIN)) {
	driver_enq(slp->port, buf, len);
	driver_select(slp->port,(ErlDrvEvent)slp->com, DO_WRITE, 1);
    }
    else if (n < len) {
	driver_enq(slp->port, buf+n, len-n);
	driver_select(slp->port,(ErlDrvEvent)slp->com, DO_WRITE, 1);
    }
#endif

    return n;    
}

static int do_write_more(sl_t* slp)
{
    DWORD n = 0;
#ifdef __WIN32__
    ErlIOVec vec;

    if ((n = driver_peekqv(slp->port, &vec)) > 0) {
	if (slp->olen < n) {
	    slp->obuf = driver_realloc(slp->obuf, n);
	    slp->olen = n;
	}
	driver_vec_to_buf(&vec, slp->obuf, n);
	driver_deq(slp->port, n);
	n = do_write_buf(slp, slp->obuf, n);
    }
#else
    SysIOVec* vector;
    int count;

    if ((vector = driver_peekq(slp->port, &count)) != NULL) {
	int n = writev(slp->com, vector, count);
	if (n > 0)
	    driver_deq(slp->port, n);
	if (driver_sizeq(slp->port) == 0)
	    driver_select(slp->port,(ErlDrvEvent)slp->com, DO_WRITE, 0);
    }
#endif
    return n;
}

/* initiate a write operation */
static int do_write_init(sl_t* slp, char* buf, int len)
{
    if (slp->com == INVALID)
	return -1;
    if (len == 0)
	return 0;

    if (slp->opending || (driver_sizeq(slp->port) > 0)) {
	driver_enq(slp->port, buf, len);
	return 0;
    }

#ifdef __WIN32__
    if ((slp->obuf == NULL) || (slp->olen > len)) {
	slp->obuf = driver_realloc(slp->obuf, len);
	slp->olen = len;
    }
    memcpy(slp->obuf, buf, len);
    return do_write_buf(slp, slp->obuf, len);
#else
    return do_write_buf(slp, buf, len);
#endif
}


static int do_open(sl_t* slp)
{
    do_close(slp);

    if (slp->dev == NULL)
	return -1;
    
#ifdef __WIN32__
    slp->com = CreateFile(slp->dev,  
			  GENERIC_READ | GENERIC_WRITE, 
			  0, 
			  0, 
			  OPEN_EXISTING,
			  FILE_FLAG_OVERLAPPED,
			  0);
    if (slp->com == INVALID)
	return -1;
    slp->in.hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
    slp->out.hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
    slp->stat.hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
    {
	DWORD mask;
	SetCommMask(slp->com, EV_RXCHAR);
	WaitCommEvent(slp->com, &mask, &slp->stat);
    }
#else
    slp->com = open(slp->dev, O_RDWR);
    if (slp->com == INVALID)
	return -1;
    fcntl(slp->com, F_SETFL, fcntl(slp->com, F_GETFL, 0) | O_NONBLOCK);
#endif
    if (get_com_state(slp) < 0)
	return -1;

    /* setup default state if not set */
    if (!(slp->flags & FL_MODE)) {
	slp->mode = SL_MODE_RAW;
	slp->flags |= FL_MODE;
    }
    if (!(slp->flags & FL_BAUD)) {
	slp->baud = 9600;
	slp->flags |= FL_BAUD;
    }
    if (!(slp->flags & FL_PARITY)) {
	slp->parity = 0;
	slp->flags |= FL_PARITY;
    }
    if (!(slp->flags & FL_ECHO)) {
	slp->echo = 0;
	slp->flags |= FL_ECHO;
    }
    if (!(slp->flags & FL_CSIZE)) {
	slp->csize = 8;
	slp->flags |= FL_CSIZE;
    }
    if (!(slp->flags & FL_HWFLOW)) {
	slp->hwflow = 1;
	slp->flags |= FL_HWFLOW;
    }
    if (!(slp->flags & FL_SWFLOW)) {
	slp->swflow = 0;
	slp->flags |= FL_SWFLOW;
    }
    /* update pending changes */
    do_update(slp);
    return 0;
}


static int do_hangup(sl_t* slp)
{
    if (slp->com == INVALID)
	return -1;
#ifdef __WIN32__
    /* FIXME */
    return -1;
#else
    {
	struct termios t = slp->tio;
	cfsetispeed(&t, B0);
	cfsetospeed(&t, B0);
	return tcsetattr(slp->com, TCSAFLUSH, &t);
    }
#endif
}


/* general control reply function */
static int ctl_reply(int rep, char* buf, int len, char** rbuf, int rsize)
{
    char* ptr;

    if ((len+1) > rsize) {
	ptr = driver_alloc(len+1);
	*rbuf = ptr;
    }
    else
	ptr = *rbuf;
    *ptr++ = rep;
    memcpy(ptr, buf, len);
    return len+1;
}

static int ctl_reply_int(int rep, char** rbuf, int rsize)
{
    char buf[5];
    buf[0] = SL_INT;
    put_int32(rep, buf+1);
    return ctl_reply(SL_OK, buf, 5, rbuf, rsize);
}


static int ctl_reply_bool(int rep, char** rbuf, int rsize)
{
    char buf[2];
    buf[0] = SL_BOOL;
    buf[1] = rep ? 1 : 0;
    return ctl_reply(SL_OK, buf, 2, rbuf, rsize);
}

/* general control error reply function */
static int ctl_error(int err, char** rbuf, int rsize)
{
    char response[256];		/* Response buffer. */
    char* s;
    char* t;
    
    switch(err) {
    case SL_ERR_OK: s = ""; break;
    case SL_ERR_BADARG: s = "badarg"; break;
    case SL_ERR_NOTOPEN: s = "not_open"; break;
    case SL_ERR_ACCESS: s = "bad_access"; break;
    case SL_ERR_NOMEM:  s= "no_mem"; break;
    default:  s = "unknown"; break;
    }
    for (t = response; *s; s++, t++)
	*t = tolower(*s);
    return ctl_reply(SL_ERROR, response, t-response, rbuf, rsize);
}

static int ctl_reply_string(char* str, char** rbuf, int rsize)
{
    char buf[1025];
    int n = strlen(str);

    if (n >= 1024)
	return ctl_error(SL_ERR_BADARG, rbuf, rsize);
    buf[0] = SL_STRING;
    memcpy(buf+1, str, n);
    return ctl_reply(SL_OK, buf, n+1, rbuf, rsize);
}


static ErlDrvData sl_start(ErlDrvPort port, char *buf)
{
    sl_t *slp = (sl_t*) driver_alloc(sizeof (sl_t));
    struct termios* tp;

    if (slp == NULL) 
	return (ErlDrvData) -1;
    slp->port = port;
    slp->com   = INVALID;
    slp->dev  = NULL;
    slp->ilen = 0;
    slp->ibuf = NULL;
    slp->ipending = 0;
    slp->olen = 0;
    slp->obuf = NULL;
    slp->opending = 0;
    memset(&slp->tio, 0, sizeof(slp->tio));
    slp->flags = 0;
    slp->bufsz = 1;
    slp->buftm = 0;

#ifdef __WIN32__
    memset(&slp->in, 0, sizeof(slp->in));
    memset(&slp->out, 0, sizeof(slp->out));
#endif
    /* set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);*/
    return (ErlDrvData) slp;
}


static void sl_stop(ErlDrvData data)
{
    sl_t* slp = (sl_t*) data;

    do_close(slp);

    if (slp->ibuf != NULL)
	driver_free(slp->ibuf);
    if (slp->obuf != NULL)
	driver_free(slp->obuf);
    driver_free(slp);
}


static void sl_output(ErlDrvData data,  char* buf, int len)
{
    sl_t* slp = (sl_t*) data;

    DEBUGF("sl_output: qsize=%d", driver_sizeq(slp->port));

    do_write_init(slp, buf, len);    
}

static void sl_ready_input(ErlDrvData data, ErlDrvEvent event)
{
    sl_t* slp = (sl_t*) data;
    DWORD n;

#ifdef __WIN32__
    if (event == slp->out.hEvent)
	sl_ready_output(data, event);
    else if (event == slp->stat.hEvent) {
	/* Manually reset the event? */
	if (do_read(slp) > 0)
	    WaitCommEvent(slp->com, &mask, &slp->stat);
    }
    else if (event == slp->in.hEvent) {
	if (!GetOverlappedResult(slp->com, &slp->in, &n, FALSE)) {
	    /* how can we handle this error? */
	    return;
	}
	else {
	    driver_ouput(slp->port, slp->ibuf, n);
	}
    }
#else
    if ((slp->ibuf == NULL) || (slp->bufsz > slp->ilen)) {
	slp->ibuf = driver_realloc(slp->ibuf, slp->bufsz);
	slp->ilen = slp->bufsz;
    }
    if ((n = read(slp->com, slp->ibuf, slp->bufsz)) > 0)
	driver_output(slp->port, slp->ibuf, n);
#endif
}


static void sl_ready_output(ErlDrvData data, ErlDrvEvent event)
{
    sl_t* slp = (sl_t*) data;

    DEBUGF("sl_ready_output: qsize=%d", driver_sizeq(slp->port));
    
#ifdef __WIN32__
    slp->opending = 0;
    driver_select(slp->port, (ErlDrvEvent)slp->out.hEvent,DO_READ,0);
    if (!GetOverlappedResult(slp->com, &slp->out, &n, FALSE)) {
	/* Output error */
	return;
    }
#endif
    do_write_more(slp);
}


static int sl_ctl(ErlDrvData data, unsigned int cmd, char* buf, int len, 
	      char** rbuf, int rsize)
{
    sl_t* slp = (sl_t*) data;
    
    switch(cmd) {
    case SL_SET_DEV: /* set device name */
	if (slp->dev != NULL)
	    driver_free(slp->dev);
	slp->dev = NULL;
	if (len > 0) {
	    if ((slp->dev = driver_alloc(len + 1)) == NULL)
		return ctl_error(SL_ERR_NOMEM, rbuf, rsize);
	    memcpy(slp->dev, buf, len);
	    slp->dev[len] = '\0';
	}
	return ctl_reply(SL_OK, NULL, 0, rbuf, rsize);

    case SL_GET_DEV: /* get device name */
	if (slp->dev == NULL)
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);
	return ctl_reply_string(slp->dev, rbuf, rsize);
	

    case SL_SET_BAUD: /* set baud rate */
	if (len != 4)
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);
	slp->baud = get_int32(buf);
	slp->flags |= FL_BAUD;
	return ctl_reply(SL_OK, NULL, 0, rbuf, rsize);

    case SL_GET_BAUD: /* get baud rate */
	if (slp->flags & FL_BAUD) { /* pending */
	    /* fprintf(stderr, "pending baud=%d\r\n", slp->baud);*/
	    return ctl_reply_int(slp->baud, rbuf, rsize);
	}
	if (slp->com != INVALID) {
	    int baud = get_baud(slp);
	    /* fprintf(stderr, "baud = %d\r\n", baud);*/
	    return ctl_reply_int(baud, rbuf, rsize);
	}
	else
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);

    case SL_SET_CSIZE:
	if (len != 4) /* set number of data bits */
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);
	slp->csize = get_int32(buf);
	slp->flags |= FL_CSIZE;
	return ctl_reply(SL_OK, NULL, 0, rbuf, rsize);

    case SL_GET_CSIZE:
	if (slp->flags & FL_CSIZE) /* pending */
	    return ctl_reply_int(slp->csize, rbuf, rsize);
	else if (slp->com != INVALID)
	    return ctl_reply_int(get_csize(slp), rbuf, rsize);
	else
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);

    case SL_SET_BUFSZ:
	if (len != 4) /* set number of bytes to buffer */
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);
	if ((slp->bufsz = get_int32(buf)) < 1)
	    slp->bufsz = 1;
	else if (slp->bufsz > 255)
	    slp->bufsz = 255;
	slp->flags |= FL_BUFSZ;
	return ctl_reply(SL_OK, NULL, 0, rbuf, rsize);

    case SL_GET_BUFSZ:
	if (slp->flags & FL_BUFSZ)
	    return ctl_reply_int(slp->bufsz, rbuf, rsize);
	else if (slp->com != INVALID)
	    return ctl_reply_int(get_bufsz(slp), rbuf, rsize);
	else
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);

    case SL_SET_BUFTM:
	if (len != 4) /* set buffer timeout */
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);
	slp->buftm = get_int32(buf);
	slp->flags |= FL_BUFTM;
	return ctl_reply(SL_OK, NULL, 0, rbuf, rsize);

    case SL_GET_BUFTM:
	if (slp->flags & FL_BUFTM)
	    return ctl_reply_int(slp->buftm, rbuf, rsize);
	else if (slp->com != INVALID)
	    return ctl_reply_int(get_buftm(slp), rbuf, rsize);
	else
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);

    case SL_SET_STOPB:
	if (len != 4) /* set buffer timeout */
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);
	slp->stopb = get_int32(buf);
	slp->flags |= FL_STOPB;
	return ctl_reply(SL_OK, NULL, 0, rbuf, rsize);	

    case SL_GET_STOPB:
	if (slp->flags & FL_STOPB)
	    return ctl_reply_int(slp->stopb, rbuf, rsize);
	else if (slp->com != INVALID)
	    return ctl_reply_int(get_stopb(slp), rbuf, rsize);
	else
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);

    case SL_SET_PARITY:
	if (len != 4)
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);
	slp->parity = get_int32(buf);
	slp->flags |= FL_PARITY;
	return ctl_reply(SL_OK, NULL, 0, rbuf, rsize);

    case SL_GET_PARITY:
	if (slp->flags & FL_PARITY)
	    return ctl_reply_int(slp->parity, rbuf, rsize);
	else if (slp->com != INVALID) 
	    return ctl_reply_int(get_parity(slp), rbuf, rsize);
	else
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);

    case SL_SET_HWFLOW:
	if (len != 4)
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);
	slp->hwflow = get_int32(buf);	
	slp->flags |= FL_HWFLOW;
	return ctl_reply(SL_OK, NULL, 0, rbuf, rsize);

    case SL_GET_HWFLOW:
	if (slp->flags & FL_HWFLOW)
	    return ctl_reply_bool(slp->hwflow, rbuf, rsize);
	else if (slp->com != INVALID)
	    return ctl_reply_bool(get_hwflow(slp), rbuf, rsize);
	else
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);

    case SL_SET_SWFLOW:
	if (len != 4)
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);
	slp->swflow = get_int32(buf);	
	slp->flags |= FL_SWFLOW;
	return ctl_reply(SL_OK, NULL, 0, rbuf, rsize);

    case SL_GET_SWFLOW:
	if (slp->flags & FL_SWFLOW)
	    return ctl_reply_bool(slp->swflow, rbuf, rsize);
	else if (slp->com != INVALID)
	    return ctl_reply_bool(get_swflow(slp), rbuf, rsize);
	else
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);

    case SL_SET_XONCHAR:
	if (len != 4)
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);
	slp->xonchar = get_int32(buf);	
	slp->flags |= FL_XONCHAR;
	return ctl_reply(SL_OK, NULL, 0, rbuf, rsize);

    case SL_GET_XONCHAR:
	if (slp->flags & FL_XONCHAR)
	    return ctl_reply_int(slp->xonchar, rbuf, rsize);
	else if (slp->com != INVALID)
	    return ctl_reply_int(get_xonchar(slp), rbuf, rsize);
	else
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);

    case SL_SET_XOFFCHAR:
	if (len != 4)
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);
	slp->xoffchar = get_int32(buf);	
	slp->flags |= FL_XOFFCHAR;
	return ctl_reply(SL_OK, NULL, 0, rbuf, rsize);

    case SL_GET_XOFFCHAR:
	if (slp->flags & FL_XOFFCHAR)
	    return ctl_reply_int(slp->xoffchar, rbuf, rsize);
	else if (slp->com != INVALID)
	    return ctl_reply_int(get_xoffchar(slp), rbuf, rsize);
	else
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);

    case SL_SET_ECHO:
	if (len != 4)
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);
	slp->echo = get_int32(buf);	
	slp->flags |= FL_ECHO;
	return ctl_reply(SL_OK, NULL, 0, rbuf, rsize);

    case SL_GET_ECHO:
	if (slp->flags & FL_ECHO)
	    return ctl_reply_bool(slp->echo, rbuf, rsize);
	else if (slp->com != INVALID)
	    return ctl_reply_bool(get_echo(slp), rbuf, rsize);
	else
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);

    case SL_SET_MODE:
	if (len != 4)
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);
	else {
	    int mode = get_int32(buf);
	    if (mode != slp->mode) {
		if (mode == SL_MODE_RAW)
		    slp->mode = SL_MODE_RAW;
		else if (mode == SL_MODE_LINE)
		    slp->mode = SL_MODE_LINE;
		else
		    return ctl_error(SL_ERR_BADARG, rbuf, rsize);
		slp->flags |= FL_MODE;
	    }
	    return ctl_reply(SL_OK, NULL, 0, rbuf, rsize);
	}

    case SL_GET_MODE:
	if (slp->flags & FL_MODE)
	    return ctl_reply_int(slp->mode, rbuf, rsize);
	else if (slp->com != INVALID)
	    return ctl_reply_int(slp->cmode, rbuf, rsize);
	else
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);
	

    case SL_CONNECT:
    case SL_OPEN:
	if (slp->dev == NULL) /* MUST set device first */
	    return ctl_error(SL_ERR_BADARG, rbuf, rsize);
	if (do_open(slp) < 0)
	    return ctl_error(SL_ERR_ACCESS, rbuf, rsize);
	/* For win32 we could? create a ComEvent handle */
#ifdef __WIN32__
	driver_select(slp->port,(ErlDrvEvent)slp->stat.hEvent,DO_READ,1);
#else
	driver_select(slp->port,(ErlDrvEvent)slp->com, DO_READ, 1);
#endif
	return ctl_reply(SL_OK, NULL, 0, rbuf, rsize);

    case SL_DISCONNECT:
	do_hangup(slp);
	/* fall through */
    case SL_CLOSE:
	do_close(slp);
	return ctl_reply(SL_OK, NULL, 0, rbuf, rsize);

    case SL_XOFF:
	do_send_xoff(slp);

	return ctl_reply(SL_OK, NULL, 0, rbuf, rsize);	

    case SL_XON:
	/* send xon */
	do_send_xon(slp);
	return ctl_reply(SL_OK, NULL, 0, rbuf, rsize);	

    case SL_BREAK:
	/* send break */
	do_send_break(slp);
	return ctl_reply(SL_OK, NULL, 0, rbuf, rsize);	

    case SL_UPDATE:
	do_update(slp);
	return ctl_reply(SL_OK, NULL, 0, rbuf, rsize);

    case SL_REVERT:
	slp->flags = 0;
	return ctl_reply(SL_OK, NULL, 0, rbuf, rsize);

    case SL_GET_RATES: {
	char rates[1+sizeof(int)*((sizeof(rtab)/sizeof(struct _rate))-1)];
	char* ptr = rates+1;
	int i = 0;
	
	rates[0] = SL_INT;
	while(rtab[i].baud != -1) {
	    put_int32(rtab[i].baud, ptr);
	    ptr += sizeof(int);
	    i++;
	}
	return ctl_reply(SL_OK, rates, sizeof(rates), rbuf, rsize);
    }
    default:
	return ctl_error(SL_ERR_BADARG, rbuf, rsize);
    }
}
	
DRIVER_INIT(sl_drv)
{
    sl_entry.init         = NULL;   /* Not used */
    sl_entry.start        = sl_start;
    sl_entry.stop         = sl_stop;
    sl_entry.output       = sl_output;
    sl_entry.ready_input  = sl_ready_input;
    sl_entry.ready_output = sl_ready_output;
    sl_entry.driver_name  = "sl_drv";
    sl_entry.finish       = NULL;
    sl_entry.control      = sl_ctl;
    sl_entry.outputv      = NULL;
    return (ErlDrvEntry*) &sl_entry;
}
