
%%%
%%% Driver<->emulator communication codes (xref with top of unixdom_drv.c)
%%%

-define(UNIXDOM_REQ_ENDIAN,	1).
-define(UNIXDOM_REQ_KNUTHHASH,	2).
-define(UNIXDOM_REQ_OPEN,	3).
-define(UNIXDOM_REQ_CONNECT,	4).
-define(UNIXDOM_REQ_CLOSE,	5).
-define(UNIXDOM_REQ_WRITE,	6).
-define(UNIXDOM_REQ_RECV,	7).
-define(UNIXDOM_REQ_SETOPTS,	8).
-define(UNIXDOM_REQ_BIND,	9).		% Actually bind + listen combo
-define(UNIXDOM_REQ_ACCEPT,	10).
-define(UNIXDOM_REQ_GETIX,	11).
-define(UNIXDOM_REQ_GETOPTS,	12).

-define(UNIXDOM_REP_OK,		1).
-define(UNIXDOM_REP_ERROR,	2).
-define(UNIXDOM_REP_WOULDBLOCK, 3).
-define(UNIXDOM_ACCEPT_WOULDBLOCK, 5).         % accept body: op would block

-define(UNIXDOM_REPBODY_LSB,	254).		% endian reply body
-define(UNIXDOM_REPBODY_MSB,	255).		% endian reply body

%%% Subcommands for UNIXDOM_REQ_SETOPTS and
%%% (XXX unimplemented) UNIXDOM_REQ_SETOPTS.

-define(UNIXDOM_OPT_ENDOFLIST,	0).		% End of list marker
-define(UNIXDOM_OPT_IGNORE,	1).		% Ignore this item
-define(UNIXDOM_OPT_ACTIVE,	2).		% Set active mode
-define(UNIXDOM_OPT_BACKLOG,	3).		% Set listen backlog size

%%% packet byte values

-define(UNIXDOM_OPT_ACTIVE_FALSE,	0).	% Active mode = false
-define(UNIXDOM_OPT_ACTIVE_TRUE,	1).	% Active mode = true
-define(UNIXDOM_OPT_ACTIVE_ONCE,	2).	% Active mode = once, XXX not implemented.


