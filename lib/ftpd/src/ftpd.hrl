-define(FTPD_VSN, "1.1").

-ifdef(debug).
-define(dbg(X,Y), error_logger:info_msg("*dbg ~p:~p: " X,
					[?MODULE, ?LINE | Y])).
-else.
-define(dbg(X,Y), ok).
-endif.

-define(AUTH_READ,   (1 bsl 0)).
-define(AUTH_WRITE,  (1 bsl 1)).
-define(AUTH_DELETE, (1 bsl 2)).

-define(bit_is_set(Fs, F), (Fs) band F =/= 0).
-define(bit_is_clr(Fs, F), (Fs) band F == 0).
-define(bit_clr(Fs,F), (Fs) band (bnot (F))).
-define(bit_set(Fs,F), (Fs) bor (F)).

%% default configuration options
-define(FTPD_PORT, 21).
-define(FTPD_MAX_CONN, 40).
-define(FTPD_LOGFILE, "ftpd.log").
-define(FTPD_IDLE_TIMEOUT, 300000). % 5 minutes
-define(FTPD_UNIQUE_PREFIX, "ftpd").

%% server conf
-record(sconf,
	{port = ?FTPD_PORT,         % which port is this server listening to
	 ip = {0,0,0,0},            % bind to this IP, {0,0,0,0} is possible
	 rootdir = "",
	 greeting_file,
	 servername = element(2, inet:gethostname()), % printed in greeting
	 max_connections = ?FTPD_MAX_CONN,
	 allow_hosts = [{{0,0,0,0},{0,0,0,0}}],
	 deny_hosts = [],           % none denied
	 users = [],                % [{User,Passwd,[{Dir,AuthFlags}]}]
	 auth_mod = ftpd,           % default impl, reject all
	 idle_timeout = ?FTPD_IDLE_TIMEOUT,
	 unique_prefix = ?FTPD_UNIQUE_PREFIX,
	 local_cs = "ISO-8859-1",   % FIXME: "" should work??
	 use_utf8_by_default = false
	}).

-record(user,
	{name,        % string()
	 passwd,      % string() | 'email_addr'
	 access = []  % [{Dir, AuthFlags}]
	 }).
