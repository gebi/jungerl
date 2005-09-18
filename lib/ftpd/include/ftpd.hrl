-ifndef(_FTPD_HRL).
-define(_FTPD_HRL, true).

-define(AUTH_READ,   (1 bsl 0)).
-define(AUTH_WRITE,  (1 bsl 1)).
-define(AUTH_DELETE, (1 bsl 2)).

%%%
%%% These are OP flags are added in order to make it possible
%%% to tighten up the security. You need to tell the ftpd server
%%% at startup what operations are allowed.
%%%
-define(OP_CWD,      (1 bsl 0)).
-define(OP_CDUP,     (1 bsl 1)).
-define(OP_PWD,      (1 bsl 2)).
-define(OP_CLNT,     (1 bsl 3)).
-define(OP_PORT,     (1 bsl 4)).
-define(OP_STRU,     (1 bsl 5)).
-define(OP_RETR,     (1 bsl 6)).
-define(OP_STOR,     (1 bsl 7)).
-define(OP_STOU,     (1 bsl 8)).
-define(OP_APPE,     (1 bsl 9)).
-define(OP_REST,     (1 bsl 10)).
-define(OP_RFNR,     (1 bsl 11)).
-define(OP_RNTO,     (1 bsl 12)).
-define(OP_DELE,     (1 bsl 13)).
-define(OP_RMD,      (1 bsl 14)).
-define(OP_MKD,      (1 bsl 15)).
-define(OP_LST,      (1 bsl 16)).
-define(OP_NLST,     (1 bsl 17)).
-define(OP_STAT,     (1 bsl 18)).
-define(OP_SIZE,     (1 bsl 19)).
-define(OP_MDTM,     (1 bsl 20)).
-define(OP_FEAT,     (1 bsl 21)).
-define(OP_OPTS,     (1 bsl 22)).
-define(OP_HELP,     (1 bsl 23)).

%%% Enable all operations
-define(OPS_I_FEEL_LUCKY, 
	(?OP_CWD bor ?OP_CDUP bor ?OP_PWD bor
	 ?OP_CLNT bor ?OP_PORT bor ?OP_STRU bor
	 ?OP_RETR bor ?OP_STOR bor ?OP_STOU bor
	 ?OP_APPE bor ?OP_REST bor ?OP_RFNR bor
	 ?OP_RNTO bor ?OP_DELE bor ?OP_RMD bor
	 ?OP_MKD bor ?OP_LST bor ?OP_NLST bor
	 ?OP_STAT bor ?OP_SIZE bor?OP_MDTM bor
	 ?OP_FEAT bor ?OP_OPTS bor ?OP_HELP)).

%%% Allow PUT, GET, LIST and PWD
-define(OPS_RESTRICTED,
	(?OP_PORT bor ?OP_RETR bor ?OP_STOR bor ?OP_LST bor ?OP_PWD)).

%%% Allow PUT and nothing else
-define(OPS_STORE_ONLY,	?OP_STOR ).


-define(bit_is_set(Fs, F), (Fs) band F =/= 0).
-define(bit_is_clr(Fs, F), (Fs) band F == 0).
-define(bit_clr(Fs,F), (Fs) band (bnot (F))).
-define(bit_set(Fs,F), (Fs) bor (F)).

-record(user,
	{name,        % string()
	 passwd,      % string() | 'email_addr'
	 access = []  % [{Dir, AuthFlags}]
	 }).


-endif.
