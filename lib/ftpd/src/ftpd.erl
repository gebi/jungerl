%%% File    : ftpd.erl
%%% Maintainer: <davidw@eidetix.com>
%%% Author  :  <tony@RIOJA>
%%% Purpose : FTP SERVER, rfc959/rfc1123 compliant
%%%           Implements:
%%%              rfc959 - basic rfc
%%%              rfc1123 - updates/clarifies rfc959
%%%              rfc2389 - Feature negotiation mechanism (FEAT and OPTS)
%%%              rfc2640 - i18n (only the utf8 part, not LANG)
%%%              draft-ietf-ftpext-utf-8-option-00 - OTPS UTF-8, updates rfc2640
%%%              draft-ietf-ftpext-mlst-16 - SIZE, MDTM, and REST in STREAM mode
%%%
%%%           Follows most recommendations at http://cr.yp.to/ftp.html
%%%           

%%% Created : 29 Jan 1998 by  <tony@RIOJA>

%%% $Id$

%%% Updates by David N. Welton <davidw@eidetix.com> May 2004.
%%% Updates by Martin Bjorklund <mbj@bluetail.com>  Dec 2004.

%%% To support UTF-8, we use iconv.  In order to make use of this,
%%% iconv must be started prior to starting ftpd.  iconv can (currently)
%%% be found in esmb in jungerl.  If iconv is not started, ftpd will still
%%% work just fine, but won't send UTF-8.
%%%
%%%
%%% To start a simple server with read-only anonymous access do:
%%%   ftpd:start([{root, "/tmp"}, {port, 2112}, {users, [anonymous]}]).
%%%
%%% Add a user test with passwd test:
%%%   ftpd:start([{root, "/tmp"}, {port, 2112},
%%%               {users, [anonymous,
%%%               {"test", "test", [{"/",[read,write,delete]}]}]}]).


%%% TODO
%%%    o  implement max connections, reply w/ 421 greeting and close
%%%    o  start as root, fork to different user after authenticaion (?)
%%%       (configurable!)
%%%    o  implement real ascii mode(?)
%%%    o  implement logging (use common log format). could re-use yaws_log.
%%%    o  add greeting file
%%%    o  finish STAT command implementation

-module(ftpd).
-author('tony@RIOJA').
-behaviour(gen_server).

-ifdef(debug).
-compile(export_all).
-endif.
-export([start/1, start/0]).
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

%% internal exports
-export([control/2]).
-export([auth/2, event/1]).

-import(lists, [reverse/1, map/2, append/1, foreach/2, foldl/3]).

-include("ftpd.hrl").
-include_lib("kernel/include/file.hrl").

-define(is_ip(X), size(X)==4, 
		   (element(1,X) bor element(2,X) bor 
		   element(3,X) bor element(4,X)) band (bnot 255) == 0).

%% ftpd state record
-record(state,
	{
	  listen,             % listen socket
	  accept,             % pid of current accept process
	  sconf,              % #sconf record
	  iconv_cd_to_utf8,
	  iconv_cd_from_utf8
	 }).

%% connection state record
%%
%% ust is the user state
%% invalid   - user is undefined
%% ident     - user is identified
%% valid
%%
%% do not store configuration parameters in cstate if they are
%% stored in state, unless the overhead of asking the server process is
%% to high.
-record(cstate,
	{
	  ust = invalid,              %% internal state
	  user = "",
	  rootwd = "",                 %% real root directory
	  homewd = "",                 %% home working directory
	  wd = "",                     %% current working directory
	  client_ip,
	  client = "",                 %% string from CLNT command
%	  structure = file,            %% file(F), record(R), page(P)
%	  mode = stream,               %% stream(S), block(B), compressed(C)
	  type = {ascii,nonprint,8},   %% ascii(A),
	  def_data_port,               %% default data port
	  data_port = undefined,       %% set by port
	  listen = undefined,          %% listen socket for pasv
	  idle_timeout,                %% copied from state - used all the time
	  state = undefined,           %% state to be remembered between cmds
	  iconv_cd_to_utf8,
	  iconv_cd_from_utf8,
	  use_utf8 = true,
	  sys_ops = 0,                 %% allowed operations, default=none !!
	  jail = true,                 %% do not allow '..' in RETR path
	  event_mod                    %% call <event_mod>:event(Event) sometimes...
	 }).

-define(HAS_ICONV(St), ((St)#cstate.iconv_cd_from_utf8 /= undefined)).
-define(USE_UTF8(St), ((St)#cstate.use_utf8 == true)).

-define(CRNL, "\r\n").

start() ->
    start([{root, "/tmp"}]).

start(Opts) when list(Opts) ->
    case options(Opts, #sconf{}) of
	{ok, SConf} ->
	    start(SConf);
	Error ->
	    Error
    end;
start(SConf) when record(SConf, sconf) ->
    gen_server:start(?MODULE, [SConf], []).

start_link(SConf) ->
    gen_server:start_link(?MODULE, [SConf], []).

%% called by control channel process
call(Req) ->
    gen_server:call(get(ftpd), Req, infinity).

%% FIXME: implement real logging
log(Level, Fmt, Args) ->
    error_logger:info_msg("ftpd: ~w:" ++ Fmt, [Level | Args]).


options([Opt | Opts], S) ->
    case Opt of
	{port,P} when P > 0, P < 65536 ->
	    options(Opts, S#sconf { port = P });
	{ip,IP} when ?is_ip(IP) ->
	    options(Opts, S#sconf { ip = IP });
	{allow, IP} when ?is_ip(IP) ->
	    options(Opts, S#sconf { allow_hosts = 
				     [{IP,{255,255,255,255}}|
				      S#sconf.allow_hosts]});
	{allow, {IP,Mask}} when ?is_ip(IP),?is_ip(Mask) ->
	    options(Opts, S#sconf { allow_hosts = 
				     [{IP,Mask}|S#sconf.allow_hosts]});
	{deny, IP} when ?is_ip(IP) ->
	    options(Opts, S#sconf { deny_hosts =
				     [{IP,{255,255,255,255}}|
				      S#sconf.deny_hosts]});
	{deny, {IP,Mask}} when ?is_ip(IP), ?is_ip(Mask) ->
	    options(Opts, S#sconf { deny_hosts =
				     [{IP,Mask}|S#sconf.deny_hosts]});
	{users, Users} when list(Users) ->
	    %% Users = [{User,Passwd,[{Dir,[read|write|delete]}]} |
	    %%          anonymous]
	    %% NOTE: Dir must be sorted with deepest dir first!
	    options(Opts, S#sconf{users=lists:flatmap(fun mk_user/1, Users)});
	{max_connections,N} when integer(N), N >= 0 ->
	    options(Opts, S#sconf { max_connections = N });
	{root, Dir} ->
	    options(Opts, S#sconf { rootdir = Dir });
	{idle_timeout, Seconds} when integer(Seconds), Seconds > 0 ->
	    options(Opts, S#sconf { idle_timeout = Seconds * 1000});
	{use_utf8_by_default, Bool} ->
	    options(Opts, S#sconf { use_utf8_by_default = Bool });
	{greeting_file, File} ->
	    options(Opts, S#sconf { greeting_file = File });
	{use_fd_srv, Bool} when Bool==true ; Bool==false ->
	    options(Opts, S#sconf { use_fd_srv = Bool });
	{event_mod, Mod} when atom(Mod) ->
	    options(Opts, S#sconf{event_mod = Mod});
	{auth_mod, Mod} when atom(Mod) ->
	    options(Opts, S#sconf{auth_mod = Mod});
	{jail, Bool} when Bool==true ; Bool==false ->
	    options(Opts, S#sconf{jail = Bool});
	{sys_ops, SysOps} when integer(SysOps) ->
	    options(Opts, S#sconf{sys_ops = SysOps});
	_ ->
	    {error, {bad_option, Opt}}
    end;
options([], S) ->
    {ok, S}.

mk_user(anonymous) ->  % convenient shorthand
    Anon = #user{passwd = email_addr, access = [{"/", ?AUTH_READ}]},
    [Anon#user{name = "anonymous"},
     Anon#user{name = "ftp"}];
mk_user({UserName, Passwd, DirAccess}) ->
    [#user{name = UserName,
	   passwd = Passwd, 
	   access = [{Dir, mk_acl_flags(ACL)} || {Dir, ACL} <- DirAccess]}].

mk_acl_flags(ACL) ->
    foldl(fun(read, Flags) -> ?bit_set(Flags, ?AUTH_READ);
	     (write, Flags) -> ?bit_set(Flags, ?AUTH_WRITE);
	     (delete, Flags) -> ?bit_set(Flags, ?AUTH_DELETE)
	  end, 0, ACL).

%%====================================================================
%% Server functions
%%====================================================================
init([SConf]) ->
    TcpOpts = [{active,false},{nodelay,true},
	       {reuseaddr,true},{ip,SConf#sconf.ip}],
    case listen_socket(SConf#sconf.port, TcpOpts, SConf#sconf.use_fd_srv) of
	{ok,Listen} ->
	    process_flag(trap_exit, true),
	    St = #state{sconf = SConf, listen = Listen},
	    LocalCs = SConf#sconf.local_cs,
	    St1 = 				
		case catch {iconv:open("UTF-8", LocalCs),
			    iconv:open(LocalCs, "UTF-8")} of
		    {{ok, CdToUtf8}, {ok, CdFromUtf8}} ->
			?dbg("using iconv", []),
			St#state{iconv_cd_to_utf8=CdToUtf8,
				 iconv_cd_from_utf8=CdFromUtf8};
		    _ ->
			log(info, "iconv not used", []),
			St
		end,
	    Accept = proc_lib:spawn_link(?MODULE,control,[self(),Listen]),
	    {ok, St1#state{accept = Accept}};
	{error, Error} -> 
	    {stop, Error}
    end.

listen_socket(Port, Opts, true) ->
    case fdsrv:bind_socket(tcp, Port) of
	{ok, Fd} ->
	    gen_tcp:listen(Port, [{fd, Fd} | Opts]);
	Error ->
	    error_logger:info_msg("Couldn't open socket, port=~p: ~p~n",
				  [Port, Error]),
	    {error, "fdsrv:bind_socket/2 failed"}
    end;
listen_socket(Port, Opts, false) ->
    gen_tcp:listen(Port, Opts).


handle_call({is_allowed, Addr, _Port}, _From, St) ->
    ?dbg("ftpd: is_allowed ? ~p:~p~n", [Addr,_Port]),
    %% 1. check if denied => false
    %% 2. check if allowed => true
    %% 3. => false
    Deny = member_address(Addr, (St#state.sconf)#sconf.deny_hosts),
    Allow = member_address(Addr, (St#state.sconf)#sconf.allow_hosts),
    Res = not Deny and Allow,
    ?dbg("ftpd: deny=~p, allow=~p, res=~p~n", [Deny, Allow, Res]),
    {reply, Res, St};
handle_call({getcfg, Item}, _From, St) ->   % get from sconf record
    {reply, element(Item, St#state.sconf), St};
handle_call({getstate, Item}, _From, St) -> % get from state record
    {reply, element(Item, St), St};
handle_call(stop, _From, St) ->
    {stop, stop, St};
handle_call(Req, _from, St) ->
    {reply, {bad_request,Req}, St}.

handle_cast(_, St) ->
    {noreply, St}.

handle_info({accepted, Accept}, St) when Accept == St#state.accept ->
    unlink(Accept),
    Accept1 = proc_lib:spawn_link(?MODULE,control,[self(),St#state.listen]),
    {noreply, St#state{accept = Accept1}};
handle_info({'EXIT',Accept,_Reason}, St) when Accept == St#state.accept ->
    Accept1 = proc_lib:spawn_link(?MODULE,control,[self(),St#state.listen]),
    {noreply, St#state{accept = Accept1}};
handle_info(_Other, St) ->
    ?dbg("ftpd: got ~p~n", [_Other]),
    {noreply, St}.

terminate(_, St) ->
    St.

code_change(_,_,_) ->
    ok.

%%
%% Default authentication module callbacks
%%
auth(_User, _Pass) ->
    false.

%%
%% Default event module callbacks
%%
event(_Event) ->
    false.

%%
%% Control channel setup
%%
control(Srv, Listen) ->
    put(ftpd, Srv),
    case gen_tcp:accept(Listen) of
	{ok,S} ->
	    Srv ! {accepted, self()},
	    control_init(S);
	_Error ->
	    exit(bad_accept)
    end.

%% 
%% Control channel init
%%
control_init(Ctl) ->
    case inet:peername(Ctl) of
	{ok,{Addr,Port}} ->
	    case call({is_allowed,Addr,Port}) of
		true ->
		    ctl_loop_init(Ctl, {Addr,Port-1});
		false ->
		    gen_tcp:close(Ctl)
	    end;
	{error,_Err} ->
	    ?dbg("ftpd: error in inet:peername ~p~n",[_Err]),
	    gen_tcp:close(Ctl)
    end.

ctl_loop_init(Ctl, {ClientIP, _Port} = DefaultDataPort) ->
    rsend(Ctl,220, [call({getcfg, #sconf.servername}),
		    " Ftp server ready."]),
    ctl_loop(Ctl,#cstate{rootwd = call({getcfg, #sconf.rootdir}),
			 idle_timeout = call({getcfg, #sconf.idle_timeout}),
			 use_utf8 = call({getcfg, #sconf.use_utf8_by_default}),
			 iconv_cd_to_utf8 = call({getstate,
						  #state.iconv_cd_to_utf8}),
			 iconv_cd_from_utf8 = call({getstate,
						    #state.iconv_cd_from_utf8}),
			 client_ip = ClientIP,
			 data_port = DefaultDataPort,
			 def_data_port = DefaultDataPort,
			 sys_ops = call({getcfg, #sconf.sys_ops}),
			 jail = call({getcfg, #sconf.jail}),
			 event_mod = call({getcfg, #sconf.event_mod})
			}, []).

-ifdef(debug).
-define(dbg_save_line(Line), put(line, Line)).
-else.
-define(dbg_save_line(_), ok).
-endif.

ctl_loop(Ctl, St, Buf) ->
    case ctl_line(Ctl,Buf,St#cstate.idle_timeout) of
	{ok,Line,Buf1} ->
	    ?dbg_save_line(Line),
	    ?dbg("got <~s>\n", [Line]),
	    case ctl_parse(Line) of
		{Fun,Args} ->
		    case catch Fun(Args,Ctl,St) of
			failed -> ctl_loop(Ctl,St,Buf1);
			quit -> true;
			init -> ctl_loop_init(Ctl, St#cstate.def_data_port);
			St1 when record(St1, cstate) ->
			    ctl_loop(Ctl,St1,Buf1);
			_Err -> %% Crash etc - e.g. bad input from client
			    ?dbg("ftpd crash: ~p", [_Err]),
			    rsend(Ctl,501,["argument error: ", Line]),
			    ctl_loop(Ctl,St,Buf1)
		    end;
		error ->
		    rsend(Ctl,500, ["syntax error: ", Line]),
		    ctl_loop(Ctl, St, Buf1)
	    end;
	{error, timeout} ->
	    rsend(Ctl,421,"Idle timeout; closing control connection"),
	    true;
	{error,closed} ->
	    true
    end.
%% parse a command and arguments
%% must be case insensitive on commands and type letters but
%% sensitive on path/user 
%% 
ctl_parse([L1,L2,L3 | T]) ->
    C1 = alpha(L1),
    C2 = alpha(L2),
    C3 = alpha(L3),
    case T of
	[] ->
	    ctl_parse(list_to_atom([C1,C2,C3]), []);
	[$ | Arg] ->
	    ctl_parse(list_to_atom([C1,C2,C3]),Arg);
	[C4] ->
	    ctl_parse(list_to_atom([C1,C2,C3,alpha(C4)]),[]);
	[C4,$  | Arg] ->
	    ctl_parse(list_to_atom([C1,C2,C3,alpha(C4)]),Arg);
	_ -> error
    end;
ctl_parse(_) -> error.


ctl_parse(abor,Arg) -> {fun cni/3, Arg};    % might be implemented
ctl_parse(acct, Arg) -> {fun cni/3, Arg};   % should probably not be implemented
ctl_parse(allo,Arg) -> {fun allo/3, Arg};
ctl_parse(appe,Arg) -> {fun appe/3, Arg};
ctl_parse(cdup,Arg) -> {fun cdup/3, Arg};
ctl_parse(clnt,Arg) -> {fun clnt/3, Arg};
ctl_parse(cwd, Arg) -> {fun cwd/3, Arg};
ctl_parse(dele,Arg) -> {fun dele/3, Arg};
ctl_parse(feat,Arg) -> {fun feat/3, Arg};
ctl_parse(help,Arg) -> {fun help/3, Arg};
ctl_parse(list,Arg) -> {fun lst/3, Arg};
ctl_parse(mdtm,Arg)  -> {fun mdtm/3, Arg};
ctl_parse(mkd,Arg)  -> {fun mkd/3, Arg};
ctl_parse(mode,Arg) -> {fun mode/3, Arg};
ctl_parse(nlst,Arg) -> {fun nlst/3, Arg};
ctl_parse(noop,Arg) -> {fun noop/3, Arg};
ctl_parse(opts,Arg) -> {fun opts/3, Arg};
ctl_parse(pass, Arg) -> {fun pass/3, Arg};
ctl_parse(pasv,Arg) -> {fun pasv/3, Arg};
ctl_parse(port,Arg) -> {fun port/3, Arg};
ctl_parse(pwd,Arg)  -> {fun pwd/3, Arg};
ctl_parse(quit,Arg) -> {fun quit/3 ,Arg};
ctl_parse(rein,Arg) -> {fun rein/3, Arg};
ctl_parse(rest,Arg) -> {fun rest/3, Arg};
ctl_parse(retr,Arg) -> {fun retr/3, Arg};
ctl_parse(rmd,Arg)  -> {fun rmd/3, Arg};
ctl_parse(rnfr,Arg) -> {fun rnfr/3, Arg};
ctl_parse(rnto,Arg) -> {fun rnto/3, Arg};
ctl_parse(site,Arg) -> {fun cni/3, Arg};   % might be implemented (CHGRP, CHMOD)
ctl_parse(size,Arg) -> {fun size/3, Arg};
ctl_parse(smnt,Arg) -> {fun cni/3, Arg};   % should probably not be implemented
ctl_parse(stat,Arg) -> {fun stat/3, Arg}; 
ctl_parse(stor,Arg) -> {fun stor/3, Arg};
ctl_parse(stou,Arg) -> {fun stou/3, Arg};
ctl_parse(stru,Arg) -> {fun stru/3, Arg};
ctl_parse(syst,Arg) -> {fun syst/3, Arg};
ctl_parse(type,Arg) -> {fun type/3, Arg};
ctl_parse(user, Arg) -> {fun user/3, Arg};
ctl_parse(xcup,Arg) -> {fun cdup/3, Arg};
ctl_parse(xcwd, Arg) -> {fun cwd/3, Arg};
ctl_parse(xmkd,Arg) -> {fun mkd/3, Arg};
ctl_parse(xpwd,Arg) -> {fun pwd/3, Arg};
ctl_parse(xrmd,Arg) -> {fun rmd/3, Arg};
ctl_parse(Cmd,Arg) ->  {fun cbad/3,{Cmd,Arg}}.


opts_parse([L1,L2,L3 | T]) ->
    C1 = alpha(L1),
    C2 = alpha(L2),
    C3 = alpha(L3),
    case T of
	[] ->
	    opts_parse(list_to_atom([C1,C2,C3]), []);
	[$ | Arg] ->
	    opts_parse(list_to_atom([C1,C2,C3]),Arg);
	[C4] ->
	    opts_parse(list_to_atom([C1,C2,C3,alnum(C4)]),[]);
	[C4,$  | Arg] ->
	    opts_parse(list_to_atom([C1,C2,C3,alnum(C4)]),Arg);
	[C4,C5] ->
	    opts_parse(list_to_atom([C1,C2,C3,alnum(C4),alnum(C5)]),[]);
	[C4,C5,$  | Arg] ->
	    opts_parse(list_to_atom([C1,C2,C3,alnum(C4),alnum(C5)]),Arg);
	_ -> error
    end;
opts_parse(_) -> error.

opts_parse(utf8, Arg) -> {fun opts_utf8/3, Arg};    % ms winwdows ftp 
opts_parse('utf-8', Arg) -> {fun opts_utf8/3, Arg}. % utf-8-option-00
 

%%%-----------------------------------------------------------------
%%% Commands
%%% Reply wiht NewState | failed | init | quit | exit(Error)
%%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Access Control Commands
%%-----------------------------------------------------------------
user(Name, Ctl, St) ->
    Users = call({getcfg, #sconf.users}),
    User = 
	case lists:keysearch(Name, #user.name, Users) of
	    {value, U} when U#user.passwd == email_addr ->
		rsend(Ctl, 331, "Anonymous login ok; send your email address"
		                " as your password"),
		U;
	    {value, U} ->
		rsend(Ctl, 331),
		U;
	    false ->
		rsend(Ctl, 331),
		Name
	end,
    St#cstate { ust = ident, user = User, homewd = "/", wd = "/" }.

pass(Password, Ctl, St) ->
    assert_ident(Ctl, St),
    %% check that we have executed user and need a password
    %% then that the password is valid
    case St#cstate.user of
	U when U#user.passwd == email_addr ->
	    rsend(Ctl, 230, ["User ", U#user.name, " logged in, proceed"]),
	    St#cstate{ust = valid};
	U when U#user.passwd == Password ->
	    rsend(Ctl, 230, ["User ", U#user.name, " logged in, proceed"]),
	    St#cstate{ust = valid};
	U when record(U, user) ->
	    rsend(Ctl, 530, "Login incorrect"),
	    St;
	UserName ->
	    AuthMod = call({getcfg, #sconf.auth_mod}),
	    case AuthMod:auth(UserName, Password) of
		true ->
		    rsend(Ctl, 230, ["User ", UserName, " logged in, proceed"]),
		    St#cstate{ust = valid};
		{true, Root, DirAccess} ->
		    ?dbg("Auth successful, DirAccess=~p~n", [DirAccess]),
		    Access = [{Dir, mk_acl_flags(ACL)} || {Dir, ACL} <- DirAccess],
		    ?dbg("Access=~p , UserName=~p~n", [Access, UserName]),
		    U2 = #user{name = UserName, access = Access},
		    ?dbg("U2=~p~n", [U2]),
		    rsend(Ctl, 230, ["User ", UserName, " logged in, proceed"]),
		    St#cstate{ust = valid, rootwd = Root, user = U2};
		false ->
		    rsend(Ctl, 530, "Login incorrect"),
		    St
	    end
    end.

%% Change working directory we must keep an absoulte path (emulated
%% so that symbolic links are transparent).
cwd(Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    auth_op(Ctl, ?OP_CWD, St),
    Dir = rel_name(Arg, St#cstate.wd),
    assert_exists(Ctl, St#cstate.rootwd, Dir, directory),
    rsend(Ctl, 250, ["new directory \"", abs_name(Dir), "\""]),
    St#cstate { wd = Dir }.

%% Change to parent directory
cdup(_Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    auth_op(Ctl, ?OP_CDUP, St),
    DirR = rel_name("..", St#cstate.wd),
    DirA = abs_name(DirR),
    assert_exists(Ctl, St#cstate.rootwd, DirR, directory),
    rsend(Ctl, 250, ["directory changed to \"", DirA, "\""]),
    St#cstate { wd = DirR }.

pwd(_Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    auth_op(Ctl, ?OP_PWD, St),
    rsend(Ctl, 257, ["\"", abs_name(St#cstate.wd), "\""]),
    St.

quit(_, Ctl, _St) ->
    rsend(Ctl, 221),
    gen_tcp:close(Ctl),
    quit.

rein(_, _Ctl, St) ->
    close_listen(St),
    init.

clnt(Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    auth_op(Ctl, ?OP_CLNT, St),
    rsend(Ctl, 200),
    St#cstate{client = Arg}.

%%-----------------------------------------------------------------
%% Transfer Parameter Commands
%%-----------------------------------------------------------------
port(Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    assert_arg(Ctl, Arg),
    auth_op(Ctl, ?OP_PORT, St),
    St1 = close_listen(St),
    case parse_address(Arg) of
	{ok, {Addr, Port} = AddrPort} when Addr == St#cstate.client_ip,
	                                   Port >= 1024 ->
	    rsend(Ctl,200),
	    St1#cstate { data_port = AddrPort, listen = undefined };
	{ok, {Addr, Port}} ->
	    log(notice, "PORT to ~s:~p rejected, should have been from ~s "
		"or port >= 1024",
		[inet_parse:ntoa(Addr), Port,
		 inet_parse:ntoa(St#cstate.client_ip)])
    end.

pasv(_Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    St1 = close_listen(St),
    {ok,{Addr,_}} = inet:sockname(Ctl),
    case gen_tcp:listen(0, [{active,false}, binary]) of
	{ok,L} ->
	    {ok,{_,Port}} = inet:sockname(L),
	    rsend(Ctl,227,["Entering Passive Mode (",
			   format_address(Addr,Port), ")."]),
	    St1#cstate { listen = L };
	{error,Err} ->
	    rsend(Ctl, 425, erl_posix_msg:message(Err)),
	    St1
    end.

type(Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    Type = case alpha(hd(Arg)) of
	       $i -> {image,nonprint,8};
	       $a -> {ascii,nonprint,8};
	       $l -> {image,nonprint,8}; % should really check that it's "L 8"
	       _ -> rsend(Ctl, 504), throw(St)
	   end,
    rsend(Ctl,200,["new type ", atom_to_list(element(1,Type))]),
    St#cstate { type = Type }.
		    
stru(Arg, Ctl, St) ->    
    assert_valid(Ctl, St),
    assert_arg(Ctl, Arg),
    auth_op(Ctl, ?OP_STRU, St),
    case alpha(hd(Arg)) of
	$f ->
	    rsend(Ctl, 200, ["new file structure ", [hd(Arg)]]);
	Char when Char == $r; Char == $p ->
	    %% proftpd, wu-ftpd and ms-ftp do not support R and P
	    %% rfc1123 recommends against implementing P
	    rsend(Ctl, 504, [hd(Arg) | " unsupported structure type"]);
	_ ->
	    rsend(Ctl, 501)
    end,
    St.

mode(Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    case alpha(hd(Arg)) of
	$s -> 
	    rsend(Ctl, 200, ["new mode ", [hd(Arg)]]);
	Char when Char == $b; Char == $c ->
	    %% proftpd and ms-ftp do not support B and C
	    rsend(Ctl, 504, [hd(Arg) | " unsupported mode"]);
	_ ->
	    rsend(Ctl, 501)
    end,
    St.

%%-----------------------------------------------------------------
%% Ftp Service Commands
%%-----------------------------------------------------------------
%% retrieve a file over a data connection file name is given by Arg
retr(Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    assert_arg(Ctl, Arg),
    auth_op(Ctl, ?OP_RETR, St),
    NameR = rel_name(from_utf8(Arg,St), St#cstate.wd),
    Name = jail(Ctl, filename:join(St#cstate.rootwd, NameR), St),
    authorize(?AUTH_READ, abs_name(NameR), Ctl, St),
    case file:open(Name, [read | file_mode(St)]) of
	{ok,Fd} ->
	    {ok, #file_info{size = FileSize}} = file:read_file_info(Name),
	    case St#cstate.state of
		{restart_pos, Pos} when Pos > FileSize ->
		    rsend(Ctl, 554, "REST position invalid"),
		    file:close(Fd),
		    throw(failed);
		{restart_pos, Pos} ->
		    file:position(Fd, Pos);
		_ ->
		    ok
	    end,
	    {Data,St1} = open_data(Ctl, St#cstate{state = undefined}),
	    case send_file(Fd, 1024, 0, Data) of
		{ok,Count} ->
		    rsend(Ctl,226, ["closing data connection, sent",
				    integer_to_list(Count), " bytes"]);
		    %%rsend(Ctl,200);

		{error,Err} ->
		    rsend(Ctl,226, "closing data connection, aborted"),
		    rsend(Ctl,550, [" error ",  erl_posix_msg:message(Err)])
	    end,
	    gen_tcp:close(Data),
	    file:close(Fd),
	    catch (St#cstate.event_mod):event({retr, Name}),
	    St1;
	{error,Err} ->
	    rsend(Ctl,550, ["error ", erl_posix_msg:message(Err)]),
	    St
    end.

%%%
%%% If jail=true then deny any '..' in the path.
%%% Else, return <name>
%%%
jail(Ctl, Name, St) when St#cstate.jail == true ->
    case string:str(Name, "..") of
	I when I > 0 ->
	    rsend(Ctl, 450, "'..' not allowed in path"),
	    throw(failed);
	0 ->
	    Name
    end;
jail(_Ctl, Name, _St) ->
    Name.
	    
    

%%
%% store file from data connection onto file given by Arg
%%
stor(Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    assert_arg(Ctl, Arg),
    auth_op(Ctl, ?OP_STOR, St),
    NameR = rel_name(from_utf8(Arg,St), St#cstate.wd),
    authorize(?AUTH_WRITE, abs_name(NameR), Ctl, St),
    Name = jail(Ctl, filename:join(St#cstate.rootwd, NameR), St),
    do_store(Name, Ctl, St, stor).

stou(_Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    auth_op(Ctl, ?OP_STOU, St),
    authorize(?AUTH_WRITE, St#cstate.wd, Ctl, St),
    Dir = jail(Ctl, filename:join(St#cstate.rootwd, St#cstate.wd), St),
    Fd = generate_unique(call({getcfg, #sconf.unique_prefix}), Dir, Ctl, St),
    do_store_fd(Fd, Ctl, St).

appe(Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    assert_arg(Ctl, Arg),
    auth_op(Ctl, ?OP_APPE, St),
    NameR = rel_name(from_utf8(Arg,St), St#cstate.wd),
    authorize(?AUTH_WRITE, abs_name(NameR), Ctl, St),
    Name = jail(Ctl, filename:join(St#cstate.rootwd, NameR), St),
    do_store(Name, Ctl, St, appe).
    
do_store(Name, Ctl, St, Cmd) ->
    Mode = if Cmd == appe -> append;
	      true -> write
	   end,
    case file:open(Name, [Mode | file_mode(St)]) of
	{ok,Fd} ->
	    {ok, #file_info{size = FileSize}} = file:read_file_info(Name),
	    case St#cstate.state of
		{restart_pos, Pos} when Pos > FileSize ->
		    rsend(Ctl, 554, "REST position invalid"),
		    file:close(Fd),
		    throw(failed);
		{restart_pos, Pos} ->
		    file:position(Fd, Pos);
		_ ->
		    ok
	    end,
	    put(name, Name),
	    do_store_fd(Fd, Ctl, St);
	{error,Err} ->
	    rsend(Ctl,550, ["error ", erl_posix_msg:message(Err)]),
	    St
    end.

do_store_fd(Fd, Ctl, St) ->
    {Data,St1} = open_data(Ctl, St),
    case recv_file(Data, 1024, 0, Fd) of
	{ok,Count} ->
	    rsend(Ctl,226, ["closing data connection, received ",
			    integer_to_list(Count), " bytes"]),
	    %% ugly...
	    catch (St#cstate.event_mod):event({store, get(name), St#cstate.rootwd});
	{error,Err} ->
	    rsend(Ctl,226, "closing data connection, aborted"),
	    rsend(Ctl,550, ["error ", erl_posix_msg:message(Err)])
    end,
    gen_tcp:close(Data),
    file:close(Fd),
    St1.

allo(_Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    rsend(Ctl, 202, "No storage allocation necessary"),
    St.

rest(Arg, Ctl, St) -> 
    assert_valid(Ctl, St),
    assert_arg(Ctl, Arg),
    auth_op(Ctl, ?OP_REST, St),
    case type(St) of
	ascii ->
	    rsend(Ctl, 501, "REST not allowed in ASCII mode"),
	    St;
	image ->
	    case list_to_integer(Arg) of
		Pos when integer(Pos), Pos >= 0 ->
		    rsend(Ctl, 350, ["Restarting at ", integer_to_list(Pos),
				     " send STOR or RETR to start transfer"]),
		    St#cstate{state = {restart_pos, Pos}}
	    end
    end.

rnfr(Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    assert_arg(Ctl, Arg),
    auth_op(Ctl, ?OP_RFNR, St),
    NameR = rel_name(from_utf8(Arg,St), St#cstate.wd),
    authorize(?AUTH_DELETE, abs_name(NameR), Ctl, St),
    Name = jail(Ctl, filename:join(St#cstate.rootwd, NameR), St),
    case file:read_file_info(Name) of
	{ok, _} ->
	    rsend(Ctl, 350, "File ok, send RNTO to rename"),
	    St#cstate{state = {rename_from, Name}};
	{error, Err} ->
	    rsend(Ctl, 550, ["error ", erl_posix_msg:message(Err)]),
	    St
    end.
    
rnto(Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    assert_arg(Ctl, Arg),
    auth_op(Ctl, ?OP_RNTO, St),
    NameR = rel_name(from_utf8(Arg,St), St#cstate.wd),
    authorize(?AUTH_WRITE, abs_name(NameR), Ctl, St),
    Name = jail(Ctl, filename:join(St#cstate.rootwd, NameR), St),
    case St#cstate.state of
	{rename_from, From} ->
	    case file:rename(From, Name) of
		ok ->
		    rsend(Ctl, 250);
		{error, exdev} ->
		    case file:copy(From, Name) of
			{ok, _} ->
			    file:delete(From),
			    rsend(Ctl, 250);
			{error, Err} ->
			    rsend(Ctl, 550, ["error ",
					     erl_posix_msg:message(Err)])
		    end;
		{error, Err} ->
		    rsend(Ctl, 550, ["error ", erl_posix_msg:message(Err)])
	    end;
	_ ->
	    rsend(Ctl, 503)
    end,
    St#cstate{state = undefined}.

%% abor(_Arg, Ctl, St)

dele(Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    auth_op(Ctl, ?OP_DELE, St),
    assert_arg(Ctl, Arg),
    FileR = rel_name(from_utf8(Arg,St), St#cstate.wd),
    authorize(?AUTH_DELETE, abs_name(FileR), Ctl, St),
    FileA = abs_name(FileR),
    File = jail(Ctl, filename:join(St#cstate.rootwd, FileR), St),
    case file:delete(File) of
	ok ->
	    rsend(Ctl, 250, ["\"", FileA, "\" deleted"]);
	{error,Err} ->
	    rsend(Ctl, 550, ["\"", FileA, "\" ",  erl_posix_msg:message(Err)])
    end,
    St. 

rmd(Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    auth_op(Ctl, ?OP_RMD, St),
    assert_arg(Ctl, Arg),
    DirR = rel_name(from_utf8(Arg,St), St#cstate.wd),
    authorize(?AUTH_DELETE, abs_name(DirR), Ctl, St),
    DirA = abs_name(DirR),
    Dir = jail(Ctl, filename:join(St#cstate.rootwd, DirR), St),
    case file:del_dir(Dir) of
	ok ->
	    rsend(Ctl, 250, [" \"", DirA, "\" removed"]);
	{error,Err} ->
	    rsend(Ctl, 550, ["\"", DirA, "\" ", erl_posix_msg:message(Err)])
    end,
    St.    

mkd(Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    auth_op(Ctl, ?OP_MKD, St),
    assert_arg(Ctl, Arg),
    DirR = rel_name(from_utf8(Arg,St), St#cstate.wd),
    authorize(?AUTH_WRITE, abs_name(DirR), Ctl, St),
    DirA = abs_name(DirR),
    Dir = jail(Ctl, filename:join(St#cstate.rootwd, DirR), St),
    case file:make_dir(Dir) of
	ok ->
	    rsend(Ctl, 257, [" \"", DirA, "\" directory created"]);
	{error,eexist} ->
	    rsend(Ctl, 521, [" \"", DirA, "\" directory exists"]);
	{error,Err} ->
	    rsend(Ctl, 521, [" \"", DirA, "\" ", 
		  erl_posix_msg:message(Err)])
    end,
    St.

lst(Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    auth_op(Ctl, ?OP_LST, St),
    DirR = rel_name(from_utf8(Arg,St), St#cstate.wd),
    authorize(?AUTH_READ, abs_name(DirR), Ctl, St),
    assert_exists(Ctl, St#cstate.rootwd, DirR, directory),
    Dir = jail(Ctl, filename:join(St#cstate.rootwd, DirR), St),
    {Data,St1} = open_data(Ctl, St),
    dir_list(Ctl, Data, Dir, DirR, St, list),
    gen_tcp:close(Data),
    St1.

    
nlst(Arg,Ctl,St) ->
    assert_valid(Ctl, St),
    auth_op(Ctl, ?OP_NLST, St),
    DirR = rel_name(from_utf8(Arg,St), St#cstate.wd),
    authorize(?AUTH_READ, abs_name(DirR), Ctl, St),
    assert_exists(Ctl, St#cstate.rootwd, DirR, directory),
    Dir = jail(Ctl, filename:join(St#cstate.rootwd, DirR), St),
    {Data,St1} = open_data(Ctl, St),
    dir_list(Ctl, Data, Dir, DirR, St, nlst),
    gen_tcp:close(Data),
    St1.
    

%% site    

syst(_Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    %% meaningless, but recommended and used by many servers
    rsend(Ctl, 215, "UNIX Type: L8"),
    St.

stat(Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    auth_op(Ctl, ?OP_STAT, St),
    if Arg == [] ->
	    rmsend(Ctl, 211,
		   ["Status of ", call({getcfg, #sconf.servername})],
		   [["  Connected from ", inet_parse:ntoa(St#cstate.client_ip)],
		    ["  Logged in as ", username(St)],
		    ["  TYPE: ",  upcase(atom_to_list(type(St))),
		     ", STRUcture: File, MODE: Stream"]],
		   "END");
       true ->
	    exit(nyi) %% FIXME
    end,
    St.

size(Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    auth_op(Ctl, ?OP_SIZE, St),
    assert_arg(Ctl, Arg),
    case type(St) of
	ascii ->
	    rsend(Ctl, 550, "SIZE not allowed in ASCII mode");
	image ->
	    FileR = rel_name(from_utf8(Arg,St), St#cstate.wd),
	    authorize(?AUTH_READ, abs_name(FileR), Ctl, St),
	    File = jail(Ctl, filename:join(St#cstate.rootwd, FileR), St),
	    case file:read_file_info(File) of
		{ok, #file_info{type = regular, size = Size}} ->
		    rsend(Ctl, 213, integer_to_list(Size));
		{ok, _} ->
		    rsend(Ctl, 550, ["\"", FileR, "\" is not a regular file"]);
		{error,Err} ->
		    rsend(Ctl, 550, ["\"", FileR, "\" ", 
				     erl_posix_msg:message(Err)])
	    end
    end,
    St. 

%% File Modification Time
mdtm(Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    auth_op(Ctl, ?OP_MDTM, St),
    assert_arg(Ctl, Arg),
    FileR = rel_name(from_utf8(Arg,St), St#cstate.wd),
    authorize(?AUTH_READ, abs_name(FileR), Ctl, St),
    File = jail(Ctl, filename:join(St#cstate.rootwd, FileR), St),
    case file:read_file_info(File) of
	{ok, #file_info{type = regular, mtime = MTime}} ->
	    rsend(Ctl, 213, timeval(MTime));
	{ok, _} ->
	    rsend(Ctl, 550, ["\"" , FileR, "\" is not a regular file"]);
	{error,Err} ->
	    rsend(Ctl, 550, ["\"", FileR, "\" ", erl_posix_msg:message(Err)])
    end,
    St. 

%% OPTS UTF8 ON|OFF
opts_utf8(OptArg, Ctl, St) when ?HAS_ICONV(St) ->
    case upcase(OptArg) of
	"ON" ++ _ ->
	    rsend(Ctl, 200, "Enabled UTF8"),
	    St#cstate{use_utf8 = true};
	"OFF" ++ _ ->
	    rsend(Ctl, 200, "Disabled UTF8"),
	    St#cstate{use_utf8 = false}
    end.

noop(_, Ctl, St) ->
    rsend(Ctl, 200),
    St.

feat(_, Ctl, St) ->
    auth_op(Ctl, ?OP_FEAT, St),
    rmsend(Ctl, 211,
	   "Extensions supported:",
	   ["  SIZE",
	    "  MDTM",
	    "  REST STREAM",
	    "  CLNT"] ++
	   if ?HAS_ICONV(St) ->
		   ["  UTF8"];
	      true ->
		   []
	   end,
	   "END"),
    St.

%% opts must be implemented when feat is implemented.
opts(Arg, Ctl, St) ->
    auth_op(Ctl, ?OP_OPTS, St),
    case opts_parse(Arg) of
	{OptFun, OptArgs} ->
	    %% in case of a crash, we'll return 501
	    OptFun(OptArgs, Ctl, St);
	error ->
	    rsend(Ctl, 501),
	    St
    end.

%% help
%% idea: could implement the command: "help cmd" by calling the cmd fun like
%% this: cmd_fun(help) -> [string()]
help(_, Ctl, St) ->
    auth_op(Ctl, ?OP_HELP, St),
    rmsend(Ctl, 214,
	   "The following commands are recognized (* =>'s unimplemented).",
	   ["  USER    PORT    STOR    RNTO    NLST    MKD     CDUP",
	    "  PASS    PASV    APPE    ABOR*   SITE*   XMKD    XCUP",
	    "  ACCT*   TYPE    DELE    SYST    RMD     STOU    SMNT*",
	    "  STRU    ALLO    CWD     STAT    XRMD    SIZE    REIN",
	    "  MODE    REST    XCWD    HELP    PWD     MDTM    QUIT",
	    "  RETR    RNFR    LIST    NOOP    XPWD    FEAT    OPTS",
	    "  CLNT"],
	   "Direct comments to davidw@eidetix.com."),
    St.

%% command not implemented
cni(_, Ctl, St) ->
    rsend(Ctl, 502),    
    St.

%% bad/unkown command
cbad({Cmd,Arg},Ctl, St) ->
    rsend(Ctl, 500, ["command not understood ", atom_to_list(Cmd), " ", Arg]),
    St.

 %% Send a single line standard message
rsend(Ctl, Code) ->
    send(Ctl, Code, [rstr(Code)++ ?CRNL]).

%% Send a single line reply with CRNL
rsend(Ctl, Code, Mesg) when integer(Code) ->
    send(Ctl, Code, [integer_to_list(Code)," ",Mesg, ?CRNL]).

%% send a multi line reply
rmsend(Ctl, Code, Mesg1, Lines, Mesg2) ->
    send(Ctl, Code, [integer_to_list(Code),"-",Mesg1, ?CRNL,
	     map(fun(M) -> [" ", M, ?CRNL] end, Lines),
	     integer_to_list(Code)," ", Mesg2, ?CRNL]).


send(Ctl, Code, Bytes) ->
%%    ?dbg("sending <~s>\n", [Bytes]),
    if Code >= 400 ->
	    ?dbg("error from <~s>\n", [get(line)]),
	    ?dbg("sending <~s>\n", [Bytes]);
       true ->
	    ok
    end,
    gen_tcp:send(Ctl, Bytes).


%% check that Name exist and is of type Type
assert_exists(Ctl, Root, Name, Type) ->
    case file:read_file_info(filename:join(Root,Name)) of
	{ok, Info} ->
	    if Info#file_info.type == Type ->
		    true;
	       true ->
		    rsend(Ctl, 550, ["\"", Name, "\" is not a ",
				     if Type == directory -> "directory";
					true -> "file"
				     end]),
		    throw(failed)
	    end;
	{error,Err} ->
	    rsend(Ctl, 550, ["\"", Name, "\" ", erl_posix_msg:message(Err)]),
	    throw(failed)
    end.
	    

%% check that a user has logged in and report errors
assert_valid(Ctl, St) ->
    case St#cstate.ust of
	invalid -> rsend(Ctl, 530), throw(failed);
	ident ->  rsend(Ctl, 331), throw(failed);
	valid -> true
    end.

assert_ident(Ctl, St) ->
    case St#cstate.ust of
	invalid -> rsend(Ctl, 530), throw(failed);
	ident ->   true;
	valid ->   rsend(Ctl, 503), throw(failed)
    end.
    
assert_arg(Ctl, "") ->
    rsend(Ctl, 501),
    throw(failed);
assert_arg(_Ctl, _Arg) ->
    true.

%%% 
%%% Introducing some security here! Every operation has to
%%% be checked if it is allowed to proceed.
%%%
auth_op(_Ctl, Op, St) when ?bit_is_set(St#cstate.sys_ops, Op) -> 
    ?dbg("Operation <~w> IS allowed, Flags=~p~n", [Op, St#cstate.sys_ops]),
    true;
auth_op(Ctl, Op, St) -> 
    ?dbg("Operation <~w> is NOT allowed, Flags=~p~n", [Op, St#cstate.sys_ops]),
    rsend(Ctl, 550, "Permission denied"),
    throw(failed).


authorize(Op, FileName, Ctl, St) ->
    case St#cstate.user of
	{_UserName, _Passwd, DirAccess} ->
	    case find_dir(DirAccess, FileName) of
		{value, {_Dir, Flags}} ->
		    if ?bit_is_set(Flags, Op) ->
			    true;
		       true ->
			    ?dbg("Dir <~s> does not allow ~p", [_Dir, Flags]),
			    rsend(Ctl, 550, "Permission denied"),
			    throw(failed)
		    end;
		_ ->
		    ?dbg("File <~s> not found", [FileName]),
		    rsend(Ctl, 550, "Permission denied"),
		    throw(failed)
	    end;
	_UserName ->  % leave decision to file system (must fork to work!)
	    true
    end.

find_dir([{Dir, _Flags} = H | T], FileName) ->
    case lists:prefix(Dir, FileName) of
	true -> {value, H};
	false -> find_dir(T, FileName)
    end;
find_dir(_, _) ->
    false.
    
username(St) ->
    case St#cstate.user of
	#user{name = UserName} ->
	    UserName;
	UserName ->
	    UserName
    end.

%% return lower letter space or ?		 
alpha(X) when X >= $A, X =< $Z -> (X-$A)+$a;
alpha(X) when X >= $a, X =< $z -> X;
alpha(X) when X == $  -> X;
alpha(_X) -> $?.
    
alnum(X) when X >= $0, X =< $9 -> X;
alnum($-) -> $-;
alnum(X) -> alpha(X).
     
upcase([H|T]) ->
    if H >= $a, H =< $z -> [(H-$a)+$A | upcase(T)];
       true -> [H | upcase(T)]
    end;
upcase([]) ->
    [].

from_utf8(Str, St) when ?HAS_ICONV(St), ?USE_UTF8(St) ->
    case iconv:conv(St#cstate.iconv_cd_from_utf8, Str) of
	{ok, BinU} -> binary_to_list(BinU);
	_ -> Str
    end;
from_utf8(Str, _) ->
    Str.

to_utf8(StrU, St) when ?HAS_ICONV(St), ?USE_UTF8(St) ->
    case iconv:conv(St#cstate.iconv_cd_to_utf8, StrU) of
	{ok, Bin} -> binary_to_list(Bin);
	_ -> StrU
    end;
to_utf8(StrU, _) ->
    StrU.

generate_unique(Prefix, Dir, Ctl, St) ->
    generate_unique(Prefix, Dir, file_mode(St), Ctl, 0, 1000).

generate_unique(Prefix, Dir, Mode, Ctl, N, Max) when N < Max ->
    {X,Y,Z} = now(),
    PostFix = integer_to_list(X) ++ "-" ++ integer_to_list(Y) ++ "-" ++
              integer_to_list(Z),
    F = Dir ++ Prefix ++ [$. | PostFix],
    case file:open(F, [read, raw]) of
	{error, enoent} ->
	    case file:open(F, [write | Mode]) of
		{ok, Fd} -> Fd;
		{error, Err} ->
		    rsend(Ctl,550, ["error ", erl_posix_msg:message(Err)]),
		    throw(failed)
	    end;
	{ok, Fd} ->
	    file:close(Fd),
	    generate_unique(Prefix, Dir, Mode, Ctl, N+1, Max);
	_ ->
	    generate_unique(Prefix, Dir, Mode, Ctl, N+1, Max)
    end.

    


ctl_line(S, Buf, IdleTimeout) ->
    case split_line(Buf) of
	more ->
	    case gen_tcp:recv(S,0,IdleTimeout) of
		{ok,Cs} ->
		    Buf1 = Buf++Cs,
		    case split_line(Buf1) of
			more -> ctl_line(S, Buf1, IdleTimeout);
			Done -> Done
		    end;
		Error -> Error
	    end;
	Done -> Done
    end.

%% split a line after CRLF
split_line(Cs) ->
    split_line(Cs, []).

split_line([$\r,$\n|Cs], Buf) ->
    {ok, reverse(Buf), Cs};
split_line([X|Cs], Buf) ->
    split_line(Cs, [X|Buf]);
split_line([], _) ->
    more.

%% Standard reply strings and theier meaning
%%
%%
rstr(110) -> "110 MARK yyyy = mmmm";             %% ARGS
rstr(120) -> "120 Service ready in nnn minutes.";  %% ARG
rstr(125) -> "125 Data connection alredy open; transfere starting.";
rstr(150) -> "150 File status okay; about to open data connection.";
rstr(200) -> "200 Command okay.";
rstr(202) -> "202 Command not implemented, superfluos at this site.";
rstr(211) -> "211 System status, or system help reply.";
rstr(212) -> "212 Directory status.";
rstr(213) -> "213 File status.";
rstr(214) -> "214 Help message.";     %% ADD HELP
rstr(215) -> "215 NAME system type";  %% set NAME
rstr(220) -> "220 Service ready for user.";
rstr(221) -> "221 Service closing control connection.";
rstr(225) -> "225 Data connection open; no transfere in progress";    
rstr(226) -> "226 Closing data connection.";  %% ADD INFO
rstr(227) -> "227 Entering Passive Mode (h1,h2,h3,h4,p1,p2).";  %% ARGS
rstr(230) -> "230 User logged in, proceed.";
rstr(250) -> "250 Requested file action okay, completed.";
rstr(257) -> "257 PATHNAME created.";  %% ARG
rstr(331) -> "331 User name okay, need password.";
rstr(332) -> "332 Need account for login.";
rstr(350) -> "350 Requested file action pending further information.";
rstr(421) -> "421 Service not available, closing control connection.";
rstr(425) -> "425 Can't open data connection.";
rstr(426) -> "426 Connection closed; transfere aborted.";
rstr(450) -> "450 Requested file action not taken.";
rstr(451) -> "451 Requested action not taken: local error in processing.";
rstr(452) -> "452 Requested action not taken.";
rstr(500) -> "500 Syntax error, command unrecognized.";  %% ADD INFO
rstr(501) -> "501 Syntax error in parameters or arguments.";
rstr(502) -> "502 Command not implemented.";
rstr(503) -> "503 Bad sequence of commands.";
rstr(504) -> "504 Command not implemented for that parameter.";
rstr(530) -> "530 Not logged in.";
rstr(532) -> "532 Need account for storing files.";
rstr(550) -> "550 Requested action not taken.";
rstr(551) -> "551 Requested action aborted: page type unkown.";
rstr(552) -> "552 Requested file action aborted.";
rstr(553) -> "553 Requested action not taken.".

%%
%% Open data connection
%%
open_data(Ctl, St) ->
    rsend(Ctl, 150),
    if St#cstate.listen =/= undefined ->
	    case gen_tcp:accept(St#cstate.listen) of
		{ok,S} ->
		    case inet:peername(S) of
			{ok, {Addr, _Port}} when Addr == St#cstate.client_ip ->
			    gen_tcp:close(St#cstate.listen),
			    {S, St#cstate {listen = undefined }};
			{ok, {Addr, _Port}} ->
			    log(notice, "Data connection from ~s rejected, "
				"should have been from ~s",
				[inet_parse:ntoa(Addr),
				 inet_parse:ntoa(St#cstate.client_ip)]),
			    gen_tcp:close(S),
			    open_data(Ctl, eaccess);
			{error, Err} ->
			    open_data_err(Ctl,Err)
		    end;
		{error,Err} ->
		    open_data_err(Ctl,Err)
	    end;
       true ->
	    {Addr,Port} = St#cstate.data_port,
	    case gen_tcp:connect(Addr,Port,[{active,false}, binary]) of
		{ok,S} ->
		    {S,St};
		{error,Err} ->
		    open_data_err(Ctl,Err)
	    end
    end.

open_data_err(Ctl,Err) ->
    rsend(Ctl, 421, ["Can't open data connection ", inet:format_error(Err)]),
    throw(failed).

close_listen(St) ->
    if St#cstate.listen == undefined ->
	    St;
       true ->
	    gen_tcp:close(St#cstate.listen),
	    St#cstate { listen = undefined }
    end.

%% Send file data over a socket
send_file(Fd, Chunk, Count, S) ->
    case file:read(Fd, Chunk) of
	eof -> {ok,Count};
	{ok,Data} ->
	    case gen_tcp:send(S, Data) of
		ok -> send_file(Fd, Chunk, Count+size(Data), S);
		Error -> Error
	    end;
	Error -> Error
    end.

%% Receive file data over a socket
recv_file(S, Chunk, Count, Fd) ->
    case gen_tcp:recv(S, 0) of
	{error,closed} -> 
	    {ok,Count};
	{ok, Data} ->
	    case file:write(Fd, Data) of
		ok -> recv_file(S, Chunk, Count+size(Data), Fd);
		Error -> Error
	    end;
	Error -> Error
    end.    

%% file mode is binary or text
file_mode(St) ->
    case St#cstate.type of
	{ascii,_,_} -> [binary]; % no reason to not use binary...
	{image,_,_} -> [raw,binary]
    end.

type(St) ->
    element(1, St#cstate.type).

%%
%% Check if an address is a member of a list of
%% Mask addresses
%%
member_address(IP, [{{MA, MB, MC, MD}, {EA, EB, EC, ED}}|Rest]) ->
    {A, B, C, D} = IP,
    if A band MA == EA,
       B band MB == EB,
       C band MC == EC,
       D band MD == ED ->
	    true;
       true ->
	    member_address(IP, Rest)
    end;
member_address(_, []) ->
    false.

%% parse address on form:
%% d1,d2,d3,d4,p1,p2  => { {d1,d2,d3,d4}, port} -- ipv4
%% h1,h2,...,h32,p1,p2 => {{n1,n2,..,n8}, port} -- ipv6
%%
parse_address(Str) ->
    paddr(Str, 0, []).

paddr([X|Xs],N,Acc) when X >= $0, X =< $9 -> paddr(Xs, N*10+(X-$0), Acc);
paddr([X|Xs],_N,Acc) when X >= $A, X =< $F -> paddr(Xs,(X-$A)+10, Acc);
paddr([X|Xs],_N,Acc) when X >= $a, X =< $f -> paddr(Xs, (X-$a)+10, Acc);
paddr([$,,$,|_Xs], _N, _Acc) -> error;
paddr([$,|Xs], N, Acc) -> paddr(Xs, 0, [N|Acc]);
paddr([],P2,[P1,D4,D3,D2,D1]) -> {ok,{{D1,D2,D3,D4}, P1*256+P2}};
paddr([],P2,[P1|As]) when length(As) == 32 ->
    case addr6(As,[]) of
	{ok,Addr} -> {ok, {Addr, P1*256+P2}};
	error -> error
    end;
paddr(_, _, _) -> error.

addr6([H4,H3,H2,H1|Addr],Acc) when H4<16,H3<16,H2<16,H1<16 ->
    addr6(Addr, [H4 + H3*16 + H2*256 + H1*4096 |Acc]);
addr6([], Acc) -> {ok, list_to_tuple(Acc)};
addr6(_, _) -> error.

format_address({A,B,C,D}, Port) ->
    integer_to_list(A) ++ "," ++
    integer_to_list(B) ++ "," ++
    integer_to_list(C) ++ "," ++
    integer_to_list(D) ++ "," ++
    integer_to_list(Port div 256) ++ "," ++
    integer_to_list(Port rem 256);
format_address({N1,N2,N3,N4,N5,N6,N7,N8},Port) ->
    h4(N1) ++ "," ++ h4(N2) ++ "," ++ h4(N3) ++ "," ++ h4(N4) ++ "," ++
    h4(N5) ++ "," ++ h4(N6) ++ "," ++ h4(N7) ++ "," ++ h4(N8) ++ "," ++
	integer_to_list(Port div 256) ++ "," ++
	integer_to_list(Port rem 256).

h4(N) ->
    [hx(N bsr 12),$,,hx(N bsr 8),$,,hx(N bsr 4),$,, hx(N)].

hx(N) ->
    N1 = N band 16#f,
    if N1 < 10 -> N1+$0;
       true -> (N1-10)+$A
    end.

%%
%% Compose file/directory names
%%
rel_name(Name, Wd) ->
    case filename:pathtype(Name) of
	relative ->
	    rel_path(filename:join(Wd, Name));
	absolute ->
	    rel_path(Name);
	volumerelative ->
	    rel_path(filename:join(Wd,Name))
    end.
%%
%% We sometime need a simulated root, then call abs_name
%%
abs_name(Name) ->
    filename:join("/", Name).

%%
%% rel_path returns a relative path i.e remove
%% and root or volume relative start components
%%
rel_path(Path) ->
    rel_path(filename:split(Path),[]).

rel_path([], []) ->
  "";

%% remove absolute or volume relative stuff
rel_path([Root|Path], RP) ->
    case filename:pathtype(Root) of
	relative -> rpath(Path, [Root|RP]);
	_ -> 
	    rpath(Path, RP)
    end.

rpath([".."|P], [_|RP]) ->  rpath(P, RP);
rpath(["."|P], RP) -> rpath(P, RP);
rpath([F|P], RP) -> rpath(P, [F|RP]);
rpath([],[]) -> "";
rpath([], RP) -> filename:join(reverse(RP)).

%%
%% Generate a directory listing
%% should normally go to the socket
%%
dir_list(Ctl, Data, Dir1, DirU, St, Type) ->
    case file:list_dir(Dir1) of
	{ok, List} ->
	    foreach(
	      fun(E) when Type == nlst ->
		      gen_tcp:send(Data, to_utf8(E, St) ++ ?CRNL);
		 (E) when Type == list ->
		      gen_tcp:send(Data, list_info(Dir1, E, St) ++ ?CRNL)
	      end,
	      List),
	    rsend(Ctl, 226);

	{error,Err} ->
	    rsend(Ctl, 550, "\"" ++ DirU ++ "\" " ++
		  file:format_error(Err))
    end.


list_info(Dir, File, St) ->
    case file:read_file_info(filename:join(Dir,File)) of
	{ok, Info} ->
	    [finfo(Info), " ", to_utf8(File, St)];
	{error,_} ->
	    "???"
    end.
%%
%% BAD: format as access(10) + size(8)+ mdate(8)+ mtime(5)+ filename(n)
%% GOOD: format as access(10) + type + user + group + mdate(8)+ mtime(5)+ filename(n)
%%
finfo(Info) ->
    fmt_type(Info#file_info.type) ++
	fmt_access(Info#file_info.mode) ++ " " ++
	fmt_number(type_num(Info#file_info.type), 2, $ ) ++ " " ++
	fmt_number(Info#file_info.uid,5,$ ) ++ " " ++
	fmt_number(Info#file_info.gid,5,$ ) ++ " "  ++
	fmt_number(Info#file_info.size,8,$ ) ++ " " ++
	printdate(Info#file_info.mtime) ++ " ".

fmt_type(regular) -> "-";
fmt_type(directory) -> "d";
fmt_type(_) -> "?".

type_num(regular) ->
    1;
type_num(directory) ->
    4;
type_num(_) ->
    0.

fmt_access(Mode) ->
    fmt_rwx(Mode bsr 6) ++ fmt_rwx(Mode bsr 3) ++ fmt_rwx(Mode).

fmt_rwx(Mode) ->
    [if Mode band 4 == 0 -> $-; true -> $r end,
     if Mode band 2 == 0 -> $-; true -> $w end,
     if Mode band 1 == 0 -> $-; true -> $x end].

fmt_number(X, N, LeftPad) when X >= 0 ->
    Ls = integer_to_list(X),
    Len = length(Ls),
    if Len >= N -> Ls;
       true ->
	    lists:duplicate(N - Len, LeftPad) ++ Ls
    end.

pmonth_day(Month, Day) ->
    io_lib:format("~s ~2.2w", [month(Month), Day]).

pyear(Year) ->
    io_lib:format(" ~5.5w", [Year]).

ptime(Hours, Min) ->
    io_lib:format(" ~2.2.0w:~2.2.0w", [Hours, Min]).

%% printdate - print the date in a style compatible with ls -l.  If
%% the year of the date is not the current year, the year is output in
%% place of the time:

%% -rw-r--r--  1     0     0      395 May 21 10:11  profile
%% -rw-r--r--  1     0     0      465 Mar 11  1999  nsswitch.conf

printdate({Date, Time}) ->
    {Year, Month, Day} = Date,
    {Hours, Min, _} = Time,
    {LDate, _LTime} = calendar:local_time(),
    {LYear, _, _} = LDate,
    pmonth_day(Month, Day) ++
	if LYear > Year ->
		pyear(Year);
	   true ->
		ptime(Hours, Min)
	end.

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

timeval(DateTime) ->
    case calendar:local_time_to_universal_time_dst(DateTime) of
	[] ->
	    "00000000000000";
	[{{Y,Mo,D},{H,Mi,S}}|_] ->
	    io_lib:format("~4.4.0w~2.2.0w~2.2.0w~2.2.0w~2.2.0w~2.2.0w",
			  [Y,Mo,D,H,Mi,S])
    end.
