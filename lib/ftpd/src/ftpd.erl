%%% File    : ftpd.erl
%%% Maintainer: <davidw@eidetix.com>
%%% Author  :  <tony@RIOJA>
%%% Purpose : FTP SERVER (RFC 765) (must update to 959)
%%% Created : 29 Jan 1998 by  <tony@RIOJA>

%%% $Id$

%%% Updates by David N. Welton <davidw@eidetix.com> May 2004.

-module(ftpd).
-author('tony@RIOJA').

-compile(export_all).
-export([start/1, start/0]).
-export([init/3, control/2]).

-import(lists, [reverse/1, map/2, append/1, foreach/2]).

-include_lib("kernel/include/file.hrl").

-define(FTPD_PORT, 21).
-define(FTPD_MAX_CONN, 40).
-define(FTPD_LOGFILE, "ftpd.log").

-define(is_ip(X), size(X)==4, 
		   (element(1,X) bor element(2,X) bor 
		   element(3,X) bor element(4,X)) band (bnot 255) == 0).

%% ftpd state record
-record(state,
	{
	  ftp_port = ?FTPD_PORT,            %% port that ftpd listens on
	  tcp_opts = [{active,false},{nodelay,true}], %% gen_tcp options
	  max_connections = ?FTPD_MAX_CONN, %% max connections
	  %% hosts allowed
	  allow_hosts = [{{0,0,0,0},{0,0,0,0}}], %% all allowed
	  %% hosts denied
	  deny_hosts = [],                  %% none denied
	  %% restricted users allowed
	  allow_ruser = [anonymous,ftp,www],
	  %% root directory
	  rootwd = "",
	  log_fd,                           %% log to file
	  log_file = ""                     %% log file name
	 }).

%% connection state record
%%
%% ust is the user state
%% invalid   - user is undefined
%% ident     - user is identified
%% valid
%%
-record(cstate,
	{
	  ust = invalid,              %% internal state
	  user = "",
	  password = "",
	  account = "",
	  rootwd = "",                 %% real root directory
	  homewd = "",                 %% home working directory
	  wd = "",                     %% current working directory
	  structure = file,            %% file(F), record(R), page(P)
	  mode = stream,               %% stream(S), block(B), compressed(C)
	  type = {ascii,nonprint,8},   %% ascii(A),
	  def_data_port,               %% default data port
	  data_port = undefined,       %% set by port
	  listen = undefined           %% listen socket for pasv
	 }).

-define(CRNL, "\r\n").
-define(R_OKAY, 200).

start() ->
    start([{root, "/tmp"}]).

start(Opts) ->
    Tag = make_ref(),
    Pid = spawn(?MODULE, init, [self(), Tag, Opts]),
    receive
	{Tag,Reply} -> Reply
    end.

stop() ->
    call(stop).

call(Req) ->
    call(ftpd, Req).
call(Srv, Req) ->
    Tag = make_ref(),
    Srv ! {call,self(),Tag,Req},
    receive
	{Tag,Reply} ->
	    Reply
    end.

reply(Pid,Tag,Reply) ->
    Pid ! {Tag,Reply}.

init(Pid,Tag,Opts) ->
    case catch register(ftpd, self()) of
	true ->
	    case options(Opts, #state { }) of
		{ok,St} ->
		    case gen_tcp:listen(St#state.ftp_port,St#state.tcp_opts) of
			{ok,Listen} ->
			    process_flag(trap_exit, true),
			    reply(Pid,Tag,{ok,ftpd}),
			    server(Listen, St, 
				   spawn_link(?MODULE,control,
					      [self(),Listen]));
			Error -> reply(Pid,Tag,Error)
		    end; 
		Error -> reply(Pid,Tag,Error)
	    end;
	{'EXIT', _} ->
	    reply(Pid,Tag,{error, already_started});
	Error ->
	    reply(Pid,Tag,Error)
    end.
%%
%% Valid options are:
%%  {port,P}            -- the ftpd listen port other than ?DEFAULT_FTPD_PORT
%%  {ip,Addr}           -- ip address to bind to {0,0,0,0} is the default
%%  {allow, IP}
%%  {deny, IP}
%%  {root, Dir}         -- set the root diretory 
%%  {max_connections,N} -- set max connections
%%
%%
options([Opt | Opts], St) ->
    case Opt of
	{port,P} when P > 0, P < 65536 ->
	    options(Opts, St#state { ftp_port = P });
	
	{ip,IP} when ?is_ip(IP) ->
	    options(Opts, St#state { tcp_opts = 
				     [{ip,IP} | St#state.tcp_opts] });
	{allow, IP} when ?is_ip(IP) ->
	    options(Opts, St#state { allow_hosts = 
				     [{IP,{255,255,255,255}}|
				      St#state.allow_hosts]});
	{allow, {IP,Mask}} when ?is_ip(IP),?is_ip(Mask) ->
	    options(Opts, St#state { allow_hosts = 
				     [{IP,Mask}|St#state.allow_hosts]});
	{deny, IP} when ?is_ip(IP) ->
	    options(Opts, St#state { deny_hosts =
				     [{IP,{255,255,255,255}}|
				      St#state.deny_hosts]});
	{deny, {IP,Mask}} when ?is_ip(IP), ?is_ip(Mask) ->
	    options(Opts, St#state { deny_hosts =
				     [{IP,Mask}|St#state.deny_hosts]});
	{max_connections,N} when integer(N), N >= 0 ->
	    options(Opts, St#state { max_connections = N });
	{root, Dir} ->
	    options(Opts, St#state { rootwd = Dir });
	_ ->
	    {error, {bad_option, Opt}}
    end;
options([], St) ->
    {ok, St}.

%%
%% Server loop
%%
server(Listen, St, Accept) ->
    receive
	{call,From,Tag,Request} ->
	    case handle_call(Request, St) of
		{reply, Reply, St1} ->
		    reply(From,Tag,Reply),
		    server(Listen,St1,Accept);
		{noreply, St1} ->
		    server(Listen,St1,Accept);
		{stop, St1} ->
		    true
	    end;
	{accepted, Accept} ->
	    unlink(Accept),
	    server(Listen, St, spawn_link(?MODULE,control,[self(),Listen]));

	{'EXIT',Accept,Reason} ->
	    server(Listen, St, spawn_link(?MODULE,control,[self(),Listen]));
	Other ->
	    io:format("ftpd: got ~p~n", [Other]),
	    server(Listen, St, Accept)
    end.

handle_call({is_allowed, Addr, Port}, St) ->
    io:format("ftpd: is_allowed ? ~p:~p~n", [Addr,Port]),
    %% 1. check if denied => false
    %% 2. check if allowed => true
    %% 3. => false
    Deny = member_address(Addr, St#state.deny_hosts),
    Allow = member_address(Addr, St#state.allow_hosts),
    Res = not Deny and Allow,
    io:format("ftpd: deny=~p, allow=~p, res=~p~n", [Deny, Allow, Res]),
    {reply, Res, St};
handle_call(rootwd, St) ->
    {reply, St#state.rootwd, St};
handle_call(stop, St) ->
    {stop, St};
handle_call(Req, St) ->
    {reply, {bad_request,Req}, St}.

%%
%% Control channel setup
%%
control(Srv, Listen) ->
    case gen_tcp:accept(Listen) of
	{ok,S} ->
	    Srv ! {accepted, self()},
	    control_init(Srv, S);
	Error ->
	    exit(bad_accept)
    end.

%% 
%% Control channel init
%%
control_init(Srv, Ctl) ->
    case inet:peername(Ctl) of
	{ok,{Addr,Port}} ->
	    case call({is_allowed,Addr,Port}) of
		true ->
		    ctl_loop_init(Ctl, call(rootwd),{Addr,Port-1});
		false ->
		    gen_tcp:close(Ctl)
	    end;
	{error,Err} ->
	    io:format("ftpd: error in inet:peername ~p~n",[Err]),
	    gen_tcp:close(Ctl)
    end.

ctl_loop_init(Ctl, Root, DefaultDataPort) ->
    {ok,Name} = inet:gethostname(),
    rsend(Ctl,220, Name ++ " Erlang Ftp server 1.0 ready."),
    ctl_loop(Ctl, #cstate { rootwd = Root,
			    data_port = DefaultDataPort,
			    def_data_port = DefaultDataPort }, []).

ctl_loop(Ctl, St, Buf) ->
    case ctl_line(Ctl,Buf) of
	{ok,Line,Buf1} ->
	    case ctl_parse(Line) of
		{Fun,Args} ->
		    case catch Fun(Args,Ctl,St) of
			failed -> ctl_loop(Ctl,St,Buf1);
			quit -> true;
			init -> ctl_loop_init(Ctl, St#cstate.rootwd,
					      St#cstate.def_data_port);
			St1 when record(St1, cstate) ->
			    ctl_loop(Ctl,St1,Buf1);
			_ -> %% Crash etc
			    rsend(Ctl,501,"argument error: " ++ Line),
			    ctl_loop(Ctl,St,Buf1)
		    end;
		error ->
		    rsend(Ctl,500,"syntax error: " ++ Line),
		    ctl_loop(Ctl, St, Buf1)
	    end;
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


ctl_parse(user, Arg) -> {fun user/3, Arg};
ctl_parse(pass, Arg) -> {fun pass/3, Arg};
ctl_parse(acct, Arg) -> {fun cni/3, Arg};
ctl_parse(cwd, Arg) -> {fun cwd/3, Arg};
ctl_parse(cdup,Arg) -> {fun cdup/3, Arg};
ctl_parse(smnt,Arg) -> {fun cni/3, Arg};
ctl_parse(quit,Arg) -> {fun quit/3 ,Arg};
ctl_parse(rein,Arg) -> {fun rein/3, Arg};
ctl_parse(port,Arg) -> {fun port/3, Arg};
ctl_parse(pasv,Arg) -> {fun pasv/3, Arg};
ctl_parse(type,Arg) -> {fun type/3, Arg};
ctl_parse(stru,Arg) -> {fun stru/3, Arg};
ctl_parse(mode,Arg) -> {fun mode/3, Arg};
ctl_parse(retr,Arg) -> {fun retr/3, Arg};
ctl_parse(stor,Arg) -> {fun stor/3, Arg};
ctl_parse(stou,Arg) -> {fun cni/3, Arg};
ctl_parse(appe,Arg) -> {fun cni/3, Arg};
ctl_parse(allo,Arg) -> {fun cni/3, Arg};
ctl_parse(rest,Arg) -> {fun cni/3, Arg};
ctl_parse(rnfr,Arg) -> {fun cni/3, Arg};
ctl_parse(rnto,Arg) -> {fun cni/3, Arg};
ctl_parse(abor,Arg) -> {fun cni/3, Arg};
ctl_parse(dele,Arg) -> {fun dele/3, Arg};
ctl_parse(rmd,Arg)  -> {fun rmd/3, Arg};
ctl_parse(xrmd,Arg) -> {fun rmd/3, Arg};
ctl_parse(pwd,Arg)  -> {fun pwd/3, Arg};
ctl_parse(xpwd,Arg) -> {fun pwd/3, Arg};
ctl_parse(mkd,Arg)  -> {fun mkd/3, Arg};
ctl_parse(xmkd,Arg) -> {fun mkd/3, Arg};
ctl_parse(list,Arg) -> {fun lst/3, Arg};
ctl_parse(nlst,Arg) -> {fun nlst/3, Arg};
ctl_parse(site,Arg) -> {fun cni/3, Arg};
ctl_parse(syst,Arg) -> {fun cni/3, Arg};
ctl_parse(stat,Arg) -> {fun cni/3, Arg};
ctl_parse(help,Arg) -> {fun help/3, Arg};
ctl_parse(noop,Arg) -> {fun noop/3, Arg};
ctl_parse(Cmd,Arg) ->  {fun cbad/3,{Cmd,Arg}}.

%% Commands
%% Reply wiht {ok, NewState}
%% or {error, Code}
%%
user(Name, S, St) ->
    rsend(S, 331),
    St#cstate { ust = ident, user = Name, homewd = "/", wd = "/" }.

pass(Password, S, St) ->
    assert_ident(S, St),
    %% check that we have executed user and need a password
    %% then that the password is valid
    rsend(S, 230, "User " ++ St#cstate.user ++ " logged in, proceed"),
    St#cstate { password = Password, ust = valid }.



%% Change working directory we must keep an absoulte path (emulated
%% so that symbolic links are transparent).
cwd(Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    Dir = rel_name(Arg, St#cstate.wd),
    assert_exists(Ctl, St#cstate.rootwd, Dir, directory),
    rsend(Ctl, 250, "new directory \"" ++ abs_name(Dir) ++ "\""),
    St#cstate { wd = Dir }.

mkd(Arg, S, St) ->
    assert_valid(S, St),
    DirR = rel_name(Arg, St#cstate.wd),
    DirA = abs_name(DirR),
    Dir = filename:join(St#cstate.rootwd, DirR),
    case file:make_dir(Dir) of
	ok ->
	    rsend(S, 257, " \"" ++ DirA ++ "\" directory created");
	{error,eexist} ->
	    rsend(S, 521, " \"" ++ DirA ++ "\" directory exists");
	{error,Err} ->
	    rsend(S, 521, " \"" ++ DirA ++ "\" " ++ 
		  erl_posix_msg:message(Err))
    end,
    St.

dele(Arg, S, St) ->
    assert_valid(S, St),
    FileR = rel_name(Arg, St#cstate.wd),
    FileA = abs_name(FileR),
    File =  filename:join(St#cstate.rootwd, FileR),
    case file:delete(File) of
	ok ->
	    rsend(S, 250, "\"" ++ FileA ++ "\" deleted");
	{error,Err} ->
	    rsend(S, 550, "\"" ++ FileA ++ "\" " ++ 
		  erl_posix_msg:message(Err))
    end,
    St. 

rmd(Arg, S, St) ->
    assert_valid(S, St),
    DirR = rel_name(Arg, St#cstate.wd),
    DirA = abs_name(DirR),
    Dir =  filename:join(St#cstate.rootwd, DirR),
    case file:del_dir(Dir) of
	ok ->
	    rsend(S, 250, " \"" ++ DirA ++ "\" removed");
	{error,Err} ->
	    rsend(S, 550, "\"" ++ DirA ++ "\" " ++ 
		  erl_posix_msg:message(Err))
    end,
    St.    
    
%% Change to parent directory
cdup(Arg, S, St) ->
    assert_valid(S, St),
    DirR = rel_name("..", St#cstate.wd),
    DirA = abs_name(DirR),
    assert_exists(S, St#cstate.rootwd, DirR, directory),
    rsend(S, 250, "directory changed to \"" ++ DirA ++ "\""),
    St#cstate { wd = DirR }.

pwd(Arg, S, St) ->
    assert_valid(S, St),
    rsend(S, 257, "\"" ++ abs_name(St#cstate.wd) ++ "\""),
    St.

quit(_, S, St) ->
    rsend(S, 221),
    gen_tcp:close(S),
    quit.

noop(_, S, St) ->
    rsend(S, 200),
    St.

mode(Arg, S, St) ->
    assert_valid(S, St),
    Mode = case alpha(hd(Arg)) of
	       $s -> stream;
	       $b -> block;
	       $c -> compressed
	   end,
    rsend(S, 200, "new mode " ++ atom_to_list(Mode)),
    St#cstate { mode = Mode }.

stru(Arg, S, St) ->    
    assert_valid(S, St),
    Stru = case alpha(hd(Arg)) of
	       $f -> file;
	       $r -> record;
	       $p -> page
	   end,
    rsend(S, 200, "new file structure " ++ atom_to_list(Stru)),
    St#cstate { structure = Stru }.    

type(Arg, S, St) ->
    assert_valid(S, St),
    Type = case alpha(hd(Arg)) of
	       $i -> {image,nonprint,8};
	       $a -> {ascii,nonprint,8};
	       _ -> rsend(S, 504), throw(St)
	   end,
    rsend(S,200,"new type " ++ atom_to_list(element(1,Type))),
    St#cstate { type = Type }.
		    
rein(_, S, St) ->
    close_listen(St),
    init.

pasv(Arg, S, St) ->
    assert_valid(S, St),
    St1 = close_listen(St),
    {ok,{Addr,_}} = inet:sockname(S),
    case gen_tcp:listen(0, [{active,false}, binary]) of
	{ok,L} ->
	    {ok,{_,Port}} = inet:sockname(L),
	    rsend(S,227,"Entering Passive Mode (" ++
		  format_address(Addr,Port) ++ ")."),
	    St1#cstate { listen = L };
	{error,Err} ->
	    rsend(S, 425, erl_posix_msg:message(Err)),
	    St1
    end.

port(Arg, S, St) ->
    assert_valid(S, St),
    St1 = close_listen(St),
    {ok,AddrPort} = parse_address(Arg),
    rsend(S,200),
    St1#cstate { data_port = AddrPort, listen = undefined }.


lst(Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    DirR = rel_name(Arg, St#cstate.wd),
    assert_exists(Ctl, St#cstate.rootwd, DirR, directory),
    Dir = filename:join(St#cstate.rootwd, DirR),
    {S,St1} = open_data(Ctl, St),
    dir_list(Ctl, S, Dir, DirR, list),
    gen_tcp:close(S),
    %% rsend(Ctl, 200),
    St1.

    
nlst(Arg,Ctl,St) ->
    assert_valid(Ctl, St),
    DirR = rel_name(Arg, St#cstate.wd),
    assert_exists(Ctl, St#cstate.rootwd, DirR, directory),
    Dir = filename:join(St#cstate.rootwd, DirR),
    {S,St1} = open_data(Ctl, St),
    dir_list(Ctl, S, Dir, DirR, nlst),
    gen_tcp:close(S),
    %% rsend(Ctl, 200), 
    St1.
    
%%
%% store file from data connection onto file given by Arg
%%
stor(Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    NameR = rel_name(Arg, St#cstate.wd),
    NameA = abs_name(NameR),
    Name = filename:join(St#cstate.rootwd, NameR),
    case file:open(Name, [write | file_mode(St)]) of
	{ok,Fd} ->
	    {S,St1} = open_data(Ctl, St),
	    case recv_file(S, 1024, 0, Fd) of
		{ok,Count} ->
		    rsend(Ctl,226, "closing data connection," ++
			  " recived " ++ 
			  integer_to_list(Count) ++ " bytes");
		    %%rsend(Ctl,200);

		{error,Err} ->
		    rsend(Ctl,226, "closing data connection," ++
			  " aborted"),
		    rsend(Ctl,550,
			  " error " ++
			  erl_posix_msg:message(Err))
	    end,
	    gen_tcp:close(S),
	    file:close(Fd),
	    St1;
	{error,Err} ->
	    rsend(Ctl,550,
		  " error " ++
		  erl_posix_msg:message(Err)),
	    St
    end.

%%
%% retrive a file over a data connection file name is given by Arg
%%
retr(Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    NameR = rel_name(Arg, St#cstate.wd),
    Name = filename:join(St#cstate.rootwd, NameR),
    case file:open(Name, [read | file_mode(St)]) of
	{ok,Fd} ->
	    {S,St1} = open_data(Ctl, St),
	    case send_file(Fd, 1024, 0, S) of
		{ok,Count} ->
		    rsend(Ctl,226, "closing data connection," ++
			  " sent " ++ 
			  integer_to_list(Count) ++ " bytes");
		    %%rsend(Ctl,200);

		{error,Err} ->
		    rsend(Ctl,226, "closing data connection," ++
			  " aborted"),
		    rsend(Ctl,550,
			  " error " ++
			  erl_posix_msg:message(Err))
	    end,
	    gen_tcp:close(S),
	    file:close(Fd),
	    St1;
	{error,Err} ->
	    rsend(Ctl,550,
		  " error " ++
		  erl_posix_msg:message(Err)),
	    St
    end.

%% command not implemented
cni(_, S, St) ->
    rsend(S, 502),    
    St.

%% help
help(_, S, St) ->
    rmsend(S, 214,
	   "The following commands are recognized (* =>'s unimplemented).",
	   ["  USER    PORT    STOR    MSAM*   RNTO    NLST    MKD     CDUP",
	    "  PASS    PASV    APPE    MRSQ*   ABOR    SITE    XMKD    XCUP",
	    "  ACCT*   TYPE    MLFL*   MRCP*   DELE    SYST    RMD     STOU",
	    "  SMNT*   STRU    MAIL*   ALLO    CWD     STAT    XRMD    SIZE",
	    "  REIN*   MODE    MSND*   REST    XCWD    HELP    PWD     MDTM",
	    "  QUIT    RETR    MSOM*   RNFR    LIST    NOOP    XPWD" ],
	   "Direct comments to davidw@eidetix.com."),
    St.

%% bad/unkown command
cbad({Cmd,Arg},S, St) ->
    rsend(S, 500, "command not understood " ++ atom_to_list(Cmd) ++
	 " " ++ Arg),
    St.

 %% Send a single line standard message
rsend(S, Code) ->
    gen_tcp:send(S, [rstr(Code)++ ?CRNL]).

%% Send a single line reply with CRNL
rsend(S, Code, Mesg) when integer(Code) ->
    gen_tcp:send(S, [integer_to_list(Code)," ",Mesg, ?CRNL]).

%% send a multi line reply
rmsend(S, Code, Mesg1, Lines, Mesg2) ->
    gen_tcp:send(S, [integer_to_list(Code),"-",Mesg1, ?CRNL,
		     map(fun(M) -> [" ", M, ?CRNL] end, Lines),
		     integer_to_list(Code)," ", Mesg2, ?CRNL]).


%% check that Name exist and is of type Type
assert_exists(S, Root, Name, Type) ->
    case file:read_file_info(filename:join(Root,Name)) of
	{ok, Info} ->
	    if Info#file_info.type == Type ->
		    true;
	       true ->
		    rsend(S, 550, "\"" ++ Name ++ "\" is not a " ++
			  if Type == directory -> "directory";
			     true -> "file"
			  end),
		    throw(failed)
	    end;
	{error,Err} ->
	    rsend(S, 550, "\"" ++ Name ++ "\" " ++ erl_posix_msg:message(Err)),
	    throw(failed)
    end.
	    

%% check that a user has logged in and report errors
assert_valid(S, St) ->
    case St#cstate.ust of
	invalid -> rsend(S, 530), throw(failed);
	ident ->  rsend(S, 331), throw(failed);
	valid -> true
    end.

assert_ident(S, St) ->
    case St#cstate.ust of
	invalid -> rsend(S, 530), throw(failed);
	ident ->   true;
	valid ->   rsend(S, 503), throw(failed)
    end.
    
%% return lower letter space or ?		 
alpha(X) when X >= $A, X =< $Z -> (X-$A)+$a;
alpha(X) when X >= $a, X =< $z -> X;
alpha(X) when X == $  -> X;
alpha(X) -> $?.
    

ctl_line(S, Buf) ->
    case split_line(Buf) of
	more ->
	    case gen_tcp:recv(S,0) of
		{ok,Cs} ->
		    Buf1 = Buf++Cs,
		    case split_line(Buf1) of
			more -> ctl_line(S, Buf1);
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
rstr(501) -> "501 Syntax error in paramters or arguments.";
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
		    gen_tcp:close(St#cstate.listen),
		    {S, St#cstate {listen = undefined }};
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
    rsend(Ctl, 421, "Can't open data connection " ++
	  inet:format_error(Err)),
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
	{ascii,_,_} -> [];
	{image,_,_} -> [binary]
    end.
	    
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
paddr([X|Xs],N,Acc) when X >= $A, X =< $F -> paddr(Xs,(X-$A)+10, Acc);
paddr([X|Xs],N,Acc) when X >= $a, X =< $f -> paddr(Xs, (X-$a)+10, Acc);
paddr([$,,$,|Xs], N, Acc) -> error;
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
dir_list(Ctl, S, Dir1, Dir, Type) ->
    case file:list_dir(Dir1) of
	{ok, List} ->
	    foreach(
	      fun(E) when Type == nlst ->
		      gen_tcp:send(S, E ++ ?CRNL);
		 (E) when Type == list ->
		      gen_tcp:send(S, list_info(Dir1, E) ++ ?CRNL)
	      end,
	      List),
	    rsend(Ctl, 226);

	{error,Err} ->
	    rsend(Ctl, 550, "\"" ++ Dir ++ "\" " ++
		  file:format_error(Err))
    end.


list_info(Dir, File) ->
    case file:read_file_info(filename:join(Dir,File)) of
	{ok, Info} ->
	    finfo(Info) ++ " " ++ File;
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
    {LDate, LTime} = calendar:local_time(),
    {LYear, _, _} = LDate,
    Result = pmonth_day(Month, Day) ++
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
