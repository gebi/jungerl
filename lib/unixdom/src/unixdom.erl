%%% --------------------------------------------------------------------
%%%
%%%   start/0  -  Start the byteorder server.
%%%   stop/0   -  Stop  the byteorder server.
%%%   test/0   -  Returns: {ok,lsb} | {ok,msb} | {error,Reason}
%%%
%%% --------------------------------------------------------------------

-module('unixdom').
-behaviour(gen_server).

-include("unixdom.hrl").
-include_lib("kernel/include/file.hrl").

-define(SERVER_NAME,  ?MODULE).
-define(DRV_NAME,     "unixdom_drv").
-define(DEFAULT_BACKLOG, 5).			% Default listen backlog
-define(Timeout, infinity).

%% Borrowed from inet_int.hrl
%% XXX This should really use bit syntax stuff now.
-define(int32(X), 
        [((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
         ((X) bsr 8) band 16#ff, (X) band 16#ff]).
-define(u32(X3,X2,X1,X0), 
        (((X3) bsl 24) bor ((X2) bsl 16) bor ((X1) bsl 8) bor (X0))).

%% External exports
-export([start_link/0, stop/1, test/1, knuthhash/2]).
-export([connect/3, connect/4, send/2, send/3, recv/2, recv/3,
	 close/1, close/2, setopts/2, listen/3, listen/4, accept/1, accept/2,
	 controlling_process/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

%% For internal use only, use at your peril!
-export([getix/1]).

-record(state, {
	  owner,				% Pid of owner process.
	  port					% Port for our dll driver
	 }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%%% Unlink gen_tcp, you've got to use start_link first.
%%% Normal return: {ok, Sock}
start_link() ->
    %% XXX because we're specifying a servername to register, we can have
    %% XXX only one of these things (at least for now).
    gen_server:start_link(?MODULE, {self()}, []).

%%% Normal return: ok
stop(Sock) ->
    Timeout = ?Timeout,
    gen_server:call(Sock, {stop}, Timeout).

%%% This has nothing to do with UNIX domain sockets.  It's here because
%%% I wanted a really simple test of how to communicate with ports.
%%% This endian check is stolen from 
%%% http://www.erlang.org/contrib/byteorder-1.0.tgz.  Many thanks
%%% to Tobbe Tornkvist (!@#$! US keyboard) for the example.
%%%
%%% Normal return: {ok, lsb} | {ok, msb}
test(Sock) ->
    Timeout = ?Timeout,
    gen_server:call(Sock, {test}, Timeout).

%%% This has nothing to do with UNIX domain sockets.  It's here because
%%% I wanted a not-so-simple simple test of how to communicate with ports.
%%% Erlang's built-in bignum support actually hampers efficient
%%% implementation of this algorithm (brute-force implementation, anyway)
%%% because it relies on integer overflow.
%%%
%%% Normal return: {ok, Hash}
knuthhash(Sock, Path) ->
    Timeout = ?Timeout,
    gen_server:call(Sock, {knuthhash, Path}, Timeout).

%%% Normal return: ok
connect(Sock, Path, OptList) ->
    connect(Sock, Path, OptList, ?Timeout).
connect(Sock, Path, OptList, Timeout) ->
    gen_server:call(Sock, {connect, Path, OptList}, Timeout).

%%% Normal return: ok | {error, ErrnoAtom}
send(Sock, Packet) ->
    send(Sock, Packet, ?Timeout).
send(Sock, Packet, Timeout) ->
    gen_server:call(Sock, {send, Packet}, Timeout).

%%% Normal return: {ok, Data} | {error, closed} | {error, ErrnoAtom}
recv(Sock, Length) ->
    recv(Sock, Length, ?Timeout).
recv(Sock, Length, Timeout) ->
    gen_server:call(Sock, {recv, Length}, Timeout).

%%% Normal return: ok
close(Sock) ->
    close(Sock, ?Timeout).
close(Sock, Timeout) ->
    gen_server:call(Sock, {close}, Timeout).

%%% Normal return: ok
setopts(Sock, OptList) ->
    setopts(Sock, OptList, ?Timeout).
setopts(Sock, OptList, Timeout) ->
    gen_server:call(Sock, {setopts, OptList}, Timeout).

%%% Normal return: ok
listen(Sock, Path, OptList) ->
    listen(Sock, Path, OptList, ?Timeout).
listen(Sock, Path, OptList, Timeout) ->
    gen_server:call(Sock, {listen, Path, OptList}, Timeout).

%%% Normal return: {ok, Sock} | {error, ErrnoAtom}
accept(Sock) ->
    accept(Sock, ?Timeout).
accept(Sock, Timeout) ->
    gen_server:call(Sock, {accept}, Timeout).

%%% Normal return: ok | {error, eperm}
controlling_process(Sock, NewOwner) ->
    gen_server:call(Sock, {controlling_process, self(), NewOwner},
		    infinity).

getix(Sock) ->					% For internal use only!
    gen_server:call(Sock, {getix}, ?Timeout).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init({OwnerPid}) ->
    Path = load_path(),
    erl_ddll:start(),
    ok = erl_ddll:load_driver(Path, ?DRV_NAME),
    Port = open_port({spawn, ?DRV_NAME}, []),
    %%QQQ io:format("XXXYYYXXX init2 1: OwnerPid = ~w, Port = ~w\n", [OwnerPid, Port]),
    %% XXX For now, we're intentionally *not* trapping exits so that we'll
    %% automatically die & close our port if our owner croaks.
    {ok, #state{owner = OwnerPid, port = Port}};
init({OwnerPid, NewPort, MasterSockPid}) ->
    Path = load_path(),
    erl_ddll:start(),
    ok = erl_ddll:load_driver(Path, ?DRV_NAME),
    if
	OwnerPid == MasterSockPid ->
	    ok;
	true ->
	    link(OwnerPid),			% XXX catch needed?
	    unlink(MasterSockPid)
    end,
    %%QQQ io:format("XXXYYYXXX init2 2: OwnerPid = ~w, NewPort = ~w\n", [OwnerPid, NewPort]),
    {ok, #state{owner = OwnerPid, port = NewPort}}.


%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({stop}, From, State) ->
    {stop, normal, ok, State};
handle_call({test}, From, State) ->
    Reply = do_test(State#state.port),
    {reply, Reply, State};
handle_call({knuthhash, Path}, From, State) ->
    Reply = do_knuthhash(Path, State#state.port),
    {reply, Reply, State};
handle_call({connect, Path, OptList}, From, State) ->
    Reply = do_connect(Path, OptList, State#state.port),
    {reply, Reply, State};
handle_call({send, Packet}, From, State) ->
    Reply = do_send_catch(Packet, State#state.port), %XXX catch
    {reply, Reply, State};
handle_call({recv, Length}, From, State) ->
    Reply = do_recv(Length, State#state.port), %XXX catch
    {reply, Reply, State};
handle_call({close}, From, State) ->
    Reply = do_close(State#state.port),
    {reply, Reply, State};
handle_call({setopts, OptList}, From, State) ->
    Reply = do_setopts(State#state.port, OptList),
    {reply, Reply, State};
handle_call({listen, Path, OptList}, From, State) ->
    case catch do_listen(State#state.port, Path, OptList) of
	ok -> {reply, ok, State};
	Else -> {stop, normal, Else, State}	% 'normal' for no logging
    end;
handle_call({accept}, From, State) ->
    Reply = do_accept(State#state.port, State),
    {reply, Reply, State};
handle_call({controlling_process, Owner, NewOwner}, From, State) ->
    case State#state.owner of
	Owner ->
	    %% XXX Deliberately brittle link xfer for now.
	    true = link(NewOwner),
	    unlink(Owner),
	    {reply, ok, State#state{owner = NewOwner}};
	_ ->
	    {reply, {error, eperm}, State}
    end;
handle_call({getix}, From, State) ->
    Reply = do_getix(State#state.port),
    {reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    %%QQQ io:format("XXXYYYXXX ~w: ~s:handle_cast got ~w\n", [self(), ?MODULE, Msg]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({unixdom, Port, Data}, State) when port(Port) ->
    %% Forward Data to our owner proc.
    State#state.owner ! {unixdom, self(), Data},
    {noreply, State};    
handle_info({closed, Port}, State) when port(Port) ->
    %% Forward Data to our owner proc.
    State#state.owner ! {unixdom, self(), closed},
    {noreply, State};    
handle_info({unixdom_reply, Port, {error, closed}}, State) when port(Port) ->
    %% Forward Data to our owner proc.
    State#state.owner ! {unixdom, self(), closed},
    {noreply, State};    
handle_info(Info, State) ->
    %%QQQ io:format("XXXYYYXXX ~w: ~s:handle_info got ~w\n", [self(), ?MODULE, Info]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    State#state.port ! {self(),close},
    erl_ddll:unload_driver(?DRV_NAME),
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Preserve server state across code upgrades
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% We assume that the ddll driver is located
%% at the same directory as the jam/beam file.
%% Thus, this code can't be preloaded or interpreted.
load_path() ->
    case code:is_loaded(?MODULE) of
        {file,File} when list(File) ->
            filename:dirname(File) ++ "/../src";
        _ ->
            Emsg = "~w: Can't find path to load driver from !~n",
            error_logger:error_msg(Emsg,[?MODULE]),
            exit(no_path)
    end.
    
do_test(Port) ->
% old way, via driver's read func:
%    Port ! {self(),{command,list_to_binary("do_endian_test")}},
%    receive
%	{Port,{data,[$l|_]}} -> {ok,lsb};
%	{Port,{data,[$m|_]}} -> {ok,msb};
%	_	             -> failed
%    end.
    case ctl_cmd(Port, ?UNIXDOM_REQ_ENDIAN, []) of
	{ok, [?UNIXDOM_REPBODY_LSB]} ->
	    {ok, lsb};
	{ok, [?UNIXDOM_REPBODY_MSB]} ->
	    {ok, msb};
	Error ->
	    Error
    end.

do_knuthhash(Path, Port) ->
    case ctl_cmd(Port, ?UNIXDOM_REQ_KNUTHHASH, Path) of
	{ok, HashByteList} ->
	    HashBin = list_to_binary(HashByteList), % Call me
	    <<Hash:32/unsigned>> = HashBin,	%     lazy....
	    {ok, Hash};
	Error ->
	    Error
    end.

do_connect(Path, OptList, Port) ->
    case ctl_cmd(Port, ?UNIXDOM_REQ_OPEN, []) of
	{ok, Foo} ->
	    %%QQQ io:format("XXXYYYXXX: ctl_cmd(OPEN) = ~w\n", [Foo]),
	    case ctl_cmd(Port, ?UNIXDOM_REQ_CONNECT, Path) of
		{ok, Foo2} ->
		    %%QQQ io:format("XXXYYYXXX: ctl_cmd(CONNECT) = ~w\n", [Foo]),
		    do_connect2(Port, OptList);
		Error ->
		    %%QQQ io:format("XXXYYYXXX: ctl_cmd(CONNECT) error = ~w\n", [Error]),
		    do_close(Port),
		    Error
	    end;
	Error ->
	    %%QQQ io:format("XXXYYYXXX: ctl_cmd(OPEN) error = ~w\n", [Error]),
	    Error
    end.

do_connect2(Port, OptList) ->
    do_setopts(Port, OptList).

do_send_catch(Packet, Port) ->			%xxx
    catch do_send(Packet, Port).		%xxx
do_send(Packet, Port) ->
    %%QQQ io:format("XXXYYYZZZ do_send: gonna do send now\n"),
    Port ! {self(), {command, Packet}},
    %%QQQ io:format("XXXYYYZZZ do_send: send done, gonna do receive now\n"),
    receive
	{unixdom_reply, Port, Response} -> Response
    end.
% The "old(er)" way of performing a write was to use a ctl_cmd()-style
% method.
%    case ctl_cmd(Port, ?UNIXDOM_REQ_WRITE, Packet) of
%	{ok, _BytesWrittenArray} ->
%	    ok;
%	Error ->
%	    %%QQQ io:format("XXXYYYXXX: do_send: got error ~w\n", [Error]),
%	    Error
%    end.

do_recv(Length, Port) ->
    %%QQQ io:format("XXXYYYXXX do_recv: Length = ~w, Port = ~w\n", [Length, Port]),
    case ctl_cmd(Port, ?UNIXDOM_REQ_RECV, ?int32(Length)) of
	{ok, _X} ->
	    %%QQQ io:format("XXXYYYXXX: do_recv/2: ctl_cmd gave {ok, ~w}\n", [_X]),
	    receive
		{unixdom, Port, Packet} ->
		    %%QQQ io:format("XXXYYYXXX do_recv: got Packet = ~w\n", [Packet]),
		    {ok, Packet};
		{unixdom_reply, Port, wouldblock} ->
		    %% Nothing for us immediately.
		    %%QQQ io:format("do_recv: Port ~w wouldblock, going to do_recv2()\n", [Port]),
		    do_recv2(Length, Port);
		{unixdom_reply, Port, Error} ->
		    %%QQQ io:format("do_recv: failure 1: ~w\n", [Error]),
		    Error;
		Error ->
		    %%QQQ io:format("do_recv: failure 2: ~w\n", [Error]),
		    Error
	    end;
	Error ->
	    %%QQQ io:format("do_recv: failure 3: ~w\n", [Error]),
	    Error
    end.

%%% Handle the case where the recv blocked for input.  The socket has been
%%% put into the select/poll loop, so we'll be notified if something
%%% happens.
do_recv2(Length, Port) ->
    %%QQQ io:format("XXXYYYXXX do_recv2: waiting for async response\n"),
    receive
	{unixdom, Port, Packet} ->
	    %%QQQ io:format("XXXYYYXXX do_recv2: got Packet = ~w\n", [Packet]),
	    {ok, Packet};
	{unixdom_reply, Port, Error} ->
	    %%QQQ io:format("do_recv2: failure 0: ~w\n", [Error]),
	    Error;
	Error ->
	    %%QQQ io:format("do_recv2: failure 1: ~w\n", [Error]),
	    Error
    end.

do_close(Port) ->
    case ctl_cmd(Port, ?UNIXDOM_REQ_CLOSE, []) of
	{ok, _} ->
	    ok;
	Error ->
	    Error
    end.

do_setopts(Port, OptList) ->
    Options = encode_options(OptList),
    %%QQQ io:format("XXXYYYXXX: set_options: port = ~w, Options = ~w\n", [Port, Options]),
    case ctl_cmd(Port, ?UNIXDOM_REQ_SETOPTS, Options) of
	{ok, Foo} ->
	    %%QQQ io:format("XXXYYYXXX do_connect2: set opts res = ~w\n", [Foo]),
	    ok;
	Error ->
	    Error
    end.

do_listen(Port, Path, OptList) ->
    %% XXX Is this being too defensive?
    case file:read_file_info(Path) of
	{ok, Info} when record(Info, file_info),
			Info#file_info.type == other ->
	    case lists:member(unlink_sock, OptList) of
		true ->
		    file:delete(Path);		% Don't care about result
		_ ->
		    throw({error, eexists})
	    end;
	{ok, _} ->
	    throw({error, eexists});
	{error, enoent} ->
	    ok;
	Error ->
	    throw(Error)
    end,
    case ctl_cmd(Port, ?UNIXDOM_REQ_OPEN, []) of
	{ok, Foo} ->
	    %%QQQ io:format("XXXYYYXXX: ctl_cmd(OPEN) = ~w\n", [Foo]),
	    Arg = ?int32(?DEFAULT_BACKLOG) ++ [Path],
	    case ctl_cmd(Port, ?UNIXDOM_REQ_BIND, Arg) of
		{ok, Foo2} ->
		    %%QQQ io:format("XXXYYYXXX do_listen: bind res = ~w\n", [Foo2]),
		    do_listen2(Port, OptList);
		Error2 ->
		    %%QQQ io:format("XXXYYYXXX: do_listen(BIND) error = ~w\n", [Error2]),
		    do_close(Port),
		    Error2
	    end;
	Error3 ->
	    %%QQQ io:format("XXXYYYXXX: do_listen(OPEN) error = ~w\n", [Error3]),
	    Error3
    end.

do_listen2(Port, OptList) ->
    %% Call to listen(2) is implicit in UNIXDOM_REQ_BIND with default
    %% listen backlog.  If a listen backlog is specified in OptList, it'll
    %% get reset to the desired depth.
    do_setopts(Port, OptList).

%%% Holy !&#W$!(&*@!W$#!, is doing an accept a royal pain in the !@#$!.
%%% Since accept(2) creates a new socket, there needs to be a new Erlang
%%% port to manage that new socket.  (There needs to be a new descriptor
%%% process to manage that port, too, but that's a simple matter.)
%%% As far as I can tell, there is no simple way for a driver to create
%%% another port descriptor within that driver.  ARRGH!  The way both
%%% R6B and R7B work around that limitation is:
%%%   1. In Erlang, open a new port.  Its state will be CLOSED, by default.
%%%   2. In Erlang, get the driver index (the "ix") of port with the
%%%      listening socket.
%%%   3. In Erlang, send the REQ_ACCEPT (along with a timeout value and
%%%      the listening port's ix) to the new port.
%%%   4. In the C driver, the listening port's ix is extracted, which
%%%      allows the driver to then find the socket associated with it,
%%%      do the accept(2), and take care of the rest of its housekeeping.
%%%
%%% So, I'll do it the same way.  {sigh}

do_accept(Port, State) ->
    case do_getix(Port) of
	{ok, IX} ->
	    case open_port({spawn, ?DRV_NAME}, []) of
		NewPort when port(NewPort) ->
		    case ctl_cmd(NewPort, ?UNIXDOM_REQ_ACCEPT, ?int32(IX)) of
			{ok, []} ->
			    do_accept2(NewPort, State);
			{ok, [?UNIXDOM_ACCEPT_WOULDBLOCK]} ->
			    do_accept1(Port, NewPort, State);
			Error ->
			    Error
		    end;
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

%%% Handle the case where the accept blocked.  We'll get an async message
%%% if a new connection comes in or if an error occurs.

do_accept1(Port, NewPort, State) ->
    receive
	{unixdom_reply, Port, ok} ->
	    do_accept2(NewPort, State);
	Error ->
	    %%QQQ io:format("XXXYYYXXX do_accept1: Error = ~w\n", [Error]),
	    Error
    end.

%%% Finish the accept by creating a new descriptor process and transfer
%%% ownership of the new port to that descriptor process.

do_accept2(NewPort, State) ->
    case gen_server:start_link(?MODULE, {State#state.owner,
					 NewPort, self()}, []) of
	{ok, DescPid} ->
	    case catch erlang:port_connect(NewPort, DescPid) of
		true ->
		    unlink(NewPort),
		    {ok, DescPid};
		{'EXIT', Reason} ->
		    {error, Reason}
	    end;
	Error ->
	    Error
    end.

do_getix(Port) ->
    case ctl_cmd(Port, ?UNIXDOM_REQ_GETIX, []) of
	{ok, [A1, A2, A3, A4]} ->
	    {ok, ?u32(A1, A2, A3, A4)};
	Error ->
	    Error
    end.

ctl_cmd(Port, Cmd, Args) ->
    case catch port_control(Port, Cmd, Args) of
        [?UNIXDOM_REP_OK | Reply]  -> {ok, Reply};
        [?UNIXDOM_REP_ERROR | Err] -> {error, list_to_atom(Err)};
	{badarg, _Backtrace}       -> {error, badarg};
        {'EXIT', E}                -> %%QQQ io:format("XXXYYYXXX ctl_cmd(~w, ~w, ~w): E = ~w\n", [Port, Cmd, Args, E]),
	                              {error, eXXXinval};
        E_                         -> {error, internalXXX, E_}
    end.

encode_options(Opts) ->
    encode_options(Opts, []).
encode_options([], Acc) ->
    lists:reverse([?UNIXDOM_OPT_ENDOFLIST|Acc]);
encode_options([H|T], Acc) ->
    encode_options(T, [enc_opt(H)|Acc]).

enc_opt({active, true}) ->
    %% XXX "once" mode not implemented
    [?UNIXDOM_OPT_ACTIVE, ?UNIXDOM_OPT_ACTIVE_TRUE];
enc_opt({active, false}) ->
    %% XXX "once" mode not implemented
    [?UNIXDOM_OPT_ACTIVE, ?UNIXDOM_OPT_ACTIVE_FALSE];
enc_opt({backlog, Size}) when integer(Size), Size >= 0 ->
    [?UNIXDOM_OPT_BACKLOG, ?int32(Size)];
enc_opt(unlink_sock) ->
    ?UNIXDOM_OPT_IGNORE;			% Option not handled by driver
enc_opt(Bogus) ->
    ?UNIXDOM_OPT_IGNORE.

