%%% --------------------------------------------------------------------
%%%
%%% unixdom2.erl: An Erlang-process-descriptor-less version of unixdom.erl
%%%
%%% --------------------------------------------------------------------

-module(unixdom2).

-include("unixdom.hrl").
-include_lib("kernel/include/file.hrl").

-define(SERVER_NAME,  ?MODULE).
-define(DRV_NAME,     "unixdom_drv").
-define(DEFAULT_BACKLOG, 5).			% Default listen backlog
-define(Timeout, 16#ffffffff).			% infinity!

%% Borrowed from inet_int.hrl
-define(u32(X3,X2,X1,X0), 
        (((X3) bsl 24) bor ((X2) bsl 16) bor ((X1) bsl 8) bor (X0))).

%% External exports
-export([start_link/0, stop/1, test/1, knuthhash/2]).
-export([connect/3, connect/4, send/2, send/3, recv/2, recv/3,
	 close/1, close/2, listen/3, listen/4, accept/1, accept/2,
	 controlling_process/2, setopts/2, getopts/2]).

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
    Path = load_path(),
    erl_ddll:start(),
    ok = erl_ddll:load_driver(Path, ?DRV_NAME),
    Port = open_port({spawn, ?DRV_NAME}, []),
    {ok, Port}.

%%% Normal return: ok
stop(Sock) when port(Sock) ->
    Res = case catch erlang:port_close(Sock) of
	      true                           -> ok;
	      {'EXIT', {Reason, _Backtrace}} -> Reason
	  end,
    erl_ddll:unload_driver(?DRV_NAME),
    Res.

%%% Normal return: {ok, lsb} | {ok, msb}
test(Sock) when port(Sock) ->
    do_test(Sock).

%%% Normal return: {ok, Hash}
knuthhash(Sock, Path) when port(Sock) ->
    do_knuthhash(Path, Sock).

%%% Normal return: ok
connect(Sock, Path, OptList) when port(Sock) ->
    connect(Sock, Path, OptList, ?Timeout).
connect(Sock, Path, OptList, Timeout) when port(Sock) ->
    do_connect(Path, OptList, Sock, Timeout).

%%% Normal return: ok | {error, ErrnoAtom}
send(Sock, Packet) when port(Sock) ->
    send(Sock, Packet, ?Timeout).
send(Sock, Packet, Timeout) when port(Sock) ->
    do_send_catch(Packet, Sock).

%%% Normal return: {ok, Data} | {error, closed} | {error, ErrnoAtom}
recv(Sock, Length) when port(Sock) ->
    recv(Sock, Length, ?Timeout).
recv(Sock, Length, Timeout) when port(Sock) ->
    do_recv(Length, Sock, Timeout).

%%% Normal return: ok
close(Sock) when port(Sock) ->
    close(Sock, ?Timeout).
close(Sock, Timeout) when port(Sock) ->
    do_close(Sock).

%%% Normal return: ok
setopts(Sock, OptList) when port(Sock) ->
    setopts(Sock, OptList, ?Timeout).
setopts(Sock, OptList, Timeout) when port(Sock) ->
    do_setopts(Sock, OptList).

%%% Normal return: {ok, R}, R = OptValue | [OptValue, ...]
getopts(Sock, OptList) when port(Sock) ->
    getopts(Sock, OptList, ?Timeout).
getopts(Sock, OptList, Timeout) when port(Sock) ->
    do_getopts(Sock, OptList, Timeout).

%%% Normal return: ok
listen(Sock, Path, OptList) when port(Sock) ->
    listen(Sock, Path, OptList, ?Timeout).
listen(Sock, Path, OptList, Timeout) when port(Sock) ->
    do_listen(Sock, Path, OptList).

%%% Normal return: {ok, Sock} | {error, ErrnoAtom}
accept(Sock) when port(Sock) ->
    accept(Sock, ?Timeout).
accept(Sock, Timeout) when port(Sock) ->
    do_accept(Sock, Timeout).

%%% Normal return: ok | {error, eperm}
controlling_process(Sock, NewOwner) when port(Sock), pid(NewOwner) ->
    do_controlling_process(Sock, NewOwner).

getix(Sock) when port(Sock) ->					% For internal use only!
    gen_server:call(Sock, {getix}, ?Timeout).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% We assume that the ddll driver is located
%% at the same directory as the jam/beam file.
%% Thus, this code can't be preloaded or interpreted.
load_path() ->
    code:priv_dir(unixdom).
    
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

do_connect(Path, OptList, Port, Timeout) ->
    case ctl_cmd(Port, ?UNIXDOM_REQ_OPEN, []) of
	{ok, Foo} ->
	    %%QQQ io:format("XXXYYYXXX: ctl_cmd(OPEN) = ~w\n", [Foo]),
	    PathBin = binify(Path),
	    case ctl_cmd(Port, ?UNIXDOM_REQ_CONNECT,
			 <<Timeout:32, PathBin/binary>>) of
		{ok, Foo2} ->
		    %%QQQ io:format("XXXYYYXXX: ctl_cmd(CONNECT) = ~w\n", [Foo]),
		    do_connect2(Port, OptList);
		{error, wouldblock} ->
		    do_connect1(Port, OptList);
		Error ->
		    %%QQQ io:format("XXXYYYXXX: ctl_cmd(CONNECT) error = ~w\n", [Error]),
		    do_close(Port),
		    Error
	    end;
	Error ->
	    %%QQQ io:format("XXXYYYXXX: ctl_cmd(OPEN) error = ~w\n", [Error]),
	    Error
    end.

%% do_connect1: our connection attempt blocked, wait for async response.
do_connect1(Port, OptList) ->
    %%QQQ io:format("XXXYYYXXX do_connect1: waiting for async response\n"),
    receive
	{unixdom_reply, Port, ok} ->
	    %%QQQ io:format("do_connect1: got ok\n"),
	    do_connect2(Port, OptList);
	{unixdom_reply, Port, Error} ->
	    %%QQQ io:format("do_connect1: got error: ~w\n", [Error]),
	    Error;
	Error ->
	    %%QQQ io:format("do_recv2: failure 1: ~w\n", [Error]),
	    Error
    end.

%% do_connect2: our connection attempt was successful, so finish setup.
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

do_recv(Length, Port, Timeout) ->
    %%QQQ io:format("XXXYYYXXX do_recv: Length = ~w, Port = ~w\n", [Length, Port]),
    case ctl_cmd(Port, ?UNIXDOM_REQ_RECV, <<Timeout:32, Length:32>>) of
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
	{unixdom_reply, Port, {error, Error}} ->
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
	    Arg = [<<?DEFAULT_BACKLOG:32>> | Path],
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

do_accept(Port, Timeout) ->
    case do_getix(Port) of
	{ok, IX} ->
	    case open_port({spawn, ?DRV_NAME}, []) of
		NewPort when port(NewPort) ->
		    case ctl_cmd(NewPort, ?UNIXDOM_REQ_ACCEPT,
				 <<IX:32, Timeout:32>>) of
			{ok, []} ->
			    do_accept2(NewPort);
			{error, wouldblock} ->
			    do_accept1(Port, NewPort);
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

do_accept1(Port, NewPort) ->
    receive
	{unixdom_reply, Port, ok} ->
	    do_accept2(NewPort);
	{unixdom_reply, NewPort, Error} ->
	    %%QQQ io:format("XXXYYYXXX do_accept1: 2 Error = ~w\n", [Error]),
	    Error;
	Error ->
	    %%QQQ io:format("XXXYYYXXX do_accept1: Error = ~w\n", [Error]),
	    Error
    end.

do_accept2(NewPort) ->
    {ok, NewPort}.

do_getix(Port) ->
    case ctl_cmd(Port, ?UNIXDOM_REQ_GETIX, []) of
	{ok, [A1, A2, A3, A4]} ->
	    {ok, ?u32(A1, A2, A3, A4)};
	Error ->
	    Error
    end.

do_controlling_process(Sock, NewOwner) ->
    case erlang:port_info(Sock, connected) of
        {connected, Pid} when Pid /= self() ->
            {error, not_owner};
        _ ->
            {ok, ActiveStatus} = getopts(Sock, active),
            setopts(Sock, [{active, false}]),
            case unixdom_sync_input(Sock, NewOwner, false) of
                true ->
                    %%  %% socket already closed, 
                    ok;
                false ->
                    case catch erlang:port_connect(Sock, NewOwner) of
                        true -> 
                            unlink(Sock), %% unlink from port
                            setopts(Sock, [{active, ActiveStatus}]),
                            ok;
                        {'EXIT', Reason} -> 
                            {error, Reason}
                    end
            end
    end.

%% unixdom_sync_input: take any Sock-related messages in our mailbox and
%% forward them to its new owner, Owner.

unixdom_sync_input(Sock, Owner, Flag) ->
    receive
        {unixdom, Sock, Data} ->
            Owner ! {unixdom, Sock, Data},
            unixdom_sync_input(Sock, Owner, Flag);
        {closed, Sock} ->
            Owner ! {closed, Sock},
            unixdom_sync_input(Sock, Owner, true);
        {unixdom_reply, Sock, Msg} ->
            Owner ! {unixdom_reply, Sock, Msg},
            unixdom_sync_input(Sock, Owner, Flag);
	%% XXX Debugging clauses
	T when tuple(T), element(1, T) == unixdom ->
	    io:format("XXXYYYZZZ Hey, got unixdom-related msg T = ~w\n", [T]),
            Owner ! T,
            unixdom_sync_input(Sock, Owner, Flag);
	T when tuple(T), element(1, T) == unixdom_reply ->
	    io:format("XXXYYYZZZ Hey, got unixdom-related msg T = ~w\n", [T]),
            Owner ! T,
            unixdom_sync_input(Sock, Owner, Flag)
    after 0 -> 
            Flag
    end.

do_getopts(Sock, Opt, Timeout) when atom(Opt) ->
    case getopts(Sock, [Opt]) of
	{ok, [{_, Value}]} ->
	     {ok, Value};
	Error ->
	    Error
    end;
do_getopts(Sock, OptList, Timeout) when list(OptList) ->
    Req = encode_opts_req(OptList),
    case ctl_cmd(Sock, ?UNIXDOM_REQ_GETOPTS, Req) of
	{ok, Res} ->
	    decode_getopt_res(Res, length(OptList));
	Error ->
	    Error
    end.

decode_getopt_res(L, Len) ->
    decode_getopt_res(L, Len, []).
decode_getopt_res([?UNIXDOM_OPT_ENDOFLIST|_], Len, Acc) ->
    if length(Acc) =/= Len -> {error, einval};
       true -> {ok, lists:reverse(Acc)}
    end;
decode_getopt_res([?UNIXDOM_OPT_IGNORE|T], Len, Acc) ->
    decode_getopt_res(T, Len, Acc);
decode_getopt_res([?UNIXDOM_OPT_ACTIVE, Status|T], Len, Acc) ->
    decode_getopt_res(T, Len, [{active, Status == 1}|Acc]);
decode_getopt_res([?UNIXDOM_OPT_BACKLOG, A1,A2,A3,A4|T], Len, Acc) ->
    B = <<A1, A2, A3, A4>>,
    <<Backlog:32/signed-integer>> = B,
    decode_getopt_res(T, Len, [{backlog, Backlog}|Acc]);
decode_getopt_res([H|T], Len, Acc) ->
    io:format("XXXYYYXXX ~s:decode_getopt_res: bogus ~w\n", [?MODULE, H]),
    decode_getopt_res(T, Len, Acc).

ctl_cmd(Port, Cmd, Args) ->
    case catch port_control(Port, Cmd, Args) of
        [?UNIXDOM_REP_OK | Reply]  -> {ok, Reply};
        [?UNIXDOM_REP_ERROR | Err] -> {error, list_to_atom(Err)};
        [?UNIXDOM_REP_WOULDBLOCK | Err] -> {error, wouldblock};
	{badarg, _Backtrace}       -> {error, badarg};
        {'EXIT', {Reason, _Backtrace}}                -> %%QQQ io:format("XXXYYYXXX ctl_cmd(~w, ~w, ~w): Reason, _Backtrace= ~w, ~w\n", [Port, Cmd, Args, Reason, _Backtrace]),
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
    [?UNIXDOM_OPT_BACKLOG, <<Size:32>>];
enc_opt(unlink_sock) ->
    ?UNIXDOM_OPT_IGNORE;			% Option not handled by driver
enc_opt(Bogus) ->
    ?UNIXDOM_OPT_IGNORE.

encode_opts_req(L) ->
    F = fun(X) -> enc_opt_req_only(X) end,
    lists:map(F, L) ++ [?UNIXDOM_OPT_ENDOFLIST].

enc_opt_req_only(active) ->
    ?UNIXDOM_OPT_ACTIVE;
enc_opt_req_only(backlog) ->
    ?UNIXDOM_OPT_BACKLOG;
enc_opt_req_only(unlink_sock) ->
    ?UNIXDOM_OPT_IGNORE;			% Option not handled by driver
enc_opt_req_only(Bogus) ->
    ?UNIXDOM_OPT_IGNORE.

binify(T) when binary(T) ->
    T;
binify(T) when list(T) ->
    list_to_binary(T).
