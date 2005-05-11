%%%
%%% Created: Sebastian Strollo <seb@erix.ericsson.se>, 1998-08-03
%%% Purpose: Passes open file descriptors between processes using
%%%          AF_UNIX stream sockets, as described in Stevens UNIX
%%%          Network programming.
%%% Modified:
%%%

-module(fdsrv).
-vsn("$Revision$ ").
-behaviour(gen_server).

%% External exports
-export([start/0, start_link/0, bind_socket/2, stop/0]).
-export([test/0, test/1]).
-export([start_driver/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, terminate/2, code_change/3]).

-record(state, {dport, sport}).

%% Bytes to unsigned
-define(u32(X3,X2,X1,X0), 
        (((X3) bsl 24) bor ((X2) bsl 16) bor ((X1) bsl 8) bor (X0))).

-define(F(Format,Args),
        lists:flatten(io_lib:format(Format,Args))).

-define(FATAL(String_),
        error_logger:error_report(fatal,{?MODULE,?LINE,false,String_})).


-define(TYPE(Type), if
			Type == tcp -> 0;
			true        -> 1
		    end).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

bind_socket(Type, IP_Port) ->
    Spec = create_spec(IP_Port),
    gen_server:call(?MODULE, {bind_socket, Type, Spec}).

stop() ->
    gen_server:call(?MODULE, stop).

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
init(_) ->
    process_flag(trap_exit, true),
    TmpName = tmpname(),
    os:cmd(["rm -f ", TmpName]),
    FdSrv = filename:join(code:priv_dir(fd_server), "fdsrv"),
    SPort = open_port({spawn, lists:flatten([FdSrv, " ", TmpName])},
		      [use_stdio, {packet, 1}]),
    receive
	{SPort, {data, "ok"}} ->
	    case start_driver(TmpName) of
		{ok, DPort} ->
		    {ok, #state{dport = DPort, sport = SPort}};
		Error ->
		    {stop, Error}
	    end;
	{'EXIT', SPort, Reason} ->
	    ?FATAL(?F("Couldn't start ~s - ~p", [FdSrv, Reason])),
	    {stop, {error, Reason}}
    
    end.

start_driver(TmpName) ->
    PrivDir = code:priv_dir(fd_server),
    case driver_check(erl_ddll:load_driver(PrivDir, "fdsrv_drv")) of
	ok ->
	    Drv = lists:flatten(["fdsrv_drv ", TmpName]),
	    DPort = open_port({spawn, Drv}, []),
	    {ok, DPort};
	Error ->
	    {error, {Error, PrivDir}}
    end.

driver_check(ok) -> ok;
driver_check({error,already_loaded}) -> ok;
driver_check(Error) -> Error.


%%----------------------------------------------------------------------
%%----------------------------------------------------------------------

handle_call({bind_socket, Type, Spec}, From, State) ->
    #state{dport = DPort, sport = SPort} = State,
    case lists:member(Type, [tcp, udp]) of
	true ->
	    SPort ! {self(), {command, [?TYPE(Type)|Spec]}},
	    {reply, recv_port(DPort, SPort), State};
	_ ->
	    {reply, {error, "Bad socket type"}, State}
    end;

handle_call(stop, From, State) ->
    {stop, normal, ok, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_info({'EXIT', _, Reason}, State) ->
    {stop, Reason, State};

handle_info({Port, {data, [1|Str]}}, State) ->
    error_logger:format("fdsrv: ~s~n", [Str]),
    {noreply, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    #state{dport = DPort, sport = SPort} = State,
    DPort ! {self(), close},
    SPort ! {self(), close}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

recv_port(Dp, Sp) ->
    recv_port(Dp, Sp , unknown).
recv_port(Dp, Sp, LastErr) ->
    receive
	{Dp, {data, [1,X1,X2,X3,X4]}} ->
	    {ok, ?u32(X1,X2,X3,X4)};
	{Dp, {data, [0]}} ->
	    {error, LastErr};
	{Dp, {data, [0|Chars]}} ->
	    error_logger:format("fdsrv: ~s~n", [Chars]),
	    recv_port(Dp, Sp, Chars);
	{Sp, {data, Error}} ->
	    {error, Error}
    end.




tmpname() ->
    no_nl(lists:reverse(os:cmd("echo /tmp/fdsrv$$"))).
no_nl([10|T]) ->
    lists:reverse(T);
no_nl(L) ->
    lists:reverse(L).

create_spec(Port) when integer(Port) ->
    [$:|integer_to_list(Port)];
create_spec({{IP1,IP2,IP3,IP4}, Port}) when integer(IP1),
					    integer(IP2),
					    integer(IP3),
					    integer(IP4),
					    integer(Port) ->
    lists:flatten(io_lib:format("~p.~p.~p.~p:~p",
				[IP1, IP2, IP3, IP4, Port])).

%%
%%
%%
test() ->
    test(8888).
test(Port) ->
    fdsrv:start(),
    case fdsrv:bind_socket(tcp, Port) of
	{ok, Fd} ->
	    {ok, LSock} = gen_tcp:listen(0, [{fd, Fd},
					     {active, false},
					     {packet, 0},
					     {reuseaddr,true}]),
	    io:format("listening on port ~w (fd ~w, sock ~w), please connect to it.~n",
		      [Port, Fd, LSock]),
	    {ok, Sock} = gen_tcp:accept(LSock),
	    {ok, Data} = gen_tcp:recv(Sock, 0),
	    gen_tcp:send(Sock, Data),
	    gen_tcp:close(Sock),
	    gen_tcp:close(LSock),
	    ok;
	Error ->
	    Error
    end.

