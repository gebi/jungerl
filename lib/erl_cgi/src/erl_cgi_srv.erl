%%% File    : erl_cgi_srv.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : CGI server 
%%% Created : 24 Sep 2002 by Tony Rogvall <tony@bix.hemma.se>

-module(erl_cgi_srv).

-include("erl_cgi.hrl").

-compile(export_all).


-define(SERVER_NAME, erl_cgi_srv).

-define(DEFAULT_CGI_PORT, 3344).
-define(ENV_DATA,         1).
-define(ENV_END,          0).
-define(LOAD_ENV_TIMEOUT, 10000).

start() ->
    start(?DEFAULT_CGI_PORT).

start(Port) ->
    case whereis(?SERVER_NAME) of
	undefined ->
	    case srv_start(Port) of
		{ok,Pid} -> {ok,Pid};
		Error -> Error
	    end;
	Pid ->
	    {ok,Pid}
    end.

stop() ->  call(stop).

%% return the port number to the cgi server
port() ->
    case call(port) of
	{ok,Port} ->
	    Port;
	{error,Reason} ->
	    exit({error,Reason})
    end.

call(Call) ->
    case start() of
	{ok,Pid} -> call(Pid,Call);
	Error -> Error
    end.

call(Pid, Call) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {call,[Ref|self()],Call},
    receive
	{Ref, Res} -> 
	    erlang:demonitor(Ref),
	    Res;
	{'DOWN', Ref, process, Pid, Reason} -> 
	    {error,Reason}
    end.

reply([Ref|Pid], Reply) ->
    Pid ! {Ref,Reply}.

srv_start(Port) ->
    Ref = make_ref(),
    Pid = spawn(?MODULE, srv_init, [[Ref|self()], Port]),
    receive
	{Ref,Res} -> Res
    end.

srv_init(Caller,Port) ->
    case catch register(?SERVER_NAME, self()) of
	{'EXIT',Reason} ->
	    case whereis(?SERVER_NAME) of
		undefined ->
		    reply(Caller, {error, internal});
		Pid ->
		    reply(Caller, {ok,Pid})
	    end;
	true ->
	    case gen_tcp:listen(Port, [{active,false},{reuseaddr,true}]) of
		{ok,Listen} ->
		    reply(Caller, {ok, self()}),
		    self() ! {next,[]},
		    process_flag(trap_exit, true),
		    srv_loop(Listen, [], make_ref());
		Error ->
		    Caller ! {self(), Error}
	    end
    end.


srv_loop(Listen, PrevPid, PrevRef) ->
    receive
	{next,PrevPid} ->
	    erlang:demonitor(PrevRef),
	    NewPid = spawn(?MODULE, srv_accept, [self(), Listen]),
	    NewRef = erlang:monitor(process, NewPid),
	    srv_loop(Listen, NewPid, NewRef);
	{'DOWN', Ref, process, Accept, Reason} -> 
	    NewPid = spawn(?MODULE, srv_accept, [self(), Listen]),
	    NewRef = erlang:monitor(process, NewPid),
	    srv_loop(Listen, NewPid, NewRef);
	{call,Caller,port} ->
	    reply(Caller, inet:port(Listen)),
	    srv_loop(Listen, PrevPid, PrevRef);
	{call,Caller,stop} ->
	    reply(Caller, ok),
	    if PrevPid =/= [] ->
		    exit(PrevPid, kill);
	       true ->
		    ok
	    end,
	    ok;
	Other ->
	    io:format("cgi_srv_loop: got ~p\n", [Other]),
	    srv_loop(Listen, PrevPid, PrevRef)
    end.

srv_accept(Loop, Listen) ->
    case gen_tcp:accept(Listen) of
	{ok,S} ->
	    Loop ! {next,self()},
	    case catch main(S) of
		{'EXIT', Reason} ->
		    io:format("Process ~p died because of : ~p\n", [self(), Reason]),
		    {error,Reason};
		Value ->
		    Value
	    end;
	Error ->
	    Loop ! {next,self()},
	    io:format("Accept error: ~p\n", [Error]),
	    false
    end.



main(S) ->
    Cgi = #cgi {
      env = ets:new(env, [set]),
      send = fun(Data) -> gen_tcp:send(S, Data) end,
      recv = fun(N) -> case gen_tcp:recv(S, N) of
			   {error,closed} -> eof;
			   {error,E} -> {error,E};
			   Other -> Other
		       end
	     end
     },
    load_env(S, Cgi#cgi.env),
    erl_cgi:run(Cgi),
    gen_tcp:close(S),
    ok.



%% Load the CGI environment
load_env(S, Tab) ->
    inet:setopts(S, [{packet,4},{active,once},{exit_on_close,false}]),
    load_env1(S, Tab),
    inet:setopts(S, [{packet,0}]),
    ok.

load_env1(S, Tab) ->
    receive
	{tcp,S,[?ENV_DATA|KV]} ->
	    case string:chr(KV, $=) of
		0 -> ets:insert(Tab, {KV,""});
		I ->
		    Key = string:substr(KV, 1, I-1),
		    Val = string:substr(KV, I+1, length(KV)),
		    ets:insert(Tab, {Key,Val})
	    end,
	    inet:setopts(S, [{active,once}]),
	    load_env1(S, Tab);
	{tcp,S,[?ENV_END]} ->
	    ok
    after ?LOAD_ENV_TIMEOUT ->
	    exit(load_env_timeout)
    end.
