-module(epop_dbg).
-export([start/1,start/0,dfilter/1]).

start(Pid) when pid(Pid) -> init(Pid).

start() ->
    case whereis(epop_server) of
	Pid when pid(Pid) ->
	    %%ok = error_logger:logfile({open,"epop_dbg.output"}),
	    %%error_logger:tty(false),
	    init(Pid);
	_ ->
	    {error,epop_server_not_found}
    end.

init(Pid) ->
    dbg:p(Pid,[m,c,b,sos]),
    dbg:filter({?MODULE,dfilter}),
    ok.

dfilter({trace, 'receive', From, {dbg, ok}}) ->
    ignore;
dfilter({trace, 'receive', From, Msg}) ->
    error_logger:info_msg( "(~p) << ~p~n", [From, Msg]);
dfilter({trace, send, To, From,{X, {set, X,_}}}) ->
    ignore;
dfilter({trace, send, To, From,Msg}) ->
    error_logger:info_msg( "(~p) ~p ! ~p~n", [From,To,Msg]);
dfilter({trace, From, call, Func}) ->
    error_logger:info_msg( "(~p) call ~s~n",
              [From, lists:flatten(ffunc(Func))]);
dfilter({trace, From, Op, Data}) ->
    error_logger:info_msg( "(~p) ~p ~p~n", [From, Op, Data]);
dfilter(X) ->
    error_logger:info_msg("~p~n", [X]).

ffunc({M,F, Argl}) ->
    io_lib:format("~p:~p(~s)", [M, F, fargs(Argl)]);
ffunc(X) -> io_lib:format("~p", [X]).
fargs([]) -> [];
fargs([A]) -> io_lib:format("~p", [A]);  %% last arg
fargs([A|Args]) -> [io_lib:format("~p,", [A]) | fargs(Args)].

