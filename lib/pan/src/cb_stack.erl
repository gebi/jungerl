%%%----------------------------------------------------------------------
%%% File    : cb_stack.erl
%%% Author  : Mats Cronqvist <etxmacr@avc386>
%%% Purpose : 
%%% Created :  8 Oct 2001 by Mats Cronqvist <etxmacr@avc386>
%%%----------------------------------------------------------------------

-module(cb_stack).
-author('etxmacr@avc386').

-export([go/1, requires/0, doc/0]).

-record(state, {}).

requires() -> [{flags, [call]}, 
	       {tps, [{'_','_','_',{'_',[],[{message,{process_dump}}]},'_'}]}].
doc() -> "prints the call return stack of the traced functions".

go([Message, Line, Out, initial]) ->
    io:fwrite(Out, "~p: initializing~n", [?MODULE]),
    go([Message, Line, Out, initial_state()]);
go([end_of_trace, Line, Out, State]) ->
    io:fwrite(Out, "~p - exiting - msgs: ~w~n", [?MODULE, Line]);
go([{call, {Pid, _} = P, {MFA, Bin}, TS}, Line, Out, State]) when binary(Bin) ->
    io:fwrite(Out, "#########~p ~s~n~p ~p~n", [node(Pid), ntform(TS), P, MFA]),
    out(Out, P, MFA, string:tokens(binary_to_list(Bin),"\n")),
    State;
go([{_, {Pid, _}, _, TS} = Message, Line, Out, State]) ->
    io:fwrite(Out, "#########~p ~s~n~p~n", [node(Pid), ntform(TS), Message]),
    State.

initial_state() ->
    #state{}.

out(Out, P, MFA, []) -> ok;
out(Out, P, MFA, [I|T]) ->
    case string:str(I, "Return addr") of
	0 -> 
	    case string:str(I, "cp = ") of
		0 -> ok;
		_ -> 
		    [_, C|_] = string:tokens(I,"()+"),
		    io:fwrite(Out, "    ~s~n", [C])
	    end;
	_ ->
	    case string:str(I, "terminate process normally") of
		0 ->
		    [_, C|_] = string:tokens(I,"()+"),
		    io:fwrite(Out, "    ~s~n", [C]);
		_ -> ok
	    end
    end,
    out(Out, P, MFA, T).

ntform({_, _, Us} = Now) ->
    {_,{H, M, S}} = calendar:now_to_datetime(Now),
    io_lib:fwrite("~2.2.0w:~2.2.0w:~2.2.0w.~3.3.0w", [H, M, S, round(Us/1000)]).
