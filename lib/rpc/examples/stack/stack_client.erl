%%% File    : stack_client.erl
%%% Author  : Tony Rogvall <tony@erix.ericsson.se>
%%% Purpose : Example program accessing a stack server
%%% Created : 20 Aug 1997 by Tony Rogvall <tony@erix.ericsson.se>
%%% Copyright (c) 2000, 2001 Sendmail, Inc.  All rights reserved.

-module(stack_client).
-author('tony@erix.ericsson.se').

-export([main/1,main/2, main/3]).

-include("stack.hrl").

open(Host) ->
    open(Host, tcp).

open(Host, Proto) ->
    rpc_clnt:open(Host, ?STACK_PROG, ?STACK_VERS, Proto).

open(Host, Proto, Port) ->
    rpc_clnt:open(Host, ?STACK_PROG, ?STACK_VERS, Proto, Port).    

close(Clnt) ->
    rpc_clnt:close(Clnt).

null(Clnt) ->
    case stack_clnt:stackproc_null_1(Clnt) of
	{ok, Reply} -> Reply;
	{error,Reason} -> exit(Reason)
    end.

pop(Clnt) ->
    case stack_clnt:stackproc_pop_1(Clnt) of
	{ok, Reply} -> Reply;
	{error, Reason} -> exit(Reason)
    end.

push(Clnt, Value) -> 
    case stack_clnt:stackproc_push_1(Clnt, Value) of
	{ok, Reply} -> Reply;
	{error, Reason} -> exit(Reason)
    end.

main(Host) -> main1(open(Host)).
main(Host, Proto) -> main1(open(Host,Proto)).
main(Host, Proto, Port) -> main1(open(Host,Proto,Port)).

main1({ok,Clnt}) ->
    L = lists:seq(0, 9),
    lists:foreach(fun(X) -> 
			  push(Clnt, X),
			  io:format("PUSHED ~w~n", [X])
		  end, L),
    lists:foreach(fun(_) ->
			  X = pop(Clnt),
			  io:format("POPPED ~w~n", [X])
		  end, L),
    close(Clnt),
    ok;
main1(Error) -> Error.



    
    
