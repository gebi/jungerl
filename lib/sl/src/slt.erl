%%% File    : slt.erl
%%% Author  : Tony Rogvall <tony@a55.hemma.se>
%%% Description : Serial line terminal
%%% Created :  8 Oct 2002 by Tony Rogvall <tony@a55.hemma.se>

-module(slt).

-compile(export_all).

start(Dev, Opts) ->
    spawn(?MODULE, init, [Dev, Opts]).

init(Dev, Opts) ->
    {ok, T} = gterm:run(),
    {ok, S} = sl:open(Dev, Opts),
    gterm:connect(T, self()),
    loop(T, S).

loop(T, S) ->
    receive
	{T, {input, Keys}} ->
	    sl:send(S, Keys),
	    loop(T, S);
	{T, eof} ->
	    sl:close(S),
	    sl:stop(S),
	    ok;
	{S, {data, Data}} ->
	    gterm:send_data(T, Data),
	    loop(T, S)
    end.
    
    


