%%%----------------------------------------------------------------------
%%% File    : ex1.erl
%%% Author  : Claes Wikstrom <klacke@kaja.hemma.net>
%%% Purpose : 
%%% Created : 22 Nov 2000 by Claes Wikstrom <klacke@kaja.hemma.net>
%%%----------------------------------------------------------------------

-module(ex1).
-author('klacke@kaja.hemma.net').

-compile(export_all).

run() ->
    slang:init_tty(7,0,1),
    slang:set_abort_signal(null),
    loop().

loop() ->
    io:format("Press any key to quit press ctl_G ",[]),
    X = slang:getkey(),
    io:format("Got key ~p~n", [X]),
    case X of
	7 ->
	    slang:reset_tty(),
	    halt(),
	    7;
	_ ->
	    loop()
    end.


