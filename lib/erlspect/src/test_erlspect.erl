%%% File    : test_erlspect.erl
%%% Author  : Mikael Karlsson <mikael.karlsson@creado.com>
%%% Description : 
%%% Created : 13 April 2004

-module(test_erlspect).

-export([ls_beam/0, telnet_http/0]).

%% Run ls_beam() from the ebin directory.
ls_beam()->
    P = erlspect:exp_spawn("ls"),
    {found,{A1,A2}} = erlspect:expect(P,[{"beam",found}]),
    io:format("Match buffer:~s, Upto match:~s ~n", [A1,A2]),
    A3 = erlspect:buffered(P),

    io:format("Buffered:~s", [A3]),

    {found,{A4,A5}} = erlspect:expect(P,[{"beam",found}]),
    io:format("Match buffer:~s, Upto match:~s ~n", [A4,A5]),

    C = erlspect:expect(P,eof),
    io:format("~w~n", [C]),
    done.

telnet_http()->    
    P = erlspect:exp_spawn("telnet www.erlang.org 80"),
    erlspect:send(P,"GET  http://www.erlang.org/ HTTP/1.0\n\n"),
    case erlspect:expect(P,[{"400",error}, {"\(HTML|html\)", found}],10) of
	{error, _} ->
	    io:format("Error:");
	{found,_} ->
	    {endofpage,{_,A}} = erlspect:expect(P,[{"\/body", endofpage}],10),
	    io:format("Up to match: ~s~n", [A]);
	Any  -> 
	    io:format("No match: ~w~n", [Any])	    
    end,
    io:format("Buffered: ~s~n", [erlspect:buffered(P)]),
    erlspect:expect(P,eof,10),
    erlspect:close(P). % Not necessary if telnet session is closed from server 

