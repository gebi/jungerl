%%% File    : sl.erl
%%% Author  : Tony Rogvall <tony@a55.hemma.se>
%%% Description : Serial line control
%%% Created :  7 Oct 2002 by Tony Rogvall <tony@a55.hemma.se>

-module(sl).


-define(SL_CONNECT,    1).
-define(SL_DISCONNECT, 2).
-define(SL_OPEN,       3).
-define(SL_CLOSE,      4).
-define(SL_XOFF,       5).
-define(SL_XON,        6).
-define(SL_BREAK,      7).
-define(SL_UPDATE,     8).
-define(SL_GET_RATES,  9).
-define(SL_REVERT,     10).

-define(SL_SET_DEV,    20).
-define(SL_SET_BAUD,   22).
-define(SL_SET_CSIZE,  26).
-define(SL_SET_BUFSZ,  28).
-define(SL_SET_BUFTM,  30).
-define(SL_SET_STOPB,  32).
-define(SL_SET_PARITY, 34).
-define(SL_SET_HWFLOW, 36).
-define(SL_SET_SWFLOW, 38).
-define(SL_SET_XONCHAR,    40).
-define(SL_SET_XOFFCHAR,   42).
-define(SL_SET_ECHO,   44).
-define(SL_SET_MODE,   46).
-define(SL_SET_EOLCHAR, 48).
-define(SL_SET_EOL2CHAR, 50).


-define(SL_GET_DEV,    21).
-define(SL_GET_BAUD,   23).
-define(SL_GET_CSIZE,  27).
-define(SL_GET_BUFSZ,  29).
-define(SL_GET_BUFTM,  31).
-define(SL_GET_STOPB,  33).
-define(SL_GET_PARITY, 35).
-define(SL_GET_HWFLOW, 37).
-define(SL_GET_SWFLOW, 39).
-define(SL_GET_XONCHAR,    41).
-define(SL_GET_XOFFCHAR,   43).
-define(SL_GET_ECHO,   45).
-define(SL_GET_MODE,   47).
-define(SL_GET_EOLCHAR, 49).
-define(SL_GET_EOL2CHAR, 51).

-define(SL_OK,    0).
-define(SL_ERROR, 1).

-define(SL_INT,   0).
-define(SL_BOOL,  1).
-define(SL_STRING,2).

-define(SL_MODE_RAW,    0).
-define(SL_MODE_LINE,   1).

-compile(export_all).

start() ->
    start([]).

start(OpenOpts) ->
    erl_ddll:load_driver(code:priv_dir(sl), "sl_drv"),
    open_port({spawn, "sl_drv"}, OpenOpts).

stop(P) when port(P) ->
    erlang:port_close(P).

options() ->
    [dev, baud, csize, bufsz, buftm, stopb, parity,
     hwflow, swflow, xonchar, xoffchar, eolchar, eol2char, 
     echo, mode].


getopts(P, [Opt|Opts]) ->
    case getopt(P, Opt) of
	{ok, Val} -> [{Opt,Val} | getopts(P, Opts)];
	Error -> getopts(P, Opts)
    end;
getopts(P, []) -> [].


getopt(P, Opt) ->
    case Opt of
	rates  -> reply0(erlang:port_control(P, ?SL_GET_RATES, []));
	dev    -> reply0(erlang:port_control(P, ?SL_GET_DEV, []));
	baud   -> reply0(erlang:port_control(P, ?SL_GET_BAUD, []));
	csize  -> reply0(erlang:port_control(P, ?SL_GET_CSIZE, []));
	bufsz  -> reply0(erlang:port_control(P, ?SL_GET_BUFSZ, []));
	buftm  -> reply0(erlang:port_control(P, ?SL_GET_BUFTM, []));
	stopb  -> reply0(erlang:port_control(P, ?SL_GET_STOPB, []));
	parity -> reply0(erlang:port_control(P, ?SL_GET_PARITY, []));
	hwflow -> reply0(erlang:port_control(P, ?SL_GET_HWFLOW, []));
	swflow -> reply0(erlang:port_control(P, ?SL_GET_SWFLOW, []));
	xonchar    -> reply0(erlang:port_control(P, ?SL_GET_XONCHAR, []));
	xoffchar   -> reply0(erlang:port_control(P, ?SL_GET_XOFFCHAR, []));
	eolchar    -> reply0(erlang:port_control(P, ?SL_GET_EOLCHAR, []));
	eol2char   -> reply0(erlang:port_control(P, ?SL_GET_EOL2CHAR, []));
	echo   -> reply0(erlang:port_control(P, ?SL_GET_ECHO, []));
	mode  ->
	    case reply0(erlang:port_control(P, ?SL_GET_MODE, [])) of
		{ok,?SL_MODE_RAW} -> {ok, raw};
		{ok,?SL_MODE_LINE} -> {ok, line};
		Other -> Other
	    end;
	_ -> {error, {bad_opt, Opt}}
    end.
    
setopt(P, Opt, Arg) ->
    case Opt of
	dev    -> reply0(erlang:port_control(P, ?SL_SET_DEV, Arg));    
	baud   -> reply0(erlang:port_control(P, ?SL_SET_BAUD, <<Arg:32>>));
	csize  -> reply0(erlang:port_control(P, ?SL_SET_CSIZE, <<Arg:32>>));
	bufsz  -> reply0(erlang:port_control(P, ?SL_SET_BUFSZ, <<Arg:32>>));
	buftm  -> reply0(erlang:port_control(P, ?SL_SET_BUFTM, <<Arg:32>>));
	stopb  -> reply0(erlang:port_control(P, ?SL_SET_STOPB, <<Arg:32>>));
	parity -> reply0(erlang:port_control(P, ?SL_SET_PARITY, <<Arg:32>>));
	hwflow -> reply0(erlang:port_control(P, ?SL_SET_HWFLOW, bool(Arg)));
	swflow -> reply0(erlang:port_control(P, ?SL_SET_SWFLOW, bool(Arg)));
	xoffchar -> reply0(erlang:port_control(P, ?SL_SET_XOFFCHAR, <<Arg:32>> ));
	xonchar  -> reply0(erlang:port_control(P, ?SL_SET_XONCHAR, <<Arg:32>>));
	eolchar -> reply0(erlang:port_control(P,?SL_SET_EOLCHAR,<<Arg:32>>));
	eol2char -> reply0(erlang:port_control(P,?SL_SET_EOL2CHAR,<<Arg:32>>));
	echo   -> reply0(erlang:port_control(P, ?SL_SET_ECHO, bool(Arg)));
	binary -> ok;
	mode   ->
	    if integer(Arg) ->
		    reply0(erlang:port_control(P, ?SL_SET_MODE,<<Arg:32>>));
		Arg==raw -> 
		    reply0(erlang:port_control(P, ?SL_SET_MODE, 
					       <<?SL_MODE_RAW:32>>));
		Arg==line ->
		    reply0(erlang:port_control(P, ?SL_SET_MODE, 
					       << ?SL_MODE_LINE:32 >>))
            end;
	_ -> {error, {bad_opt,Opt}}
    end.

setopts(P, []) -> ok;
setopts(P, Opts) -> 
    case catch setopts0(P, Opts) of
	ok -> update(P);
	Error -> revert(P), Error
    end.

setopts0(P, [{Opt,Arg}|Opts]) ->
    case setopt(P, Opt, Arg) of
	ok -> setopts0(P, Opts);
	Error -> Error
    end;
setopts0(P, []) -> ok.

	    
open(Dev, Opts) ->
    PortOpts = case lists:keysearch(binary, 1, Opts) of
		   {value, {_, true}} -> [binary];
		   _ -> []
	       end,
    P = start(PortOpts),
    case setopt(P, dev, Dev) of
	ok ->
	    case open(P) of
		ok -> 
		    case setopts(P, Opts) of
			ok -> {ok,P};
			Error -> stop(P), Error
		    end;
		Error ->
		    stop(P), Error
	    end;
	Error ->
	    stop(P),
	    Error
    end.

open(P) ->
    reply0(erlang:port_control(P, ?SL_OPEN, "")).

connect(P) ->
    reply0(erlang:port_control(P, ?SL_CONNECT, "")).

disconnect(P) ->
    reply0(erlang:port_control(P, ?SL_DISCONNECT, "")).

close(P) ->
    reply0(erlang:port_control(P, ?SL_CLOSE, "")).

revert(P) ->
    reply0(erlang:port_control(P, ?SL_REVERT, "")).

update(P) ->
    reply0(erlang:port_control(P, ?SL_UPDATE, "")).

send(P, Data) ->
    erlang:port_command(P, Data).



bool(true) ->  <<1:32>>;
bool(false) -> <<0:32>>.

reply0(R) when list(R)   -> reply(list_to_binary(R));
reply0(R) when binary(R) -> reply(R).

reply(<<?SL_OK>>) -> ok;
reply(<<?SL_OK,?SL_STRING,Str/binary>>) -> {ok,binary_to_list(Str)};
reply(<<?SL_OK,?SL_INT, I0:32>>) -> {ok, I0};
reply(<<?SL_OK,?SL_INT, I0:32, Tail/binary>>) ->{ok, [I0|intlist(Tail)]};
reply(<<?SL_OK,?SL_BOOL, 0>>) -> {ok,false};
reply(<<?SL_OK,?SL_BOOL, _>>) -> {ok,true};
reply(<<?SL_ERROR, Err/binary>>) -> {error, list_to_atom(binary_to_list(Err))}.

intlist(<<I:32, Tail/binary>>) -> [I | intlist(Tail)];
intlist(<<>>) -> [].

	


	




