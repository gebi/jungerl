%%% File    : stdio.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : Stdio functions
%%% Created : 22 Aug 2002 by Tony Rogvall <tony@bix.hemma.se>

-module(stdio).

-export([open_stdin/0, open_stdout/0]).
-export([close/1]).
-export([read/0, read/1, read/2]).
-export([readn/1, readn/2, readn/3]).
-export([write/1, write/2]).

-import(lists,[reverse/1]).

%% requires -nouser !!
open_stdin() ->
    P = open_port({fd, 0, 0},[in,eof]),
    erlang:register(stdin, P),
    P.

open_stdout() ->
    P = open_port({fd, 1, 1},[out]),
    erlang:register(stdout, P),
    P.

close(P) when port(P) ->
    erlang:port_close(P);
close(undefined) -> ok;
close(P) when atom(P) ->
    close(whereis(P)).

read() ->
    read(stdin).
read(P) ->
    read(P, infinity).

read(P,Tmo) when port(P) ->
    receive
        {P,{data,Data}} ->
            {ok,Data};
        {P, eof} ->
            eof;
	Other -> Other
    after Tmo ->
	    timeout
    end;
read(undefined,Tmo) -> eof;
read(P,Tmo) when atom(P) ->
    read(whereis(P), Tmo).

readn(N) ->
    readn(stdin,N).
readn(P,N) ->
    readn(P,N,infinity).
readn(P,0,Tmo) -> read(P,Tmo);

readn(P,N,Tmo) when port(P), integer(N), N > 0 ->
    Buf = case erlang:port_get_data(P) of
	      eof -> eof;
	      undefined -> [];
	      Buf0 -> Buf0
	  end,
    readn_buf(P,N,Tmo,Buf);
readn(undefined,N,Tmo) -> eof;
readn(P,N,Tmo) when atom(P) ->
    readn(whereis(P),N,Tmo).

write(Data) ->
    write(stdout, Data).

write(P, Data) when port(P) ->
    erlang:port_command(P, Data);
write(undefined, Data) -> 
    {error,epipe};
write(P, Data) when atom(P) ->
    write(whereis(P), Data).
    

%% internal
readn_buf(P,N,Tmo,Buf) ->
    if length(Buf) >= N ->
	    {Data,Buf1} = splitn(Buf,N),
	    erlang:port_set_data(P,Buf1),
	    {ok,Data};
       true ->
	    receive
		{P,{data,Data}} ->
		    readn_buf(P,N,Tmo,Buf++Data);
		{P, eof} ->
		    case erlang:port_get_data(P) of
			undefined -> eof;
			"" -> eof;
			Buf0 -> 
			    erlang:port_set_data(P,eof),
			    {data,Buf0}
		    end;
		Other ->
		    erlang:port_set_data(P,Buf),
		    Other
	    after Tmo ->
		    erlang:port_set_data(P,Buf),
		    timeout
	    end
    end.

splitn(Buf, N) ->
    splitn(Buf,N,[]).

splitn(Buf,0,Acc) ->
    {reverse(Acc), Buf};
splitn([C|Buf],N,Acc) ->
    splitn(Buf,N-1,[C|Acc]).




