%%% File    : mktab.erl
%%% Author  : Tony Rogvall <tony@bit.hemma.se>
%%% Description : reverse bits table
%%% Created :  2 Apr 2003 by Tony Rogvall <tony@bit.hemma.se>

-module(mktab).

-export([start/0]).

%% convert a hex byte into two ascii letters
hex8(X) ->
    [nib((X bsr 4) band 16#f),nib(X band 16#f)].

nib(N) when N =< 9 -> N+$0;
nib(N) -> (N-10)+$A.

bin(N) ->
    bin(N,8,[]).

bin(N, 0, Acc) -> 
    [$2,$#|Acc];
bin(N, I, Acc) ->
    bin(N bsr 1, I-1, [$0+(N band 1) | Acc]).


revbits(N) ->
    revbits(N,8,0).

revbits(N,0,A) -> 
    A;
revbits(N,I,A) ->
    revbits(N bsr 1,I-1,(A bsl 1) bor (N band 1)).

start() ->
    lists:foreach(fun(I) ->
			  io:format("	~s -> ~s;\n",
				    [bin(I), bin(revbits(I))])
		  end, lists:seq(0, 255)).

