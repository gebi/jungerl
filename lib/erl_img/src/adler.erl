%%% File    : adler.erl
%%% Author  : Tony Rogvall <tony@bit.hemma.se>
%%% Description : adler checksum
%%% Created :  8 Apr 2003 by Tony Rogvall <tony@bit.hemma.se>

-module(adler).

-export([adler32/1, adler32/2]).

-define(BASE, 65521). %% largest prime smaller than 65536 
-define(NMAX, 5552).

adler32(Bin) ->
    adler32(0, Bin).


adler32(Adler, Bin) ->
    S1 = Adler band 16#ffff,
    S2 = (Adler bsr 16) band 16#ffff,
    adler_n(Adler, 0, S1, S2, Bin, 0).

adler_n(Adler, Offs, S1, S2, Bin, 0) ->
    S11 = S1 rem ?BASE,
    S12 = S2 rem ?BASE,
    Len = size(Bin) - Offs,
    K  = if Len < ?NMAX -> Len; true -> ?NMAX end,
    if K == 0 ->
	    (S2 bsl 16) bor S1;
       true ->
	    adler_n(Adler, Offs, S11, S12, Bin, K)
    end;
adler_n(Adler, Offs, S1, S2, Bin, I) when I >= 8 ->
    <<_:Offs/binary, C0,C1,C2,C3,C4,C5,C6,C7,_/binary>> = Bin,
    S11 = S1+C0,
    adler_n(Adler, Offs+8,
 	    S11+C1+C2+C3+C4+C5+C6+C7,  
 	    S2+8*S11+7*C1+6*C2+5*C3+4*C4+3*C5+2*C6+C7, Bin, I-8);
adler_n(Adler, Offs, S1, S2, Bin, I) ->
    <<_:Offs/binary, C0,_/binary>> = Bin,
    adler_n(Adler, Offs+1, S1+C0, S2+S1+C0, Bin, I-1).

    


