%% Copyright (C) 1999, Bluetail AB
%% File    : base64.erl
%% Authors : Joe Armstrong <joe@bluetail.com>,
%%	     Robert Virding <rv@bluetail.com>
%% Purpose : Base64 encoding and decoding
%% Created : 21 August 1999

%% This code was origonally written by Jocke and Mattias I (Joe)
%% removed some bugs ... And I (rv) rewrote the conversion functions
%% to be reentrant so they could be used for LARGE input strings.

-module(base64).
-vsn("$Revision$ ").

-export([to_string/1,from_string/1,from_string/2,from_end/1]).

-export([base64_2_str/1, str_2_base64/1]).

-export([str_2_b64/1]).

-import(lists, [reverse/1,reverse/2]).

to_string(B64) ->
    base64_2_str(B64).

base64_2_str(Str) ->
    b642str(Str, 0, 0, []).

b642str([$=|_], Acc, N, Out) ->
    case N of
	2 ->
	    %% If I have seen two characters before the =
	    %% Them I'm encoding one byte
	    reverse([(Acc bsr 4)|Out]);
	3 ->
	    %% If I have seen three characters before the =
	    %% Them I'm encoding two bytes
	    B1 = Acc bsr 10,
	    B2 = (Acc bsr 2) band 16#ff,
	    reverse([B2,B1|Out]);
	_ ->
	    exit({bad,b64,N})
    end;
b642str([H|T], Acc, N, Out) ->
    case d(H) of
	no ->
	    b642str(T, Acc, N, Out);
	I  -> 
	    Acc1 = (Acc bsl 6) bor I,
	    case N of 
		3 ->
		    B1 = Acc1 bsr 16,
		    B2 = (Acc1 band 16#ffff) bsr 8,
		    B3 = (Acc1 band 16#ff),
		    b642str(T, 0, 0, [B3,B2,B1|Out]);
		_ ->
		    b642str(T, Acc1, N+1, Out)
	    end
    end;
b642str([], 0, 0, Out) ->
    reverse(Out).

%%----------------------------------------------------------------------
	
%% from_string(String) -> Base64Str.
%%  Base64 convert a (multiline) string into a multiple line string
%%  with each line terminated by "\n".

from_string(String) ->
    case from_string(String, []) of
	{ok,Line,Rest} -> Line ++ "\n" ++ from_string(Rest);
	{more,Cont} -> from_end(Cont)
    end.

str_2_b64(String) ->
    case from_string_nonl(String, []) of
	{ok,Line,[]} -> Line;
	{more, Cont} -> from_end(Cont)
    end.

str_2_base64(Str) -> from_string(Str).

%% from_string(String, Continuation) -> {more,Cont} | {ok,Line,Rest}.
%%  Return one 72 char line of base64 converted characters.  If there
%%  are not enough input characters then {more,Cont} is returned
%%  signaling more characters are needed for this line.

from_string(S, []) -> from_string(S, [], 0);
from_string(S, {Rest,Acc,N}) -> from_string(Rest ++ S, Acc, N).

from_string(S, Out, 72) -> {ok,reverse(Out),S};
from_string([C1,C2,C3|S], Out, N) ->
    O1 = e(C1 bsr 2),
    O2 = e(((C1 band 16#03) bsl 4) bor (C2 bsr 4)),
    O3 = e(((C2 band 16#0f) bsl 2) bor (C3 bsr 6)),
    O4 = e(C3 band 16#3f),
    from_string(S, [O4,O3,O2,O1|Out], N+4);
from_string(S, Out, N) ->
    {more,{S,Out,N}}.

from_string_nonl([C1,C2,C3|S], Out) ->
    O1 = e(C1 bsr 2),
    O2 = e(((C1 band 16#03) bsl 4) bor (C2 bsr 4)),
    O3 = e(((C2 band 16#0f) bsl 2) bor (C3 bsr 6)),
    O4 = e(C3 band 16#3f),
    from_string_nonl(S, [O4,O3,O2,O1|Out]);
from_string_nonl(S, Out) ->
    {more,{S,Out,1}}.
    
%% from_end(Continuation) -> Line. 
%%  End a base64 conversion.  Assume we have run from_string
%%  until end.

from_end({[C1,C2],Out,N}) ->
    O1 = e(C1 bsr 2),
    O2 = e(((C1 band 16#03) bsl 4) bor (C2 bsr 4)),
    O3 = e((C2 band 16#0f) bsl 2),
    reverse(Out, [O1,O2,O3,$=]);
from_end({[C1],Out,N}) ->
    O1 = e(C1 bsr 2),
    O2 = e((C1 band 16#03) bsl 4),
    reverse(Out, [O1,O2,$=,$=]);
from_end({[],Out,N}) -> reverse(Out);
from_end([]) -> [].

%%                    Table 1: The Base64 Alphabet
%%
%%     Value Encoding  Value Encoding  Value Encoding  Value Encoding
%%         0 A            17 R            34 i            51 z
%%         1 B            18 S            35 j            52 0
%%         2 C            19 T            36 k            53 1
%%         3 D            20 U            37 l            54 2
%%         4 E            21 V            38 m            55 3
%%         5 F            22 W            39 n            56 4
%%         6 G            23 X            40 o            57 5
%%         7 H            24 Y            41 p            58 6
%%         8 I            25 Z            42 q            59 7
%%         9 J            26 a            43 r            60 8
%%        10 K            27 b            44 s            61 9
%%        11 L            28 c            45 t            62 +
%%        12 M            29 d            46 u            63 /
%%        13 N            30 e            47 v
%%        14 O            31 f            48 w         (pad) =
%%        15 P            32 g            49 x
%%        16 Q            33 h            50 y

d(X) when X >= $A, X =<$Z -> X - $A;
d(X) when X >= $a, X =<$z -> X - $a + 26;
d(X) when X >= $0, X =<$9 -> X - $0 + 52;
d($+)                     -> 62;
d($/)                     -> 63;
d(_)                      -> no.

e(X) when X >= 0, X < 26 -> X + $A;
e(X) when X >= 26, X < 52 -> X + $a - 26;
e(X) when X >= 52, X < 62 -> X + $0 - 52;
e(62) -> $+;
e(63) -> $/;
e(X) -> erlang:fault({badchar,X}).
