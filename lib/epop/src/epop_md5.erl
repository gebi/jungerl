-module(epop_md5).
-author('tony@erix.ericsson.se').
%%% --------------------------------------------------------------------
%%% File    : md5.erl
%%% Author  : Tony Rogvall <tony@erix.ericsson.se>
%%% Purpose : Implementation of MD5 in erlang
%%% Created : 30 Oct 1997 by Tony Rogvall <tony@erix.ericsson.se>
%%% ====================================================================
%%% License to copy and use this software is granted provided that it
%%% is identified as the "RSA Data Security, Inc. MD5 Message-Digest
%%% Algorithm" in all material mentioning or referencing this software
%%% or this function.
%%%  
%%% License is also granted to make and use derivative works provided
%%% that such works are identified as "derived from the RSA Data
%%% Security, Inc. MD5 Message-Digest Algorithm" in all material
%%% mentioning or referencing the derived work.
%%% ====================================================================
%%% Adopted : 17 Aug 1998 by tobbe@serc.rmit.edu.au
%%%           Made it into an epop module and cleaned it up a bit.
%%% --------------------------------------------------------------------
-export([string/1]).

-import(lists, [reverse/1]).

-record(md5_ctx, 
	{
	 state = { 16#67452301, 16#efcdab89, 16#98badcfe, 16#10325476 },
	 count = 0,    %% number of bits (64 bit)
	 buffer = []    %% input buffer (16 bytes)
	}).

-define(S11, 7).
-define(S12, 12).
-define(S13, 17).
-define(S14, 22).
-define(S21, 5).
-define(S22, 9).
-define(S23, 14).
-define(S24, 20).
-define(S31, 4).
-define(S32, 11).
-define(S33, 16).
-define(S34, 23).
-define(S41, 6).
-define(S42, 10).
-define(S43, 15).
-define(S44, 21).

%% F, G, H and I are basic MD5 functions.

-define(F(X, Y, Z), (((X) band (Y)) bor ((bnot (X)) band (Z)))).
-define(G(X, Y, Z), (((X) band (Z)) bor ((Y) band (bnot (Z))))).
-define(H(X, Y, Z), ((X) bxor (Y) bxor (Z))).
-define(I(X, Y, Z), ((Y) bxor ((X) bor (bnot (Z))))).

-define(U32(X), ((X) band 16#ffffffff)).

-define(ROTATE_LEFT(X,N), rotate_left(X,N)).

%% FF, GG, HH, and II transformations for rounds 1, 2, 3, and 4.
%% Rotation is separate from addition to prevent recomputation.
%%
-define(FF(A, B, C, D, X, S, AC),
	?ROTATE_LEFT(A + ?F((B), (C), (D)) + (X) + (AC),(S)) + (B)).

-define(GG(A, B, C, D, X, S, AC), 
	?ROTATE_LEFT(A + ?G((B), (C), (D)) + (X) + (AC),(S)) + (B)).

-define( HH(A, B, C, D, X, S, AC), 
	?ROTATE_LEFT(A + ?H((B), (C), (D)) + (X) + (AC),(S)) + (B)).

-define(II(A, B, C, D, X, S, AC),
	?ROTATE_LEFT(A +  ?I((B), (C), (D)) + (X) + (AC),(S)) + (B)).

%% ---------------------------------
%% Exported function: string/1
%% Do a message digest on a string
%% ---------------------------------

string(Str) ->
    format(final(update(init(), Str))).

format([X | Xs]) ->
    [hex(X bsr 4), hex(X) | format(Xs)];
format([]) -> [].

hex(X) ->
    X4 = (X band 16#f),
    if X4 < 10 -> X4 + $0;
       true -> (X4-10) + $a
    end.

init() ->
    #md5_ctx {}.

update(CTX, Input) ->
    Buffer = CTX#md5_ctx.buffer,
    LenI = length(Input),
    Len = LenI + length(Buffer),
    update(Buffer ++ Input, Len,CTX#md5_ctx.state,
	   CTX#md5_ctx.count+(LenI bsl 3)).

%%
%% update state, count reflects number of bytes 
%% including bytes in buffer
%%
update(Buf0, Len0, State0, Count) when Len0 >= 64 ->
    {Xs,Buf1} = decode(Buf0, 64),
    State1 = transform(State0, Xs),
    update(Buf1, Len0 - 64, State1, Count);
update(Buf0, Len0, State0, Count) ->
    #md5_ctx { state = State0, count = Count, buffer = Buf0 }.

%% produce a digest
final(CTX) ->
    %% pad out to a length 56 (we later add a count that makes 64)
    Count = CTX#md5_ctx.count,      %% number of bits
    Index =  (Count bsr 3) rem 64,  %% number of bytes
    PadLen = if Index < 56 ->
		     56 - Index;
		true -> 120 - Index
	     end,
    CTX1 = update(CTX, padding(PadLen,[])),
    CTX2 = update(CTX1, encode([?U32(Count), ?U32(Count bsr 32)])),
    encode(tuple_to_list(CTX2#md5_ctx.state)).

%% generate padding info to final    
padding(0,Acc) -> Acc;
padding(1,Acc) -> [16#80 | Acc];
padding(N,Acc) -> padding(N-1, [0 | Acc]).

%% rotate X as 32-bit unsigned left N bits
rotate_left(X, N) ->
    ?U32(X bsl N) bor (?U32(X) bsr (32 - N)).

%%
%% decodes Len number of bytes into 32 bit integers
%% returns {Xs, Tail}
%%
decode(Buf, Len) ->
    decode(Buf, Len, []).

decode(Buf, 0, Acc) -> 
    {reverse(Acc), Buf};
decode([A0,A1,A2,A3 | Buf], N, Acc) ->
    decode(Buf, N-4, [ A0 + (A1 bsl 8) + (A2 bsl 16) + (A3 bsl 24) | Acc]).

%%
%% Encodes input 32-bit ints into byte buffer output. 
%%
encode(Xs) -> encode(Xs, []).

encode([X | Xs], Acc) ->
    encode(Xs, [(X bsr 24) band 16#ff,
		(X bsr 16) band 16#ff,
		(X bsr 8) band 16#ff,
		X  band 16#ff | Acc]);
encode([], Acc) -> reverse(Acc).

    
transform({A0,B0,C0,D0}, Xs) ->
    [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15] = Xs,

    %% Round 1 
    A1 = ?FF (A0, B0, C0, D0, X0, ?S11, 16#d76aa478),
    D1 = ?FF (D0, A1, B0, C0, X1, ?S12, 16#e8c7b756),
    C1 = ?FF (C0, D1, A1, B0, X2, ?S13, 16#242070db),
    B1 = ?FF (B0, C1, D1, A1, X3, ?S14, 16#c1bdceee),

    A2 = ?FF (A1, B1, C1, D1, X4, ?S11, 16#f57c0faf),
    D2 = ?FF (D1, A2, B1, C1, X5, ?S12, 16#4787c62a),
    C2 = ?FF (C1, D2, A2, B1, X6, ?S13, 16#a8304613),
    B2 = ?FF (B1, C2, D2, A2, X7, ?S14, 16#fd469501),

    A3 = ?FF (A2, B2, C2, D2, X8, ?S11, 16#698098d8),
    D3 = ?FF (D2, A3, B2, C2, X9, ?S12, 16#8b44f7af),
    C3 = ?FF (C2, D3, A3, B2, X10, ?S13, 16#ffff5bb1),
    B3 = ?FF (B2, C3, D3, A3, X11, ?S14, 16#895cd7be),

    A4 = ?FF (A3, B3, C3, D3, X12, ?S11, 16#6b901122),
    D4 = ?FF (D3, A4, B3, C3, X13, ?S12, 16#fd987193),
    C4 = ?FF (C3, D4, A4, B3, X14, ?S13, 16#a679438e),
    B4 = ?FF (B3, C4, D4, A4, X15, ?S14, 16#49b40821),

    %% Round 2 
    A5 = ?GG (A4, B4, C4, D4, X1, ?S21, 16#f61e2562),
    D5 = ?GG (D4, A5, B4, C4, X6, ?S22, 16#c040b340),
    C5 = ?GG (C4, D5, A5, B4, X11, ?S23, 16#265e5a51),
    B5 = ?GG (B4, C5, D5, A5, X0, ?S24, 16#e9b6c7aa),

    A6 = ?GG (A5, B5, C5, D5, X5, ?S21, 16#d62f105d),
    D6 = ?GG (D5, A6, B5, C5, X10, ?S22,  16#2441453),
    C6 = ?GG (C5, D6, A6, B5, X15, ?S23, 16#d8a1e681),
    B6 = ?GG (B5, C6, D6, A6, X4, ?S24, 16#e7d3fbc8),

    A7 = ?GG (A6, B6, C6, D6, X9, ?S21, 16#21e1cde6),
    D7 = ?GG (D6, A7, B6, C6, X14, ?S22, 16#c33707d6),
    C7 = ?GG (C6, D7, A7, B6, X3, ?S23, 16#f4d50d87),
    B7 = ?GG (B6, C7, D7, A7, X8, ?S24, 16#455a14ed),

    A8 = ?GG (A7, B7, C7, D7, X13, ?S21, 16#a9e3e905),
    D8 = ?GG (D7, A8, B7, C7, X2, ?S22, 16#fcefa3f8),
    C8 = ?GG (C7, D8, A8, B7, X7, ?S23, 16#676f02d9),
    B8 = ?GG (B7, C8, D8, A8, X12, ?S24, 16#8d2a4c8a),

 %% Round 3
    A9 = ?HH (A8, B8, C8, D8, X5, ?S31, 16#fffa3942),
    D9 = ?HH (D8, A9, B8, C8, X8, ?S32, 16#8771f681),
    C9 = ?HH (C8, D9, A9, B8, X11, ?S33, 16#6d9d6122),
    B9 = ?HH (B8, C9, D9, A9, X14, ?S34, 16#fde5380c),

    A10 = ?HH (A9, B9, C9, D9, X1, ?S31, 16#a4beea44),
    D10 = ?HH (D9, A10, B9, C9, X4, ?S32, 16#4bdecfa9),
    C10 = ?HH (C9, D10, A10, B9, X7, ?S33, 16#f6bb4b60),
    B10 = ?HH (B9, C10, D10, A10, X10, ?S34, 16#bebfbc70),

    A11 = ?HH (A10, B10, C10, D10, X13, ?S31, 16#289b7ec6),
    D11 = ?HH (D10, A11, B10, C10, X0, ?S32, 16#eaa127fa),
    C11 = ?HH (C10, D11, A11, B10, X3, ?S33, 16#d4ef3085),
    B11 = ?HH (B10, C11, D11, A11, X6, ?S34,  16#4881d05),

    A12 = ?HH (A11, B11, C11, D11, X9, ?S31, 16#d9d4d039),
    D12 = ?HH (D11, A12, B11, C11, X12, ?S32, 16#e6db99e5),
    C12 = ?HH (C11, D12, A12, B11, X15, ?S33, 16#1fa27cf8),
    B12 = ?HH (B11, C12, D12, A12, X2, ?S34, 16#c4ac5665),

 %% Round 4
    A13 = ?II (A12, B12, C12, D12, X0, ?S41, 16#f4292244),
    D13 = ?II (D12, A13, B12, C12, X7, ?S42, 16#432aff97),
    C13 = ?II (C12, D13, A13, B12, X14, ?S43, 16#ab9423a7),
    B13 = ?II (B12, C13, D13, A13, X5, ?S44, 16#fc93a039),

    A14 = ?II (A13, B13, C13, D13, X12, ?S41, 16#655b59c3),
    D14 = ?II (D13, A14, B13, C13, X3, ?S42, 16#8f0ccc92),
    C14 = ?II (C13, D14, A14, B13, X10, ?S43, 16#ffeff47d),
    B14 = ?II (B13, C14, D14, A14, X1, ?S44, 16#85845dd1),

    A15 = ?II (A14, B14, C14, D14, X8, ?S41, 16#6fa87e4f),
    D15 = ?II (D14, A15, B14, C14, X15, ?S42, 16#fe2ce6e0),
    C15 = ?II (C14, D15, A15, B14, X6, ?S43, 16#a3014314),
    B15 = ?II (B14, C15, D15, A15, X13, ?S44, 16#4e0811a1),

    A16 = ?II (A15, B15, C15, D15, X4, ?S41, 16#f7537e82),
    D16 = ?II (D15, A16, B15, C15, X11, ?S42, 16#bd3af235),
    C16 = ?II (C15, D16, A16, B15, X2, ?S43, 16#2ad7d2bb),
    B16 = ?II (B15, C16, D16, A16, X9, ?S44, 16#eb86d391),
    
    {?U32(A0+A16), ?U32(B0+B16), ?U32(C0+C16), ?U32(D0+D16)}.

