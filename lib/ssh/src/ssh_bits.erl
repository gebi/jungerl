%%% File    : ssh_bits.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : SSH 1/2 pdu elements encode/decode
%%% Created :  1 Sep 2004 by Tony Rogvall <tony@bix.hemma.se>

-module(ssh_bits).

-include("../include/ssh.hrl").

-export([encode/1, encode/2]).
-export([decode/1, decode/2, decode/3]).
-export([mpint/1,  bignum/1, string/1, name_list/1]).
-export([b64_encode/1, b64_decode/1]).
-export([install_messages/1, uninstall_messages/1]).

%% integer utils
-export([isize/1]).
-export([irandom/1, irandom/3]).
-export([random/1, random/3]).
-export([xor_bits/2, fill_bits/2]).
-export([i2bin/2, bin2i/1]).

-import(lists, [foreach/2, reverse/1]).

-define(name_list(X), 
	(fun(B) -> ?string(B) end)(list_to_binary(name_concat(X)))).


name_concat([Name]) when atom(Name) -> atom_to_list(Name);
name_concat([Name]) when list(Name) -> Name;
name_concat([Name|Ns]) -> 
    if atom(Name) ->
	    [atom_to_list(Name),"," | name_concat(Ns)];
       list(Name) ->
	    [Name,"," | name_concat(Ns)]
    end;
name_concat([]) -> [].


name_list(Ns) ->
    ?name_list(Ns).
    

string(Str) ->
    ?string(Str).


%% MP representaion  (SSH2)
mpint(X) when X < 0 ->
    if X == -1 ->
	    <<0,0,0,1,16#ff>>;	    
       true ->
	    mpint_neg(X,0,[])
    end;
mpint(X) ->
    if X == 0 ->
	    <<0,0,0,0>>;
       true ->
	    mpint_pos(X,0,[])
    end.

mpint_neg(-1,I,Ds=[MSB|_]) ->
    if MSB band 16#80 =/= 16#80 ->
	    <<?UINT32((I+1)), (list_to_binary([255|Ds]))/binary>>;
       true ->
	    (<<?UINT32(I), (list_to_binary(Ds))/binary>>)
    end;
mpint_neg(X,I,Ds)  ->
    mpint_neg(X bsr 8,I+1,[(X band 255)|Ds]).
    
mpint_pos(0,I,Ds=[MSB|_]) ->
    if MSB band 16#80 == 16#80 ->
	    <<?UINT32((I+1)), (list_to_binary([0|Ds]))/binary>>;
       true ->
	    (<<?UINT32(I), (list_to_binary(Ds))/binary>>)
    end;
mpint_pos(X,I,Ds) ->
    mpint_pos(X bsr 8,I+1,[(X band 255)|Ds]).


%% BIGNUM representation SSH1
bignum(X) ->
    XSz = isize(X),
    Pad = (8 - (XSz rem 8)) rem 8,
    <<?UINT16(XSz),0:Pad/unsigned-integer,X:XSz/big-unsigned-integer>>.


install_messages(Codes) ->
    foreach(fun({Name, Code, Ts}) ->
		    ?dbg(true, "install msg: ~s = ~w ~w\n", 
			 [Name,Code,Ts]),
		    put({msg_name,Code}, {Name,Ts}),
		    put({msg_code,Name}, {Code,Ts})
	    end, Codes).

uninstall_messages(Codes) ->
    foreach(fun({Name, Code, Ts}) ->
		    ?dbg(true, "uninstall msg: ~s = ~w ~w\n", 
			 [Name,Code,Ts]),
		    erase({msg_name,Code}),
		    erase({msg_code,Name})
	    end, Codes).

%%
%% Encode a record, the type spec is expected to be 
%% in process dictionary under the key {msg_code, RecodeName}
%%
encode(Record) ->
    case get({msg_code, element(1, Record)}) of
	undefined -> 
	    {error, unimplemented};
	{Code, Ts} ->
	    Data = enc(tl(tuple_to_list(Record)), Ts),
	    list_to_binary([Code, Data])
    end.

encode(List, Types) ->
    list_to_binary(enc(List, Types)).

%%
%% Encode record element
%%
enc(Xs, Ts) ->
    enc(Xs, Ts, 0).

enc(Xs, [Type|Ts], Offset) ->
    case Type of
	boolean ->
	    X=hd(Xs), 
	    [?boolean(X) | enc(tl(Xs), Ts, Offset+1)];
	byte ->
	    X=hd(Xs),
	    [?byte(X) | enc(tl(Xs), Ts,Offset+1)];
	uint16 ->  
	    X=hd(Xs),
	    [?uint16(X) | enc(tl(Xs), Ts,Offset+2)];
	uint32 ->
	    X=hd(Xs),
	    [?uint32(X) | enc(tl(Xs), Ts,Offset+4)];
	uint64 ->
	    X=hd(Xs),
	    [?uint64(X) | enc(tl(Xs), Ts,Offset+8)];
	mpint ->
	    Y=mpint(hd(Xs)),
	    [Y | enc(tl(Xs), Ts,Offset+size(Y))];
	bignum ->  
	    Y=bignum(hd(Xs)),
	    [Y | enc(tl(Xs),Ts,Offset+size(Y))];
	string ->
	    X0=hd(Xs),
	    Y=?string(X0),
	    [Y | enc(tl(Xs),Ts,Offset+size(Y))];
	binary ->
	    X0=hd(Xs),
	    Y=?string(X0),
	    [Y | enc(tl(Xs), Ts,Offset+size(Y))];
	name_list -> 
	    X0=hd(Xs),
	    Y=?name_list(X0),
	    [Y | enc(tl(Xs), Ts, Offset+size(Y))];
	cookie -> 
	    [random(16) | enc(tl(Xs), Ts, Offset+16)];
	{pad,N} ->
	    K = (N - (Offset rem N)) rem N,
	    [fill_bits(K,0) | enc(Xs, Ts, Offset+K)];
	'...' when Ts==[] ->
	    X=hd(Xs),
	    if binary(X) -> 
		    [X];
	       list(X) ->
		    [list_to_binary(X)]
	    end
    end;
enc([], [],_) ->
    [].



%%
%% Decode a SSH record the type is encoded as the first byte
%% and the type spec MUST be installed in {msg_name, ID}
%%

decode(Binary = <<?BYTE(ID), _/binary>>) ->
    case get({msg_name, ID}) of
	undefined -> 
	    {error, unimplemented};
	{Name, Ts} ->
	    {_, Elems} = decode(Binary,1,Ts),
	    {ok,list_to_tuple([Name | Elems])}
    end.

%%
%% Decode a binary form offset 0
%%

decode(Binary, Types) when binary(Binary), list(Types) ->
    {_,Elems} = decode(Binary, 0, Types),
    Elems.


%%
%% Decode a binary from byte offset Offset
%% return {UpdatedOffset, DecodedElements}
%%
decode(Binary, Offset, Types) ->
    decode(Binary, Offset, Types, []).

decode(Binary, Offset, [Type|Ts], Acc) ->
    case Type of
	boolean ->
	    <<_:Offset/binary, ?BOOLEAN(X0), _/binary>> = Binary,
	    X = if X0 == 0 -> false; true -> true end,
	    decode(Binary, Offset+1, Ts, [X | Acc]);

	byte ->
	    <<_:Offset/binary, ?BYTE(X), _/binary>> = Binary,
	    decode(Binary, Offset+1, Ts, [X | Acc]);

	uint16 ->
	    <<_:Offset/binary, ?UINT16(X), _/binary>> = Binary,
	    decode(Binary, Offset+2, Ts, [X | Acc]);

	uint32 ->
	    <<_:Offset/binary, ?UINT32(X), _/binary>> = Binary,
	    decode(Binary, Offset+4, Ts, [X | Acc]);

	uint64 ->
	    <<_:Offset/binary, ?UINT64(X), _/binary>> = Binary,
	    decode(Binary, Offset+8, Ts, [X | Acc]);

	mpint ->
	    <<_:Offset/binary, ?UINT32(L), X0:L/binary,_/binary>> = Binary,
	    Sz = L*8,
	    <<X:Sz/big-signed-integer>> = X0,
	    decode(Binary, Offset+4+L, Ts, [X | Acc]);

	bignum ->
	    <<_:Offset/binary, ?UINT16(Bits),_/binary>> = Binary,
	    L = (Bits+7) div 8,
	    Pad = (8 - (Bits rem 8)) rem 8,
	    <<_:Offset/binary, _:16, _:Pad, X:Bits/big-unsigned-integer,
	     _/binary>> = Binary,
	    decode(Binary, Offset+2+L, Ts, [X | Acc]);

	string ->
	    <<_:Offset/binary,?UINT32(L), X:L/binary,_/binary>> = Binary,
	    decode(Binary, Offset+4+L, Ts, [binary_to_list(X) | Acc]);

	binary ->
	    <<_:Offset/binary,?UINT32(L), X:L/binary,_/binary>> = Binary,
	    decode(Binary, Offset+4+L, Ts, [X | Acc]);

	name_list ->
	    <<_:Offset/binary,?UINT32(L), X:L/binary,_/binary>> = Binary,
	    List = string:tokens(binary_to_list(X), ","),
	    decode(Binary, Offset+4+L, Ts, [List | Acc]);

	cookie ->
	    <<_:Offset/binary, X:16/binary, _/binary>> = Binary,
	    decode(Binary, Offset+16, Ts, [X | Acc]);

	{pad,N} -> %% pad offset to a multiple of N
	    K = (N - (Offset rem N)) rem N,
	    decode(Binary, Offset+K, Ts, Acc);
	    
	
	'...' when Ts==[] ->
	    <<_:Offset/binary, X/binary>> = Binary,
	    {Offset+size(X), reverse([X | Acc])}
    end;
decode(Binary, Offset, [], Acc) ->
    {Offset, reverse(Acc)}.



%% HACK WARNING :-)
-define(VERSION_MAGIC, 131).
-define(SMALL_INTEGER_EXT, $a).
-define(INTEGER_EXT,       $b).
-define(SMALL_BIG_EXT,     $n).
-define(LARGE_BIG_EXT,     $o).

isize(N) when N > 0 ->
    case term_to_binary(N) of
	<<?VERSION_MAGIC, ?SMALL_INTEGER_EXT, X>> ->
	    isize_byte(X);
	<<?VERSION_MAGIC, ?INTEGER_EXT, X3,X2,X1,X0>> ->
	    isize_bytes([X3,X2,X1,X0]);
	<<?VERSION_MAGIC, ?SMALL_BIG_EXT, S:8/big-unsigned-integer, 0,
	 Ds:S/binary>> ->
	    K = S - 1,
	    <<_:K/binary, Top>> = Ds,
	    isize_byte(Top)+K*8;
	<<?VERSION_MAGIC, ?LARGE_BIG_EXT, S:32/big-unsigned-integer, 0,
	 Ds:S/binary>> ->
	    K = S - 1,
	    <<_:K/binary, Top>> = Ds,
	    isize_byte(Top)+K*8
    end;
isize(0) -> 0.

%% big endian byte list
isize_bytes([0|L]) ->
    isize_bytes(L);
isize_bytes([Top|L]) ->
    isize_byte(Top) + length(L)*8.

%% Well could be improved
isize_byte(X) ->
    if X >= 2#10000000 -> 8;
       X >= 2#1000000 -> 7;
       X >= 2#100000 -> 6;
       X >= 2#10000 -> 5;
       X >= 2#1000 -> 4;
       X >= 2#100 -> 3;
       X >= 2#10 -> 2;
       X >= 2#1 -> 1;
       true -> 0
    end.

%% Convert integer into binary 
%% When XLen is the wanted size in octets of the output
i2bin(X, XLen) ->
    XSz = isize(X),
    Sz = XLen*8,
    if Sz < XSz -> 
	    exit(integer_to_large);
       true ->
	    (<<X:Sz/big-unsigned-integer>>)
    end.

%% Convert a binary into an integer
%%
bin2i(X) ->
    Sz = size(X)*8,
    <<Y:Sz/big-unsigned-integer>> = X,
    Y.

%%
%% Create a binary with constant bytes 
%%
fill_bits(N,C) ->
    list_to_binary(fill(N,C)).

fill(0,C) -> [];
fill(1,C) -> [C];
fill(N,C) ->
    Cs = fill(N div 2, C),
    Cs1 = [Cs,Cs],
    if N band 1 == 0 ->
	    Cs1;
       true ->
	    [C,Cs,Cs]
    end.

%% xor 2 binaries
xor_bits(XBits, YBits) ->
    XSz = size(XBits)*8,
    YSz = size(YBits)*8,
    Sz = if XSz < YSz -> XSz; true -> YSz end, %% min
    <<X:Sz, _/binary>> = XBits,
    <<Y:Sz, _/binary>> = YBits,
    <<(X bxor Y):Sz>>.

%%
%% irandom(N)
%%
%%  Generate a N bits size random number
%%  note that the top most bit is always set
%%  to guarantee that the number is N bits
%%
irandom(Bits) ->
    irandom(Bits, 1, 0).

irandom_odd(Bits) ->
    irandom(Bits, 1, 1).

%%
%% irandom(N, Top, Bottom)
%%
%%  Generate a N bits size random number
%% Where Top = 0 - do not set top bit
%%           = 1 - set the most significant bit
%%           = 2 - set two most significant bits
%%       Bot = 0 - do not set the least signifcant bit
%%       Bot = 1 - set the least signifcant bit (i.e always odd)
%%
irandom(0, Top, Bottom) -> 
    0;
irandom(Bits, Top, Bottom) ->
    Bytes = (Bits+7) div 8,
    Skip  = (8-(Bits rem 8)) rem 8,
    TMask = case Top of
		  0 -> 0;
		  1 -> 16#80;
		  2 -> 16#c0
	      end,
    BMask = case Bottom of
		0 -> 0;
		1 -> (1 bsl Skip)
	    end,
    <<X:Bits/big-unsigned-integer, _:Skip>> = random(Bytes, TMask, BMask),
    X.

%%
%% random/1
%%   Generate N random bytes
%%
random(N) ->
    random(N, 0, 0).

random(N, TMask, BMask) ->
    list_to_binary(rnd(N, TMask, BMask)).

%% random/3
%%   random(Bytes, TopMask, BotMask)
%% where 
%% Bytes is the number of bytes to generate
%% TopMask is bitwised or'ed to the first byte
%% BotMask is bitwised or'ed to the last byte
%%
rnd(0, TMask, BMask) ->
    [];
rnd(1, TMask, BMask) ->
    [(rand8() bor TMask) bor BMask];
rnd(N, TMask, BMask) ->
    [(rand8() bor TMask) | rnd_n(N-1, BMask)].

rnd_n(1, BMask) ->
    [rand8() bor BMask];
rnd_n(I, BMask) ->
    [rand8() | rnd_n(I-1, BMask)].

rand8() ->
    (rand32() bsr 8) band 16#ff.

rand32() ->
    random:uniform(16#100000000) -1.

%%
%% Base 64 encode/decode
%%

b64_encode(Bs) when list(Bs) -> 
    b64_enc(list_to_binary(Bs));    
b64_encode(Bin) when binary(Bin) ->
    b64_enc(Bin).

b64_enc(<<C1:6, C2:6, C3:6, C4:6, Bs/binary>>) ->
    [b64e(C1), b64e(C2), b64e(C3), b64e(C4)| b64_enc(Bs)];
b64_enc(<<B:2/binary>>) ->
    <<C1:6, C2:6, C3:6, _:6>> = <<B/binary, 0>>,
    [b64e(C1), b64e(C2), b64e(C3), $=];
b64_enc(<<B:1/binary>>) ->
    <<C1:6, C2:6, _:4>> = <<B/binary, 0>>,
    [b64e(C1), b64e(C2), $=, $=];
b64_enc(<<>>) ->
    [].

b64e(C) when C =< 25 -> C+$A;
b64e(C) when C =< 51 -> (C-26)+$a;
b64e(C) when C =< 61 -> (C-52)+$0;
b64e(62) -> $+;
b64e(63) -> $/.

b64_decode(Bin) when binary(Bin) -> 
    list_to_binary(b64_dec(binary_to_list(Bin)));
b64_decode(Cs) when list(Cs) -> 
    list_to_binary(b64_dec(Cs)).

b64_dec([$\s|Cs]) -> b64_dec(Cs);
b64_dec([$\t|Cs]) -> b64_dec(Cs);
b64_dec([$\r,$\n|Cs]) -> b64_dec(Cs);
b64_dec([$\n|Cs]) -> b64_dec(Cs);
b64_dec([C1,C2,$=,$=|_]) ->
    <<B1, _:16>> = <<(b64d(C1)):6, (b64d(C2)):6, 0:12>>,
    [B1];
b64_dec([C1,C2,C3,$=|_]) ->
    <<B1, B2, _:8>> = <<(b64d(C1)):6,(b64d(C2)):6,(b64d(C3)):6, 0:6>>,
    [B1, B2];
b64_dec([C1,C2,C3,C4| Cs]) ->
    Bin = <<(b64d(C1)):6, (b64d(C2)):6, (b64d(C3)):6, (b64d(C4)):6>>,
    [Bin| b64_dec(Cs)];
b64_dec([]) ->
    [].

b64d(C) when C >= $A, C =< $Z -> C-$A;
b64d(C) when C >= $a, C =< $z -> (C-$a)+26;
b64d(C) when C >= $0, C =< $9 -> (C-$0)+52;
b64d($+) -> 62;
b64d($/) -> 63.
