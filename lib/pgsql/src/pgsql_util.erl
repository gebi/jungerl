%%% File    : pgsql_util.erl
%%% Author  : Christian Sunesson
%%% Description : utility functions used in implementation of 
%%% postgresql driver.
%%% Created : 11 May 2005 by Blah <cos@local>

-module(pgsql_util).

%% Key-Value handling
-export([option/2]).

%% Networking
-export([socket/1]).
-export([send/2, send_int/2, send_msg/3]).
-export([recv_msg/2, recv_msg/1, recv_byte/2, recv_byte/1]).

%% Protocol packing
-export([string/1, make_pair/2, split_pair/1]).
-export([split_pair_rec/1]).
-export([count_string/1, to_string/1]).
-export([oids/2, coldescs/2, datacoldescs/3]).
-export([decode_row/2, decode_descs/1]).
-export([errordesc/1]).

-export([zip/2]).

%% Constructing authentication messages.
-export([pass_plain/1, pass_md5/3]).
-import(erlang, [md5/1]).
-export([hexlist/2]).

-record(desc, {column, name, type, format, size, mod, table}).


%% Lookup key in a plist stored in process dictionary under 'options'.
%% Default is returned if there is no value for Key in the plist.
option(Key, Default) ->
    Plist = get(options),
    case proplists:get_value(Key, Plist, Default) of
	Default ->
	    Default;
	Value ->
	    Value
    end.


%% Open a TCP connection
socket({tcp, Host, Port}) ->
    gen_tcp:connect(Host, Port, [{active, false}, binary, {packet, raw}], 5000).

send(Sock, Packet) ->
    gen_tcp:send(Sock, Packet).
send_int(Sock, Int) ->
    Packet = <<Int:32/integer>>,
    gen_tcp:send(Sock, Packet).

send_msg(Sock, Code, Packet) when binary(Packet) ->
    Len = size(Packet) + 4,
    Msg = <<Code:8/integer, Len:4/integer-unit:8, Packet/binary>>,
    gen_tcp:send(Sock, Msg).

recv_msg(Sock, Timeout) ->
    {ok, Head} = gen_tcp:recv(Sock, 5, Timeout),
    <<Code:8/integer, Size:4/integer-unit:8>> = Head,
    %%io:format("Code: ~p, Size: ~p~n", [Code, Size]),
    if 
	Size > 4 ->
	    {ok, Packet} = gen_tcp:recv(Sock, Size-4, Timeout),
	    {ok, Code, Packet};
	true ->
	    {ok, Code, <<>>}
    end.
recv_msg(Sock) ->
    recv_msg(Sock, infinity).


recv_byte(Sock) ->
    recv_byte(Sock, infinity).
recv_byte(Sock, Timeout) ->
    case gen_tcp:recv(Sock, 1, Timeout) of
	{ok, <<Byte:1/integer-unit:8>>} ->
	    {ok, Byte};
	E={error, _Reason} ->
	    throw(E)
    end.

string(String) ->
    Bin = list_to_binary(String),
    <<Bin/binary, 0/integer>>.

%%% Two zero terminated strings.
make_pair(Key, Value) when atom(Key) ->
    make_pair(atom_to_list(Key), Value);
make_pair(Key, Value) when atom(Value) ->
    make_pair(Key, atom_to_list(Value));
make_pair(Key, Value) ->
    BinKey = list_to_binary(Key),
    BinValue = list_to_binary(Value),
    <<BinKey/binary, 0/integer, 
     BinValue/binary, 0/integer>>.

split_pair(Bin) when binary(Bin) ->
    split_pair(binary_to_list(Bin));
split_pair(Str)  ->
    split_pair_rec(Str, norec).

split_pair_rec(Bin) when binary(Bin) ->
    split_pair_rec(binary_to_list(Bin));
split_pair_rec(Arg)  ->
    split_pair_rec(Arg,[]).

split_pair_rec([], Acc) ->
    lists:reverse(Acc);
split_pair_rec([0], Acc) ->
    lists:reverse(Acc);
split_pair_rec(S, Acc) ->
    Fun = fun(C) -> C /= 0 end,
    {Key, [0|S1]} = lists:splitwith(Fun, S),
    {Value, [0|Tail]} = lists:splitwith(Fun, S1),
    case Acc of 
        norec -> {Key, Value};
        _ ->
            split_pair_rec(Tail, [{Key, Value}| Acc])
    end.


count_string(Bin) when binary(Bin) ->
    count_string(Bin, 0).

count_string(<<>>, N) ->
    {N, <<>>};
count_string(<<0/integer, Rest/binary>>, N) ->
    {N, Rest};
count_string(<<_C/integer, Rest/binary>>, N) ->
    count_string(Rest, N+1).

to_string(Bin) when binary(Bin) ->    
    {Count, _} = count_string(Bin, 0),
    <<String:Count/binary, _/binary>> = Bin,
    {binary_to_list(String), Count}.

oids(<<>>, Oids) ->
    lists:reverse(Oids);
oids(<<Oid:32/integer, Rest/binary>>, Oids) ->
    oids(Rest, [Oid|Oids]).
    
coldescs(<<>>, Descs) ->
    lists:reverse(Descs);
coldescs(Bin, Descs) ->
    {Name, Count} = to_string(Bin),
    <<_:Count/binary, 0/integer,
     TableOID:32/integer,
     ColumnNumber:16/integer,
     TypeId:32/integer,
     TypeSize:16/integer-signed,
     TypeMod:32/integer-signed,
     FormatCode:16/integer,
     Rest/binary>> = Bin,
    Format = case FormatCode of 
		 0 -> text; 
		 1 -> binary 
	     end,
    Desc = #desc{name=Name,
		 format=Format,
		 column=ColumnNumber,
		 type=TypeId,
		 size=TypeSize,
		 mod=TypeMod,
		 table=TableOID},
    coldescs(Rest, [Desc|Descs]).

datacoldescs(N, <<-1:32/signed-integer, Rest/binary>>, Descs) 
  when N >= 0 ->
    datacoldescs(N-1, Rest, [null|Descs]);
datacoldescs(N, <<Len:32/integer, Data:Len/binary, Rest/binary>>, Descs) 
  when N >= 0 ->
    datacoldescs(N-1, Rest, [Data|Descs]);
datacoldescs(_N, _, Descs) ->
    lists:reverse(Descs).

decode_descs(Cols) ->
    Cols2 = [Col#desc{type=decode_oid(Oid)} || #desc{type=Oid}=Col <- Cols],
    {ok, Cols2}.


decode_oid(Oid) ->
    case Oid of
	16 -> bool;
	17 -> bytea;
	18 -> char;
	19 -> name;
	20 -> int8;
	21 -> int2;
	22 -> int2vector;
	23 -> int4;
	24 -> regproc;
	25 -> text;
	26 -> oid;
	27 -> tid;
	28 -> xid;
	29 -> cid;
	30 -> oidvector;
	71 -> pg_type;
	75 -> pg_attribute;
	81 -> pg_proc;
	83 -> pg_class;
	210 -> smgr;
	600 -> point;
	601 -> lseg;
	602 -> path;
	603 -> box;
	604 -> polygon;
	628 -> line;
	700 -> float4;
	701 -> float8;
	702 -> abstime;
	703 -> reltime;
	704 -> tinterval;
	705 -> unknown;
	790 -> money;
	829 -> macaddr;
	869 -> inet;
	650 -> cidr;
	1033 -> aclitem;
	1042 -> bpchar;
	1043 -> varchar;
	1082 -> date;
	1083 -> time;
	1114 -> timestamp;
	1184 -> timestamptz;
	1186 -> interval;
	1266 -> timetz;
	1560 -> bit;
	1562 -> varbit;
	1700 -> numeric;
	Oid ->
	    throw({unknown_oid, Oid})
    end.


decode_row(Types, Values) ->
    decode_row(Types, Values, []).
decode_row([], [], Out) ->
    {ok, lists:reverse(Out)};
decode_row([Type|TypeTail], [Value|ValueTail], Out0) ->
    Out1 = decode_col(Type, Value),
    decode_row(TypeTail, ValueTail, [Out1|Out0]).

decode_col(_, null) ->
    null;
decode_col(#desc{format=text, type=Int}, Value) 
  when Int =:= int2; Int =:= int4; Int =:= int8 ->
    list_to_integer(binary_to_list(Value));
decode_col(#desc{format=text, type=varchar}, Value) ->
    binary_to_list(Value);
decode_col(#desc{format=text, type=Float}, Value) 
  when Float =:= float4; Float =:= float8 ->
    list_to_float(binary_to_list(Value));
decode_col(#desc{format=text, type=bool}, Value) ->
    case Value of 
	<<"t">> -> true;
	<<"f">> -> false
    end;
decode_col(#desc{format=text, type=date}, Value) ->
    <<Year:4/binary, $-, Month:2/binary, $-, Day:2/binary>> = Value,
    {b2i(Year), b2i(Month), b2i(Day)};
decode_col(#desc{format=text, type=time}, Value) ->
    <<Hour:2/binary, $:, Minute:2/binary, $:, Second:2/binary,_/binary>> = Value,
    {b2i(Hour), b2i(Minute), b2i(Second)};
decode_col(#desc{format=text, type=timestamp}, Value) ->
    <<Year:4/binary, _, Month:2/binary, _, Day:2/binary, $\s, 
     Hour:2/binary, $:, Minute:2/binary, $:, Second:2/binary,_/binary>> = Value,
    {{b2i(Year), b2i(Month), b2i(Day)}, {b2i(Hour), b2i(Minute), b2i(Second)}};
decode_col(#desc{format=text}, Value) ->
    Value;
decode_col(#desc{format=binary}, Value) ->
    Value;
decode_col(_, Value) ->
    throw({unknown_format, Value}).

b2i(Bin) ->
    list_to_integer(binary_to_list(Bin)).

errordesc(Bin) ->
    errordesc(Bin, []).

errordesc(<<0/integer, _Rest/binary>>, Lines) ->
    lists:reverse(Lines);
errordesc(<<Code/integer, Rest/binary>>, Lines) ->
    {String, Count} = to_string(Rest),
    <<_:Count/binary, 0, Rest1/binary>> = Rest,
    Msg = case Code of 
	      $S ->
		  {severity, list_to_atom(String)};
	      $C ->
		  {code, String};
	      $M ->
		  {message, String};
	      $D ->
		  {detail, String};
	      $H ->
		  {hint, String};
	      $P ->
		  {position, list_to_integer(String)};
	      $p ->
		  {internal_position, list_to_integer(String)};
	      $W ->
		  {where, String};
	      $F ->
		  {file, String};
	      $L ->
		  {line, list_to_integer(String)};
	      $R ->
		  {routine, String};
	      Unknown ->
		  {Unknown, String}
	  end,
    errordesc(Rest1, [Msg|Lines]).

%%% Zip two lists together
zip(List1, List2) ->
    zip(List1, List2, []).
zip(List1, List2, Result) when List1 =:= []; 
			       List2 =:= [] ->
    lists:reverse(Result);
zip([H1|List1], [H2|List2], Result) ->
    zip(List1, List2, [{H1, H2}|Result]).

%%% Authentication utils

pass_plain(Password) ->
	Pass = [Password, 0],
	list_to_binary(Pass). 

%% MD5 authentication patch from
%%    Juhani Rankimies <juhani@juranki.com>
%% (patch slightly rewritten, new bugs are mine :] /Christian Sunesson)

%%
%% MD5(MD5(password + user) + salt)
%%

pass_md5(User, Password, Salt) ->
	Digest = hex(md5([Password, User])),
	Encrypt = hex(md5([Digest, Salt])),
	Pass = ["md5", Encrypt, 0],
	list_to_binary(Pass).

hex(B) when binary(B) ->
	hexlist(binary_to_list(B), []).

hexlist([], Acc) ->
	lists:reverse(Acc);
hexlist([N|Rest], Acc) ->
	HighNibble = (N band 16#f0) bsr 4,
	LowNibble = (N band 16#0f),
	hexlist(Rest, [hexdigit(LowNibble), hexdigit(HighNibble)|Acc]).

hexdigit(0) -> $0;
hexdigit(1) -> $1;
hexdigit(2) -> $2;
hexdigit(3) -> $3;
hexdigit(4) -> $4;
hexdigit(5) -> $5;
hexdigit(6) -> $6;
hexdigit(7) -> $7;
hexdigit(8) -> $8;
hexdigit(9) -> $9;
hexdigit(10) -> $a;
hexdigit(11) -> $b;
hexdigit(12) -> $c;
hexdigit(13) -> $d;
hexdigit(14) -> $e;
hexdigit(15) -> $f.


