%%% File    : pgsql_util.erl
%%% Author  : Christian Sunesson
%%% Description : utility functions used in implementation of 
%%% postgresql driver.
%%% Created : 11 May 2005 by Blah <cos@local>

-module(pgsql_util).

%% Key-Value handling
-export([lookup/3]).

%% Networking
-export([socket/1]).
-export([send/2, send_int/2, send_msg/3]).
-export([recv_msg/2, recv_msg/1, recv_byte/2, recv_byte/1]).

%% Protocol packing
-export([make_pair/2, split_pair/1]).
-export([count_string/1, to_string/1]).
-export([coldescs/2, datacoldescs/3]).
-export([errordesc/1]).

-export([zip/2]).

%% Cosmetics on proplists interfaces.
lookup(Plist, Key, Default) ->
    case proplists:get_value(Key, Plist, Default) of
	{Key, Value} ->
	    Value;
	Else ->
	    Default
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

recv_int(Sock) ->
    case gen_tcp:recv(Sock, 4) of
	{ok, Package} ->
	    <<Int:4/integer-unit:8>> = Package,
	    {ok, Int};
	{error, Reason} ->
	    throw({error, Reason})
    end.

recv_byte(Sock) ->
    recv_byte(Sock, infinity).
recv_byte(Sock, Timeout) ->
    case gen_tcp:recv(Sock, 1, Timeout) of
	{ok, <<Byte:1/integer-unit:8>>} ->
	    {ok, Byte};
	E={error, Reason} ->
	    throw(E)
    end.


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
split_pair(S) ->
    {Key, [0|S1]} = lists:splitwith(fun(C) ->
					  C /= 0
				  end,
				  S),
    {Value, _} = lists:splitwith(fun(C) ->
					 C /= 0
				 end,
				 S1),
    {Key, Value}.

count_string(Bin) when binary(Bin) ->
    count_string(Bin, 0).

count_string(<<>>, N) ->
    {N, <<>>};
count_string(<<0/integer, Rest/binary>>, N) ->
    {N, Rest};
count_string(<<C/integer, Rest/binary>>, N) ->
    count_string(Rest, N+1).

to_string(Bin) when binary(Bin) ->    
    {Count, _} = count_string(Bin, 0),
    <<String:Count/binary, _/binary>> = Bin,
    {binary_to_list(String), Count}.
    
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
    Desc = {Name, Format, ColumnNumber, 
	    TypeId, TypeSize, TypeMod, 
	    TableOID},
    coldescs(Rest, [Desc|Descs]).

datacoldescs(N, 
	     <<Len:32/integer, Data:Len/binary, Rest/binary>>, 
	     Descs) when N >= 0 ->
    datacoldescs(N-1, Rest, [binary_to_list(Data)| Descs]);
datacoldescs(N, _, Descs) ->
    lists:reverse(Descs).

errordesc(Bin) ->
    errordesc(Bin, []).

errordesc(<<0/integer, Rest/binary>>, Lines) ->
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
