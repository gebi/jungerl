%%% File    : ipcalc.erl
%%% Author  :  <klacke@bluetail.com>
%%% Purpose : various operations on ip addresses/network addresses
%%% Created : 12 May 2003 by  <klacke@bluetail.com>



%% This module provides various operations and calculations
%% on ipaddresses 
%% an IP is an arity 4 tuple


-module(ipcalc).
-author('klacke@bluetail.com').

-export([format/1,                 %% Ipaddress -> deep list
	 format/2,                 %% Ipaddresses, Separator -> deep list
	 fformat/1,                %% ipaddress -> string
	 fformat/2,                %% ipaddresses, Separator -> string
	 format_network/2,         %% format to  IP/BITS
	 fformat_network/2,        %% flat format to  IP/BITS
	 parse/1,                  %% string   -->  Ipaddr
	 parse_network/1,          %% string/bits    -> {Net, Mask}
	 network/2,                %% extract the network part of an IP+Mask
	 broadcast/2,              %% construct the broadcast add from IP+Mask
	 min_host/2,               %% The smalles possible IP on this network
	 max_host/2,               %% The biggest possible IP on this network
	 next_host/1,              %% increment IP by one
	 compare/2,                %% size compare IP addrs ret: lt | gt | eq
	 in_network/3,             %% check if Ip is in a network
	 network_size/1,           %% Mask --> NumBits
	 valid_ip/1,               %% bool
	 ones_mask_left/1,         %% all ones shifted left
	 ones_mask_right/1,        %% return Ones bits integer shifted to right
	 ip2int/1,                 %% format an IP as a 32 bit integer
	 int2ip/1                  %% format a 32 bit int as an IP
	]).

-compile(export_all).

-define(byte2list(Int), integer_to_list(Int)).

-define(u32(X3,X2,X1,X0), 
        (((X3) bsl 24) bor ((X2) bsl 16) bor ((X1) bsl 8) bor (X0))).



%% return a formatted ip address as a (deep) string
format({X1,X2, X3, X4}) ->
    [?byte2list(X1),".",
     ?byte2list(X2),".",
     ?byte2list(X3),".",
     ?byte2list(X4)];
%% or a list of ip addresses as a space separated (deep) string
format(IPs) when list(IPs) ->
    format(IPs, " ").

%% or a list of ip addresses as a String separated (deep) string
format([IP], _String) ->
    [format(IP)];
format([IP|IPs], String) ->
    [format(IP), String |format(IPs, String)].



%% return a formatted ip address as a (flat) string
fformat({X1,X2, X3, X4}) ->
    ?byte2list(X1) ++ 
	[$.|?byte2list(X2)] ++ 
	[$.|?byte2list(X3)] ++
	[$.|?byte2list(X4)];
fformat(IPs) when list(IPs) ->
    fformat(IPs, " ").

fformat(IPs, String) ->
    lists:flatten(format(IPs, String)).


ip2int(IP) ->
    tuple_to_u32(IP).

int2ip(Int) ->
    u32_to_tuple(Int).
    


%% Format a pair of IP,Mask on the form
%% i.e. 192.168.128.0/24

format_network(Net,Mask) ->
    SZ = network_size(Mask),
    [format(Net), io_lib:format("/~w",[SZ])].
fformat_network(Net,Mask) ->
    SZ = network_size(Mask),
    fformat(Net) ++ [$/|integer_to_list(SZ)].





%% string --> IP 
parse(Str) ->
    {ok, IP} = inet_parse:ipv4_address(Str),
    IP.



%% takes a string on the form "Network/Bits" and returnd
%% {Net, Mask} as erlang tuples

parse_network(Str) ->
    [NET,MaskStr] = string:tokens(Str, "/") ,
    NumBits = list_to_integer(MaskStr),
    Net = parse(NET),
    Network = network(Net, NumBits),
    {Network, u32_to_tuple(ones_mask_left(NumBits))}.





%% return the network part of
%% an IP given either the mask as a tuple or in
%% forexample network({192,168,128,1}, {255,255,255,0}) ==
%%            network({192,168,128,1}, 24) ==
%%                         {192,168,128,0}


network(IP, Mask) when tuple(Mask) ->
    u32_to_tuple(tuple_to_u32(IP) band tuple_to_u32(Mask));

network(IP, NumBits) when integer(NumBits) ->
    u32_to_tuple(tuple_to_u32(IP) band 
		 (ones_mask_right(NumBits) bsl (32 - NumBits))).




next_host(IP) ->
    u32_to_tuple(tuple_to_u32(IP) + 1).


%% the "smallest" possible IP in this network

min_host(IP, Mask) when Mask == {255,255,255,255};
			Mask == 32 ->
    IP;
min_host(IP, Mask) when tuple(Mask) ->
    u32_to_tuple((tuple_to_u32(IP) band tuple_to_u32(Mask)) bor 1);
min_host(IP, NumBits) when integer(NumBits) ->
    X = tuple_to_u32(IP) band 
	(ones_mask_right(NumBits) bsl (32 - NumBits)),
    u32_to_tuple(X bor 1).



%% the "biggest" possible IP in this network

max_host(IP, Mask) when tuple(Mask) ->
    NET = network(IP, Mask),
    u32_to_tuple(tuple_to_u32(NET) bor 
		 ((bnot tuple_to_u32(Mask) bsr 1 bsl 1)));

max_host(IP, NumBits) when integer(NumBits) ->
    NET = network(IP, NumBits),
    u32_to_tuple(tuple_to_u32(NET) bor 
		 (ones_mask_right(32 - NumBits) bsr 1 bsl 1)).



%% Return the broadcast address, given an IP and a mask

broadcast(IP, Mask) when tuple(Mask) ->
    u32_to_tuple((tuple_to_u32(IP) 
		  bor (bnot (tuple_to_u32(Mask)))));
    

broadcast(IP, NumBits)  when integer(NumBits) ->
    u32_to_tuple((tuple_to_u32(IP) bor ones_mask_right(32 - NumBits))).
		  
    


%% return Ones bits integer shifted to right
ones_mask_right(Ones) ->
    (1 bsl Ones) - 1.

%% return Ones bits integer shifted to left
ones_mask_left(Ones) ->
    ones_mask_right(Ones) bsl (32 - Ones).



%% compare 2 IP addresses

compare({X1,X2,X3,X4}, {Y1,Y2,Y3,Y4}) ->
    if
	X1 < Y1 ->
	    lt;
	X1 > Y1 ->
	    gt;
	%% X1 == Y1
	X2 < Y2 ->
	    lt;
	X2 > Y2 ->
	    gt;
	X3 < Y3 ->
	    lt;
	X3 > Y3 ->
	    gt;
	X4 < Y4 ->
	    lt;
	X4 > Y4 ->
	    gt;
	true ->
	    eq
    end.


%% check wether an IP is part of a Network or not

in_network(IP, Network, NetMask)  ->
    Min = min_host(Network, NetMask),
    Max = max_host(Network, NetMask),
    case {compare(IP, Min), compare(IP, Max)} of
	{gt, lt} ->
	    true;
	{eq, _} ->
	    true;
	{_, eq} ->
	    true;
	_ ->
	    false
    end.

	    

%% Given a mask, return the slash number for that mask
%% for example {255,255,255,0} -> 24
%% or          {255,255,0,0} ->   16
    

network_size(Mask) when tuple(Mask) ->
    network_size(tuple_to_u32(Mask), 0);
network_size(Int) when integer(Int) ->
    Int.

network_size(I, Res) ->
    case I band 1 of
	1 ->
	    32 - Res;
	0 ->
	    network_size(I bsr 1, Res+1)
    end.


%% bool , checks wether an IP is valid or not
%% not entirely correct function .. but almost
valid_ip({X1,X2,X3,X4}) ->
    if
	integer(X1),
	integer(X2),
	integer(X3),
	integer(X4),
	X1 =< 254,
	X2 =< 255,
	X3 =< 255,
	X4 =< 254,
	X4 /= 0 ->
	    true;
	true ->
	    false
    end.


tuple_to_u32({X3,X2,X1,X0}) ->
    (((X3) bsl 24) bor ((X2) bsl 16) bor ((X1) bsl 8) bor (X0)).

u32_to_tuple(X) ->
    {((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
     ((X) bsr 8) band 16#ff, (X) band 16#ff}.





