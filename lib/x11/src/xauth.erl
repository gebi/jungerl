%%% File    : xauth.erl
%%% Author  : Tony Rogvall <tony@a55.hemma.se>
%%% Description : X auth fucntions
%%% Created : 27 Jan 2003 by Tony Rogvall <tony@a55.hemma.se>

-module(xauth).

-compile(export_all).

-include("x.hrl").
-include_lib("kernel/include/inet.hrl").

parse(Fname) ->
    case file:read_file(Fname) of
	{ok,Bin} ->
	    case catch parse_xauth(Bin) of
		{'EXIT',Reason} -> {error,Reason};
		Error -> Error
	    end;
	Error -> Error
    end.


parse_xauth(<<Family:16,Alen:16,D0/binary>>) ->
    <<Address:Alen/binary, NLen:16, D1/binary>> = D0,
    <<Number:NLen/binary, Len:16, D2/binary>> = D1,
    <<Name:Len/binary, DLen:16, D3/binary>> = D2,
    <<Data:DLen/binary, Rest/binary>> = D3,
    [ #xauth{ family = Family,
	      address = fam(Family,Address),
	      number  = Number,
	      name    = Name,
	      data=Data} | parse_xauth(Rest)];
parse_xauth(<<>>) ->
    [].

fam(?FAMILY_IP_ADDRESS,<<A,B,C,D>>) ->
    {A,B,C,D};
fam(?FAMILY_LOCAL, String) ->
    binary_to_list(String);
fam(_, Data) ->
    Data.


hent({A,B,C,D}) ->
    inet:gethostbyaddr({A,B,C,D}, 2000);
hent(Name) ->
    case inet_parse:address(Name) of
	{ok,IP} ->
	    inet:gethostbyaddr(IP, 2000);
	_ ->
	    inet:gethostbyname(Name, inet, 2000)
    end.
    
lookup(Host, Auth) ->
    case hent(Host) of
	{ok,HEnt} ->
	    case lists:keysearch(HEnt#hostent.h_name,#xauth.address,Auth) of
		{value,A} -> {value,A};
		false ->
		    case lists:keysearch(hd(HEnt#hostent.h_addr_list),
					 #xauth.address,Auth) of
			{value,A} ->
			    {value,A};
			false ->
			    false
		    end
	    end;
	_ -> false
    end.


