-module(ex11_xauth).
-author('tnt@home.se').
%%%---------------------------------------------------------------------
%%% Created : 15 Feb 1999 by tnt@home.se
%%% Function: Read all entries of the .Xauthority file.
%%% ====================================================================
%%% The contents of this file are subject to the Erlang Public License
%%% License, Version 1.0, (the "License"); you may not use this file
%%% except in compliance with the License. You may obtain a copy of the
%%% License at http://www.eddieware.org/EPL
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is x11-0-1
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1999, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%
%%%
%%% Modified: 23 Feb 1998 by tony@cslab.ericsson.se
%%%           To make it work under Windows. Added filename/0.
%%%
%%%---------------------------------------------------------------------
-export([read/1,host2cookie/2,filename/0]).

-import(ex11_utils,[first/2,i16/2,split_list/2]).

-include_lib("kernel/include/inet.hrl").
-include("ex11.hrl").

-define(FAMILY_LOCAL,          256).  % not part of X standard (i.e. X.h) 
-define(FAMILY_WILD,         65535). 
-define(FAMILY_NETNAME,        254).  % not part of X standard 
-define(FAMILY_KRB5_PRINCIPAL, 253).  % Kerberos 5 principal name 
-define(FAMILY_LOCALHOST,      252).  % for local non-net authentication 
-define(FAMILY_IP_ADDRESS,       0).  % ...as it seems...

%% -------------------------------------------
%% Return the name of the authority file

filename() ->
    case os:getenv("XAUTHORITY") of
	false ->
	    case os:getenv("HOME") of
		false ->
		    case os:type() of
			{win32,_} ->
			    case os:getenv("USERNAME") of
				false -> "";
				Name ->
				    filename:join(["/users/",Name,
						   ".Xauthority"])
			    end;
			_ -> ""
		    end;
		Home -> filename:join(Home, ".Xauthority")
	    end;
	File -> File
    end.

%% -------------------------------------------
%% Return all entries in the .Xauthority file

read(Fname) ->
    case file:read_file(Fname) of
	{ok,Bin} ->
	    List = binary_to_list(Bin),
	    case catch parse_xauth(List) of
		{'EXIT',Reason} -> 
		    {error,Reason};
		Else -> {ok,Else}
	    end;
	Error ->
	    Error
    end.

%% ---------------------------------------------
%% Get corresponding cookie for given hostname

host2cookie(Host,Adata) ->
    case addr2cookie(Host,Adata) of
	{true,Cookie} -> {true,Cookie};
	false -> addr2cookie(host2ip(Host),Adata)
    end.

addr2cookie(Address,Adata) ->
    F = fun(A) when A#xauth.address == Address -> {true,A#xauth.data};
	   (_) -> false
	end,
    first(F,Adata).

    
parse_xauth([]) -> [];
parse_xauth([Fam1,Fam0,Alen1,Alen0|D0]) ->
    Family = i16(Fam1,Fam0),
    Alen = i16(Alen1,Alen0),
    {Address,D1} = split_list(Alen,D0),
    [Nlen1,Nlen0|D2] = D1,
    Nlen = i16(Nlen1,Nlen0),
    {Number,D3} = split_list(Nlen,D2),
    [Len1,Len0|D4] = D3,
    Len = i16(Len1,Len0),
    {Name,D5} = split_list(Len,D4),
    [Dlen1,Dlen0|D6] = D5,
    Dlen = i16(Dlen1,Dlen0),
    {Data,Rest} = split_list(Dlen,D6),
    [#xauth{family=Family,
	    address=is_ip(Family,Address),
	    number=Number,
	    name=Name,
	    data=Data}|
     parse_xauth(Rest)].

is_ip(?FAMILY_IP_ADDRESS,[X3,X2,X1,X0]) -> 
    ip2str(X3,X2,X1,X0);
is_ip(_,WhatEver) -> WhatEver.

host2ip(Host) ->
    {ok,E} = inet:gethostbyname(Host),
    ip2str(hd(E#hostent.h_addr_list)).


ip2str({X3,X2,X1,X0}) -> ip2str(X3,X2,X1,X0);
ip2str([X3,X2,X1,X0]) -> ip2str(X3,X2,X1,X0).

ip2str(X3,X2,X1,X0) ->
    lists:flatten(io_lib:format("~w.~w.~w.~w",[X3,X2,X1,X0])).
