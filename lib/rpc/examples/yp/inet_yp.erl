%%% File    : inet_yp.erl
%%% Author  : Tony Rogvall <tony@erix.ericsson.se>
%%% Purpose : YP (NIS) interface to gethostbyname & gethostbyaddr
%%% Created : 17 Nov 1997 by Tony Rogvall <tony@erix.ericsson.se>
%%% Copyright (c) 2000, 2001 Sendmail, Inc.  All rights reserved.

-module(inet_yp).
-author('tony@erix.ericsson.se').

-export([gethostbyname/1, gethostbyaddr/1]).

-include("yp.hrl").

gethostbynis(Name, Map) ->
    case rpc_clnt:open("super", ?YPPROG, ?YPVERS, udp) of
	{ok,R} ->
	    YpReqKey = { "duper", Map, Name },
	    Res = yp_clnt:ypproc_match_2(R, YpReqKey),
	    rpc_clnt:close(R),
	    Res;
	Error -> Error
    end.


gethostbyname(Name) ->
    gethostbynis(Name, "hosts.byname").

gethostbyaddr(Addr) ->
    gethostbynis(Addr, "hosts.byaddr").




	    
    


