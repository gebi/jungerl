-ifndef(_ERADIUS).
-define(_ERADIUS, true).
%%%-------------------------------------------------------------------
%%% File        : eradius.hrl
%%% Author      : tobbe@bluetail.com
%%% Description : Definitions for the eradius interface.
%%% Created     : 22 Sep 2003 by tobbe@bluetail.com
%%%
%%% $Id$
%%%-------------------------------------------------------------------

%%% Error codes
-define(AL_Bad_Passwd,            1).   % bad passwd or user name
-define(AL_Backend_Unreachable,   2).
-define(AL_Backend_Error,         3).   % other backend related, e.g. bad pdu
-define(AL_Internal_Error,        4).


%%% Result codes passed on to the (user provided) statistic fun.
-define(STAT_ACCEPTED,     1).
-define(STAT_REJECTED,     2).
-define(STAT_TIMEDOUT,     3).

%%% Calling the statistics fun
-define(STATFUN_ACCEPTED(E, Ip, Port), 
	(E#eradius.statfun)(?STAT_ACCEPTED, E, Ip, Port)).
-define(STATFUN_REJECTED(E, Ip, Port), 
	(E#eradius.statfun)(?STAT_REJECTED, E, Ip, Port)).
-define(STATFUN_TIMEDOUT(E, Ip, Port), 
	(E#eradius.statfun)(?STAT_TIMEDOUT, E, Ip, Port)).

%%% Calling the trace fun
-define(TRACEFUN(E, Str, Args), (E#eradius.tracefun)(E, Str, Args) ).

%%% Null funs
-define(SF, fun(_,_,_,_) -> true end).
-define(TF, fun(_,_,_) -> true end).

-record( eradius , {
	   servers=[],     % a list of [Ip,Port,Secret] tripplets
	   state = <<>>,   % the radius state
	   user,           % user id
	   passwd,         % password/challenge-reply
	   statfun=?SF,    % fun(StatRes, Eradius{}, Ip, Port)
	   tracefun=?TF,   % fun(Eradius{}, Str, Args)
	   nas_ip_address, % our IP address
	   vendor_id,
	   vendor_type,
	   opaque,         % opaque user data
	   timeout=10000}).% timeout in milliseconds

-endif.
