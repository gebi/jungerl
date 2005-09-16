%%%-------------------------------------------------------------------
%%% Created : 16 Sep 2005 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Desc.   : Setup some test examples.
%%%
%%% $Id$
%%%-------------------------------------------------------------------
-module(ftp_test).

-include("ftpd.hrl").

-export([start/0]).

%%% Callback
-export([auth/2, event/1]).

-define(elog(X,Y), error_logger:info_msg("*elog ~p:~p: " X,
					 [?MODULE, ?LINE | Y])).

-define(HOME_DIR, "/tmp/tobbe/ftp").
-define(ROOT_DIR, ?HOME_DIR).

start() ->
    fdsrv:start(),
    os:cmd("mkdir -p " ++ ?HOME_DIR),
    ?elog("SYS_OPS = ~p~n", [?OPS_RESTRICTED]),
    ftpd:start([{root,"/tmp/tobbe"}, 
		{use_fd_srv,true}, 
		{auth_mod, ?MODULE},
		{event_mod, ?MODULE},
		{sys_ops, ?OPS_RESTRICTED},
		{jail, true}]).


auth("tobbe", "qwe123") ->
    ?elog("Inside ftp_test:auth callback, auth successful!~n", []),
    {true, ?ROOT_DIR, [{?HOME_DIR, [read,write,delete]}]};
auth(_User, _Passwd) ->
    false.


event(Event) ->
    ?elog("Got Event <~p>~n", [Event]).



