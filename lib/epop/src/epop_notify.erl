-module(epop_notify).
-author('tobbe@serc.rmit.edu.au').
%%%---------------------------------------------------------------------
%%% File    : epop_notify.erl 
%%% Created : 26 Aug 1998 by tobbe@serc.rmit.edu.au
%%% Function: My little notification extension.
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
%%% The Original Code is epop-2-3
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%
%%%---------------------------------------------------------------------
-vc('$Id$ ').
-export([start/0,stop/0,order/2,order/3,notify/1]).
-export([notify/3,init/1]).

-import(error_logger,[error_msg/2]).
-import(epop_db,[db/0,db_delete/2,db_in/3,db_out/2]).

-define(SERVER_NAME, epop_notifier).
-define(POP3_PORTNO, 110).

%% ------------------
%% Exported interface
%% ------------------

start() -> 
    case whereis(?SERVER_NAME) of
	Pid when pid(Pid) -> {ok,Pid};
	_ ->
	    Pid = spawn_link(?MODULE, init, [self()]),
	    register(?SERVER_NAME,Pid),
	    {ok,Pid}
    end.

stop() ->
    ?SERVER_NAME ! {self(),stop},
    receive stopped -> ok end.

%% ------------------------------------
%% To be called by the epop_server when 
%% notification is ordered.
%% ------------------------------------

order(Name,Host) ->
    order(Name,Host,?POP3_PORTNO).

order(Name,Host,Port) ->
    ?SERVER_NAME ! {self(),order,{Name,Host,Port}},
    receive {Pid,Answer} -> Answer end.

%% ----------------------------------
%% To be called whenever the maildrop
%% has been accessed.
%% ----------------------------------

notify(Name) ->
    ?SERVER_NAME ! {notify,Name}.

%% ----------
%% The server
%% ----------

init(Epop) ->
    loop(Epop,db()).
    
loop(Epop,Db) ->
    receive
	{From,order,{Name,Host,Port}} when pid(From) ->
	    From ! {self(),ok},
	    NewDb = do_order(Db,Name,Host,Port),
	    loop(Epop,NewDb);
	{notify,Name} ->
	    NewDb = do_notify(Db,Name),
	    loop(Epop,NewDb);
	{Epop,stop} ->
	    Epop ! stopped,
	    exit(stopped);
	{'EXIT',Epop,Reason} ->
	    exit(Reason);
	_ ->
	    loop(Epop,Db)
    end.

do_order(Db,Name,Host,Port) ->
    db_in(Db,Name,{Host,Port}).

do_notify(Db,Name) ->
    case db_out(Db,Name) of
	{ok,{Host,Port}} ->
	    do_notify(Name,Host,Port),
	    db_delete(Db,Name);
	_ -> Db
    end.

do_notify(Name,Host,Port) ->
    spawn(?MODULE,notify,[Name,Host,Port]).

notify(Name,Host,Port) ->
    epop:initrc(),
    Options = [{packet,raw},{reuseaddr,true},{active,false}],
    case gen_tcp:connect(Host,Port,Options) of
	{ok,Sock} -> send_notification(Sock,Name);
	_ -> error_msg("epop_notify: Couldn't connect to host ~s:~w~n",
		       [Host,Port])
    end.

%% -----------------------------------------------
%% NB: This function is very similar to the
%% authenticate/1 function in the epop_server.
%% The difference is that instead of the standard
%% greeting message beeing sent, we will send a
%% NTFY message before entering the standard
%% authentication state of POP3.
%% -----------------------------------------------

send_notification(Sock,Name) ->
    case epop:read_const(epop_auth) of
	upass ->
	    send_msg(Sock,ntfy_msg(Name)),
	    Db = db(),
	    epop_server:authorize(Sock,Db,upass);
	apop  ->
	    TS = epop:apop_timestamp(),
	    send_msg(Sock,ntfy_msg(Name,TS)),
	    Db = db_in(db(),banner_timestamp,TS),
	    epop_server:authorize(Sock,Db,apop);
	Else  ->
	    error_msg("epop_notify: No (or wrong) authentication "
		      "method: ~p , server terminating...~n",[Else]),
	    exit(epop_auth)
    end.

send_msg(S,Msg) -> gen_tcp:send(S,Msg).
        
ntfy_msg(Name) ->
    "NTFY " ++ Name ++ "\r\n".

ntfy_msg(Name,TS) ->
    "NTFY " ++ Name ++ " " ++ TS ++ "\r\n".
