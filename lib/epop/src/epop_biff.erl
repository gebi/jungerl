-module(epop_biff).
-author('tobbe@serc.rmit.edu.au').
%%%---------------------------------------------------------------------
%%% File    : epop_biff.erl
%%% Created : 6 Aug 1998 by tobbe@serc.rmit.edu.au
%%% Function: Mail notifier for Epop.
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
-export([start/1]).
%% Internal
-export([init/1,worker/7,listener/7]).

-import(epop,[initrc/0,server_port/0,server_host/0,users/1,auth/0]).

%% To work with: erl -s epop_biff start User -noshell
start([User]) when atom(User) ->
    spawn(?MODULE,init,[atom_to_list(User)]);
%% To work from the shell
start(User) when list(User) ->
    spawn(?MODULE,init,[User]).

init(User) ->
    process_flag(trap_exit,true),
    initrc(),
    Lsock = setup_listen_sock(),
    UserData = get_pop_data(User),
    {ok,Port} = inet:port(Lsock),
    Hname = epop:hostname(),
    {Seen,Accept} = order_notification(User,Port,Hname,Lsock,UserData),
    listener(User,Port,Hname,Lsock,Accept,Seen,UserData).

%% --------------------
%% The listener process

listener(User,Port,Hname,Lsock,Accept,Seen,UserData) ->
    receive
	{'EXIT',Accept,_} ->
	    NewAccept = do_accept(User,Port,Hname,Lsock,Seen,UserData),
	    ?MODULE:listener(User,Port,Hname,Lsock,NewAccept,Seen,UserData);
	stop ->
	    exit(normal);
	_ ->
	    ?MODULE:listener(User,Port,Hname,Lsock,Accept,Seen,UserData) 
    end.

do_accept(User,Port,Hname,Lsock,Seen,UserData) ->
    spawn_link(?MODULE,worker,[self(),User,Port,Hname,Lsock,Seen,UserData]).

setup_listen_sock() ->
    Opts =  [{packet,raw},{reuseaddr,true},{active,false}],
    case catch gen_tcp:listen(0,Opts) of
	{ok, Lsock} -> Lsock;
	Else        -> exit(Else)
    end.

get_pop_data(User) ->
    POPport = server_port(),
    POPhost = server_host(),
    {POPuser,POPpasswd,_} = users(User), 
    {POPuser,POPpasswd,POPhost,POPport}.

%% ----------------------------------------------
%% First try and use the (optional) UIDL command,
%% and if that fails, use the STAT command.

order_notification(User,Port,Hname,Lsock,UserData) ->
    {POPuser,POPpasswd,POPhost,POPport} = UserData,
    Addr = POPuser ++ "@" ++ POPhost,
    Auth = auth(),
    case epop:connect(Addr,POPpasswd,[Auth,{port,POPport}]) of
	{ok,S} ->
	    case epop:uidl(S) of
		{error,_} ->
		    case epop:stat(S) of
			{ok,{N,_}} ->
			    epop:notify(S,Hname,Port),
			    Seen = {stat,N},
			    Accept = do_accept(User,Port,Hname,Lsock,Seen,UserData),
			    %% To avoid a possible race condition, give
			    %% the accept process above a chance to start.
			    sleep(1), 
			    epop:quit(S),
			    {Seen,Accept};
			_ ->
			    epop:quit(S),
			    exit({error,order_notification})
		    end;
		{ok,List} ->
		    epop:notify(S,Hname,Port),
		    Seen = {uidl,List},
		    Accept = do_accept(User,Port,Hname,Lsock,Seen,UserData),
		    %% To avoid a possible race condition, give
		    %% the accept process above a chance to start.
		    sleep(1), 
		    epop:quit(S),
		    {Seen,Accept}
	    end;
	_ ->
	    exit({error,order_notification})
    end.

%% ------------------
%% The Accept process
%% ------------------

worker(Listener,User,Port,Hname,Lsock,Seen,UserData) ->
    {Win,Biff} = mk_window(User),
    go_up(Seen,Biff),
    initrc(),
    {_,POPpasswd,_,_} = UserData,
    wloop(Lsock,Port,Hname,Biff,Seen,POPpasswd).

%% Init check for mail
go_up({stat,N},Biff) when N>0         -> up(Biff);
go_up({uidl,L},Biff) when length(L)>0 -> up(Biff);
go_up(_,Biff)                         -> down(Biff).

wloop(Lsock,Port,Hname,Biff,Seen,POPpasswd) ->
    case epop:accept(Lsock,POPpasswd) of
	{ok,S} ->
	    NewSeen = any_unseen(S,Seen,Biff),
	    epop:notify(S,Hname,Port),
	    epop:quit(S),
	    wloop(Lsock,Port,Hname,Biff,NewSeen,POPpasswd);
	Else ->
	    exit(Else)
    end.

%% Depending on which method to use, decide
%% weather we have got new mail or not.

any_unseen(S,{stat,N},Biff) ->
    case epop:stat(S) of
	{ok,{I,_}} when I>N ->
	    up(Biff),
	    {stat,I};
	{ok,{I,_}} ->
	    down(Biff),
	    {stat,I};
	{error,Reason} -> exit(Reason)
    end;
any_unseen(S,{uidl,L1},Biff) ->
    case epop:uidl(S) of
	{ok,[]} ->
	    down(Biff),
	    {uidl,[]};
	{ok,L2} ->
	    case (L2 -- L1) of
		[] -> down(Biff);
		_  -> up(Biff)
	    end,
	    {uidl,L2};
	{error,Reason} -> exit(Reason)
    end.

up({_,Up}) -> gs:config(Up,raise).

down({Down,_}) -> gs:config(Down,raise).

mk_window(User) ->
    GS = gs:start(),
    WH = [{width,50},{height,50}],
    Win = gs:window(GS,[{configure,true},{title,"Mail for " ++ User}|WH]),
    Canvas = gs:create(canvas,Win,WH),
    Dir = gif_dir(),
    Up = gs:create(image,Canvas,[{load_gif,Dir ++ "/mail-up.gif"}]),
    Down = gs:create(image,Canvas,[{load_gif,Dir ++ "/mail-down.gif"}]),
    gs:config(Win,{map,true}),
    {Win,{Down,Up}}.

gif_dir() ->
    {file,Fname} = code:is_loaded(?MODULE),
    filename:dirname(Fname).

sleep(T) -> receive after T -> true end.

