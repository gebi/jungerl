-module(ex11_db).
-author('tnt@home.se').
%%%---------------------------------------------------------------------
%%% Created : 17 Feb 1999 by tnt@home.se
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
%%%---------------------------------------------------------------------
-export([init/0,get_display/1,set_display/2,put_window/3,put_pixmap/3,
	 put_gc/3,put_id/4,get_id/2,store_req/4,get_reply/2]).

-include("ex11.hrl").

%% Create various keys
-define(ID_KEY(Id),              {id,Id}).
-define(PID_KEY(Pid),          {pid,Pid}).
-define(SEQNO_KEY(SeqNo),  {seqno,SeqNo}).


init() -> 
    ets:new(?MODULE,[public,set]).

get_display(X) when ?IS_XLIB_HANDLE(X) ->
    get_display(X#xlib.db);
get_display(Db) ->
    case ets:lookup(Db,display) of
	[{_,Dpy}] when ?IS_DISPLAY(Dpy) -> Dpy;
	Else -> exit({error,{get_display,Else}})
    end.

set_display(X,Dpy) when ?IS_XLIB_HANDLE(X) ->
    set_display(X#xlib.db,Dpy);
set_display(Db,Dpy) when ?IS_DISPLAY(Dpy) ->
    ets:insert(Db,{display,Dpy}).

put_window(Db,Id,Pid) -> put_id(Db,Id,Pid,window).
put_pixmap(Db,Id,Pid) -> put_id(Db,Id,Pid,pixmap).
put_gc(Db,Id,Pid)     -> put_id(Db,Id,Pid,gc).

put_id(Db,Id,Pid,Type) ->
    ets:insert(Db,{?ID_KEY(Id),Pid,Type}),
    ets:insert(Db,{?PID_KEY(Pid),Id,Type}).

get_id(Db,Pid) when pid(Pid) ->
    case ets:lookup(Db,?PID_KEY(Pid)) of
	[{_,Id,Type}] -> {ok,{Id,Type}};
	_ -> {error,not_found}
    end;
get_id(Db,Id) when integer(Id) ->
    case ets:lookup(Db,?ID_KEY(Id)) of
	[{_,Pid,Type}] -> {ok,{Pid,Type}};
	_ -> {error,not_found}
    end.

%% Store info telling who is expecting the reply
%% to the this sequence number, and what type of
%% reply it is.
store_req(Db,SeqNo,From,ReplyType) ->
    io:format("Storing: seqno=~w  pid=~w  type=~w ~n",
	      [SeqNo,From,ReplyType]),
    ets:insert(Db,{?SEQNO_KEY(SeqNo),From,ReplyType}).

get_reply(Db,SeqNo) when integer(SeqNo) ->
    Key = ?SEQNO_KEY(SeqNo),
    case ets:lookup(Db,Key) of
	[{_,Pid,ReplyType}] -> 
	    io:format("GOOOTTE 1: ~w~n",[{Pid,ReplyType}]),
	    ets:delete(Db,Key),
	    {ok,{Pid,ReplyType}};
	XX -> 
	    io:format("GOOOTTE 2: seqno=~w ~w~n",[SeqNo,XX]),
	    {error,not_found}
    end.

    
