-module(epop_db).
-author('tobbe@serc.rmit.edu.au').
%%%---------------------------------------------------------------------
%%% File    : epop_db.erl
%%% Created : 4 Mar 1998 by tobbe@serc.rmit.edu.au
%%% Function: Some simple functions for manipulating a session database.
%%%           The session database consists of a collection of {Key,Value}
%%%           tuples. 
%%%=====================================================================
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
-export([db/0,db_in/3,db_out/2,db_replace/3,db_filter/2,
	 db_foreach/2,db_member/2,db_member/3,db_delete/2]).


db() -> [].

db_in(Db,Key,Value) -> [{Key,Value}|Db].

db_out(Db,Key) ->
    case lists:keysearch(Key,1,Db) of
	{value,{_,Value}} -> {ok,Value};
	_ -> {error,keynotfound}
    end.

db_delete(Db,Key) ->
    lists:keydelete(Key,1,Db).

db_replace(Db,Key,Value) ->
    lists:keyreplace(Key,1,Db,{Key,Value}).

db_filter(Db,F) ->
    lists:filter(F,Db).

db_foreach(Db,F) ->
    lists:foreach(F,Db).

db_member(Db,Key) ->
    lists:keymember(Key,1,Db).

db_member(Db,Key,Value) ->
    lists:member({Key,Value},Db).
