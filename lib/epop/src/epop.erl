-module(epop).
-author('tobbe@serc.rmit.edu.au').
%%%---------------------------------------------------------------------
%%% File    : epop.erl
%%% Created : 3 Mar 1998 by tobbe@serc.rmit.edu.au
%%% Function: A POP3 server/client package.
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
-export([start/0,stop/0,version/0]).
-export([connect/2,connect/3,stat/1,scan/1,scan/2,retrieve/2,delete/2,top/3,
	 reset/1,quit/1,store_mail/3,rmail/1,uidl/1,uidl/2,accept/2,accept/3]).
-export([notify/3,lcase/1]).
-export([initrc/0,read_const/1,recv_sl/1,recv_sl/2,recv_ml/1,recv_ml_on_ok/1,
	 tokenize/1,server_port/0,server_host/0,users/1,auth/0,hostname/0]).

-import(error_logger,[error_msg/1]).

-include_lib("kernel/include/inet.hrl").

-define(POP3_PORTNO, 110).

-define(CR, 13).
-define(LF, 10).

%% ------------------------
%% Exported client commands
%% ------------------------

connect(User,Passwd)      -> epop_client:connect(User,Passwd).
connect(User,Passwd,Opts) -> epop_client:connect(User,Passwd,Opts).
accept(Sock,Passwd)       -> epop_client:accept(Sock,Passwd).
accept(Sock,Passwd,Opts)  -> epop_client:accept(Sock,Passwd,Opts).
stat(S)                   -> epop_client:stat(S).
scan(S)                   -> epop_client:scan(S).
scan(S,MsgNum)            -> epop_client:scan(S,MsgNum).
uidl(S)                   -> epop_client:uidl(S).
uidl(S,MsgNum)            -> epop_client:uidl(S,MsgNum).
top(S,MsgNum,Lines)       -> epop_client:top(S,MsgNum,Lines).
retrieve(S,MsgNum)        -> epop_client:retrieve(S,MsgNum).
delete(S,MsgNum)          -> epop_client:delete(S,MsgNum).
reset(S)                  -> epop_client:reset(S).
quit(S)                   -> epop_client:quit(S).

notify(S,Host,Port)       -> epop_client:notify(S,Host,Port).

%% ------------------------------------
%% Exported commands for local delivery
%% ------------------------------------

store_mail(User,MsgID,Mail)      -> epop_dets:store_mail(User,MsgID,Mail).

%% ---------------------------------
%% Exported command for mail reading
%% ---------------------------------

rmail(User) -> epop_mh:rmail(User).

%% --------------------------------------------------------
%% Exported commands to deal whith the user configure file.
%%
%% Try and find the file in the $EPOP_HOME directory or the
%% $HOME directory. Read the file .epop and store the values
%% (in the process dictionary).
%% NB: The values are regarded to be constants so there exist
%% no write function !!
%% --------------------------------------------

initrc() ->
    case os:getenv("EPOP_HOME") of
	false ->
	    case os:getenv("HOME") of
		false -> {error,no_home_found};
		Home  -> init(file:consult(Home ++ "/.epop"))
	    end;
	EpopHome ->
	    init(file:consult(EpopHome ++ "/.epop"))
    end.

init({ok,Vs}) ->
    F = fun({K,V}) -> put(K,V) end,
    lists:foreach(F,Vs),
    ok;
init(Else) ->
    error_logger:error_msg("epop: Error in .epop file: ~p",[Else]),
    throw({error,init_file}).

read_const(K) -> get(K).

%% Default is port 110
server_port() ->
    case read_const(epop_server_port) of
	Port when integer(Port) -> Port;
	_ ->
	    ?POP3_PORTNO
    end.

server_host() ->
    case read_const(epop_server_host) of
	Host when list(Host) -> Host;
	_ ->
	    error_msg("epop: No server host specified !"),
	    exit({error,no_server_host})
    end.

auth() ->
    case read_const(epop_auth) of
	upass -> upass;
	apop  -> apop;
	_ ->
	    error_msg("epop: No (or wrong) authentication method specified !"),
	    exit({error,no_auth})
    end.

users(User) when list(User) ->
    case read_const(epop_users) of
	Users when list(Users) ->
	    case lists:keysearch(User,1,Users) of
		{value,{_,Passwd,Opts}} -> {User,Passwd,Opts};
		_ ->
		    error_msg("epop: No user data found !"),
		    exit({error,no_user_data})
	    end;
	_ ->
	    error_msg("epop: No server host specified !"),
	    exit({error,no_server_host})
    end.

%% ------------------------
%% Exported server commands
%% ------------------------

start()      -> epop_server:start().
stop()       -> epop_server:stop().
version()    -> epop_server:version().


%% ----------
%% Misc stuff
%% ----------

lcase([C|Cs]) when C>=$A,C=<$Z -> [C+32|lcase(Cs)]; % A-Z
lcase([C|Cs])                  -> [C|lcase(Cs)];
lcase([])                      -> [].


%% ---------------------------------------------------------
%% If we are receiving a positive response, then receive
%% it as a multi-line response. Otherwise as a single-line.
%% ---------------------------------------------------------

recv_ml_on_ok(S) ->
    case recv_3_chars(S) of
	[$+,$O,$K|T] ->
	    recv_ml(S,[$+,$O,$K|T]);
	Else ->
	    recv_sl(S,Else)
    end.

recv_3_chars(S) -> recv_3_chars(S,recv(S)).

recv_3_chars(S,Cs) when length(Cs)>=3 -> Cs;
recv_3_chars(S,Cs) -> recv_3_chars(S,Cs ++ recv(S)).
		    
%% ------------------------------------------
%% Receive a CRLF.CRLF terminated multi-line.
%% ------------------------------------------

recv_ml(S) ->
    recv_ml(S,[]).

recv_ml(S,Cc) ->
    rml(1,S,Cc,[]).

%% A simple state-event machine to handle the byte stuffing
%% of the termination octet. See also page.2 in the RFC-1939.
%% Since we are using a raw socket we are using this
%% continuation based style of programming.

rml(1,S,[?CR|T],Mline)        -> rml(2,S,T,[?CR|Mline]);     % goto next state
rml(1,S,[?LF|T],Mline)        -> rml(3,S,T,[?LF|Mline]);     % goto next state
rml(1,S,[H|T],Mline)          -> rml(1,S,T,[H|Mline]);       % stay

rml(2,S,[?LF|T],Mline)        -> rml(3,S,T,[?LF|Mline]);     % goto next state
rml(2,S,[H|T],Mline)          -> rml(1,S,[H|T],Mline);       % continue

rml(3,S,[$.|T],Mline)         -> rml(4,S,T,[$.|Mline]);      % goto next state
rml(3,S,[H|T],Mline)          -> rml(1,S,[H|T],Mline);       % continue

rml(4,S,[?CR|T],Mline)        -> rml(5,S,T,[?CR|Mline]);     % goto next state
rml(4,S,[?LF|T],Mline)        -> rml(6,S,T,[?LF|Mline]);     % goto next state
rml(4,S,[H|T],[$.|Mline])     -> rml(1,S,[H|T],Mline);       % continue

rml(5,S,[?LF|T],Mline)        -> rml(6,S,T,[?LF|Mline]);     % goto next state
rml(5,S,[H|T],[$.|Mline])     -> rml(1,S,[H|T],Mline);       % (de-)byte stuff

rml(6,S,T,[?LF,?CR,$.|Mline]) -> {lists:reverse(Mline),T};   % accept
rml(6,S,T,[?LF,$.|Mline])     -> {lists:reverse(Mline),T};   % accept

rml(State,S,[],Mline)         -> rml(State,S,recv(S),Mline). % get more


%% -----------------------------------------------------
%% Receive a complete single-line (ended by a CRLF pair.
%% Returns: {Single-Line, Continuation-Characters (Cc) }
%% Where Cc is the characters next to be processed.
%% -----------------------------------------------------

recv_sl(S) ->
    recv_sl(S,[]).

recv_sl(S,Cc) ->
    complete_sl(S,Cc,[]).

complete_sl(S,[?CR|T],Line) ->
    complete_sl_lf(S,T,[?CR|Line]);
complete_sl(S,[H|T],Line) ->
    complete_sl(S,T,[H|Line]);
complete_sl(S,[],Line) ->
    complete_sl(S,recv(S),Line).

complete_sl_lf(S,[?LF|T],Line) ->
    {lists:reverse([?LF|Line]),T};
complete_sl_lf(S,[H|T],Line) ->
    complete_sl(S,T,[?LF|Line]);
complete_sl_lf(S,[],Line) ->
    complete_sl_lf(S,recv(S),Line).

recv(S) ->
    case gen_tcp:recv(S,0) of
	{ok,Packet} -> Packet;
	Else        -> exit(Else)
    end.

%% -----------------------------------------------
%% Tokenize using \r\n as the two separators.
%% -----------------------------------------------

tokenize(L) ->
    tokenize(skip(L),[]).

tokenize([],Acc) ->
    lists:reverse(Acc);
tokenize(L,Acc) ->
    {Token,Rest} = get_token(L),
    tokenize(skip(Rest),[Token|Acc]).

skip([$\r,$\n|T]) -> skip(T);
skip(L)       -> L.

get_token(L) -> get_token(L,[]).

get_token([H|T],Acc) when H=/=$\r,H=/=$\n ->
    get_token(T,[H|Acc]);
get_token(L,Acc) -> {lists:reverse(Acc),L}.

%% ----------------------
%% Get our local hostname
%% ----------------------

hostname() ->
    {ok,Hname} = inet:gethostname(),
    {ok,H} = inet:gethostbyname(Hname),
    H#hostent.h_name.

