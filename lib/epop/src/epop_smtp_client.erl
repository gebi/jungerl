-module(epop_smtp_client).
-author('tobbe@serc.rmit.edu.au').
%%%---------------------------------------------------------------------
%%% File    : epop_smtp_client.erl
%%% Created : 18 Aug 1998 by tobbe@serc.rmit.edu.au
%%% Function: A simple SMTP client (see also RFC-821).
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
-export([async_deliver/3,async_deliver/4,deliver/3,deliver/4]).
-export([init/5]).

-import(error_logger,[error_msg/2,info_msg/2]).

-include_lib("kernel/include/inet.hrl").

-record(sk, {user,            % User name.
	     host,            % SMTP host.
	     domain,          % Our domain name.
	     from,            % The <From> field in the mail.
	     sockfd,          % Socket filedesc.
	     port=25,         % The SMTP server port number
	     snoop=false}).   % Trace on/off

%% -----------------------------------------------
%% Exported interface. We don't care about weather
%% the delivery was successful or not.
%% -----------------------------------------------

async_deliver(User,Host,Mail) ->
    spawn(?MODULE,deliver,[User,Host,Mail]).

async_deliver(User,Host,Mail,Opts) ->
    spawn(?MODULE,deliver,[User,Host,Mail,Opts]).

%% -----------------------------------------------------
%% Exported interface. We return true if we successfully
%% delivered the mail, otherwise we return false.
%% -----------------------------------------------------

deliver(User,Host,Mail) ->
    deliver(User,Host,Mail,[]).

deliver(User,Host,Mail,Opts) ->
    Pid = spawn(?MODULE,init,[self(),User,Host,Mail,Opts]),
    receive {Pid,Answer} -> Answer end.

%% ------------------------
%% Execute in a new process

init(Pid,User,Host,Mail,Opts) ->
    Answer = case catch doit(User,Host,Mail,Opts) of
		 {'EXIT',Reason} -> log_error({crashed,Reason}),false;
		 {error,Reason}  -> log_error(Reason),false;
		 {ok,Result}     -> log_result(Result),true
	     end,
    Pid ! {self(),Answer}.


%% -----------------------
%% Start the SMTP session.
%% -----------------------

doit(User,Host,Mail,Opts) ->
    From = get_from(Mail),
    S = init_session(User,Host,From,Opts),
    connect(S,Mail).

get_from([$F,$r,$o,$m,$:|Ms]) ->
    case regexp:match(Ms,"<.*>") of
	{match,Start,Length} ->
	    string:substr(Ms,Start,Length);
	_ ->
	    throw({error,from_field})
    end;
get_from([H|T]) ->
    get_from(T).

%% Initiate the session key

init_session(User,Host,From,Options) ->
    set_options(Options,#sk{user=User,host=Host,from=From,domain=domain()}).

set_options([{snoop,Flag}|T],S) ->
    set_options(T,S#sk{snoop=Flag});
set_options([{port,Port}|T],S) ->
    set_options(T,S#sk{port=Port});
set_options([X|_],_) ->
    throw({error,{unknown_option,X}});
set_options([],S) ->
    S.

%% ----------------------
%% Connect to SMTP server

connect(S,Mail) ->
    Opts = [{packet,raw},{reuseaddr,true},{active,false}],
    case gen_tcp:connect(S#sk.host,S#sk.port,Opts) of
	{ok,Sock} -> get_greeting(S#sk{sockfd=Sock},Mail);
	_         -> throw({error,{connect_failed,S#sk.host}})
    end.

%% We are expecting to receive a 220 reply which we
%% answer with a HELO identification, which in turn
%% is expected to be replied with a 250 reply.

get_greeting(S,Mail) ->
    {Got,_} = expect(S,"220"),
    if_snoop(S,sender,"220" ++ Got),
    Msg = "HELO " ++ S#sk.domain,
    send(S,Msg),
    if_snoop(S,client,Msg),
    {Got2,_} = expect(S,"250"),
    if_snoop(S,sender,"250" ++ Got2),
    mail_from(S,Mail).
	    
mail_from(S,Mail) ->
    Msg = "MAIL FROM:" ++ S#sk.from,
    send(S,Msg),
    if_snoop(S,client,Msg),
    {Got,_} = expect(S,"250"),
    if_snoop(S,sender,"250" ++ Got),
    mail_to(S,Mail).

mail_to(S,Mail) ->
    To = mk_to(S#sk.user,S#sk.host),
    Msg = "RCPT TO:" ++ To,
    send(S,Msg),
    if_snoop(S,client,Msg),
    {Got,_} = expect(S,"250"),
    if_snoop(S,sender,"250" ++ Got),
    data(S,Mail).

mk_to(User,Host) -> mk_path(User ++ "@" ++ Host).

mk_path(X) -> "<" ++ X ++ ">".

data(S,Mail) ->
    Msg = "DATA",
    send(S,Msg),
    if_snoop(S,client,Msg),
    {Got,_} = expect(S,"354"),
    if_snoop(S,sender,"354" ++ Got),
    transfer_data(S,Mail).

transfer_data(S,Mail) ->
    send(S,byte_stuff_it(Mail)),
    send(S,"."),
    Msg = string:substr(Mail,1,20) ++ ".....",    
    if_snoop(S,client,Msg),
    {Got,_} = expect(S,"250"),
    if_snoop(S,sender,"250" ++ Got),
    quit(S).

quit(S) ->
    Msg = "QUIT",
    send(S,Msg),
    if_snoop(S,client,Msg),
    {Got,_} = expect(S,"221"),
    if_snoop(S,sender,"221" ++ Got),
    gen_tcp:close(S#sk.sockfd),
    {ok,{finished,S#sk.user,S#sk.host}}.

%% -------------
%% Misc routines
%% -------------

%% ---------------------
%% Perform byte stuffing
    
byte_stuff_it([$\r,$\n,$.|Mail]) ->
    [$\r,$\n,$.,$.|byte_stuff_it(Mail)];
byte_stuff_it([H|T]) ->
    [H|byte_stuff_it(T)];
byte_stuff_it([]) ->
    [].

%% Receive a message. The beginning of the message
%% must match the 'Expected' string. When the match
%% is complete, return the rest of the message,
%% otherwise abort the connection.

expect(S,Expected) ->
    expect(S,Expected,recv(S)).

expect(S,[H|T1],[H|T2]) -> expect(S,T1,T2);
expect(S,[],T)          -> recv_sl(S,T);
expect(S,T,[])          -> expect(S,T,recv(S));
expect(S,_,_)           -> abort(S).

abort(S) ->
    Msg = "QUIT",
    send(S,Msg),
    if_snoop(S,client,Msg),
    gen_tcp:close(S#sk.sockfd),
    throw({error,abort}).
    
%% -----------------------------------------------------
%% Receive a complete single-line (ended by a CRLF pair.
%% Returns: {Single-Line, Continuation-Characters (Cc) }
%% Where Cc is the characters next to be processed.

%recv_sl(S) ->
%    recv_sl(S,[]).

recv_sl(S,Cc) ->
    complete_sl(S,Cc,[]).

-define(CR, $\r).
-define(LF, $\n).

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
    case gen_tcp:recv(S#sk.sockfd,0) of
	{ok,Packet} -> Packet;
	Else        -> exit(Else)
    end.

%% ---------------------------------------
%% Print trace info if snoop option is set

if_snoop(S,Who,Msg) when S#sk.snoop==true ->
    io:fwrite("~s: ~s~n",[who(Who),Msg]);
if_snoop(_,_,_) ->
    true.

who(sender) -> "S";
who(client) -> "C".

%% -----------------------------
%% Send a CRLF terminated string

send(S,Msg) -> gen_tcp:send(S#sk.sockfd,Msg ++ "\r\n").

%% -----------------------------------------------------
%% Get our domain name by first getting our hostname and
%% then stripping off the first part separated by a dot.

domain() ->
    {ok,Hname} = inet:gethostname(),
    {ok,H} = inet:gethostbyname(Hname),
    get_domain(H#hostent.h_name).

get_domain([$.|T]) -> T;
get_domain([H|T])  -> get_domain(T);
get_domain([])     -> throw({error,domain}).

%% ------------
%% Log handling

log_error({unknown_option,X}) ->
    error_msg("epop_smtp_client: Unknown option ~p , aborting...~n",[X]);
log_error({connect_failed,Host}) ->
    error_msg("epop_smtp_client: Couldn't connect to host ~s , aborting...~n",
	      [Host]);
log_error(from_field) ->
    error_msg("epop_smtp_client: Couldn't extract a from field !! ~n",[]);
log_error(abort) ->
    error_msg("epop_smtp_client: Aborting... Connection closed !! ~n",[]);
log_error(domain) ->
    error_msg("epop_smtp_client: Couldn't get domain name~n",[]);
log_error({crashed,X}) ->
    error_msg("epop_smtp_client: <INTERNAL ERROR> ,reason was: ~p , "
	      "aborting...~n",[X]).

log_result({finished,User,Host}) ->
    info_msg("epop_smtp_client: Mail delivered to: ~s at: ~s~n",[User,Host]);
log_result(_) -> ok.
