%% Copyright (C) 2003 Joakim Grebenö <jocke@gleipnir.com>.
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met: 
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer. 
%% 2. Redistributions in binary form must reproduce the above
%%    copyright notice, this list of conditions and the following
%%    disclaimer in the documentation and/or other materials provided
%%    with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
%% OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
%% GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(tcp_serv).
-author('jocke@gleipnir.com').
-export([start_link/1, start_link/2]).
-export([init/2, start_session/3]).

-include("log.hrl").

-record(state, {
	  %% int()
	  max_sessions,
	  %% {M, F, A}
	  %% M = F = atom()
	  %% A = [term()]
	  session_handler,
	  %% [pid()]
	  session_list,
	  %% socket()
	  listen_socket
	 }).

%% Exported: start_link/{1,2}

start_link(Args) -> start_link(Args, infinity).
    
start_link(Args, Timeout) ->
    Pid = spawn_link(?MODULE, init, [self(), Args]),
    receive
	{Pid, started} -> {ok, Pid};
	{Pid, Reason} -> Reason
    after Timeout -> {error, timeout}
    end.

%% Exported: init/2 (internal)

init(Parent, [Port, MaxSessions, OptionList, SessionHandler]) ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, OptionList) of
	{ok, ListenSocket} ->
	    self() ! start_session,
	    Parent ! {self(), started},
	    loop(#state{max_sessions = MaxSessions,
			session_handler = SessionHandler,
			session_list = [],
			listen_socket = ListenSocket});
	Result ->
	    Parent ! {self(), {not_started, Result}}
    end.

loop(#state{session_list = SessionList,
	    listen_socket = ListenSocket} = State) ->
    receive
	stop -> gen_tcp:close(ListenSocket);
	start_session when length(SessionList) > State#state.max_sessions ->
	    timer:sleep(5000),
	    self() ! start_session,
	    loop(State);
	start_session ->
	    Args = [self(), State#state.session_handler, ListenSocket],
	    Pid = spawn_link(?MODULE, start_session, Args),
	    loop(State#state{session_list = [Pid|SessionList]});
	{'EXIT', Pid, Reason} ->
	    case lists:member(Pid, SessionList) of
		true ->
		    PurgedSessionList = lists:delete(Pid, SessionList),
		    loop(State#state{session_list = PurgedSessionList});
		false ->
		    gen_tcp:close(ListenSocket),
		    exit(Reason)
	    end;
	UnknownMessage ->
	    ?ERROR_LOG({unknown_message, UnknownMessage}),
	    loop(State)
    end.

%% Exported: start_seesion/3

start_session(Parent, {M, F, A}, ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
	{ok, Socket} ->
	    Parent ! start_session,
	    case apply(M, F, [Socket|A]) of
		ok -> gen_tcp:close(Socket);
		{error, closed} -> ok;
		{error, Reason} ->
		    ?ERROR_LOG({M, F, Reason}),
		    gen_tcp:close(Socket)
	    end;
	{error, Reason} ->
	    Parent ! start_session,
	    timer:sleep(5000)
    end.
