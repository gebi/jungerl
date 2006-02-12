%%%-------------------------------------------------------------------
%%% File        : start_swarm.erl
%%% Author      : Gordon Guthrie <gordonguthrie@backawinner.gg>
%%% Description : 
%%%
%%% Created     : 24 Jul 2005 by Gordon Guthrie <gordonguthrie@backawinner.gg>
%%%-------------------------------------------------------------------
-module(start_app).

-export([start/0, start_kill_canary/0,
	 stop/0, remote_stop/1]).

%%%-------------------------------------------------------------------
%%% Exported Functions
%%%-------------------------------------------------------------------

start() ->
    [application:start(myapp)].

start_kill_canary()->
	[start()), 
	internal_wait(10000000), % I know - cheap'n'nasty
	kill_canary()].

stop() ->
    application:stop(myapp),
    halt().

%% Call this from another node to stop the app
%% this code nicked wholesale from wiki.erl in the Jungerl
remote_stop([Node]) ->
    {ok, HostName} = inet:gethostname(),
    FQNode=list_to_atom(lists:concat([Node, "@", HostName])),
    io:format("Stop:~p~n",[FQNode]),
    case net:ping(FQNode) of
	pong -> ok;
	pang ->
	    io:format("There is no node with this name~n")
    end,
    rpc:cast(FQNode, start_swarm, stop, []),
    init:stop().

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
kill_canary()->
    %% The canary is file whose presence marks state
    %% delete it to signal that we are finished loading up the application
    os:cmd("rm /tmp/erlang_automated_build.canary").

internal_wait(0) ->
    ok;
internal_wait(N) ->
    internal_wait(N-1).
