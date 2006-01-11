%%%------------------------------------------------------------------------
%%% File: $Id$
%%%------------------------------------------------------------------------
%%% @doc This module implements a guard process for custom event handlers
%%% @author Serge Aleynikov <serge@hq.idt.net>
%%% @version $Rev: 318 $
%%%          $LastChangedDate: 2005-12-21 14:22:57 -0500 (Wed, 21 Dec 2005) $
%%% @end
%%%------------------------------------------------------------------------
-module(lama_guard).
-author('serge@corp.idt.net').
-id("$Id$").

%%%
%%% A guard process that facilitates re-addition of alarm and log handlers
%%% in case if they crash
%%%

-export([start_link/3,
         init/4,
         system_continue/3,
         system_terminate/4,
         system_code_change/4]).

%% @spec start_link(GuardName::atom(), HandlerModule::atom(), Options::list()) ->
%%         {ok, Pid} | {error, {already_started, Pid}} | {error, Reason}
%% @doc Use this function to link this process to a supervisor.  Note that it uses
%% proc_lib so that this process would comply with OTP release management principles
%% @end
%%
start_link(GuardName, HandlerModule, Options) ->
    Self = self(),
    proc_lib:start_link(?MODULE, init, [GuardName, HandlerModule, Options, Self], infinity).

init(GuardName, HandlerModule, Options, Parent) ->
    case catch erlang:register(GuardName, self()) of
    true ->
        case catch HandlerModule:start_link(Options) of
        ok ->
            proc_lib:init_ack(Parent, {ok, self()}),
            loop(Parent, GuardName);
        already_started ->
            proc_lib:init_ack(Parent, {error, {already_started, HandlerModule}});
        Error ->
            proc_lib:init_ack(Parent, {error, Error})
        end;
    _Error ->
        Pid = whereis(GuardName),
        proc_lib:init_ack(Parent, {error, {already_started, Pid}})
    end.

loop(Parent, GuardName) ->
    receive
    %% gen_event sends this message if a handler was added using
    %% gen_event:add_sup_handler/3 function
    {gen_event_EXIT, Handler, Reason}
        when Handler == lama_alarm_h;
             Handler == lama_syslog_h ->
        io:format("~w: ~p detected handler ~p shutdown:~n~p~n",
                  [?MODULE, GuardName, Handler, Reason]),
        exit({handler_died, Handler, Reason});
    %% This one is for testing purposes
    stop ->
        io:format("~w: ~p received stop request~n",
                  [?MODULE, GuardName]),
        exit(normal);
    %% Handle OTP system messages
    {system, From, Msg} ->
        sys:handle_system_msg(Msg, From, Parent, ?MODULE, [], GuardName);
    {get_modules, From} ->
        From ! {modules, [?MODULE]};
    %% This should never happen
    Other ->
        io:format("~w: ~p received unknown message:~n~p~n",
                  [?MODULE, GuardName, Other]),
        loop(Parent, GuardName)
    end.

%% Callbacks of the sys module to process system messages.
%%
system_continue(Parent, _Debug, GuardName) ->
    loop(Parent, GuardName).

system_terminate(_Reason, Parent, _Debug, GuardName) ->
    loop(Parent, GuardName).

system_code_change(GuardName, _Module, _OldVsn, _Extra) ->
    {ok, GuardName}.
