%%%----------------------------------------------------------------------
%%% File    : disk_log_h.erl
%%% Author  : Martin Bjorklund <mbj@bluetail.com>
%%% Purpose : gen_event handler for disk_log.  The process which owns
%%%           the log is the gen_event process, thus no extra process
%%%           communcation overhead is involved.
%%%           The alternative is to let the gen_event process send the
%%%           message to a disk_log process, which then sends it to
%%%           disk.  In this case, the disk_log process is avoided.
%%%
%%%           This module is intended to replace log_mf_h.erl.
%%% Created :  1 Dec 2000 by Martin Bjorklund <mbj@bluetail.com>
%%%----------------------------------------------------------------------
-module(disk_log_h).
-author('mbj@bluetail.com').

-behaviour(gen_event).

%% External exports
-export([init/2, info/2]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2]).

-record(state, {log, cnt, func}).

-include_lib("kernel/src/disk_log.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%%-----------------------------------------------------------------
%% This module is intended to be used as a gen_event handler. Instead
%% of duplicating the functions to gen_event (add_handler etc), it
%% described here hoe to use these function with this module.
%%
%% The init function expects a list [Func, Opts], where:
%%        Func = fun(Event) -> false | binary() | [binary()]
%%        Opts = <as disk_log:open>
%% To add a hander to a gen_event process, call e.g.:
%%   gen_event:add_handler(EventMgr, disk_log_h, [Func, Opts])
%%
%% No terminate arguments are needed.
%%
%% Here's a minimal but working example:
%%
%%   tr_event(Event) ->
%%       list_to_binary(io_lib:format("tr_event: ~p\n", [Event])).
%%
%%   start() ->
%%     Args = disk_log_h:init({?MODULE, tr_event}, [{name, tst},
%%                                                  {format, external},
%%                                                  {file, "/tmp/tst.log"}]),
%%     gen_event:add_handler(error_logger, {disk_log_h, tst}, Args).
%%
%%   stop ->
%%     gen_event:delete_handler(error, logger, {disk_log_h, tst}).
%%
%%-----------------------------------------------------------------
init(Func, DiskLogOpts) ->
    [Func, DiskLogOpts].


info(EventMgr, Handler) ->
    gen_event:call(EventMgr, Handler, info).
    

%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, S}          |
%%          Other
%%----------------------------------------------------------------------
init([Func, Opts]) ->
    case disk_log:ll_open(Opts) of
	{ok, _, Log, Cnt} ->
	    {ok, #state{log = Log, cnt = Cnt, func = Func}};
	Error ->
	    Error
    end.

%%----------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, S}                                |
%%          {swap_handler, Args1, S1, Mod2, Args2} |
%%          remove_handler                              
%%----------------------------------------------------------------------
handle_event(Event, S) ->
    case (S#state.func)(Event) of
	false ->
	    {ok, S};
	Bin ->
	    case disk_log:do_log(S#state.log, [Bin]) of
		{N, L1} when integer(N) ->
		    {_, L2} = disk_log:do_sync(L1),
		    {ok, S#state{cnt = S#state.cnt+N, log = L2}};
		{error, {error, {full, _Name}}, L1, N} ->
		    {_, L2} = disk_log:do_sync(L1),
		    {ok, S#state{cnt = S#state.cnt+N, log = L2}};
		{error, Error, L1, N} ->
		    Error;
		Error ->
		    Error
	    end
    end.

%%----------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, S}                                |
%%          {swap_handler, Reply, Args1, S1, Mod2, Args2} |
%%          {remove_handler, Reply}                            
%%----------------------------------------------------------------------
handle_call(info, S) ->
    Reply = disk_log:do_info(S#state.log, S#state.cnt),
    {ok, Reply, S}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, S}                                |
%%          {swap_handler, Args1, S1, Mod2, Args2} |
%%          remove_handler                              
%%----------------------------------------------------------------------
handle_info({emulator, GL, Chars}, S) ->
    %% this is very unfortunate...
    handle_event({emulator, GL, Chars}, S);
handle_info(Info, S) ->
    {ok, S}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%%----------------------------------------------------------------------
terminate(Arg, S) ->
    disk_log:ll_close(S#state.log).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
