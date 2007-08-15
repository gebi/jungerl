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
%%%
%%%           It now works with OTP R10B-9 without the need to patch
%%%           disk_log.erl.  However, disk_log in OTP uses the process
%%%           dictionary, which means that only one disk_log_h can be
%%%           installed in a single gen_event process.
%%%
%%% Created :  1 Dec 2000 by Martin Bjorklund <mbj@bluetail.com>
%%% Modified:  4 Jan 2006 by Martin Bjorklund <mbj@tail-f.com>
%%%              o  Added option {force_size, true} (which really should be
%%%                 in disk_log instead)
%%%              o  Added function change_size/3
%%%----------------------------------------------------------------------
-module(disk_log_h).
-author('mbj@bluetail.com').

-behaviour(gen_event).

%% External exports
-export([init/2, info/2, change_size/3]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2]).

-record(state, {cnt, func}).

-include_lib("kernel/src/disk_log.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%%-----------------------------------------------------------------
%% This module is intended to be used as a gen_event handler. Instead
%% of duplicating the functions to gen_event (add_handler etc), it
%% described here how to use these function with this module.
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

change_size(EventMgr, Handler, NewSize) ->
    gen_event:call(EventMgr, Handler, {change_size, NewSize}).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, S}          |
%%          Other
%%----------------------------------------------------------------------
init([Func, Opts]) ->
    Opts1 = lists:keydelete(force_size, 1, Opts),
    case disk_log:ll_open(Opts1) of
	{ok, _, Log, Cnt} ->
	    put(log, Log),
	    {ok, #state{cnt = Cnt, func = Func}};
	{error, {size_mismatch, _, NewSize}} = Error ->
	    case lists:keysearch(force_size, 1, Opts) of
		{value, {force_size, true}} ->
		    %% open w/o size
		    Opts2 = lists:keydelete(size, 1, Opts1),
		    case disk_log:ll_open(Opts2) of
			{ok, _, Log, Cnt} ->
			    %% we should really call check_size as well...
			    case catch do_change_size(Log, NewSize) of
				ok ->
				    {ok, #state{cnt = Cnt, func = Func}};
				Else ->
				    {error, Else}
			    end;
			NError ->
			    NError
		    end;
		_ ->
		    Error
	    end;
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
	    case disk_log:do_log(get(log), [Bin]) of
		N when integer(N) ->
		    disk_log:do_sync(get(log)),
		    {ok, S#state{cnt = S#state.cnt+N}};
		{error, {error, {full, _Name}}, N} ->
		    disk_log:do_sync(get(log)),
		    {ok, S#state{cnt = S#state.cnt+N}};
		{error, Error, N} ->
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
    Reply = disk_log:do_info(get(log), S#state.cnt),
    {ok, Reply, S};
handle_call({change_size, NewSize}, S) ->
    {ok, catch do_change_size(get(log), NewSize), S}.

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
    disk_log:ll_close(get(log)).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% hack so that disk_log doesn't have to be patched - code copied
%% from disk_log:do_change_size/2
do_change_size(L, NewSize) ->
    #log{extra = Extra, version = Version} = L,
    {ok, Handle} = disk_log_1:change_size_wrap(Extra, NewSize, Version),
    erase(is_full),
    put(log, L#log{extra = Handle}),
    ok.
