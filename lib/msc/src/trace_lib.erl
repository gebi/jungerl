%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (C) 2003 Thomas Lindgren <thomasl_erlang@yahoo.com>.
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
%%
%%		      LIBRARY FOR ERLANG TRACING
%%
%% Author: Thomas Lindgren (021211-)
%%
%% A library for simple operations on erlang:trace streams.
%%
%% Basic structure:
%% - set up tracing conditions
%% - start a 'trace collector' process at max priority
%% - run the benchmark (which sends events to the collector)
%% - analyze the trace
%%
%% API
%%   fold_trace(Prof_fun, Trace_setup_fun, Admin_fun, Trace_fun, State)
%%     where
%%      Prof_fun() -> * 
%%        is the function you want to profile
%%      Trace_setup_fun() -> ok 
%%        runs before profiling starts (e.g, set trace
%%        conditions here
%%      Admin_fun(Msg, St) -> {stop, NewSt} | {continue, NewSt}
%%      Trace_fun(Msg, St) -> {stop, NewSt} | {continue, NewSt}
%%        These two take a message (trace or admin) and a state, and
%%        produce a new state + an indicator whether tracing is done
%%      St is an arbitrary state, like in lists:fold*
%%
%% See example/1, calls_to/2 for example usage.

-module(trace_lib).
-export([fold_trace/5, fold_trace/6, fold_trace/7]).
%%-export([
-export([example/1, calls_to/2]).
-export([trace_mark/1]).

%% Trace a running function
%%
%% PreFun() -> ok
%% PostFun() -> ok
%%   set up tracing the way you want it (PreFun), and stop tracing (PostFun)
%% AdminFun(Msg, St) -> {stop, St} | {continue, St}
%%   handle non-trace messages
%% TraceFun(Msg, St) -> NewSt
%%   handle trace messages
%% ProfFun() -> ok
%%   function to run for profiling
%% St: <state>, used by TraceFun and AdminFun
%% Timeout: timeout value, ms
%%
%% Notes:
%% - we can just trace the system for a given time
%%   ProfFun = fun() -> sleep(Time) end
%%   will sleep, but we also need to stop the trace process (or dump it)
%% - TraceAdminFun should have some support
%% - should we pick apart the messages a bit more? less matching
%% - should we separate timestamp traces from non-timestamped ones?
%% - R9 compatible?

fold_trace(ProfFun, PreFun, AdminFun, TraceFun, St) ->
    fold_trace(ProfFun, PreFun, AdminFun, TraceFun, fun id/0, St).

id() ->
    ok.

fold_trace(ProfFun, PreFun, AdminFun, TraceFun, PostFun, St) ->
    fold_trace(ProfFun, PreFun, AdminFun, TraceFun, PostFun, St, infinity).
    
fold_trace(ProfFun, PreFun, AdminFun, TraceFun, PostFun, St, Timeout) ->
    %% set tracing properties
    This = self(),
    Ref = erlang:make_ref(),
    Tracer =
	spawn(
	  fun() ->
		  %% the process calling this will receive trace msgs
		  PreFun(),
		  This ! {continue, Ref},
		  tracer(AdminFun, TraceFun, St, This, Ref),
		  PostFun()
	  end),
    receive
	{continue, Ref} ->
	    ok
    end,
    spawn(
      fun() ->
	      Res = (catch ProfFun()),
	      Tracer ! stop,
	      %% io:format("traced code returns ~p~n", [Res]),
	      This ! {function_result, Ref, Res}
      end),
    receive
	%% {function_result, Ref, Res} -> ... (uninteresting) ...
	{trace_result, Ref, NewSt} ->
	    NewSt
    after Timeout ->
	    {error, timeout}
    end.

%% Ret_proc = process awaiting reply
%% Ref = unique message ref

tracer(AdminFun, Fun, St, Ret_proc, Ref) ->
    process_flag(priority, max),
    process_flag(trap_exit, true),
    NewSt = trace_loop(AdminFun, Fun, St),
    %% io:format("tracer returns ~p~n", [NewSt]),
    Ret_proc ! {trace_result, Ref, NewSt}.

trace_loop(AdminFun, Fun, St) ->
    receive
	{trace, Pid, 'receive', Msg0}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));
	{trace, Pid, send_to_non_existing_process, Msg0, To}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));
	{trace, Pid, call, MFA}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));
	{trace_ts, Pid, call, MFA, Ts}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));
	{trace, Pid, return_to, MFA}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));
	{trace, Pid, return_from, MFA, RetVal}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));
	{trace, Pid, spawn, NewPid}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));
	{trace, Pid, exit, Rsn}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));
	{trace, Pid, link, OtherPid}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));
	{trace, Pid, unlink, OtherPid}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));
	{trace, Pid, getting_linked, LinkingPid}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));	    
	{trace, Pid, in, MFA}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));	    
	{trace, Pid, out, MFA}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));	    
	{trace, Pid, gc_start, Info}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));	    
	{trace, Pid, gc_end, Info}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));
	{trace_ts, Pid, 'receive', Msg0, Ts}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));
	{trace_ts, Pid, send_to_non_existing_process, Msg0, To, Ts}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));
	{trace_ts, Pid, call, MFA, Ts}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));
	{trace_ts, Pid, return_to, MFA, Ts}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));
	{trace_ts, Pid, return_from, MFA, RetVal, Ts}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));
	{trace_ts, Pid, spawn, NewPid, Ts}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));
	{trace_ts, Pid, exit, Rsn, Ts}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));
	{trace_ts, Pid, link, OtherPid, Ts}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));
	{trace_ts, Pid, unlink, OtherPid, Ts}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));
	{trace_ts, Pid, getting_linked, LinkingPid, Ts}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));	    
	{trace_ts, Pid, in, MFA, Ts}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));	    
	{trace_ts, Pid, out, MFA, Ts}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));	    
	{trace_ts, Pid, gc_start, Info, Ts}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));	    
	{trace_ts, Pid, gc_end, Info, Ts}=Msg ->
	    trace_loop(AdminFun, Fun, Fun(Msg, St));
	stop ->
	    St;
	NonTrace ->
	    %% unhandled trace messages also end up here!
	    case AdminFun(NonTrace, St) of
		{stop, NewSt} ->
		    NewSt;
		{continue, NewSt} ->
		    trace_loop(AdminFun, Fun, NewSt)
	    end
    end.

	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Count the number of GCs started while running Prof_fun

example(Prof_fun) ->
    fold_trace(
      Prof_fun,
      fun() ->
	      erlang:trace(all, true, [garbage_collection, timestamp])
      end,
      fun(Msg, St) ->
	      io:format("received ~p~n", [Msg]),
	      {continue, St}
      end,
      fun({trace_ts, PID, gc_start, Info, Ts}, N) ->
	      N+1;
	 ({trace, PID, gc_start, Info}, N) ->
	      N+1;
	 (_, N) ->
	      N
      end,
      fun() ->
	      erlang:trace(all, false, [])
      end,
      0).

%% get the calls to (single) MFA

calls_to(MFA, Prof_fun) ->
    fold_trace(
      Prof_fun,
      fun() ->
	      erlang:trace(all, true, [call]),
	      erlang:trace_pattern(MFA, true, [local])
      end,
      fun(AdmMsg, St) ->
	      io:format("received ~p~n", [AdmMsg]),
	      {continue, St}
      end,
      fun({trace, PID, call, MFA}, St) ->
	      [MFA|St];
	 (Other, St) ->
	      io:format("ignored other: ~p~n", [Other]),
	      St
      end,
      fun() ->
	      erlang:trace(all, false, []),
	      erlang:trace_pattern(MFA, false, [local])
      end,
      []
     ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% This is just a suitable function for tracing.
%%
%% - even better: we could conditionally call a function f2 inside trace_mark,
%%   which means we can trace f2; the trace items only appear when the
%%   condition triggers
%% - include ?LINE etc in the Item

trace_mark(Item) ->
    ok.

