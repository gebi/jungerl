%%%-------------------------------------------------------------------
%%% File    : proc_reg.erl
%%% Author  : Ulf Wiger <etxuwig@wsb221>
%%% Description : 
%%%
%%% Created :  2 Jul 2004 by Ulf Wiger <etxuwig@wsb221>
%%%-------------------------------------------------------------------
-module(proc_reg).
-behaviour(gen_server).

-export([reg/2,
	 unreg/1,
	 where/1,
	 send/2]).


-export([start_link/0]).
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).


reg(Id, Pid) when pid(Pid), node(Pid) == node() ->
    case do_reg(Id, Pid) of
	false ->
	    case where(Id) of
		undefined ->
		    gen_server:call(proc_reg, {check_id, Id}),
		    case do_reg(Id, Pid) of
			false ->
			    exit(badarg);
			true ->
			    true
		    end;
		OtherPid when pid(OtherPid) ->
		    exit(badarg)
	    end;
	true ->
	    true
    end;
reg(_Id, _RemoteOrBadPid) ->
    exit(badarg).


do_reg(Id, Pid) ->
    Now = erlang:now(),
    RegEntry = {Id, Pid, Now},
    case ets:insert_new(proc_reg, RegEntry) of
	false ->
	    false;
	true ->
	    gen_server:cast(proc_reg, {new_reg, Id, Pid, Now}),
	    true
    end.


unreg(Id) ->
    case ets:lookup(proc_reg, Id) of
	[] ->
	    exit(badarg);
	[{_, Pid, Check} = Obj] ->
	    ets:delete_object(proc_reg, Obj),  % Safe because of Check
	    gen_server:cast(proc_reg, {unreg, Id, Pid, Check}),
	    true
    end.


where(Id) ->
    case ets:lookup(proc_reg, Id) of
	[{_, Pid, _}] ->
	    case erlang:is_process_alive(Pid) of
		true ->
		    Pid;
		false ->
		    %% This could happen for two reasons:
		    %% - race condition; pid just died, but proc_reg has not yet
		    %%   been notified. This is not necessarily an uncommon case, esp.
		    %%   if we have code like:
		    %%      exit(Pid, kill), proc_reg:whereis(foo).
		    %% - A bug in this module, causing an inconsistency.
		    %% We will not write code to address the second possiblity.
		    undefined
	    end;
	[] ->
	    undefined
    end.


send(Id, Msg) ->
    case where(Id) of
	Pid when pid(Pid) ->
	    Pid ! Msg;
	undefined ->
	    exit(badarg)
    end.



%%%-------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, proc_reg}, ?MODULE, [], []).

init([]) ->
    process_flag(priority, high),
    proc_reg_tabs:attach(),
    ok = set_monitors(),
    {ok, []}.


handle_call({check_id, Id}, _From, S) ->
    case ets:lookup(proc_reg, Id) of
	[] ->
	    ok;
	[{_Id, Pid, _Now}] ->
	    case erlang:is_process_alive(Pid) of
		true ->
		    ok;
		false ->
		    process_is_down(Pid)
	    end
    end,
    {reply, ok, S};
handle_call(_Req, _From, S) ->
    {stop, unknown_call, S}.

handle_cast({new_reg, Id, Pid, Check}, S) ->
    MRef = erlang:monitor(process, Pid),
    ets:insert(proc_reg_rev, {{Pid, Id}, MRef, Check}),
    {noreply, S};
handle_cast({unreg, Id, Pid, Check}, S) ->
    case ets:lookup(proc_reg_rev, {Pid, Id}) of
	[{_, MRef, Check}] ->    % Here, we use Check
	    erlang:demonitor(MRef),
	    ets:delete(proc_reg_rev, {Pid, Id});
	_ ->
	    skip
    end,
    {noreply, S}.

handle_info({'DOWN', _MRef, process, Pid, _Why}, S) ->
    process_is_down(Pid),
    {noreply, S}.

process_is_down(Pid) ->
    Pattern = {{Pid,'_'},'_','_'}, 
    Regs = ets:match_object(proc_reg_rev, Pattern),
    lists:foreach(
      fun({{_,Id},_,_}) ->
	      ets:delete(proc_reg, Id)
      end, Regs),
    ets:match_delete(proc_reg_rev, Pattern).

terminate(_Reason, _S) ->
    ok.

code_change(_FromVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------

set_monitors() ->
    io:format("set_monitors...~n", []),
    ets:delete_all_objects(proc_reg_rev),
    ets:foldl(fun({Id, Pid, Check}, Acc) ->
		      MRef = erlang:monitor(process, Pid),
		      ets:insert(proc_reg_rev, {{Pid, Id}, MRef, Check}),
		      Acc
	      end, ok, proc_reg).

