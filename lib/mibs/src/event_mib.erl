%%%-------------------------------------------------------------------
%%% File    : event_mib.erl
%%% Created : 12 May 2004 by  <mbj@bluetail.com>
%%%
%%% I'm using edoc with two changes: No html tags and no @hidden.
%%% It clutters down the code.  This makes it work fine with fdoc/distel :)
%%%
%%% @author Martin Björklund <mbj@bluetail.com>
%%% @reference  <a href="http://www.ietf.org/rfc/rfc2981.txt">rfc2981</a>
%%% @version 1.0
%%% @doc Implements DISMAN-EVENT-MIB (rfc2981).
%%%
%%% The configuration data is persistently stored in mnesia.
%%% The caller can choose to use local_content to have all
%%% configuration per node, instead of globally in the cluster.
%%% Statistics is kept per host.  Events are triggered per
%%% host, since they very well can sample host specific data.
%%% If they sample cluster global data, all hosts will trigger
%%% the same event.
%%%
%%% The implementation relies on mnesia's snmp capability.
%%%
%%% There is one process which implements the triggers, and sends
%%% notifications as necessary.  This process subscribes to mnesia
%%% events for the mteTriggerTable in order to create/delete triggers.
%%% This means that regardless how these tables change (through SNMP
%%% or any other way), the server will be notified.  The
%%% instrumentation functions rely on this - they just write data to
%%% mnesia, and the server is notified thourgh the subscription
%%% mechanism.  NOTE: this means that an application that wants to
%%% modify the mnesia tables directly, must use mnesia transactions,
%%% since dirty writes to mnesia does not generate mnesia_events.
%%%
%%% Local sampling only is implemented, thus mteSetTable is
%%% not implemented,  which is fine for an agent.
%%%
%%% If a user creates entries directly in mnesia (i.e. w/o going
%%% through SNMP or functions in this module), the user MUST make sure
%%% rows are created consistently (e.g. that a row in
%%% mteTriggerExistenceTable is created if the 'existence' bit is set
%%% in mteTriggerTest).
%%%
%%% The user must create the mnesia tables once by calling
%%% {@link create_tables/2}.  The loading and unloading of DISMAN-EVENT-MIB
%%% is handled by the server.
%%% 
%%% The implementation's compliance to the mib is formally defined
%%% in the ERL-DISMAN-EVENT-CAP.txt AGENT-CAPABILITIES template file.
%%% This template can be modified and distributed to end users.  If used,
%%% the user must add the capability to the sysORTable by calling
%%% snmpa:add_agent_caps/2.
%%%
%%% @end
%%%-------------------------------------------------------------------

%% TODO
%%   o  handle overloaded system, i.e. drop timeout events if overloaded
%%   o  enforce security, i.e. make sure a user cannot sample objects
%%      he doesn't have access to.  same check should be done to the
%%      objects in the notifications.  this may require changes in the
%%      OTP snmp agent.

-module(event_mib).
-behaviour(gen_server).

%% external exports
-export([start_link/0, start_link/1, start_link/2]).

-export([db_tables/0, create_tables/1, create_tables/3]).

-export([mte_variable/2, mte_variable/3,
	 mte_stat_variables/2,
	 mteTriggerTable/3, mteEventTable/3]).
-export([table_func/4,
	 unimplemented_table/3,
	 unimplemented_variable/1, unimplemented_variable/2]).
-export([op2str/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

%% include files
-include("event_mib.hrl").
-include("../include/DISMAN-EVENT-MIB.hrl").
-include_lib("snmp/include/SNMPv2-TC.hrl").

%% records
-record(state, {triggers = []}).

%% constants
-define(SERVER, ?MODULE).


-ifdef(debug).
-define(dbg(X,Y), error_logger:info_msg("*dbg ~p:~p: " X,
                                        [?MODULE, ?LINE | Y])).
-compile(export_all).
-else.
-define(dbg(X,Y), ok).
-endif.

%%====================================================================
%% External functions
%%====================================================================
%% @spec start_link() -> {ok, pid()}
%% @equiv start_link(mibs, snmp_master_agent)
start_link() ->
    start_link(mibs, snmp_master_agent).
%% @spec start_link(App) -> {ok, pid()}
%% @equiv start_link(App, snmp_master_agent)
start_link(App) ->
    start_link(App, snmp_master_agent).
%% @spec start_link(App, Agent) -> {ok, pid()}
%%       App = atom()
%%       Agent = pid() | atom()
%% @doc Starts the event_mib server and loads the mib in Agent.
%% The mib is unloaded when the server terminates.  Notifications
%% which are generated when triggers fire are sent to Agent.
%% App is the name of the application which contains event_mib.
start_link(App, Agent) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [App, Agent], []).

%% @spec db_tables() -> [TableName]
%%       TableName = atom()
%% @doc Returns a list of all mnesia tables used by this module.
db_tables() ->
    [mteTriggerTable, mteTriggerDeltaTable, mteTriggerExistenceTable,
     mteTriggerBooleanTable, mteTriggerThresholdTable, mteObjectsTable,
     mteEventTable, mteEventNotificationTable, mteVariables].

-define(table(TAB, SNMP), {TAB, record_info(fields, TAB), SNMP}).

%% @spec create_tables(Nodes) -> ok | {error, string()}
%% @equiv create_tables(event_mib:db_tables(), Nodes, [])
create_tables(Nodes) ->
    create_tables(db_tables(), Nodes, [{disc_copies, Nodes}]).
%% @spec create_tables(Tabs, Nodes, MnesiaOptions::list()) ->
%%    ok | {error, string()}
%%       Tabs = list()
%%       String = list()
create_tables(Tabs, Nodes, MnesiaOpts) ->
    Specs = [?table(mteTriggerTable, {string, fix_string}),
	     ?table(mteTriggerDeltaTable, {string, fix_string}),
	     ?table(mteTriggerExistenceTable, {string, fix_string}),
	     ?table(mteTriggerBooleanTable, {string, fix_string}),
	     ?table(mteTriggerThresholdTable, {string, fix_string}),
	     ?table(mteObjectsTable, {string, string, integer}),
	     ?table(mteEventTable, {string, fix_string}),
	     ?table(mteEventNotificationTable, {string, fix_string}),
	     {mteVariables, record_info(fields, mteVariables), []}],
    DoTabs = [Spec || Spec <- Specs, lists:member(element(1, Spec), Tabs)],
    create_tables2(Nodes, DoTabs, MnesiaOpts).
    
create_tables2(Nodes, [{Name, RecordInfo, SNMP} | Rest], Opts) ->
    SnmpS = if SNMP == [] -> [];
	       true -> [{snmp, [{key, SNMP}]}]
	    end,
    case mnesia:create_table(Name, [{attributes, RecordInfo}]++SnmpS++Opts) of
	{atomic, ok} ->
	    create_tables2(Nodes, Rest, Opts);
	{aborted, Reason} ->
	    {error, Reason}
    end;
create_tables2(_Nodes, [], _Opts) ->
    F = fun() ->
		create_variables_t([{mteResourceSampleMinimum, 1},
				    {mteResourceSampleInstanceMaximum, 0}])
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    {error, Reason}
    end.

create_variables_t([{Name, Val} | T]) ->
    mnesia:write(#mteVariables{name = Name, val = Val}),
    create_variables_t(T);
create_variables_t([]) ->
    ok.


%%====================================================================
%% Instrumentation functions
%%====================================================================
mte_variable(get, Var) ->
    [Obj] = mnesia:dirty_read({mteVariables, Var}),
    {value, Obj#mteVariables.val}.
mte_variable(set, Name, Val) ->
    mnesia_write(#mteVariables{name = Name, val = Val}),
    noError;
mte_variable(undo, _, _) ->
    undoFailed.

mte_stat_variables(get, Var) ->
    Val = gen_server:call(event_mib, {get_stat, Var}, infinity),
    {value, Val}.

unimplemented_table(get, _, _) ->
    {noValue, noSuchInstance};
unimplemented_table(get_next, _Row, Cols) ->
    lists:duplicate(length(Cols), endOfTable);
unimplemented_table(is_set_ok, _, Cols) ->
    lists:duplicate(length(Cols), noCreation).

unimplemented_variable(get) ->
    {noValue, noSuchInstance}.
unimplemented_variable(is_set_ok, _) ->
    noCreation.

table_func(Op, RowIndex, Cols, Tab) ->
    snmp_generic:table_func(Op, RowIndex, Cols, {Tab, mnesia}).

%% This is an enhanced version of snmp_generic_mnesia:table_func(set,...)
%% This version takes an extra fun() which is executed within the transaction.
table_func_set(RowIndex, Cols, Tab, F) ->
    case mnesia:transaction(
	   fun() ->
		   F(),
		   snmp_generic:table_set_row(
		     {Tab, mnesia}, nofunc,
		     {snmp_generic_mnesia, table_try_make_consistent},
		     RowIndex, Cols)
	   end) of
	{atomic, Value} ->
	    Value;
	{aborted, Reason} ->
	    snmpa_error:user_err("set transaction aborted. Tab ~w, RowIndex"
				 " ~w, Cols ~w. Reason ~w",
				 [Tab, RowIndex, Cols, Reason]),
	    {Col, _Val} = hd(Cols),
	    {commitFailed, Col}
    end.


mteTriggerTable(is_set_ok, RowIndex, Cols) ->
    case chk_trigger_cols(Cols) of
	ok ->
	    table_func(is_set_ok, RowIndex, Cols, mteTriggerTable);
	Else ->
	    Else
    end;
mteTriggerTable(set, RowIndex, Cols) ->
    %% If rowstatus is modified, possibly create/delete rows in the
    %% other mteTrigger* tables.
    case find_col(?mteTriggerEntryStatus, Cols, undefined) of
	?RowStatus_createAndGo ->
            %% all vals either present in Cols or default
	    Type = create,
	    Test = find_col(?mteTriggerTest, Cols, ?boolean),
	    SampleType = find_col(?mteTriggerSampleType, Cols, ?absoluteValue);
        ?RowStatus_active ->
            %% all values are either present in Cols or already in mnesia
	    [T] = mnesia:dirty_read({mteTriggerTable, RowIndex}),
	    Type = create,
	    Test = find_col(?mteTriggerTest, Cols, T#mteTriggerTable.test),
	    SampleType = find_col(?mteTriggerSampleType, Cols,
				  T#mteTriggerTable.sampleType);
	?RowStatus_destroy ->
	    Type = delete,
	    Test = 0,
	    SampleType = 0;
	 _ ->
	    Type = noop,
	    Test = 0,
	    SampleType = 0
    end,
    Key = trig_row_index2key(RowIndex),
    F =	fun() ->
		case Type of
		    create ->
			if ?bit_is_set(Test, ?existence) ->
				mnesia:write(#mteTriggerExistenceTable{
				                key = Key});
			   true ->
				ok
			end,
			if ?bit_is_set(Test, ?boolean) ->
				mnesia:write(#mteTriggerBooleanTable{
                                                key = Key});
			   true ->
				ok
			end,
			if ?bit_is_set(Test, ?threshold) ->
				mnesia:write(#mteTriggerThresholdTable{
                                                key = Key});
			   true ->
				ok
			end,
			if SampleType == ?deltaValue ->
				mnesia:write(#mteTriggerDeltaTable{
                                                key = Key});
			   true ->
				ok
			end;
		    delete ->
			mnesia:delete({mteTriggerExistenceTable, Key}),
			mnesia:delete({mteTriggerBooleanTable, Key}),
			mnesia:delete({mteTriggerThresholdTable, Key}),
			mnesia:delete({mteTriggerDeltaTable, Key});
		    noop ->
			ok
		end
	end,
    table_func_set(RowIndex, Cols, mteTriggerTable, F);
mteTriggerTable(Op, RowIndex, Cols) ->
    table_func(Op, RowIndex, Cols, mteTriggerTable).

trig_row_index2key([N | T]) ->
    lists:split(N, T).

chk_trigger_cols([{?mteTriggerTargetTag, _} | _]) ->
    %% we don't support write to this object (i.e. no remote system)
    {notWritable, ?mteTriggerTargetTag};
chk_trigger_cols([{?mteTriggerValueID, Val} | T]) ->
    %% FIXME check that the user has access to the object to sample
    chk_trigger_cols(T);
chk_trigger_cols([{?mteTriggerContextNameWildcard, ?TruthValue_true} | _]) ->
    %% we don't support context wildcarding - we don't have the list of
    %% available context
    {wrongValue, ?mteTriggerContextNameWildcard};
chk_trigger_cols([{?mteTriggerFrequency, Val} | T]) ->
    case mnesia:dirty_read({mteVariables, mteResourceSampleMinimum}) of
	[#mteVariables{val = Min}] when Val < Min ->
	    {wrongValue, ?mteTriggerFrequency};
	_ ->
	    chk_trigger_cols(T)
    end;
chk_trigger_cols([_ |T]) ->
    chk_trigger_cols(T);
chk_trigger_cols([]) ->
    ok.


mteEventTable(is_set_ok, RowIndex, Cols) ->
    case chk_event_cols(Cols) of
	ok ->
	    table_func(is_set_ok, RowIndex, Cols, mteTriggerTable);
	Else ->
	    Else
    end;
mteEventTable(set, RowIndex, Cols) ->
    %% If rowstatus is modified, possibly create/delete rows in
    %% mteEventNotificationTable
    case find_col(?mteEventEntryStatus, Cols, undefined) of
	?RowStatus_createAndGo ->
	    %% all values are either present in Cols or default
	    Type = create,
	    Actions = find_col(?mteEventActions, Cols, 0);
        ?RowStatus_active ->
	    %% all values are either present in Cols or already in mnesia
	    [E] = mnesia:dirty_read({mteEventTable, RowIndex}),
	    Type = create,
	    Actions = find_col(?mteEventActions, Cols, E#mteEventTable.actions);
	?RowStatus_destroy ->
	    Type = delete,
	    Actions = 0;
	undefined ->
	    %% modifing an existing row
	    Actions = find_col(?mteEventActions, Cols, undefined),
	    case mnesia:dirty_read({mteEventTable, RowIndex}) of
		[E] when E#mteEventTable.actions /= Actions ->
		    if ?bit_is_set(Actions, ?notification) ->
			    Type = create;
		       true ->
			    Type = delete
		    end;
		_ ->
		    Type = noop
	    end;
	 _ ->
	    Type = noop,
	    Actions = 0
    end,
    F = fun() ->
		case Type of
		    create ->
			if ?bit_is_set(Actions, ?notification) ->
				mnesia:write(#mteEventNotificationTable{
                                               key = RowIndex});
			   true ->
				ok
			end;
		    delete ->
			mnesia:delete({mteEventNotificationTable, RowIndex});
		    _ ->
			ok
		end
	end,
    table_func_set(RowIndex, Cols, mteEventTable, F);
mteEventTable(Op, RowIndex, Cols) ->
    table_func(Op, RowIndex, Cols, mteEventTable).
    
chk_event_cols([{?mteEventActions, Val} | _]) when ?bit_is_set(Val, ?set) ->
    %% we don't support remote set
    {wrongValue, ?mteEventActions};
chk_event_cols([_ | T]) ->
    chk_event_cols(T);
chk_event_cols([]) ->
    ok.

%%====================================================================
%% Server functions
%%====================================================================
init([App, Agent]) ->
    process_flag(trap_exit, true),
    put(agent, Agent),
    put(instances, 0),
    put(instancesHigh, 0),
    put(instanceLacks, 0),
    put(failures, 0),
    mnesia:subscribe({table, mteTriggerTable}),
    mnesia:subscribe({table, mteTriggerDeltaTable}),
    mnesia:subscribe({table, mteTriggerExistenceTable}),
    mnesia:subscribe({table, mteTriggerBooleanTable}),
    mnesia:subscribe({table, mteTriggerThresholdTable}),
    EventMib = filename:join(code:priv_dir(App), "DISMAN-EVENT-MIB.bin"),
    ?dbg("loading ~p\n", [EventMib]),
    snmpa:load_mibs(Agent, [EventMib]),
    {ok, #state{triggers = start_triggers()}}.

handle_call({get_stat, Var}, _From, S) ->
    {reply, get(Var), S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info({mnesia_table_event, MnesiaEvent}, S) ->
    %% any one of the trigger specification tables has changed;
    %% they all have the same key.
    case MnesiaEvent of
	{write, T, _} when element(1, T) == schema ->
	    %% why do we get this message??
	    {noreply, S};
	{write, T, _} ->
	    %% the oid, frequency, values to compare with... may have been
	    %% modified; delete and recreate the trigger
	    Key = element(2, T),
	    Triggers0 = del_trigger(Key, S#state.triggers),
	    Triggers1 = if record(T, mteTriggerTable) ->
				start_trigger(T, Triggers0);
			   true ->
				start_trigger_by_key(Key, Triggers0)
			end,
	    {noreply, S#state{triggers = Triggers1}};
	{delete, {_, Key}, _} ->
	    Triggers0 = del_trigger(Key, S#state.triggers),
	    {noreply, S#state{triggers = Triggers0}};
	{delete_object, T, _} ->
	    Key = element(2, T),
	    Triggers0 = del_trigger(Key, S#state.triggers),
	    {noreply, S#state{triggers = Triggers0}}
    end;
handle_info({trigger_timeout, Key}, S) ->
    {noreply, trigger_timeout(Key, S)};
handle_info(_Info, S) ->
    {noreply, S}.

terminate(_Reason, _S) ->
    snmpa:unload_mibs(get(agent), ["DISMAN-EVENT-MIB"]).

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%====================================================================
%% Internal functions
%%====================================================================
mnesia_write(Obj) ->
    {atomic, _} = mnesia:transaction(fun() -> mnesia:write(Obj) end).
			       
%% start all triggers at startup
start_triggers() ->
    {atomic, Trigs} =
	mnesia:transaction(
	  fun() -> mnesia:foldl(fun mk_trig_t/2, [], mteTriggerTable) end),
    %% start the timers and do init check outside the transaction
    lists:zf(fun(Trig) -> start_trigger(Trig) end, Trigs).

mk_trig_t(T,Trigs) when T#mteTriggerTable.entryStatus == ?RowStatus_active,
                        T#mteTriggerTable.enabled == ?TruthValue_true ->
    [mk_trig_t(T) | Trigs];
mk_trig_t(_T, Trigs) ->
    ?dbg("ignoring inactive/disabled trigger ~p\n", [_T#mteTriggerTable.key]),
    Trigs.


start_trigger({Key, Freq, Obj}) ->
    case check_trigger_startup(Key, Obj) of
	{true, ChkState} ->
	    {ok, TimerRef} = timer:send_interval(Freq*1000,
						 {trigger_timeout, Key}),
	    {true, {Key, TimerRef, Obj, ChkState}};
	false ->
	    false
    end.

%% start a single trigger b/c it has been created or modified
start_trigger(T, Triggers) ->
    case mk_trig(T) of
	[Trig] -> [start_trigger(Trig) | Triggers];
	[] -> Triggers
    end.

%% start a single trigger b/c the mteTrigger<X>Table has been modified
start_trigger_by_key(Key, Triggers) ->
    case mk_trig_by_key(Key) of
	[Trig] -> [start_trigger(Trig) | Triggers];
	[] -> Triggers
    end.

mk_trig(T) ->
    {atomic, Trigs} = mnesia:transaction(fun() -> mk_trig_t(T, []) end),
    Trigs.

mk_trig_by_key(Key) ->
    {atomic, Trigs} = 
	mnesia:transaction(fun() ->
				   [T] = mnesia:read({mteTriggerTable, Key}),
				   mk_trig_t(T, [])
			   end),
    Trigs.

mk_trig_t(T) ->
    Key = T#mteTriggerTable.key,
    Freq = T#mteTriggerTable.frequency,
    Obj = {T#mteTriggerTable.valueID,
	   T#mteTriggerTable.valueIDWildcard == ?TruthValue_true,
	   T#mteTriggerTable.contextName,
	   mk_sample_data_t(T),
	   mk_test_data_t(T)},
    ?dbg("create trigger key: ~p freq: ~p\nobj: ~p\n", [Key, Freq, Obj]),
    {Key, Freq, Obj}.

mk_sample_data_t(T) ->
    case T#mteTriggerTable.sampleType of
	?absoluteValue ->
	    [];
	?deltaValue ->
	    [Delta] = mnesia:read({mteTriggerDeltaTable,T#mteTriggerTable.key}),
	    if Delta#mteTriggerDeltaTable.discontinuityID==?sysUpTimeInstance ->
		    %% sysUptime changes => we must restart => no need to
		    %% check for discontinuity in sysUpTime
		    [];
	       true ->
		    Delta
	    end
    end.

mk_test_data_t(T) ->
    Key = T#mteTriggerTable.key,
    Test = T#mteTriggerTable.test,
    if ?bit_is_set(Test, ?existence) ->
	    mnesia:read({mteTriggerExistenceTable, Key});
       true -> []
    end ++
    if ?bit_is_set(Test, ?boolean) ->
	    mnesia:read({mteTriggerBooleanTable, Key});
       true -> []
    end ++
    if ?bit_is_set(Test, ?threshold) ->
	    mnesia:read({mteTriggerThresholdTable, Key});
       true -> []
    end.

del_trigger(Key, Triggers) ->
    case lists:keysearch(Key, 1, Triggers) of
	{value, {_Key, TimerRef, _Obj, ChkState}} ->
	    ?dbg("delete trigger ~p\n", [Key]),
	    put(instances, get(instances) - length(ChkState)),
	    timer:cancel(TimerRef),
	    lists:keydelete(Key, 1, Triggers);
	_ ->
	    ?dbg("could not delete trigger ~p\n", [Key]),
	    Triggers
    end.

trigger_timeout(Key, S) ->
    case lists:keysearch(Key, 1, S#state.triggers) of
	{value, {_Key, TimerRef, Obj, CheckState0}} ->
	    ?dbg("trigger ~p trigged\n", [Key]),
	    case check_trigger(Key, Obj, CheckState0) of
		CheckState0 -> % optmimization - nothing changed
		    S;
		CheckState1 ->
		    NewT = {Key, TimerRef, Obj, CheckState1},
		    Ts = lists:keyreplace(Key, 1, S#state.triggers, NewT),
		    S#state{triggers = Ts}
	    end;
	_ ->
	    ?dbg("unknown trigger ~p trigged\n", [Key]),
	    S
    end.
		    
%% Returns: ChkState.
%%   ChkState contains the old value for each instance sampled (might be several
%%   if wildcarding is used).
%% ChkState() = [{Instance, Value, PrevValue, DiscoData, Fire}]
check_trigger_startup(_Key, {OID, GetNextP, Context, SampleData, TestData}) ->
    NewValues = sample(OID, GetNextP, Context, SampleData),
    NewInstances = length(NewValues) + get(instances),
    case mnesia:dirty_read({mteVariables, mteResourceSampleInstanceMaximum}) of
	[#mteVariables{val = Max}] when Max > 0, NewInstances > Max ->
	    put(instanceLacks, get(instanceLacks) + length(NewValues)),
	    false;
	_ ->
	    High = get(instancesHigh),
	    if NewInstances > High ->
		    put(instancesHigh, NewInstances);
	       true ->
		    ok
	    end,
	    put(instances, NewInstances),
	    ?dbg("trigger ~p read at startup oid(s):~p val(s): ~p\n",
		 [_Key, OID, NewValues]),
	    {true, check_values(NewValues, [], TestData, true)}
    end.
    
%% Returns: ChkState.
check_trigger(_Key, {OID, GetNextP, Context, SampleData, TestData},OldValues) ->
    NewValues = sample(OID, GetNextP, Context, SampleData),
    ?dbg("trigger ~p read oid(s):~p val(s): ~p\n", [_Key, OID, NewValues]),
    check_values(NewValues, OldValues, TestData, false).

%% Returns: ChkState.
check_values([{Inst, Val0, Dis0} | T0],  [{Inst, Val1, Val2, Dis1, Fire1} | T1],
	     TestData, IsNew) ->
    %% same instance as before
    case discontinuity_occured(Dis0, Dis1) of
	true -> % discontinuity found, reset sampled values
	    [{Inst, Val0, '$undefined', Dis0, 0} |
	     check_values(T0, T1, TestData, IsNew)];
	false ->
	    Fire = check_test(TestData, Inst, Val0, Val1, Val2, false, Fire1),
	    [{Inst, Val0, Val1, Dis0, Fire} |
	     check_values(T0, T1, TestData, IsNew)]
    end;
check_values([{Inst0, Val0, Dis0} | T0], [{Inst1, _, _, _, _} | _] = T1,
	     TestData, IsNew) when Inst0 < Inst1 ->
    %% new instance found
    Fire = check_test(TestData, Inst0, Val0, '$undefined', '$undefined',
		      IsNew, 0),
    [{Inst0, Val0, '$undefined', Dis0, Fire} |
     check_values(T0, T1, TestData, IsNew)];
check_values(T0, [{Inst1, Val1, Val2, _Dis, Fire1} | T1], TestData, IsNew) ->
    %% old instance deleted
    check_test(TestData, Inst1, '$undefined', Val1, Val2, false, Fire1),
    check_values(T0, T1, TestData, IsNew);
check_values([{Inst0, Val0, Dis0} | T0], [], TestData, IsNew) ->
    %% new instance found
    Fire = check_test(TestData, Inst0, Val0, '$undefined', '$undefined',
		      IsNew, 0),
    [{Inst0, Val0, '$undefined', Dis0, Fire} |
     check_values(T0, [], TestData, IsNew)];
check_values([], [], _TestData, _IsNew) ->
    [].
	    
%% Ret: [{Instance, Value, {DiscoType, DiscoValue} | undefined}]
sample(OID, false, Context, SampleData) ->
    case get_values([OID], Context) of
	[Value] when SampleData == [] ->
	    [{[], Value, undefined}];
	[Value] ->
	    DiscoData = get_disco_data(SampleData, [], Context),
	    [{[], Value, DiscoData}];
	_ ->
	    []
    end;
sample(OID, true, Context, SampleData) ->
    samples(OID, OID, Context, SampleData).

samples(OID, Prefix, Context, SampleData) ->
    case get_next_values([OID], Context) of
	[{NextOID, Value}] ->
	    case prefix(Prefix, NextOID) of
		{true, Instance} when SampleData == [] ->
		    [{Instance, Value, undefined} 
		     | samples(NextOID, Prefix, Context, SampleData)];
		{true, Instance} ->
		    DiscoData = get_disco_data(SampleData, Instance, Context),
		    [{Instance, Value, DiscoData}
		     | samples(NextOID, Prefix, Context, SampleData)];
		false ->
		    []
	    end;
	_ ->
	    []
   end.

get_disco_data(Delta, Instance, Context) ->
    OID = Delta#mteTriggerDeltaTable.discontinuityID ++ Instance,
    case get_values([OID], Context) of
	[Val] ->
	    {Delta#mteTriggerDeltaTable.discontinuityIDType, Val};
	_ ->
	    %% FIXME; report failure?
	    undefined
    end.

%% This is not obvious from the rfc, and this is not how net-snmp
%% does it.  But it seems reasonable to say that a discontinuity
%% has occured if type is TimeTicks and New < Old, or for
%% TimeStamp and DateAndTime if New != Old.
discontinuity_occured(undefined, _) -> false;
discontinuity_occured(_, undefined) -> false;
discontinuity_occured({Type, NewVal}, {_, OldVal}) ->
    if Type == ?timeTicks -> % discontinuity if new < old
	    NewVal < OldVal;
       true -> % otherwise, discontinuity if new != old
	    NewVal /= OldVal
    end.
	    

-define(mask_boolean, (1 bsl 0)).
-define(mask_rising, (1 bsl 1)).
-define(mask_falling, (1 bsl 2)).
-define(mask_delta_rising, (1 bsl 1)).
-define(mask_delta_falling, (1 bsl 2)).

%% Ret: FireMask (bitmask with ?mask_* - which triggers are fired)
check_test([#mteTriggerExistenceTable{test = Test, startup = StartUp} = H | T],
	   Instance, NewValue, OldValue, OldValue2, IsNew, OldFireMask) ->
    if IsNew == true, ?bit_is_set(StartUp, ?present),
       ?bit_is_set(Test, ?present),
       NewValue /= '$undefined' ->
	    %% instance exists at activation
	    fire_existence(H, Instance);
       IsNew == true, ?bit_is_set(StartUp, ?absent),
       ?bit_is_set(Test, ?absent),
       NewValue == '$undefined' ->
	    %% instance doesn't exist at activation
	    fire_existence(H, Instance);
       IsNew == true ->
	    %% should not fire at activation b/c no bits are set
	    no_fire;
       ?bit_is_set(Test, ?present),
       OldValue == '$undefined',
       NewValue /= '$undefined' ->
	    fire_existence(H, Instance);
       ?bit_is_set(Test, ?absent),
       OldValue /= '$undefined',
       NewValue == '$undefined' ->
	    fire_existence(H, Instance);
       ?bit_is_set(Test, ?changed),
       OldValue /= NewValue ->
	    fire_existence(H, Instance);
       true ->
	    no_fire
    end,
    check_test(T, Instance, NewValue, OldValue, OldValue2, IsNew, OldFireMask);
check_test([#mteTriggerBooleanTable{comparison = Op, value = Val} = H | T],
	   Instance, NewValue, OldValue, OldValue2, IsNew, OldFireMask) ->
    NewFireMask =
	if NewValue == '$undefined' ->
		%% the instance does no longer exist
		%% FIXME: rfc is unclear; should we generate a failure here?
		?bit_clr(OldFireMask, ?mask_boolean);
	   OldValue == '$undefined',
	   H#mteTriggerBooleanTable.startup == ?TruthValue_false ->
		%% a new instance, but we should not do the test the first time
		?bit_clr(OldFireMask, ?mask_boolean);
	   is_integer(NewValue) == false ->
		fail(badType, H#mteTriggerBooleanTable.key, Instance),
		?bit_clr(OldFireMask, ?mask_boolean);
	   Op == ?unequal, NewValue /= Val ->
		fire_boolean(H, Instance, NewValue, Op, Val, OldFireMask);
	   Op == ?equal, NewValue == Val ->
		fire_boolean(H, Instance, NewValue, Op, Val, OldFireMask);
	   Op == ?less, NewValue < Val ->
		fire_boolean(H, Instance, NewValue, Op, Val, OldFireMask);
	   Op == ?lessOrEqual, NewValue =< Val ->
		fire_boolean(H, Instance, NewValue, Op, Val, OldFireMask);
	   Op == ?greater, NewValue > Val ->
		fire_boolean(H, Instance, NewValue, Op, Val, OldFireMask);
	   Op == ?greaterOrEqual, NewValue >= Val ->
		fire_boolean(H, Instance, NewValue, Op, Val, OldFireMask);
	   true ->
		?dbg("no fire b/c NOT(~p ~s ~p)\n", [NewValue,op2str(Op),Val]),
		?bit_clr(OldFireMask, ?mask_boolean)
	end,
    check_test(T, Instance, NewValue, OldValue, OldValue2, IsNew, NewFireMask);
check_test([#mteTriggerThresholdTable{startup = StartUp,
				      rising = Rising, falling = Falling,
				      deltaRising = DeltaRising,
				      deltaFalling = DeltaFalling} = H | T],
	   Instance, NewValue, OldValue, OldValue2, IsNew, OldFireMask) ->
    ?dbg("new: ~p, old: ~p, old2: ~p, ris: ~p, fal: ~p\n",
	 [NewValue, OldValue, OldValue2, Rising, Falling]),
    NewFireMask =
	if NewValue == '$undefined' ->
		%% the instance does no longer exist
		%% FIXME: rfc is unclear; should we generate a failure here?
		OldFireMask;
	   is_integer(NewValue) == false ->
		fail(badType, H#mteTriggerThresholdTable.key, Instance),
		OldFireMask;
	   OldValue == '$undefined' ->
		%% this is the first sampling, check startup
		%% FIXME: rfc is unclear; should we also check that New == new,
		%% i.e. only when the entry becomes active?  Now we treat
		%% startup for new instances the same as when the entry becomes
		%% active.
		if StartUp /= ?mteTriggerThresholdStartup_falling,
		   NewValue >= Rising ->
			fire_threshold_rising(H, Instance, NewValue),
			TmpFireMask = ?bit_clr(OldFireMask, ?mask_falling),
			?bit_set(TmpFireMask, ?mask_rising);
		   StartUp /= ?mteTriggerThresholdStartup_rising,
		   NewValue < Falling ->
			fire_threshold_falling(H, Instance, NewValue),
			TmpFireMask = ?bit_clr(OldFireMask, ?mask_rising),
			?bit_set(TmpFireMask, ?mask_falling);
		   true ->
			OldFireMask
		end;
	   NewValue >= Rising, OldValue < Rising ->
		if ?bit_is_set(OldFireMask, ?mask_rising) -> no_fire;
		   true -> fire_threshold_rising(H, Instance, NewValue)
		end,
		TmpFireMask = ?bit_clr(OldFireMask, ?mask_falling),
		?bit_set(TmpFireMask, ?mask_rising);
	   NewValue =< Falling, OldValue > Falling ->
		if ?bit_is_set(OldFireMask, ?mask_falling) -> no_fire;
		   true -> fire_threshold_falling(H, Instance, NewValue)
		end,
		TmpFireMask = ?bit_clr(OldFireMask, ?mask_rising),
		?bit_set(TmpFireMask, ?mask_falling);
	   OldValue2 == '$undefined' ->
		%% second sampling; no delta
		OldFireMask;
	   DeltaRising /= 0, DeltaFalling /= 0 ->
		Delta1 = NewValue - OldValue,
		Delta2 = OldValue - OldValue2,
		if Delta1 >= DeltaRising, Delta2 < DeltaRising ->
			if ?bit_is_set(OldFireMask, ?mask_delta_rising) ->
				no_fire;
			   true ->
				fire_threshold_delta_rising(H, Instance,
							    NewValue)
			end,
			TmpFireMask = ?bit_clr(OldFireMask,?mask_delta_falling),
			?bit_set(TmpFireMask, ?mask_delta_rising);
		   Delta1 =< DeltaFalling, Delta2 > DeltaFalling ->
			if ?bit_is_set(OldFireMask, ?mask_delta_falling) ->
				no_fire;
			   true ->
				fire_threshold_delta_falling(H, Instance,
							     NewValue)
			end,
			TmpFireMask = ?bit_clr(OldFireMask, ?mask_delta_rising),
			?bit_set(TmpFireMask, ?mask_delta_falling);
		   true ->
			OldFireMask
		end;
	   true ->
		OldFireMask
	end,
    check_test(T, Instance, NewValue, OldValue, OldValue2, IsNew, NewFireMask);
check_test([], _Instance, _NewValue, _OldValue, _OldValue2, _IsNew, FireMask) ->
    FireMask.

fire_existence(#mteTriggerExistenceTable{key = Key,
					 objectsOwner = ObjOwner,
					 objects = Obj,
					 eventOwner = EventOwner,
					 event = Event},
	       Instance) ->
    fire_event(Key, Instance, undefined, EventOwner, Event, ObjOwner, Obj).

fire_boolean(#mteTriggerBooleanTable{key = Key,
				     objectsOwner = ObjOwner,
				     objects = Obj,
				     eventOwner = EventOwner,
				     event = Event},
	    Instance, Value, _Op, _OldVal, OldFireMask) ->
    ?dbg("boolean trigger fire (~p ~s ~p)\n", [Value, op2str(_Op), _OldVal]),
    if ?bit_is_set(OldFireMask, ?mask_boolean) ->
	    ?dbg("not fired b/c already fired\n", []),
	    OldFireMask;
       true ->
	    fire_event(Key, Instance, Value, EventOwner, Event, ObjOwner, Obj),
	    ?bit_set(OldFireMask, ?mask_boolean)
    end.

fire_threshold_rising(
  #mteTriggerThresholdTable{key = Key,
			    objectsOwner = ObjOwner,
			    objects = Obj,
			    risingEventOwner = EventOwner,
			    risingEvent = Event},
	    Instance, Value) ->
    fire_event(Key, Instance, Value, EventOwner, Event, ObjOwner, Obj).

fire_threshold_falling(
  #mteTriggerThresholdTable{key = Key,
			    objectsOwner = ObjOwner,
			    objects = Obj,
			    fallingEventOwner = EventOwner,
			    fallingEvent = Event},
	    Instance, Value) ->
    fire_event(Key, Instance, Value, EventOwner, Event, ObjOwner, Obj).

fire_threshold_delta_rising(
  #mteTriggerThresholdTable{key = Key,
			    objectsOwner = ObjOwner,
			    objects = Obj,
			    deltaRisingEventOwner = EventOwner,
			    deltaRisingEvent = Event},
	    Instance, Value) ->
    fire_event(Key, Instance, Value, EventOwner, Event, ObjOwner, Obj).

fire_threshold_delta_falling(
  #mteTriggerThresholdTable{key = Key,
			    objectsOwner = ObjOwner,
			    objects = Obj,
			    deltaFallingEventOwner = EventOwner,
			    deltaFallingEvent = Event},
	    Instance, Value) ->
    fire_event(Key, Instance, Value, EventOwner, Event, ObjOwner, Obj).

fire_event(_Key, _Instance, _Value, _EventOwner, "", _ObjOwner, _Obj) ->
    ?dbg("trigger ~p fired (~p = ~p), but does not have an "
	 "event configured\n", [_Key, _Instance, _Value]),
    fire;
fire_event(Key, Instance, Value, EventOwner, Event, ObjOwner, Obj) ->
    EventKey = {EventOwner, Event},
    case mnesia:dirty_read({mteEventTable, EventKey}) of
	[E] when E#mteEventTable.entryStatus == ?RowStatus_active,
	         E#mteEventTable.enabled == ?TruthValue_true,
                 ?bit_is_set(E#mteEventTable.actions, ?notification) ->
	    %% Only notification is supported, the test above is strictly
            %% speaking unnecessary
	    ?dbg("trigger ~p fired (~p = ~p)\n", [Key, Instance, Value]),
	    [T] = mnesia:dirty_read({mteTriggerTable, Key}),
	    Context = T#mteTriggerTable.contextName,
	    Objs0 = objects(T#mteTriggerTable.objectsOwner,
			    T#mteTriggerTable.objects,
			    Context, Instance),
	    Objs1 = objects(ObjOwner, Obj, Context, Instance),
	    fire_notification(EventKey, T, Instance, Value, Objs0 ++ Objs1),
	    fire;
	_ ->
	    ?dbg("trigger ~p fired (~p = ~p), but the event ~p "
		 "does not exist\n", [Key, Instance, Value, EventKey]),
	    fire
    end.

fire_notification(EventKey, T, Instance, Value, Objs) ->
    case mnesia:dirty_read({mteEventNotificationTable, EventKey}) of
	[N] ->
	    Objs3 = objects(N#mteEventNotificationTable.objectsOwner,
			    N#mteEventNotificationTable.objects,
			    T#mteTriggerTable.contextName,
			    Instance),
	    Notif = N#mteEventNotificationTable.notification,
	    case snmpa_symbolic_store:oid_to_aliasname(Notif) of
		{value, Alias} ->
		    if Alias == mteTriggerFired;
		       Alias == mteTriggerRising;
		       Alias == mteTriggerFalling ->
			    Objs0 = hot_objs(T, Instance, Value);
		       true ->
			    Objs0 = []
		    end,
		    Objs4 = Objs0 ++ Objs ++ Objs3,
		    ?dbg("sending notification ~p ~p\n", [Alias, Objs4]),
		    snmpa:send_notification(get(agent), Alias,
					    no_receiver, Objs4);
		_ ->
		    ?dbg("the notification ~p is not defined\n", [Notif]),
		    no_fire
	    end;
	_ ->
	    ?dbg("the eventNotification ~p does not exist\n", [EventKey]),
	    no_fire
    end.

%% FIXME: Document in Agent caps that mteHotTargetName is not included,
%% and that mteHotValue is not present for existence triggers (the value
%% may be absent and/or not an integer)
hot_objs(#mteTriggerTable{key = {_,Name}, contextName = Context, valueID = ID},
	 Instance, Value) ->
    [{?mteHotTrigger_instance, Name},
     {?mteHotContextName_instance, Context},
     {?mteHotOID_instance, ID ++ Instance} |
     if integer(Value) ->
	     [{?mteHotValue_instance, Value}];
	true ->
	     []
     end
     ].

objects(Owner, Name, Context, Instance) ->
    Prefix = [length(Owner) | Owner] ++ [length(Name) | Name],
    get_objects(Prefix, Prefix, Context, Instance).

get_objects(RowIndex0, Prefix, Context, Instance) ->
    case mnesia:snmp_get_next_index(mteObjectsTable, RowIndex0) of
	{ok, RowIndex1} ->
	    case lists:prefix(Prefix, RowIndex1) of
		true -> % add this object
		    case mnesia:snmp_get_row(mteObjectsTable, RowIndex1) of
			{ok,
			 #mteObjectsTable{iD = ID, iDWildcard = Wild,
					  entryStatus = ?RowStatus_active}} ->
			    if Wild == ?TruthValue_true ->
				    OID = ID ++ Instance;
			       true ->
				    OID = ID
			    end,
			    case get_values([OID], Context) of
				[Val] ->
				    [{OID, Val} | 
				     get_objects(RowIndex1, Prefix,
						 Context, Instance)];
				_ ->
				    %% FIXME: error, increment counter??
				    get_objects(RowIndex1, Prefix,
						Context, Instance)
			    end;
			_ ->
			    get_objects(RowIndex1, Prefix, Context, Instance)
		    end;
		false ->
		    []
	    end;
	_ ->
	    []
    end.

get_values(OIDs, Context) ->
    snmpa:get(get(agent), OIDs, Context).

get_next_values(OIDs, Context) ->
    snmpa:get_next(get(agent), OIDs, Context).
    

find_col(Col, [{Col, Val} | _], _) ->
    Val;
find_col(Col, [_ | T], Def) ->
    find_col(Col, T, Def);
find_col(_, [], Def) ->
    Def.

fail(Reason, Key, Instance) ->
    ?dbg("fail: ~p ~p\n", [Reason, Key]),
    put(failures, get(failures) + 1),
    maybe_send_fail_notification(Reason, Key, Instance).

-ifdef(debug).
maybe_send_fail_notification(Reason, Key, Instance) ->
    %% FIXME: send mteTriggerFailure notification here.  NOTE we should have
    %% a mechanism for enabling/disabling this event before implementing it.
    ReasonN =
	case Reason of
	    inconsistentName -> ?FailureReason_inconsistentName;
	    notWritable -> ?FailureReason_notWritable;
	    authorizationError -> ?FailureReason_authorizationError;
	    undoFailed -> ?FailureReason_undoFailed;
	    commitFailed -> ?FailureReason_commitFailed;
	    resourceUnavailable -> ?FailureReason_resourceUnavailable;
	    inconsistentValue -> ?FailureReason_inconsistentValue;
	    noCreation -> ?FailureReason_noCreation;
	    wrongValue -> ?FailureReason_wrongValue;
	    wrongEncoding -> ?FailureReason_wrongEncoding;
	    wrongLength -> ?FailureReason_wrongLength;
	    wrongType -> ?FailureReason_wrongType;
	    noAccess -> ?FailureReason_noAccess;
	    genErr -> ?FailureReason_genErr;
	    readOnly -> ?FailureReason_readOnly;
	    badValue -> ?FailureReason_badValue;
	    noSuchName -> ?FailureReason_noSuchName;
	    tooBig -> ?FailureReason_tooBig;
	    noError -> ?FailureReason_noError;
	    sampleOverrun -> ?FailureReason_sampleOverrun;
	    badType -> ?FailureReason_badType;
	    noResponse -> ?FailureReason_noResponse;
	    destinationUnreachable -> ?FailureReason_destinationUnreachable;
	    badDestination -> ?FailureReason_badDestination;
	    localResourceLack -> ?FailureReason_localResourceLack
	end,
    [#mteTriggerTable{key = {_,Name}, contextName = Context, valueID = ID}] = 
	mnesia:dirty_read({mteTriggerTable, Key}),
    Objs = [{?mteHotTrigger_instance, Name},
	    {?mteHotContextName_instance, Context},
	    {?mteHotOID_instance, ID ++ Instance},
	    {?mteFailedReason, ReasonN}],
    snmpa:send_notification(get(agent), mteTriggerFailure,
			    no_receiver, Objs).
-else.
maybe_send_fail_notification(_Reason, _Key, _Instance) ->
    ok.
-endif.

prefix([H|T1], [H|T2]) ->
    prefix(T1, T2);
prefix([], T) ->
    {true, T};
prefix(_, _) ->
    false.

op2str(?unequal)        -> "/=";
op2str(?equal)          -> "==";
op2str(?less)           -> "<";
op2str(?lessOrEqual)    -> "=<";
op2str(?greater)        -> ">";
op2str(?greaterOrEqual) -> ">=";
op2str(_)               -> "?".
