%%%----------------------------------------------------------------------
%%% File    : spread.erl
%%% Author  : Scott Lystig Fritchie <slfritchie@snookles.com>
%%% Purpose : Erlang-friendly interface to the spread_drv driver.
%%%
%%% See the file "UserGuide.txt" to answer your many questions.  :-)
%%%
%%%----------------------------------------------------------------------

-module(spread).
-behaviour(gen_server).
-compile(debug_info).

-define(NAME, ?MODULE).
-define(DRV, spread_drv).
-define(Timeout, infinity).

-include("spread_drv.hrl").

%% External exports
-export([start_link/0, start_link/4, stop/1]).
-export([subscribe/2, subscribe/3, unsubscribe/2,
	 multicast/5, mg_multicast/5]).

%% Debugging
-export([dump_state/1, rec/1, set_active/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
	  port,					% Spread port
	  subtab,				% ETS table of subscribers
	  membtab,				% ETS table of group members
	  privgrp				% Name of Spread private group
	 }).

-record(sub, {
	  pid,					% Subscriber's pid
	  gnotify,				% Flag: want group notify
	  monref				% monitor ref
	 }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    start_link("4803@localhost", "", 0, 1).
start_link(SpreadName, PrivateName, Priority, GroupMembership) ->
    gen_server:start_link(?MODULE,
			  [SpreadName, PrivateName, Priority, GroupMembership],
			  []).

stop(Pid) ->
    gen_server:call(Pid, {stop}, ?Timeout).

subscribe(Pid, Group) ->
    subscribe(Pid, Group, false).
subscribe(Pid, Group, WantMemberChanges) ->
    gen_server:call(Pid, {subscribe, self(), Group, WantMemberChanges}, ?Timeout).

unsubscribe(Pid, Group) ->
    gen_server:call(Pid, {unsubscribe, self(), Group}, ?Timeout).

multicast(Pid, ServiceType, Group, MessType, Mess) ->
    mg_multicast(Pid, ServiceType, [Group], MessType, Mess).
mg_multicast(Pid, ServiceType, GroupList, MessType, Mess) ->
    gen_server:call(Pid, {mg_multicast, ServiceType, GroupList,
			  MessType, Mess}, ?Timeout).

%%% Debugging....
dump_state(Pid) ->
    gen_server:call(Pid, {dump_state}, ?Timeout).
rec(Pid) ->
    gen_server:call(Pid, {rec}, ?Timeout).
set_active(Pid, Value) ->
    gen_server:call(Pid, {set_active, Value}, ?Timeout).


%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([SpreadName, PrivateName, Priority, GroupMembership]) ->
    {ok, Port} = ?DRV:start(),
    ?DRV:debug(Port, 0),
    {ok, PrivateGroup} = ?DRV:sp_connect(Port, SpreadName, PrivateName,
					 Priority, GroupMembership),
    {ok, 1} = ?DRV:set_active(Port, 1),
    STab = ets:new(subscribers, [private, set]),
    MTab = ets:new(members, [private, set]),
    {ok, #state{port = Port, subtab = STab, membtab = MTab,
		privgrp = PrivateGroup}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({stop}, From, State) ->
    {stop, normal, ok, State};
handle_call({set_active, Value}, From, State) ->
    {ok, Value} = ?DRV:set_active(State#state.port, Value),
    Reply = {ok, Value},
    {reply, Reply, State};
handle_call({subscribe, FromPid, Group, WantMemberChanges}, From, State) ->
    {Reply, NewState} = do_subscribe(State, FromPid, Group, WantMemberChanges),
    {reply, Reply, NewState};
handle_call({unsubscribe, FromPid, Group}, From, State) ->
    {Reply, NewState} = do_unsubscribe(State, FromPid, Group),
    {reply, Reply, State};
handle_call({mg_multicast, ServiceType, GroupList, MessType, Mess},
	    From, State) ->
    {Reply, NewState} = do_mg_multicast(State, ServiceType, GroupList,
					MessType, Mess),
    {reply, Reply, State};


handle_call({dump_state}, From, State) ->
    Reply = do_dumpstate(State),
    {reply, Reply, State};
handle_call({rec}, From, State) ->
    Reply = ?DRV:sp_receive(State#state.port, 3, 200),
    {reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    io:format("XXXYYYXXX ~w: ~s:handle_cast got ~w\n", [self(), ?MODULE, Msg]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({Port, active, Msg}, State) when Port == State#state.port ->
    NewState = process_active_msg(State, Msg),
    {noreply, NewState};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    NewState = unsubscribe_from_all(State, Pid),
    {noreply, NewState};
handle_info(Info, State) ->
    io:format("XXXYYYXXX ~w: ~s:handle_info got ~w\n", [self(), ?MODULE, Info]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

do_subscribe(State, From, Group, WantMemberChanges) when is_list(Group) ->
    do_subscribe(State, From, list_to_binary(Group), WantMemberChanges);
do_subscribe(State, From, Group, WantMemberChanges) ->
    case already_subscribed(State, From, Group) of
	{true, WantMemberChanges} ->
	    {{error, already_subscribed}, State};
	{true, true} ->
	    NewState = set_memberchanges(State, From, Group, WantMemberChanges),
	    {ok, NewState};
	{true, false} ->
	    NewState = set_memberchanges(State, From, Group, WantMemberChanges),
	    send_current_membership(State, From, Group),
	    {ok, NewState};
	false ->
	    NewState = add_subscriber(State, From, Group, WantMemberChanges),
	    case WantMemberChanges of
		true -> send_current_membership(State, From, Group);
		_ -> ok
	    end,
	    {ok, NewState}
    end.
		    
do_unsubscribe(State, From, Group) when is_list(Group) ->
    do_unsubscribe(State, From, list_to_binary(Group));
do_unsubscribe(State, From, Group) ->
    case already_subscribed(State, From, Group) of
	{true, _} ->
	    NewState = del_subscriber(State, From, Group),
	    {ok, NewState};
	false ->
	    {{error, not_subscribed}, State}
    end.
		    
do_mg_multicast(State, ServiceType, GroupList, MessType, Mess) ->
    ServiceTypeNum = convert_servicetype(ServiceType),
    Res = (catch ?DRV:sp_multigroup_multicast(State#state.port, ServiceTypeNum,
					      GroupList, MessType, Mess)),
    {Res, State}.

already_subscribed(State, From, Group) ->
    case ets:lookup(State#state.subtab, Group) of
	[] ->
	    false;
	[{Group, SubList}] ->
	    case lists:keysearch(From, #sub.pid, SubList) of
		false ->
		    false;
		{value, Sub} ->
		    {true, Sub#sub.gnotify}
	    end
    end.
	
%% We assume that the caller knows that From _is_ subscribed to Group.
set_memberchanges(State, From, Group, WantMemberChanges) ->
    [{Group, SubList}] = ets:lookup(State#state.subtab, Group),
    Sub = lists:keysearch(From, #sub.pid, SubList),
    NewSub = #sub{pid = From, gnotify = WantMemberChanges,
		  monref = Sub#sub.monref},
    NewSubList = [NewSub | lists:keydelete(From, #sub.pid, SubList)],
    ets:insert(State#state.subtab, {Group, NewSubList}),
    State.

%% We assume that the caller knows that From is _not_ subscribed to Group.
add_subscriber(State, From, Group, WantMemberChanges) ->
    Sub = #sub{pid = From, gnotify = WantMemberChanges,
	       monref = erlang:monitor(process, From)},
    case ets:lookup(State#state.subtab, Group) of
	[] ->
	    case catch ?DRV:sp_join(State#state.port, Group) of
		{ok, _} ->
		    ets:insert(State#state.subtab, {Group, [Sub]}),
		    State;
		_ ->
		    io:format("XXX add_subscriber: what to do here!?\n"),
		    State
	    end;
	[{Group, SubList}] ->
	    NewSubList = [Sub|SubList],
	    ets:insert(State#state.subtab, {Group, NewSubList}),
	    State
    end.

%% We assume that the caller knows that From _is_ subscribed to Group.
del_subscriber(State, From, Group) ->
    case ets:lookup(State#state.subtab, Group) of
	[{Group, SubList}] ->
	    NewSubList = lists:keydelete(From, #sub.pid, SubList),
	    case NewSubList of
		[] ->
		    ets:delete(State#state.subtab, Group),
		    catch ?DRV:sp_leave(State#state.port, Group);
		_ ->
		    ets:insert(State#state.subtab, {Group, NewSubList})
	    end;
	[] ->
	    ok
    end,
    State.

send_current_membership(State, ToPid, Group) ->
    case ets:lookup(State#state.membtab, Group) of
	[{Group, Members}] ->
	    ets:lookup(State#state.membtab, Group),
	    ToPid ! {spread, self(), membership, Group, Members};
	_ ->
	    ok
    end.

process_active_msg(State, Msg) ->
    {ServiceType, Sender, GroupList, MessType, EndianMismatch, Mess} = Msg,
    case make_servicelist(ServiceType) of
	{reg_memb_mess, _Reason} ->
	    % Sender = group name, GroupList = new membership list
	    NewState = update_membership(State, Sender, GroupList),
	    notify_member_change(State, Sender, GroupList),
	    NewState;
	{regular_mess, Type} ->
	    NewState = forward_message(State, GroupList, Type, Sender,
				       MessType, Mess),
	    NewState;
	_ ->
	    throw({err, "XXX Unknown message type, cannot process"})
    end.

%% XXX NOTE: This does not take REJECT_MESS into account, nor does it
%%           pay attention to some other service bits that may or may
%%           not be important.  {shrug}
make_servicelist(ServiceType) ->
    Memb = ServiceType band 16#3000,
    Reason = ServiceType band 16#0f00,
    MessFlavor = ServiceType band ?REGULAR_MESS,
    First = decode_memb(Memb, Reason),
    Second = if Memb > 0 -> decode_reason(Reason, Memb);
		true     -> decode_messflavor(MessFlavor)
	     end,
    {First, Second}.

decode_memb(0, 0) -> regular_mess;
decode_memb(0, ?CAUSED_BY_LEAVE) -> reg_memb_mess; % XXX new type for self leave??
decode_memb(?REG_MEMB_MESS, _Reason) -> reg_memb_mess;
decode_memb(?TRANSITION_MESS, _Reason) -> transition_mess.

decode_reason(?CAUSED_BY_JOIN, _) -> caused_by_join;
decode_reason(?CAUSED_BY_LEAVE, _) -> caused_by_leave;
decode_reason(?CAUSED_BY_DISCONNECT, _) -> caused_by_disconnect;
decode_reason(?CAUSED_BY_NETWORK, _) -> caused_by_network;
decode_reason(0, ?TRANSITION_MESS) -> caused_by_network. % XXX is this wise?

decode_messflavor(?UNRELIABLE_MESS) -> unreliable_mess;
decode_messflavor(?RELIABLE_MESS) -> reliable_mess;
decode_messflavor(?FIFO_MESS) -> fifo_mess;
decode_messflavor(?CAUSAL_MESS) -> causal_mess;
decode_messflavor(?AGREED_MESS) -> agreed_mess;
decode_messflavor(?SAFE_MESS) -> safe_mess;     
decode_messflavor(0) -> self_leave.		%XXX is this wise?
     
update_membership(State, Group, []) ->
    %% This is the self-leave case: we left the group, so we don't know
    %% who remains in the group.
    ets:delete(State#state.membtab, Group),
    State;
update_membership(State, Group, Members) ->
    ets:insert(State#state.membtab, {Group, Members}),
    State.

notify_member_change(State, Group, Members) ->
    case ets:lookup(State#state.subtab, Group) of
	[{Group, SubList}] ->
	    notify_member_change2(SubList, Group, Members);
	[] ->
	    ok
    end.

notify_member_change2([], Group, Members) ->
    ok;
notify_member_change2([H|T], Group, Members) when H#sub.gnotify == true ->
    H#sub.pid ! {spread, self(), membership, Group, Members},
    notify_member_change2(T, Group, Members);
notify_member_change2([H|T], Group, Members) ->
    notify_member_change2(T, Group, Members).

forward_message(State, [], Type, Sender, MessType, Mess) ->
    State;
forward_message(State, [H|T], Type, Sender, MessType, Mess) ->
    forward_msg(State, H, Type, Sender, MessType, Mess),
    forward_message(State, T, Type, Sender, MessType, Mess).
    
forward_msg(State, Group, Type, Sender, MessType, Mess) ->
    case ets:lookup(State#state.subtab, Group) of
	[{Group, SubList}] ->
	    [Pid ! {spread, self(), msg, Group, Type, Sender,
		    MessType, Mess} || #sub{pid = Pid} <- SubList];
	_ ->
	    ok					% XXX really ok?
    end.

unsubscribe_from_all(State, Pid) ->
    do_dumpstate(State),
    Groups = [G || [G] <- ets:match(State#state.subtab, {'$1', '_'})],
    Fun = fun (Group, State) ->
		  del_subscriber(State, Pid, Group) % Returns new state!
	  end,
    NewState = lists:foldl(Fun, State, Groups),
    NewState.


convert_servicetype(unreliable_mess) -> ?UNRELIABLE_MESS;
convert_servicetype(reliable_mess) -> ?RELIABLE_MESS;
convert_servicetype(fifo_mess) -> ?FIFO_MESS;
convert_servicetype(causal_mess) -> ?CAUSAL_MESS;
convert_servicetype(agreed_mess) -> ?AGREED_MESS;
convert_servicetype(safe_mess) -> ?SAFE_MESS.

do_dumpstate(State) ->
    M = io_lib:format("State = ~p\n", [State]) ++
	io_lib:format("subtab = ~p\n", [ets:tab2list(State#state.subtab)]) ++
	io_lib:format("membtab = ~p\n", [ets:tab2list(State#state.membtab)]),
    io:format(M),
    lists:flatten(M).

