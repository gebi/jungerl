%%%-------------------------------------------------------------------
%%% File    : snmp_smux_server.erl
%%% Author  : Martin Bjorklund <mbj@bluetail.com>
%%% Description : Acts as a subagent towards the snmp master agent, and
%%%               as a SMUX master towards the real SMUX subagent.
%%%               There is one such process per SMUX connection.
%%% Created :  1 Nov 2003 by Martin Bjorklund <mbj@bluetail.com>
%%%-------------------------------------------------------------------
-module(snmp_smux_server).

-behaviour(gen_server).

-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-define(VMODULE, "SMUX").
% PRE R10 -include_lib("snmp/src/snmp_verbosity.hrl").
-include_lib("snmp/src/misc/snmp_verbosity.hrl").
-include_lib("snmp/include/snmp_types.hrl").
-include("snmp_smux.hrl").

-record(state, {sock,
		reqid = 1,
		timeout,
		agent}).

%%====================================================================
%% External functions
%%====================================================================
start_link(Socket, Agent, SMUXTimeout) ->
    gen_server:start_link(?MODULE, [Socket, Agent, SMUXTimeout], []).

%%====================================================================
%% Server functions
%%====================================================================
init([Socket, Agent, SMUXTimeout]) ->
    inet:setopts(Socket, [{active, once}]),
    put(verbosity, trace),
    {ok, #state{sock = Socket, agent = Agent, timeout = SMUXTimeout}}.

handle_call({subagent_get, Vbs, PduData, IsNotification}, From, S0) ->
    Pdu = mk_pdu('get-request', Vbs, S0#state.reqid),
    S1 = S0#state{reqid = S0#state.reqid + 1},
    ok = snmp_smux:send_pdu(S0#state.sock, Pdu),
    case get_reply(S1, S0#state.reqid, Vbs) of
	{ok, Reply, S2} ->
	    {reply, Reply, S2};
	closed ->
	    Reply = {genErr, (hd(Vbs))#varbind.org_index, []},
	    {stop, closed, Reply, S1}
    end;
handle_call({subagent_get_next, MibView, Vbs, PduData}, From, S0) ->
    Pdu = mk_pdu('get-next-request', Vbs, S0#state.reqid),
    S1 = S0#state{reqid = S0#state.reqid + 1},
    ok = snmp_smux:send_pdu(S0#state.sock, Pdu),
    case get_reply(S1, S0#state.reqid, Vbs) of
	{ok, Reply, S2} ->
	    {reply, Reply, S2};
	closed ->
	    Reply = {genErr, (hd(Vbs))#varbind.org_index, []},
	    {stop, closed, Reply, S1}
    end;
handle_call({subagent_set, Arguments, PduData}, From, S0) ->
    %% FIXME: only works with snmp_set or comatible as set_module
    %% FIXME: should check each vb to see if any is ro, and return error
    case Arguments of
	[phase_one, Vbs] ->
	    Pdu = mk_pdu('set-request', Vbs, S0#state.reqid),
	    S1 = S0#state{reqid = S0#state.reqid + 1},
	    ok = snmp_smux:send_pdu(S0#state.sock, Pdu),
	    case get_reply(S1, S0#state.reqid, Vbs) of
		{ok, Reply, S2} ->
		    {ErrStatus, ErrIndex, _Vbs} = Reply,
		    {reply, {ErrStatus, ErrIndex}, S2};
		closed ->
		    Reply = {genErr, (hd(Vbs))#varbind.org_index, []},
		    {stop, closed, Reply, S1}
	    end;
	[phase_two, Code, _Vbs] ->
	    SmuxCode = if Code == set -> commit;
			  Code == undo -> rollback
		       end,
	    Pdu = #smux_sout{code = SmuxCode},
	    ok = snmp_smux:send_pdu(S0#state.sock, Pdu),
	    {reply, {noError, 0}, S0};
	_ ->
	    %% handle set_module's compatible with snmp_set
	    {reply, {noError, 0}, S0}
    end.


handle_cast(Msg, S) ->
    {noreply, S}.

handle_info({tcp, _Sock, Bytes}, S) ->
    inet:setopts(S#state.sock, [{active, once}]),
    case snmp_smux:decode(Bytes) of
	{ok, SmuxPdu} ->
	    case handle_smux_pdu(SmuxPdu, S) of
		{ok, S1} ->
		    {noreply, S1};
		closed ->
		    {stop, closed, S}
	    end;
	{error, Else} ->
	    snmp_smux:close(S#state.sock, packetFormat, Else),
	    {stop, closed, S}
    end;
handle_info({tcp_closed, _Sock}, S) ->
    {stop, closed, S};
handle_info({tcp_error, _Sock, Reason}, S) ->
    Addr = snmp_smux:addr(S#state.sock),
    ?vlog("SMUX peer ~p tcp error: ~p", [Addr, Reason]),
    gen_tcp:close(S#state.sock),
    {stop, closed, S}.

    

terminate(Reason, S) ->
    if Reason == closed ->
	    ok;
       true->
	    snmp_smux:close(S#state.sock, goingDown, Reason)
    end.

code_change(OldVsn, S, Extra) ->
    {ok, S}.

%%====================================================================
%% Internal functions
%%====================================================================
mk_pdu(Type, Vbs, ReqId) ->
    #pdu{type = Type,
	 request_id = ReqId,
	 error_status = noError,
	 error_index = 0,
	 varbinds = Vbs}.

%% Ret: {ok, NewState} | closed
handle_smux_pdu(Pdu, S) ->
    if record(Pdu, smux_rreq) ->
	    handle_rreq_pdu(Pdu, S);
       record(Pdu, trappdu) ->
	    handle_trap(Pdu, S);
       record(Pdu, smux_close) ->
	    Addr = snmp_smux:addr(S#state.sock),
	    ?vlog("SMUX peer ~p closed: ~p", [Addr, Pdu#smux_close.code]),
	    closed;
       true ->
	    snmp_smux:close(S#state.sock, protocolError, Pdu),
	    closed
    end.
	
%% Ret: {ok, NewState} | closed
handle_rreq_pdu(#smux_rreq{subtree = SubTree, priority = Prio, operation = Op},
		S) ->
    case Op of
	delete ->
	    Addr = snmp_smux:addr(S#state.sock),
	    ?vlog("SMUX peer ~p send registration delete request - ignored",
		  [Addr]);
	_ ->
	    case snmp_smux_admin:add_registration(self(), SubTree, Prio) of
		ok -> 
		    Pdu = #smux_rrsp{code = 0},
		    ok = snmp_smux:send_pdu(S#state.sock, Pdu);
		error ->
		    Pdu = #smux_rrsp{code = ?rrsp_failure},
		    ok = snmp_smux:send_pdu(S#state.sock, Pdu)
	    end
    end,
    {ok, S}.
		    
%% Ret: {ok, NewState} | closed
handle_trap(TrapPDU, S) ->
    %% NOTE: depends on internal snmp agent API
    #trappdu{enterprise = Enterprise,
	     generic_trap = Generic,
	     specific_trap = Specific,
	     varbinds = Vbs} = TrapPDU,
    %% FIXME: assert(Generic == ?enterpriseSpecific)
    Trap = #trap{enterpriseoid = Enterprise,
		 specificcode = Specific},
    Vars = lists:map(
	     fun(Vb) ->
		     {Vb#varbind.oid,
		      #asn1_type{bertype = Vb#varbind.variabletype},
		      {value, Vb#varbind.value}}
	     end, Vbs),
    S#state.agent ! {forward_trap, Trap, "", "", no_receiver, Vars},
    {ok, S}.


%% Ret: {ok, {ErrStatus, ErrIndex, Vbs}, NewState} | closed
get_reply(S, ReqId, OrigVbs) ->
    case snmp_smux:get_pdu_active(S#state.sock, S#state.timeout) of
	{ok, PDU} when record(PDU, pdu),
                       PDU#pdu.type == 'get-response',
	               PDU#pdu.request_id == ReqId ->
	    inet:setopts(S#state.sock, [{active, once}]),
	    ?vtrace("got ~p\n", [PDU]),
	    case PDU#pdu.error_status of
		noError ->
		    Vbs = snmp_smux:fixvbs(PDU#pdu.varbinds, OrigVbs),
		    {ok, {noError, 0, Vbs}, S};
		ErrStatus ->
		    OrigIndex = orig_index(PDU#pdu.error_index, OrigVbs),
		    {ok, {ErrStatus, OrigIndex, []}, S}
	    end;
	{ok, PDU} when record(PDU, pdu),
                       PDU#pdu.type == 'get-response' ->
	    snmp_smux:close(S#state.sock, protocolError, badRequestId),
	    closed;
	{ok, OtherPdu} ->
	    inet:setopts(S#state.sock, [{active, once}]),
	    case handle_smux_pdu(OtherPdu, S) of
		{ok, S1} ->
		    %% we got e.g. a trap; it's now handled, try to read again
		    get_reply(S1, ReqId, OrigVbs);
		error ->
		    closed
	    end;
	{error, timeout} ->
	    Addr = snmp_smux:addr(S#state.sock),
	    gen_tcp:close(S#state.sock),
	    ?vlog("SMUX request timeout from ~p", [Addr]),
	    closed;
	{error, closed} ->
	    Addr = snmp_smux:addr(S#state.sock),
	    ?vlog("SMUX closed at ~p", [Addr]),
	    closed
    end.

orig_index(Idx, OrigVbs) ->
    orig_index(Idx, OrigVbs, OrigVbs).
orig_index(1, [Vb | _], _OrigVbs) ->
    Vb#varbind.org_index;
orig_index(N, [_ | T], OrigVbs) ->
    orig_index(N-1, T, OrigVbs);
orig_index(_Idx, [], OrigVbs) ->
    %% index from smux peer out of range!  use first orig index
    ?vlog("smux index ~p out of range", [_Idx]),
    (hd(OrigVbs))#varbind.org_index.
