%%%-------------------------------------------------------------------
%%% File    : snmp_agent_adapter.erl
%%% Author  : Martin Bjorklund <mbj@bluetail.com>
%%% Description : Acts as a subagent towards the snmp master agent, and
%%%               as a SNMP v1/v2c manager towards the real SNMP agent.
%%%               This setup is useful when you want to have a 
%%%               master-subagent relation, but the agent doesn't support
%%%               SMUX or AgentX.
%%%               The foreign SNMP agent must be configured to send traps
%%%               to this process.
%%%               There is one process per agent connection.
%%%  NOTE: Two-phase SET cannot be implemented!  This means that in the
%%%        worst case, we'll have to send a undoFailed.  In the normal
%%%        case, where the subagent implements a whole MIB tree, each
%%%        SET request to the master agent will contain either no or 
%%%        all varbinds to this subagent.
%%% Created :  5 Nov 2003 by Martin Bjorklund <mbj@bluetail.com>
%%%-------------------------------------------------------------------
%%% TODO: o  handle timeouts (and retransmits ?)
%%%       o  test!
%%%-------------------------------------------------------------------
-module(snmp_agent_adapter).

-behaviour(gen_server).

-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

% PRE R10 -include_lib("snmp/src/snmp_verbosity.hrl").
-include_lib("snmp/src/misc/snmp_verbosity.hrl").
-define(SNMP_USE_V3, true).
-include_lib("snmp/include/snmp_types.hrl").

-record(state, {sock,
		reqid = 1,
		agent,
		conf}).

-record(conf, {agent_ip = {127,0,0,1},
	       agent_port,
	       mgr_ip = {127,0,0,1},
	       mgr_port,
	       timeout = 5000,
	       community = "public",
	       version = 'version-1',
	       oids = []}).

-define(c(S), (S#state.conf)#conf).

%%====================================================================
%% External functions
%%====================================================================
%% agent_port and mgr_port are mandatory
start_link(Agent, Options) ->
    Conf = mk_conf(Options, #conf{}),
    gen_server:start_link(?MODULE, [Agent, Conf], []).

%%====================================================================
%% Server functions
%%====================================================================
init([Agent, C]) ->
    put(verbosity, trace),
    {ok, Sock} = gen_udp:open(C#conf.mgr_port, [{reuse_addr, true}]),
    lists:foreach(
      fun(Oid) ->
	      ok = snmp:register_subagent(Agent, Oid, self())
      end, C#conf.oids),
    {ok, #state{sock = Sock, agent = Agent, conf = C}}.

handle_call({subagent_get, Vbs, PduData, IsNotification}, From, S0) ->
    Msg = mk_msg('get-request', Vbs, S0),
    S1 = S0#state{reqid = S0#state.reqid + 1},
    ok = send_msg(S0#state.sock, Msg),
    case get_reply(S1, S0#state.reqid, Vbs) of
	{ok, Reply, S2} ->
	    {reply, Reply, S2};
	{error, Reason} ->
	    Reply = {genErr, (hd(Vbs))#varbind.org_index, []},
	    {stop, Reason, Reply, S1}
    end;
handle_call({subagent_get_next, MibView, Vbs, PduData}, From, S0) ->
    Msg = mk_msg('get-next-request', Vbs, S0),
    S1 = S0#state{reqid = S0#state.reqid + 1},
    ok = send_msg(S0#state.sock, Msg), 
    case get_reply(S1, S0#state.reqid, Vbs) of
	{ok, Reply, S2} ->
	    {reply, Reply, S2};
	{error, Reason} ->
	    Reply = {genErr, (hd(Vbs))#varbind.org_index, []},
	    {stop, Reason, Reply, S1}
    end;
handle_call({subagent_set, Arguments, PduData}, From, S0) ->
    %% FIXME: only works with snmp_set or comatible as set_module
    case Arguments of
	[phase_one, Vbs] ->
	    %% we can't do anything here...
	    {reply, {noError, 0}, S0};
	[phase_two, commit, Vbs] ->
	    Msg = mk_msg('set-request', Vbs, S0),
	    S1 = S0#state{reqid = S0#state.reqid + 1},
	    ok = send_msg(S0#state.sock, Msg),
	    case get_reply(S1, S0#state.reqid, Vbs) of
		{ok, Reply, S2} ->
		    {ErrStatus, ErrIndex, _Vbs} = Reply,
		    {reply, {ErrStatus, ErrIndex}, S2};
		{error, Reason} ->
		    Reply = {genErr, (hd(Vbs))#varbind.org_index, []},
		    {stop, Reason, Reply, S1}
	    end;
	[phase_two, undo, Vbs] ->
	    %% bad case.  this is why a real master/subagent protocol is needed!
	    {reply, {undoFailed, (hd(Vbs))#varbind.org_index}, S0};
	_ ->
	    %% handle set_module's compatible with snmp_set
	    {reply, {noError, 0}, S0}
    end.


handle_cast(Msg, S) ->
    {noreply, S}.

handle_info({udp, _Sock, Ip, Port, Bytes}, S) when Ip == ?c(S).agent_ip ->
    case catch snmp_pdus:dec_message(Bytes) of
	{'EXIT', Reason} ->
	    {stop, {packetFormat, Reason}, S};
	SnmpMsg when record(SnmpMsg#message.data, trappdu) ->
	    case handle_trap(SnmpMsg#message.data, S) of
		{ok, S1} ->
		    {noreply, S1};
		{error, Reason} ->
		    {stop, Reason, S}
	    end;
	SnmpMsg ->
	    {stop, {protocolError, badPacket, SnmpMsg}, S}
    end;
handle_info({udp, _Sock, _Ip, _Port, _Bytes}, S) ->
    ?vlog("got udp packet from unknown ip ~p\n", [_Ip]),
    {noreply, S}.

terminate(Reason, S) ->
    ?vlog("terminating, reason: ~p\n", [Reason]).

code_change(OldVsn, S, Extra) ->
    {ok, S}.

%%====================================================================
%% Internal functions
%%====================================================================
mk_conf([{agent_ip, Ip} | T], C) when tuple(Ip) ->
    mk_conf(T, C#conf{agent_ip = Ip});
mk_conf([{agent_port, Port} | T], C) when integer(Port), Port > 0 ->
    mk_conf(T, C#conf{agent_port = Port});
mk_conf([{mgr_ip, Ip} | T], C) when tuple(Ip) ->
    mk_conf(T, C#conf{mgr_ip = Ip});
mk_conf([{mgr_port, Port} | T], C) when integer(Port), Port > 0 ->
    mk_conf(T, C#conf{mgr_port = Port});
mk_conf([{timeout, Tm} | T], C) when integer(Tm), Tm > 0 ->
    mk_conf(T, C#conf{timeout = Tm});
mk_conf([{community, Str} | T], C) when list(Str) ->
    mk_conf(T, C#conf{community = Str});
mk_conf([{version, v1} | T], C) ->
    mk_conf(T, C#conf{version = 'version-1'});
mk_conf([{version, v2c} | T], C) ->
    mk_conf(T, C#conf{version = 'version-2'});
mk_conf([{oid, Oid} | T], C) when list(Oid) ->
    mk_conf(T, C#conf{oids = [Oid | C#conf.oids]});
mk_conf([], C) ->
    C;
mk_conf([X | _], _C) ->
    erlang:fault({unknown_option, X}).


mk_msg(Type, Vbs, S) ->
    #message{version = ?c(S).version,
	     vsn_hdr = ?c(S).community,
	     data = #pdu{type = Type,
			 request_id = S#state.reqid,
			 error_status = noError,
			 error_index = 0,
			 varbinds = Vbs}}.

send_msg(Sock, Msg) ->
    Bytes = snmp_pdus:enc_message(Msg),
    ok = gen_udp:send(Sock, Bytes).

%% Ret: {ok, {ErrStatus, ErrIdx, Vbs}, NewState} | {error, Reason}
get_reply(S, ReqId, OrigVbs) ->
    receive
	{udp, _Sock, Ip, Port, Bytes} when Ip == ?c(S).agent_ip ->
	    case catch snmp_pdus:dec_message(Bytes) of
		{'EXIT', Reason} ->
		    {error, {packetFormat, Reason}};
		SnmpMsg when record(SnmpMsg#message.data, pdu) ->
		    Pdu = SnmpMsg#message.data,
		    case Pdu#pdu.request_id of
			ReqId ->
			    {ok, {Pdu#pdu.error_status,
				  Pdu#pdu.error_index,
				  fixvbs(Pdu#pdu.varbinds, OrigVbs)}};
			_ ->
			    ?vlog("got unknown reqid pdu ~p\n", [SnmpMsg]),
			    {error, {protocolError, badRequestId}}
		    end;
		SnmpMsg when record(SnmpMsg#message.data, trappdu) ->
		    case handle_trap(SnmpMsg#message.data, S) of
			{ok, S1} ->
			    get_reply(S, ReqId, OrigVbs);
			Error ->
			    Error
		    end;
		SnmpMsg ->
		    ?vlog("got unknown reqid pdu ~p\n", [SnmpMsg]),
		    {error, {protocolError, badPacket, SnmpMsg}}
	    end
    end.

handle_trap(TrapPdu, S) ->
    %% NOTE: depends on internal snmp agent API
    #trappdu{enterprise = Enterprise,
	     generic_trap = Generic,
	     specific_trap = Specific,
	     varbinds = Vbs} = TrapPdu,
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
    
fixvbs([Vb | Vbs], [OrigVb | OrigVbs]) ->
    [Vb#varbind{org_index = OrigVb#varbind.org_index} | fixvbs(Vbs, OrigVbs)];
fixvbs([], _) ->
    [].
