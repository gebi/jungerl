%% Implements a (fake) SMUX peer.

-module(smux_fake_peer).

-compile(export_all).

-define(ID, [1,2,3,4]).

-include("TEST-MIB.hrl").
-include("../src/snmp_smux.hrl").
-include_lib("snmp/include/snmp_types.hrl").

start() ->
    start({127,0,0,1}, 1999, "secret").


-record(state, {sock}).

start(Ip, Port, Secret) ->
    {ok, Sock} = gen_tcp:connect(Ip, Port, [binary]),
    Open = #smux_open{version = ?smux_version_1,
		      identity = ?ID,
		      description = "test fake peer",
		      password = Secret},
    ok = snmp_smux:send_pdu(Sock, Open),
    RegPdu = #smux_rreq{subtree = ?test,
			priority = 42,
			operation = readWrite},
    ok = snmp_smux:send_pdu(Sock, RegPdu),
    loop(#state{sock = Sock}),
    exit(cleanup).

loop(S) ->
    receive
	{tcp, _Sock, Bytes} ->
	    io:format("got ~p\n", [Bytes]),
	    {ok, SmuxPdu} = snmp_smux:decode(Bytes),
	    io:format("decoded ~p\n", [SmuxPdu]),
	    handle_smux_pdu(SmuxPdu, S);
	{tcp_closed, _Sock} ->
	    io:format("tcp closed\n"),
	    ok;
	Else ->
	    io:format("got unknown ~p\n", [Else])
    end.

handle_smux_pdu(#smux_rrsp{code = ?rrsp_failure}, S) ->
    io:format("registration failed\n");
handle_smux_pdu(#smux_rrsp{code = Prio}, S) ->
    io:format("registration succeeded at prio ~p\n", [Prio]),
    send_trap(S),
    loop(S);
handle_smux_pdu(#pdu{type = 'get-request',
		     request_id = ReqId,
		     varbinds = Vbs},
		S) ->
    ResVbs = lists:map(fun(#varbind{oid = ?testStr_instance} = Vb) ->
			       Vb#varbind{variabletype = 'OCTET STRING',
					  value = "testStr"};
			  (#varbind{oid = ?testInt_instance} = Vb) ->
			       Vb#varbind{variabletype= 'INTEGER',
					  value = 42}
		       end, Vbs),
    ResMsg = #pdu{type = 'get-response',
		  request_id = ReqId,
		  error_status = noError,
		  error_index = 0,
		  varbinds = ResVbs},
    ok = snmp_smux:send_pdu(S#state.sock, ResMsg),
    loop(S);
handle_smux_pdu(#pdu{type = 'get-next-request',
		     request_id = ReqId,
		     varbinds = Vbs},
		S) ->
    ResVbs = 
	lists:map(fun(#varbind{oid = OID} = Vb) ->
			  if OID < ?testStr_instance ->
				  Vb#varbind{oid = ?testStr_instance,
					     variabletype = 'OCTET STRING',
					     value = "testStr"};
			     OID < ?testInt_instance ->
				  Vb#varbind{oid = ?testInt_instance,
					     variabletype= 'INTEGER',
					     value = 42};
			     true ->
				  Vb#varbind{value = endOfMibView}
			  end
		  end, Vbs),
    ResMsg = #pdu{type = 'get-response',
		  request_id = ReqId,
		  error_status = noError,
		  error_index = 0,
		  varbinds = ResVbs},
    ok = snmp_smux:send_pdu(S#state.sock, ResMsg),
    loop(S);
handle_smux_pdu(#smux_close{code = Code}, S) ->
    {close, Code}.


send_trap(S) ->    
    Trap = #trappdu{enterprise = [1,3,6,1,1],
		    agent_addr = [127,0,0,1],
		    generic_trap = 4,
		    specific_trap = 6,
		    time_stamp = 234,
		    varbinds = [#varbind{oid = ?testInt_instance,
					 variabletype = 'INTEGER',
					 value = 43}]},
    ok = snmp_smux:send_pdu(S#state.sock, Trap).

    
