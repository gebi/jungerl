-module(agentx_pdu).

-compile(export_all).

-include("../src/snmp_agentx.hrl").

run() ->
    Open1 = #ax_open{timeout = 242,
		     id = [1,2,3,4,5,6],
		     descr = "hej hopp"},
    tst(?AX_OPEN, Open1),

    Close1 = #ax_close{reason = ?AX_REASON_PARSE_ERROR},
    tst(?AX_CLOSE, Close1),

    Reg1 = #ax_register{subtree = [1,2,3,4]},
    tst(?AX_REGISTER, Reg1),

    Reg2 = #ax_register{context = "context", subtree = [1,2,3,4]},
    tst(?AX_REGISTER, Reg2, ?AX_F_NON_DEFAULT_CTXT),

    Reg3 = #ax_register{context = "context", subtree = [1,2,3,4],
			range_subid = 5, upper_bound = 7},
    tst(?AX_REGISTER, Reg3, ?AX_F_NON_DEFAULT_CTXT),

    ok.

tst(Type, Pdu) ->
    tst(Type, Pdu, 0).
tst(Type, Pdu, Flags) ->
    Msg = mk_msg(Type, Pdu, Flags),
    B = list_to_binary(snmp_agentx:enc_msg(Msg)),
    cmp(snmp_agentx:dec_msg(B), Msg, B).

mk_msg(Type, Pdu, Flags) ->
    Hdr = #ax_hdr{type = Type,
		  flags = Flags bor ?AX_F_NETWORK_BYTE_ORDER,
		  sessionID = 3421, transactionID = 46573, packetID = 3},
    #ax_msg{hdr = Hdr, pdu = Pdu}.

cmp({ok, M}, M, _) ->    
    io:format("~p ok\n", [M]);
cmp({ok, N}, M, B) ->
    io:format("expected: ~p\ngot     : ~p\n~p\n", [M, N, B]),
    exit(error);
cmp(Error, M, B) ->
    io:format("expected: ~p\ngot     : ~p\n~p\n", [M, Error, B]),
    exit(error).
