-module(smux_pdu).

-compile(export_all).

-include("../src/snmp_smux.hrl").

run() ->
    Open1 = #smux_open{version = ?smux_version_1,
		      identity = [1,2,3,4,5,6],
		      description = "hej hopp",
		      password = ""},
    B11 = list_to_binary(snmp_smux:enc_pdu(Open1)),
    Open1 = snmp_smux:dec_pdu(B11),
    
    Close1 = #smux_close{code = ?close_goingDown},
    B21 = list_to_binary(snmp_smux:enc_pdu(Close1)),
    Close1 = snmp_smux:dec_pdu(B21),

    Close2 = #smux_close{code = 42},
    B22 = list_to_binary(snmp_smux:enc_pdu(Close2)),
    Close2 = snmp_smux:dec_pdu(B22),

    RReq1 = #smux_rreq{subtree = [1,2,3,4711],
		       priority = -1,
		       operation = ?rreq_readWrite},
    B31 = list_to_binary(snmp_smux:enc_pdu(RReq1)),
    RReq1 = snmp_smux:dec_pdu(B31),

    RRsp1 = #smux_rrsp{code = -1},
    B41 = list_to_binary(snmp_smux:enc_pdu(RRsp1)),
    RRsp1 = snmp_smux:dec_pdu(B41),

    SOut1 = #smux_sout{code = 1},
    B51 = list_to_binary(snmp_smux:enc_pdu(SOut1)),
    SOut1 = snmp_smux:dec_pdu(B51),
    
    ok.
