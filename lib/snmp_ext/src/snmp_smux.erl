%%%-------------------------------------------------------------------
%%% File    : snmp_smux.erl
%%% Author  : Martin Bjorklund <mbj@bluetail.com>
%%% Description : Utility functions for SMUX.
%%% Created :  1 Nov 2003 by Martin Bjorklund <mbj@bluetail.com>
%%%-------------------------------------------------------------------
-module(snmp_smux).
-export([addr/1]).
-export([get_pdu/1, get_pdu_active/2, decode/1, send_pdu/2, close/3]).
-export([fixvbs/2]).
-export([dec_pdu/1, enc_pdu/1]).

-define(VMODULE, "SMUX").

% PRE R10 -include_lib("snmp/src/snmp_verbosity.hrl").
-include_lib("snmp/src/misc/snmp_verbosity.hrl").
-include_lib("snmp/include/snmp_types.hrl").

-include("snmp_smux.hrl").

get_pdu(Socket) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, Bytes} ->
	    decode(Bytes);
	Else ->
	    Else
    end.

get_pdu_active(Socket, Timeout) ->
    receive
	{tcp, Socket, Bytes} ->
	    ?vtrace("got ~p\n", [Bytes]),
	    decode(Bytes);
	{tcp_closed, Socket} ->
	    {error, closed}
    after Timeout ->
	    {error, timeout}
    end.

decode(Bytes) ->
    {ok, dec_pdu(Bytes)}.

send_pdu(Socket, Pdu) ->
    Bytes = enc_pdu(Pdu),
    ?vlog("bytes: ~p\n", [Bytes]),
    ok = gen_tcp:send(Socket, Bytes).

close(Socket, Reason, Info) ->
    Addr = addr(Socket),
    ?vlog("closing SMUX peer ~p: ~p ~p", [Addr, Reason, Info]),
    send_pdu(Socket, #smux_close{code = Reason}),
    gen_tcp:close(Socket).


%% Ret: string()
addr(Sock) ->
    case inet:peername(Sock) of
	{ok, {Ip, Port}} ->
	    {A,B,C,D} = Ip,
	    lists:flatten(io_lib:format("~w.~w.~w.~w:~w",
					[A, B, C, D, Port]));
	_ ->
	    "unknown address"
    end.

fixvbs([Vb | Vbs], [OrigVb | OrigVbs]) ->
    [Vb#varbind{org_index = OrigVb#varbind.org_index} | fixvbs(Vbs, OrigVbs)];
fixvbs([], _) ->
    [].


%%-----------------------------------------------------------------
%% ASN.1 handling
%%   Don't use the ASN.1 compiler, b/c we want use the same internal
%%   representation as SNMP for the SNMP pdus.  Also rolling your
%%   own gives more straigh forward datastructures...
%%-----------------------------------------------------------------
dec_pdu(<<PduTag, B1/binary>>) ->
    case PduTag of
	96 ->
	    dec_open_pdu(B1);
	65 ->
	    {Code, _}  = dec_int_notag(B1),
	    #smux_close{code = Code};
	98 ->
	    dec_rreq_pdu(B1);
	67 ->
	    {Code, _}  = dec_int_notag(B1),
	    #smux_rrsp{code = Code};
	68 ->
	    {Code, _}  = dec_int_notag(B1),
	    #smux_sout{code = Code};
	_ ->
	    snmp_pdus:dec_pdu([PduTag | binary_to_list(B1)])
    end.

dec_open_pdu(Bytes) ->
    {T0, _} = dec_len(Bytes),
    {Version, T1} = dec_int_tag(T0),
    {Identity, T2} = dec_oid_tag(T1),
    {Descr, T3} = dec_oct_str_tag(T2),
    {Passwd, _} = dec_oct_str_tag(T3),
    #smux_open{version = Version,
	       identity = Identity,
	       description = Descr,
	       password = Passwd}.

dec_rreq_pdu(Bytes) ->
    {T0, _} = dec_len(Bytes),
    {SubTree, T1} = dec_oid_tag(T0),
    {Prio, T2} = dec_int_tag(T1),
    {Op, _} = dec_int_tag(T2),
    #smux_rreq{subtree = SubTree,
	       priority = Prio,
	       operation = Op}.

%% Ret: {Val/binary, Tail/binary}
dec_len(<<128, _/binary>>) ->
    %% indefinite form - not allowed in SNMP
    exit({asn1_error, indefinite_length});
dec_len(<<0:1, Len:7, T0/binary>>) ->
    <<Val:Len/binary, T1/binary>> = T0,
    {Val, T1};
dec_len(<<1:1,LL:7,T0/binary>>) ->
    <<Len:LL/unit:8,T1/binary>> = T0,
    <<Val:Len/binary, T2/binary>> = T1,
    {Len, T2}.

dec_oid_tag(<<6, B1/binary>>) ->
    {Val, T} = dec_len(B1),
    [AddedObjVal|ObjVals] = dec_subidentifiers(Val,0,[]), 
    {Val1, Val2} = if 
		       AddedObjVal < 40 -> 
			   {0, AddedObjVal}; 
		       AddedObjVal < 80 -> 
			   {1, AddedObjVal - 40}; 
		       true -> 
			   {2, AddedObjVal - 80} 
		   end, 
    {[Val1, Val2 | ObjVals], T}.

dec_subidentifiers(<<>>,_Av,Al) -> 
    lists:reverse(Al); 
dec_subidentifiers(<<1:1,H:7,T/binary>>,Av,Al) -> 
    dec_subidentifiers(T,(Av bsl 7) + H,Al); 
dec_subidentifiers(<<H,T/binary>>,Av,Al) -> 
    dec_subidentifiers(T,0,[((Av bsl 7) + H)|Al]). 

dec_oct_str_tag(<<4, B1/binary>>) ->
    {Val, T} = dec_len(B1),
    {binary_to_list(Val), T}.

dec_int_tag(<<2, B1/binary>>) ->
    dec_int_notag(B1).

dec_int_notag(Bin) ->
    {Val, T} = dec_len(Bin),
    {dec_int(Val), T}.

dec_int(Bin = <<0:1,_:7,_/binary>>) ->
    Len = size(Bin),
    <<Int:Len/unit:8>> = Bin,
    Int;
%% decoding negative integer values.
dec_int(Bin = <<1:1,B2:7,Bs/binary>>)  ->
    Len = size(Bin),
    <<N:Len/unit:8>> = <<B2,Bs/binary>>,
    Int = N - (1 bsl (8 * Len - 1)),
    Int.


enc_pdu(#smux_open{version = Version,
		   identity = Identity,
		   description = Descr,
		   password = Passwd}) ->
    {Bytes, _Len} =
	'SMUX':enc_SimpleOpen({'SimpleOpen', Version, list_to_tuple(Identity),
			       Descr, Passwd}, [<<96>>]),
    Bytes;
enc_pdu(#smux_close{code = Code}) ->
    {Bytes, _Len} = 'SMUX':enc_ClosePDU({'ClosePDU', Code}, [<<65>>]),
    Bytes;
enc_pdu(#smux_rreq{subtree = SubTree,
		   priority = Prio,
		   operation = Op}) ->
    {Bytes, _Len} = 'SMUX':enc_RReqPDU({'RReqPDU', list_to_tuple(SubTree),
					Prio, Op}, [<<98>>]),
    Bytes;
enc_pdu(#smux_rrsp{code = Code}) ->
    {Bytes, _Len} = 'SMUX':enc_RRspPDU({'RRspPDU', Code}, [<<67>>]),
    Bytes;
enc_pdu(#smux_sout{code = Code}) ->
    {Bytes, _Len} = 'SMUX':enc_SOutPDU({'SOutPDU', Code}, [<<68>>]),
    Bytes;

enc_pdu(Pdu) ->
    snmp_pdus:enc_pdu(Pdu).
