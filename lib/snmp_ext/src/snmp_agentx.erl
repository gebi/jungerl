%%%-------------------------------------------------------------------
%%% File    : snmp_agentx.erl
%%% Author  :  <mbj@bluetail.com>
%%% Description : 
%%% Created : 12 Nov 2003 by  <mbj@bluetail.com>
%%%-------------------------------------------------------------------
-module(snmp_agentx).

-export([enc_msg/1, dec_msg/1]).

-include("snmp_ext.hrl").
-include("snmp_agentx.hrl").

enc_msg(#ax_msg{hdr = Hdr0, pdu = Pdu}) ->
    {ExtraFlags, PduType, PduL, PduB} = enc_pdu(Pdu),
    Flags = Hdr0#ax_hdr.flags bor ExtraFlags bor ?AX_F_NETWORK_BYTE_ORDER,
    Hdr = Hdr0#ax_hdr{type = PduType, flags = Flags},
    [enc_hdr(Hdr, PduL), PduB].

enc_hdr(#ax_hdr{version = Vsn, type = Type, flags = Flags,
		sessionID = SID, transactionID = TID, packetID = PID},
       PduLen) ->
    <<Vsn:8, Type:8, Flags:8, 0:8, SID:32, TID:32, PID:32, PduLen:32>>.

enc_pdu(#ax_open{timeout = Tm, id = ID, descr = Descr}) ->
    {IDL, IDB} = enc_oid(ID),
    {DescrL, DescrB} = enc_octet_str(Descr),
    {0, ?AX_OPEN, 4 + IDL + DescrL, [<<Tm:8, 0:24>>, IDB, DescrB]};
enc_pdu(#ax_close{reason = Reason}) ->
    {0, ?AX_CLOSE, 4, <<Reason:8, 0:24>>};
enc_pdu(#ax_register{context = Ctx, timeout = Tm, priority = Prio,
		     range_subid = RSubId, subtree = SubTree,
		     upper_bound = Upper, instance_registration = InstReg}) ->
    {CtxF, {CtxL, CtxB}} = enc_ctx(Ctx),
    {SubTreeL, SubTreeB} = enc_oid(SubTree),
    InstF = if InstReg -> ?AX_F_INSTANCE_REGISTRATION;
	       true -> 0
	    end,
    {OptULen, OptUpper} = if RSubId == 0 -> {0, []};
			     true -> {4, <<Upper:32>>}
			  end,
    {CtxF bor InstF, ?AX_REGISTER,
     CtxL + 4 + SubTreeL + OptULen, 
     [CtxB, <<Tm:8, Prio:8, RSubId:8, 0:8>>, SubTreeB, OptUpper]};
enc_pdu(#ax_unregister{context = Ctx, priority = Prio,
		       range_subid = RSubId, subtree = SubTree,
		       upper_bound = Upper}) ->
    {CtxF, {CtxL, CtxB}} = enc_ctx(Ctx),
    {SubTreeL, SubTreeB} = enc_oid(SubTree),
    {OptULen, OptUpper} = if RSubId == 0 -> {0, []};
			     true -> {4, <<Upper:32>>}
			  end,
    {CtxF, ?AX_UNREGISTER,
     CtxL + 4 + SubTreeL + OptULen, 
     [CtxB, <<0:8, Prio:8, RSubId:8, 0:8>>, SubTreeB, OptUpper]};
enc_pdu(#ax_get{context = Ctx, oids = OIDs}) ->
    {CtxF, {CtxL, CtxB}} = enc_ctx(Ctx),
    {Len, ByteList} = lists:foldl(fun(OID, {AccL, AccB}) ->
					  SR = #sr{oid1 = OID},
					  {L, B} = enc_sr(SR),
					  {L + AccL, [B | AccB]}
				  end, {CtxL, [CtxB]}, OIDs),
    {CtxF, ?AX_GET, Len, lists:reverse(ByteList)};
enc_pdu(#ax_get_next{context = Ctx, srs = SRs}) ->
    {CtxF, {CtxL, CtxB}} = enc_ctx(Ctx),
    {Len, ByteList} = lists:foldl(fun(SR, {AccL, AccB}) ->
					  {L, B} = enc_sr(SR),
					  {L + AccL, [B | AccB]}
				  end, {CtxL, [CtxB]}, SRs),
    {CtxF, ?AX_GET_NEXT, Len, lists:reverse(ByteList)};
enc_pdu(#ax_get_bulk{context = Ctx, 
		     non_repeaters = NR, max_repetitions = MR,
		     srs = SRs}) ->
    {CtxF, {CtxL, CtxB}} = enc_ctx(Ctx),
    {Len, ByteList} = lists:foldl(fun(SR, {AccL, AccB}) ->
					  {L, B} = enc_sr(SR),
					  {L + AccL, [B | AccB]}
				  end, {0, []}, SRs),
    {CtxF, ?AX_GET_BULK, CtxL + 4 + Len,
     [CtxB, ?int16(NR), ?int16(MR) | lists:reverse(ByteList)]};
enc_pdu(#ax_test_set{context = Ctx, vbs = VBs}) ->
    {CtxF, {CtxL, CtxB}} = enc_ctx(Ctx),
    {Len, ByteList} = lists:foldl(fun(VB, {AccL, AccB}) ->
					  {L, B} = enc_vb(VB),
					  {L + AccL, [B | AccB]}
				  end, {CtxL, [CtxB]}, VBs),
    {CtxF, ?AX_TEST_SET, Len, lists:reverse(ByteList)};
enc_pdu(ax_commit_set) ->
    {0, ?AX_COMMIT_SET, 0, []};
enc_pdu(ax_undo_set) ->
    {0, ?AX_UNDO_SET, 0, []};
enc_pdu(ax_cleanup_set) ->
    {0, ?AX_CLEANUP_SET, 0, []};
enc_pdu(#ax_notify{context = Ctx, vbs = VBs}) ->
    {CtxF, {CtxL, CtxB}} = enc_ctx(Ctx),
    {Len, ByteList} = lists:foldl(fun(VB, {AccL, AccB}) ->
					  {L, B} = enc_vb(VB),
					  {L + AccL, [B | AccB]}
				  end, {CtxL, [CtxB]}, VBs),
    {CtxF, ?AX_NOTIFY, Len, lists:reverse(ByteList)};
enc_pdu(#ax_ping{context = Ctx}) ->
    {CtxF, {CtxL, CtxB}} = enc_ctx(Ctx),
    {CtxF, ?AX_PING, CtxL, CtxB};
enc_pdu(#ax_index_allocate{context = Ctx, vbs = VBs,
			   new_index = NI, any_index = AI}) ->
    {CtxF, {CtxL, CtxB}} = enc_ctx(Ctx),
    NIF = if NI -> ?AX_F_NEW_INDEX;
	     true -> 0
	    end,
    AIF = if AI -> ?AX_F_ANY_INDEX;
	     true -> 0
	    end,
    {Len, ByteList} = lists:foldl(fun(VB, {AccL, AccB}) ->
					  {L, B} = enc_vb(VB),
					  {L + AccL, [B | AccB]}
				  end, {CtxL, [CtxB]}, VBs),
    {CtxF bor NIF bor AIF, ?AX_INDEX_ALLOCATE, Len, lists:reverse(ByteList)};
enc_pdu(#ax_index_deallocate{context = Ctx, vbs = VBs}) ->
    {CtxF, {CtxL, CtxB}} = enc_ctx(Ctx),
    {Len, ByteList} = lists:foldl(fun(VB, {AccL, AccB}) ->
					  {L, B} = enc_vb(VB),
					  {L + AccL, [B | AccB]}
				  end, {CtxL, [CtxB]}, VBs),
    {CtxF, ?AX_INDEX_DEALLOCATE, Len, lists:reverse(ByteList)};
enc_pdu(#ax_add_agent_caps{context = Ctx, id = Id, descr = Descr}) ->
    {CtxF, {CtxL, CtxB}} = enc_ctx(Ctx),
    {IDL, IDB} = enc_oid(Id),
    {DescrL, DescrB} = enc_octet_str(Descr),
    {CtxF, ?AX_ADD_AGENT_CAPS, CtxL + IDL + DescrL, [CtxB, IDB, DescrB]};
enc_pdu(#ax_remove_agent_caps{context = Ctx, id = Id}) ->
    {CtxF, {CtxL, CtxB}} = enc_ctx(Ctx),
    {IDL, IDB} = enc_oid(Id),
    {CtxF, ?AX_ADD_AGENT_CAPS, CtxL + IDL, [CtxB, IDB]};
enc_pdu(#ax_response{sysUpTime = SysUpTime, error = Error, index = Idx}) ->
    {0, 8, <<SysUpTime:32, Error:16, Idx:16>>}.


enc_vb(#varbind{oid = OID, variabletype = VarType, value = Val}) ->
    {Type, {ValLen, ValBytes}} = enc_type_val(VarType, Val),
    {OIDLen, OIDBytes} = enc_oid(OID),
    {4 + OIDLen + ValLen, [<<Type:16, 0:16>>, OIDBytes, ValBytes]}.

enc_sr(#sr{oid1 = OID1, included = Inc, oid2 =  OID2}) ->
    {OID1L, OID1B} = enc_oid(OID1, Inc),
    {OID2L, OID2B} = enc_oid(OID2, 0),
    {OID1L + OID2L, [OID1B, OID2B]}.

enc_ctx([]) -> {0, {0, []}};
enc_ctx(Ctx) -> {?AX_F_NON_DEFAULT_CTXT, enc_octet_str(Ctx)}.
    

enc_oid(OID) ->
    enc_oid(OID, 0).
enc_oid([1,3,6,1,X|T], Inc) ->
    Len = length(T),
    {4 + Len*4, [<<Len:8, X:8, Inc:8, 0:8>>, enc_oid_subids(T)]};
enc_oid(OID, Inc) ->
    Len = length(OID),
    {4 + Len*4, [<<Len:8, 0:8, Inc:8, 0:8>>, enc_oid_subids(OID)]}.

enc_oid_subids([H|T]) ->
    [?int32(H) | enc_oid_subids(T)];
enc_oid_subids([]) ->
    [].

enc_octet_str(Str) ->
    Len = length(Str),
    Padding = case Len rem 4 of
		  0 -> [];
		  1 -> [0];
		  2 -> [0,0];
		  3 -> [0,0,0]
	      end,
    {4 + Len + (Len rem 4), [?int32(Len), Str, Padding]}.

enc_int32(V) ->
    {4, ?int32(V)}.

enc_int64(V) ->
    {8, ?int64(V)}.

enc_null() ->
    {0, []}.

enc_type_val('INTEGER', V) ->
    {?AX_INTEGER, enc_int32(V)};
enc_type_val('OCTET STRING', V) ->
    {?AX_OCTET_STRING, enc_octet_str(V)};
enc_type_val('OBJECT IDENTIFIER', V) ->
    {?AX_OBJECT_IDENTIFIER, enc_oid(V)};
enc_type_val('IpAddress', V) ->
    {?AX_IPADDRESS, enc_octet_str(V)};
enc_type_val('Counter32', V) ->
    {?AX_COUNTER32, enc_int32(V)};
enc_type_val('Gauge32', V) ->
    {?AX_GAUGE32, enc_int32(V)};
enc_type_val('TimeTicks', V) ->
    {?AX_TIMETICKS, enc_int32(V)};
enc_type_val('Opaque', V) ->
    {?AX_OPAQUE, enc_octet_str(V)};
enc_type_val('Counter64', V) ->
    {?AX_COUNTER64, enc_int64(V)};
enc_type_val('NULL', 'NULL') ->
    {?AX_NULL, enc_null()};
enc_type_val('NULL', noSuchObject) ->
    {?AX_NOSUCHOBJECT, enc_null()};
enc_type_val('NULL', noSuchInstance) ->
    {?AX_NOSUCHINSTANCE, enc_null()};
enc_type_val('NULL', endOfMibView) ->
    {?AX_ENDOFMIBVIEW, enc_null()}.


dec_msg(<<?AX_VSN_1:8, Type:8, Flags:8, _Rsvd:8, T0/binary>>) 
  when size(T0) >= 16 ->
    Endian = if ?tst_flag(?AX_F_NETWORK_BYTE_ORDER, Flags) -> big;
		true -> little
	     end,
    case Endian of
	big ->
	    <<SID:32, TID:32, PID:32, PduLen:32, T1/binary>> = T0;
	little ->
	    <<SID:32/little, TID:32/little, PID:32/little, PduLen:32/little,
	     T1/binary>> = T0
    end,
    if size(T1) /= PduLen ->
	    {error, {bad_pdu_len, PduLen, size(T1)}};
       true ->
	    Hdr = #ax_hdr{type = Type,
			  flags = Flags,
			  sessionID = SID,
			  transactionID = TID,
			  packetID = PID},
	    case catch dec_pdu(Type, Endian, Flags, T1) of
		{ok, Pdu} ->
		    {ok, #ax_msg{hdr = Hdr,
				 pdu = Pdu}};
		{'EXIT', R} ->
		    exit(R);
		Error ->
		    Error
	    end
    end;
dec_msg(<<?AX_VSN_1:8, _/binary>>) ->
    {error, bad_hdr};
dec_msg(_) ->
    {error, bad_vsn}.

dec_pdu(?AX_OPEN, E, Flags, <<TM:8, _Rsvd:24, T0/binary>>) ->
    {{Id, _Included}, T1} = dec_oid(E, T0),
    {Descr, <<>>} = dec_octet_str(E, T1),
    {ok, #ax_open{timeout = TM, id = Id, descr = Descr}};
dec_pdu(?AX_CLOSE, _E, Flags, <<Reason:8, _Rsvd:24>>) ->
    {ok, #ax_close{reason = Reason}};
dec_pdu(?AX_REGISTER, E, Flags, T0) ->
    NonDefCtx = ?tst_flag(?AX_F_NON_DEFAULT_CTXT, Flags),
    {Ctx, T1} = if NonDefCtx -> dec_octet_str(E, T0);
		   true -> {"", T0}
		end,
    <<Tm:8, Prio:8, RSubId:8, _Rsvd:8, T2/binary>> = T1,
    {{SubTree, _Included}, T3} = dec_oid(E, T2),
    Upper = if RSubId == 0 -> <<>> = T3,  0;
	       true -> <<U:32>> = T3, U
	    end,
    InstReg = ?tst_flag(?AX_F_INSTANCE_REGISTRATION, Flags),
    {ok, #ax_register{context = Ctx, timeout = Tm, priority = Prio,
		      range_subid = RSubId, subtree = SubTree,
		      upper_bound = Upper,
		      instance_registration = InstReg}};
dec_pdu(?AX_UNREGISTER, E, Flags, T0) ->
    NonDefCtx = ?tst_flag(?AX_F_NON_DEFAULT_CTXT, Flags),
    {Ctx, T1} = if NonDefCtx -> dec_octet_str(E, T0);
		   true -> {"", T0}
		end,
    <<_Rsvd0:8, Prio:8, RSubId:8, _Rsvd1:8, T2/binary>> = T1,
    {{SubTree, _Included}, T3} = dec_oid(E, T2),
    Upper = if RSubId == 0 -> <<>> = T3, 0;
	       true -> <<U:32>> = T3, U
	    end,
    {ok, #ax_unregister{context = Ctx, priority = Prio,
			range_subid = RSubId, subtree = SubTree,
			upper_bound = Upper}};
dec_pdu(?AX_GET, E, Flags, T0) ->
    NonDefCtx = ?tst_flag(?AX_F_NON_DEFAULT_CTXT, Flags),
    {Ctx, T1} = if NonDefCtx -> dec_octet_str(E, T0);
		   true -> {"", T0}
		end,
    SRs = dec_sr_list(E, T1),
    OIDs = lists:map(fun(#sr{oid1 = Oid}) -> Oid end, SRs),
    {ok, #ax_get{context = Ctx, oids = OIDs}};
dec_pdu(?AX_GET_NEXT, E, Flags, T0) ->
    NonDefCtx = ?tst_flag(?AX_F_NON_DEFAULT_CTXT, Flags),
    {Ctx, T1} = if NonDefCtx -> dec_octet_str(E, T0);
		   true -> {"", T0}
		end,
    SRs = dec_sr_list(E, T1),
    {ok, #ax_get_next{context = Ctx, srs = SRs}};
dec_pdu(?AX_GET_BULK, E, Flags, T0) ->
    NonDefCtx = ?tst_flag(?AX_F_NON_DEFAULT_CTXT, Flags),
    {Ctx, T1} = if NonDefCtx -> dec_octet_str(E, T0);
		   true -> {"", T0}
		end,
    {NR, T2} = dec_int16(E, T1),
    {MR, T3} = dec_int16(E, T2),
    SRs = dec_sr_list(E, T3),
    {ok, #ax_get_bulk{context = Ctx,
		      non_repeaters = NR, max_repetitions = MR,
		      srs = SRs}};
dec_pdu(?AX_TEST_SET, E, Flags, T0) ->
    NonDefCtx = ?tst_flag(?AX_F_NON_DEFAULT_CTXT, Flags),
    {Ctx, T1} = if NonDefCtx -> dec_octet_str(E, T0);
		   true -> {"", T0}
		end,
    VBs = dec_vb_list(E, T1),
    {ok, #ax_test_set{context = Ctx, vbs = VBs}};
dec_pdu(?AX_COMMIT_SET, _E, _Flags, <<>>) ->
    {ok, ax_commit_set};
dec_pdu(?AX_UNDO_SET, _E, _Flags, <<>>) ->
    {ok, ax_undo_set};
dec_pdu(?AX_CLEANUP_SET, _E, _Flags, <<>>) ->
    {ok, ax_cleanup_set};
dec_pdu(?AX_NOTIFY, E, Flags, T0) ->
    NonDefCtx = ?tst_flag(?AX_F_NON_DEFAULT_CTXT, Flags),
    {Ctx, T1} = if NonDefCtx -> dec_octet_str(E, T0);
		   true -> {"", T0}
		end,
    VBs = dec_vb_list(E, T1),
    {ok, #ax_notify{context = Ctx, vbs = VBs}};
dec_pdu(?AX_PING, E, Flags, T0) ->
    NonDefCtx = ?tst_flag(?AX_F_NON_DEFAULT_CTXT, Flags),
    {Ctx, T1} = if NonDefCtx -> dec_octet_str(E, T0);
		   true -> {"", T0}
		end,
    {ok, #ax_ping{context = Ctx}};    
dec_pdu(?AX_INDEX_ALLOCATE, E, Flags, T0) ->
    NonDefCtx = ?tst_flag(?AX_F_NON_DEFAULT_CTXT, Flags),
    {Ctx, T1} = if NonDefCtx -> dec_octet_str(E, T0);
		   true -> {"", T0}
		end,
    VBs = dec_vb_list(E, T1),
    NI = ?tst_flag(?AX_F_NEW_INDEX, Flags),
    AI = ?tst_flag(?AX_F_ANY_INDEX, Flags),
    {ok, #ax_index_allocate{context = Ctx, vbs = VBs,
			    new_index = NI, any_index = AI}};
dec_pdu(?AX_INDEX_DEALLOCATE, E, Flags, T0) ->
    NonDefCtx = ?tst_flag(?AX_F_NON_DEFAULT_CTXT, Flags),
    {Ctx, T1} = if NonDefCtx -> dec_octet_str(E, T0);
		   true -> {"", T0}
		end,
    VBs = dec_vb_list(E, T1),
    {ok, #ax_index_deallocate{context = Ctx, vbs = VBs}};
dec_pdu(?AX_ADD_AGENT_CAPS, E, Flags, T0) ->
    NonDefCtx = ?tst_flag(?AX_F_NON_DEFAULT_CTXT, Flags),
    {Ctx, T1} = if NonDefCtx -> dec_octet_str(E, T0);
		   true -> {"", T0}
		end,
    {{Id, _Included}, T2} = dec_oid(E, T1),
    {Descr, <<>>} = dec_octet_str(E, T2),
    {ok, #ax_add_agent_caps{context = Ctx, id = Id, descr = Descr}};
dec_pdu(?AX_REMOVE_AGENT_CAPS, E, Flags, T0) ->
    NonDefCtx = ?tst_flag(?AX_F_NON_DEFAULT_CTXT, Flags),
    {Ctx, T1} = if NonDefCtx -> dec_octet_str(E, T0);
		   true -> {"", T0}
		end,
    {{Id, _Included}, <<>>} = dec_oid(E, T1),
    {ok, #ax_remove_agent_caps{context = Ctx, id = Id}};
dec_pdu(?AX_RESPONSE, E, Flags, T0) ->
    case E of
	big ->
	    <<SysUpTime:32, Error:16, Index:16>> = T0,
	    {ok, #ax_response{sysUpTime = SysUpTime,
			      error = Error, index = Index}};
	little ->
	    <<SysUpTime:32/little, Error:16/little, Index:16/little>> = T0,
	    {ok, #ax_response{sysUpTime = SysUpTime,
			      error = Error, index = Index}}
    end.
    

%% Ret: {{[int()], Included}, Rest}
dec_oid(E, <<N:8, Prefix:8, Inc:8, _Rsvd:8, Sub:N/binary-unit:32,T0/binary>>) ->
    PreL = if Prefix /= 0 -> [1,3,6,1,Prefix];
	      Prefix == 0 -> []
	   end,
    L = dec_sub_ids(E, Sub),
    {{PreL ++ L, Inc}, T0};
dec_oid(_E, X) ->
    throw({error, {bad_oid, X}}).

dec_sub_ids(big, T) -> dec_sub_ids_big(T);
dec_sub_ids(little, T) -> dec_sub_ids_little(T).
dec_sub_ids_big(<<Sub:32, T/binary>>) ->
    [Sub | dec_sub_ids_big(T)];
dec_sub_ids_big([]) ->
    [].
dec_sub_ids_little(<<Sub:32/little, T/binary>>) ->
    [Sub | dec_sub_ids_little(T)];
dec_sub_ids_little([]) ->
    [].

dec_octet_str(big, <<L:32, Str:L/binary, T/binary>>) ->
    {binary_to_list(Str), strip_padding(L, T)};
dec_octet_str(little, <<L:32/little, Str:L/binary, T/binary>>) ->
    {binary_to_list(Str), strip_padding(L, T)}.

strip_padding(L, T) ->
    do_strip_padding(L rem 4, T).
do_strip_padding(0, T) -> T;
do_strip_padding(1, <<_:8, T/binary>>) -> T;
do_strip_padding(2, <<_:16, T/binary>>) -> T;
do_strip_padding(3, <<_:24, T/binary>>) -> T.

dec_sr_list(E, <<>>) ->
    [];
dec_sr_list(E, T0) ->
    {Sr, T1} = dec_sr(E, T0),
    [Sr | dec_sr_list(E, T1)].

dec_sr(E, T0) ->
    {{Oid1, Included}, T1} = dec_oid(E, T0),
    {{Oid2, _}, T2} = dec_oid(E, T1),
    {#sr{oid1 = Oid1, included = Included, oid2 = Oid2}, T2}.

dec_int16(big, <<V:16, T/binary>>) ->
    {V, T};
dec_int16(little, <<V:16/little, T/binary>>) ->
    {V, T}.

dec_int32(big, <<V:32, T/binary>>) ->
    {V, T};
dec_int32(little, <<V:32/little, T/binary>>) ->
    {V, T}.

dec_int64(big, <<V:64, T/binary>>) ->
    {V, T};
dec_int64(little, <<V:64/little, T/binary>>) ->
    {V, T}.

dec_vb_list(E, T) ->
    dec_vb_list(E, T, 1).
dec_vb_list(E, <<>>, _N) ->
    [];
dec_vb_list(E, T0, N) ->
    {Vb, T1} = dec_vb(E, T0),
    [Vb#varbind{org_index = N} | dec_vb_list(E, T1, N+1)].

dec_vb(big, <<Type:16, _Rsvd:16, T0/binary>>) ->
    {{Oid, _Included}, T1} = dec_oid(big, T0),
    {VarType, {Val, T2}} = dec_type_val(Type, big, T1),
    {#varbind{oid = Oid, variabletype = VarType, value = Val}};
dec_vb(little, <<Type:16/little, _Rsvd:16/little, T0/binary>>) ->
    {{Oid, _Included}, T1} = dec_oid(little, T0),
    {VarType, {Val, T2}} = dec_type_val(Type, little, T1),
    {#varbind{oid = Oid, variabletype = VarType, value = Val}}.

dec_type_val(?AX_INTEGER, E, T0) ->
    {'INTEGER', dec_int32(E, T0)};
dec_type_val(?AX_OCTET_STRING, E, T0) ->
    {'OCTET STRING', dec_octet_str(E, T0)};
dec_type_val(?AX_OBJECT_IDENTIFIER, E, T0) ->
    {{Oid, _}, T1} = dec_oid(E, T0),
    {'OBJECT IDENTIFIER', {Oid, T1}};
dec_type_val(?AX_IPADDRESS, _E, <<A,B,C,D, T0/binary>>) ->
    {'IpAddress', {[A,B,C,D], T0}};
dec_type_val(?AX_COUNTER32, E, T0) ->
    {'Counter32', dec_int32(E, T0)};
dec_type_val(?AX_GAUGE32, E, T0) ->
    {'Gauge32', dec_int32(E, T0)};
dec_type_val(?AX_OPAQUE, E, T0) ->
    {'Opaque', dec_octet_str(E, T0)};
dec_type_val(?AX_TIMETICKS, E, T0) ->
    {'TimeTicks', dec_int32(E, T0)};
dec_type_val(?AX_COUNTER64, E, T0) ->
    {'Counter64', dec_int64(E, T0)};
dec_type_val(?AX_NULL, _E, T0) ->
    {'NULL', {'NULL', T0}};
dec_type_val(?AX_NOSUCHOBJECT, _E, T0) ->
    {'NULL', {noSuchObject, T0}};
dec_type_val(?AX_NOSUCHINSTANCE, _E, T0) ->
    {'NULL', {noSuchInstance, T0}};
dec_type_val(?AX_ENDOFMIBVIEW, _E, T0) ->
    {'NULL', {endOfMibView, T0}}.

