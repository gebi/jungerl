-module(eradius_lib).
%%%-------------------------------------------------------------------
%%% File        : eradius_lib.erl
%%% Author      : Martin Bjorklund <mbj@bluetail.com>
%%% Description : Radius encode/decode routines (RFC-2865).
%%% Created     :  7 Oct 2002 by Martin Bjorklund <mbj@bluetail.com>
%%%
%%% $Id$
%%%-------------------------------------------------------------------
-export([enc_pdu/1, dec_packet/1, enc_accreq/3]).
-export([mk_authenticator/0, mk_password/3]).

-export([dec_packet/1]).  %% useful when debugging

-include("eradius_lib.hrl").
-include("eradius_dict.hrl").
-include("dictionary.hrl").

%%====================================================================
%% Create Attributes
%%====================================================================

%%% Generate an unpredictable 16 byte token.   FIXME !!
mk_authenticator() ->
    {_, A2, A3} = now(),
    A = erlang:phash(node(), A2),
    B = erlang:phash(A2, A3),
    erlang:md5(<<A3, A2, A, B>>).

mk_password(Secret, Auth, Passwd) ->
    scramble(Secret, Auth, Passwd).

scramble(Secret, Auth, Passwd) ->
    B = erlang:md5([Secret, Auth]),
    case xor16(Passwd, B) of
	{C, <<>>}   -> C;
	{C, Tail} -> concat_binary([C, scramble(Secret, C, Tail)])
    end.

xor16(Passwd, B) when size(Passwd) < 16 ->
    xor16(pad16(Passwd), B);
xor16(<<P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,T/binary>>,
      <<B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15,B16>>) ->
    {<<(P1 bxor B1),
       (P2  bxor B2),
       (P3  bxor B3),
       (P4  bxor B4),
       (P5  bxor B5),
       (P6  bxor B6),
       (P7  bxor B7),
       (P8  bxor B8),
       (P9  bxor B9),
       (P10 bxor B10),
       (P11 bxor B11),
       (P12 bxor B12),
       (P13 bxor B13),
       (P14 bxor B14),
       (P15 bxor B15),
       (P16 bxor B16)>>,
     T}.

pad16(Passwd) ->
    concat_binary([Passwd, list_to_binary(zero(16 - size(Passwd)))]).

zero(0) -> [];
zero(N) -> [0 | zero(N-1)].


%%====================================================================
%% Encode/Decode Functions
%%====================================================================

%% Ret: io_list()
enc_pdu(Pdu) ->
    {Cmd, CmdPdu} = enc_cmd(Pdu#rad_pdu.cmd),
    [<<Cmd:8, (Pdu#rad_pdu.reqid):8, (io_list_len(CmdPdu) + 20):16, 
      (Pdu#rad_pdu.authenticator):16/binary>>,
     CmdPdu].

enc_attrib(Pos, R, Def, AttrName, Type) ->
    V = element(Pos, R),
    if  V == element(Pos, Def) ->
	    [];
	true -> 
	    Val =
		case Type of
		    binary -> V;
		    int32 -> <<V:32>>;
		    ip -> {A,B,C,D} = V,
			  <<A:8, B:8, C:8, D:8>>
	        end,
	    <<AttrName, (size(Val) + 2):8, Val/binary>>
    end.
			 
enc_cmd(R) when record(R, rad_request) ->
    Def = #rad_request{},
    {?RAccess_Request,
     [enc_attrib(#rad_request.user,      R, Def, ?RUser_Name,       binary),
      enc_attrib(#rad_request.passwd,    R, Def, ?RUser_Passwd,     binary),
      enc_attrib(#rad_request.nas_ip,    R, Def, ?RNAS_Ip_Address,  ip),
      enc_attrib(#rad_request.state,     R, Def, ?RState,           binary)
     ]};
enc_cmd(R) when record(R, rad_accept) ->
    Def = #rad_accept{},
    {?RAccess_Accept,
     [enc_attrib(#rad_accept.user,        R, Def, ?RUser_Name,       binary),
      lists:map(fun(RM) ->
			<<?RVendor_Specific:8, (size(RM)+2):8, RM/binary>>
		end,
		R#rad_accept.vendor_specifics)]
    };
enc_cmd(R) when record(R, rad_challenge) ->
    Def = #rad_challenge{},
    {?RAccess_Challenge,
     [enc_attrib(#rad_challenge.state,   R, Def, ?RState,           binary),
      lists:map(fun(RM) -> <<?RReply_Msg:8, (size(RM)+2):8, RM/binary>> end,
		R#rad_challenge.reply_msgs)]
    };
enc_cmd(R) when record(R, rad_reject) ->
    {?RAccess_Reject,
     lists:map(fun(RM) -> <<?RReply_Msg:8, (size(RM)+2):8, RM/binary>> end,
	       R#rad_reject.reply_msgs)
    };
enc_cmd(R) when record(R, rad_accreq) ->
    Def = #rad_accreq{},
    {?RAccounting_Request,
     [enc_attrib(#rad_accreq.status_type, R, Def, ?RStatus_Type,     int32),
      enc_attrib(#rad_accreq.session_time,R, Def, ?RSession_Time,    int32),
      enc_attrib(#rad_accreq.session_id,  R, Def, ?RSession_Id,      binary),
      enc_attrib(#rad_accreq.term_cause,  R, Def, ?RTerminate_Cause, int32),
      enc_attrib(#rad_accreq.user,        R, Def, ?RUser_Name,       binary),
      enc_attrib(#rad_accreq.nas_ip,      R, Def, ?RNAS_Ip_Address,  ip),
      enc_vendor_info(R,Def)
     ]}. 

enc_vendor_info(R,Def) ->
    case {R#rad_accreq.vend_attrs,Def#rad_accreq.vend_attrs} of
	{X,X}  -> [];
	{As,_} ->
	    Vid = R#rad_accreq.vend_id,
	    {Vbin, Size} = enc_vendor_attrs(As),
	    Tsz = Size + 6,
	    [<<?RVendor_Specific:8, Tsz:8, Vid:32 >> | Vbin]
    end.

enc_vendor_attrs(Vas) ->
    F = fun({Type,Bin}, {Acc,Size}) ->
		Z = size(Bin) + 2,
		{[<<Type:8,Z:8,Bin/binary>> | Acc],
		  Size + Z}
	end,
    lists:foldl(F, {[],0}, Vas).


io_list_len(L) -> io_list_len(L, 0).
io_list_len([H|T], N) ->
    if
	H >= 0, H =< 255 -> io_list_len(T, N+1);
	list(H) -> io_list_len(T, io_list_len(H,N));
	binary(H) -> io_list_len(T, size(H) + N)
    end;
io_list_len(H, N) when binary(H) ->
    size(H) + N;
io_list_len([], N) ->
    N.

%% Ret: #rad_pdu | Reason
dec_packet(Packet) ->
    case catch dec_packet0(Packet) of
	{'EXIT', _R} ->
	    io:format("_R = ~p~n",[_R]),
	    bad_pdu;
	Else ->
	    Else
    end.

dec_packet0(Packet) ->
    <<Cmd:8, ReqId:8, Len:16, Auth:16/binary, Attribs0/binary>> = Packet,
    Size = size(Attribs0),
    Attr_len = Len - 20,
    Attribs = 
	if 
	    Attr_len > Size -> 
		throw(bad_pdu);
	    Attr_len == Size -> 
		Attribs0;
	    true ->
		<<Attribs1:Attr_len/binary, _/binary>> = Attribs0,
		Attribs1
	end,
    P = #rad_pdu{reqid = ReqId, authenticator = Auth},
    case Cmd of
	?RAccess_Request ->
	    P#rad_pdu{cmd = {request, dec_attributes(Attribs)}};
	?RAccess_Accept ->
	    P#rad_pdu{cmd = {accept, dec_attributes(Attribs)}};
	?RAccess_Challenge ->
	    P#rad_pdu{cmd = {challenge, dec_attributes(Attribs)}};
	?RAccess_Reject ->
	    P#rad_pdu{cmd = {reject, dec_attributes(Attribs)}};
	?RAccounting_Request ->
	    P#rad_pdu{cmd = {accreq, dec_attributes(Attribs)}};
	?RAccounting_Response ->
	    P#rad_pdu{cmd = {accresp, dec_attributes(Attribs)}}
    end.

-define(dec_attrib(A0, Type, Val, A1),
	<<Type:8, __Len0:8, __R/binary>> = A0,
	__Len1 = __Len0 - 2,
	<<Val:__Len1/binary, A1/binary>> = __R).

 
dec_attributes(As) -> 
    dec_attributes(As, []).

dec_attributes(<<>>, Acc) -> Acc;
dec_attributes(A0, Acc) ->
    ?dec_attrib(A0, Type, Val, A1),
    case eradius_dict:lookup(Type) of
	[A] when record(A, attribute) ->
	    dec_attributes(A1, dec_attr_val(A,Val) ++ Acc);
	_ ->
	    dec_attributes(A1, [{Type, Val} | Acc])
    end.

dec_attr_val(A, Bin) when A#attribute.type == string -> 
    [{A, binary_to_list(Bin)}];
dec_attr_val(A, <<I/integer>>) when A#attribute.type == integer -> 
    [{A, I}];
dec_attr_val(A, <<B,C,D,E>>) when A#attribute.type == ipaddr -> 
    [{A, {B,C,D,E}}];
dec_attr_val(A, Bin) when A#attribute.type == octets -> 
    case A#attribute.id of
	?Vendor_Specific ->
	    <<VendId:32/integer, VendVal/binary>> = Bin,
	    dec_vend_attr_val(VendId, VendVal);
	_ ->
	    [{A, Bin}]
    end;
dec_attr_val(A, Val) -> 
    io:format("Uups...A=~p~n",[A]),
    {A, Val}.

dec_vend_attr_val(_VendId, <<>>) -> [];
dec_vend_attr_val(VendId, <<Vtype:8, Vlen:8, Vbin/binary>>) ->
    Len = Vlen - 2,
    <<Vval:Len/binary,Vrest/binary>> = Vbin,
    Vkey = {VendId,Vtype},
    case eradius_dict:lookup(Vkey) of
	A when record(A, attribute) ->
	    dec_attr_val(Vkey, Vval) ++ dec_vend_attr_val(VendId, Vrest);
	_ ->
	    [{Vkey,Vval} | dec_vend_attr_val(VendId, Vrest)]
    end.
    

%%% ====================================================================
%%% Radius Accounting specifics
%%% ====================================================================

enc_accreq(Id, Secret, Req) ->
    Rpdu = #rad_pdu{reqid = Id,
		    authenticator = zero16(),
		    cmd = Req},
    PDU = enc_pdu(Rpdu),
    patch_authenticator(PDU, l2b(Secret)).

patch_authenticator(Req,Secret) ->
    case {erlang:md5([Req,Secret]),concat_binary(Req)} of
	{Auth,<<Head:4/binary, _:16/binary, Rest/binary>>} ->
	    B = l2b(Auth),
	    <<Head/binary, B/binary, Rest/binary>>;
	_Urk ->
	    exit(patch_authenticator)
    end.

%%% An empty Acc-Req Authenticator
zero16() ->
    zero_bytes(16).

zero_bytes(N) ->
    <<0:N/?BYTE>>.

l2b(L) when list(L)   -> list_to_binary(L);
l2b(B) when binary(B) -> B.
