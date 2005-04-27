%%%-------------------------------------------------------------------
%%% Created : 21 Apr 2005 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Desc.   : Collection of NetBIOS related code
%%%
%%% $Id$
%%%-------------------------------------------------------------------
-module(esmb_netbios).

-export([called_name/1, calling_name/1, nbss_session_request/3,
	 nbss_session_service/2, nbss_datagram_service/2,
	 nbss_src_name/1, nbss_dst_name/1, nb_dec_name/1,
	 tt_ename/1, tt_dname/1, dec_nbns/2, positive_query_response_pdu/2,
	 dec_nbss_datagram/1]).

-import(esmb, [b2l/1, ip2bin/1, bin2ip/1, ip2str/1, lcase/1, sizeof/1]).

-include("esmb_lib.hrl").
-include("esmb_netbios.hrl").


%%% --------------------------------------------------------------------
%%% NetBIOS code
%%% --------------------------------------------------------------------

nbss_session_request(S, Called, Calling) ->
    send_recv(S, nbss_session_request_pdu(Called, Calling)).

nbss_session_service(S, SMB_pdu) ->
    send_recv(S, nbss_session_service_pdu(SMB_pdu)).

nbss_datagram_service(Dgm, Pdu) ->
    Res = gen_udp:send(Dgm#netbios_dgm.sock, 
		       "192.168.128.255",
		       ?NETBIOS_DGM_PORT,
		       nbss_datagram_pdu(Dgm, Pdu)),
    Res.


dec_nbns(<<Tid:16,                 % Tid
	  0:1,                     % Op(0)==Req, Op(1)==Resp
	  0:4,                     % Query(0),Registration(5),Release(6),WACK(7),Refresh(8)
	  0:1,                     % Authorative Answer flag
	  0:1,                     % Truncation flag
	  1:1,                     % Recursion Desired flag
	  0:1,                     % Recursion Available flag
	  0:1,
	  0:1,
	  BC:1,                    % Broadcast flag
	  0:4,                     % Return Code
	  1:16,                    % Questions
	  0:16,                    % Answer
	  0:16,                    % Authority
	  0:16,                    % Additional
	  QName:34/binary,
	  16#0020:16,              % Type: NB
	  16#0001:16>>,            % Class: IN
	 Ns) ->
    {ok, Ns#nbt{op   = ?NS_QUERY_REQ,
		tid  = Tid,
		name = element(1,nb_dec_name(QName))}};% FIXME
dec_nbns(Bin, _Ns) ->
    Bin.  % NYI !!
   

dec_msg(<<?POSITIVE_SESSION_RESPONSE,Flags,Length:16>>) ->
    {ok, ?POSITIVE_SESSION_RESPONSE};
dec_msg(<<?SESSION_SERVICE, _, Length:16, SMB_pdu/binary>>) ->
    {ok, ?SESSION_SERVICE, get_more(Length, sizeof(SMB_pdu), [SMB_pdu])};
dec_msg(<<?SESSION_KEEP_ALIVE, _/binary>>) ->
    {ok, ?SESSION_KEEP_ALIVE};
dec_msg(<<?NEGATIVE_SESSION_RESPONSE,Flags,Length:16,Ecode>>) ->
    Emsg =  neg_sess_resp(Ecode),
    {error, neg_sess_resp(Ecode)};
dec_msg(Bin) ->
    ?elog("dec_msg got: ~p~n",[Bin]),
    {error, Bin}.

get_more(Expected, Got, Bins) when Got < Expected ->
    receive 
	{tcp,_,Bin} ->
	    get_more(Expected, Got + size(Bin), [Bin | Bins])
    end;
get_more(_, _, Bins) ->
    concat_binary(lists:reverse(Bins)).

neg_sess_resp(16#80) -> "Not listening on called name";
neg_sess_resp(16#81) -> "Not listening for calling name";
neg_sess_resp(16#82) -> "Called name not present";
neg_sess_resp(16#83) -> "Called name present, but insufficient resources";
neg_sess_resp(16#8F) -> "Unspecified error";
neg_sess_resp(_)     -> "Unknown error code".



positive_query_response_pdu(Ns, OurName) ->
    Tid = Ns#nbt.tid,
    Trunc = 0, % FIXME , hardcoded for now..
    Bcast = 0,
    QName = nbss_src_name(OurName),
    TTL = 3600, % seconds  FIXME , hardcoded for now..
    G = 0,
    ONT = 0,
    Ip = ip2bin("192.168.128.32"),   % FIXME , hardcoded for now..
    <<Tid:16,                 % Tid
     1:1,                     % Op(0)==Req, Op(1)==Resp
     0:4,                     % Query(0),Registration(5),Release(6),WACK(7),Refresh(8)
     1:1,                     % Authorative Answer flag
     Trunc:1,                 % Truncation flag
     1:1,                     % Recursion Desired flag
     0:1,                     % Recursion Available flag
     0:1,
     0:1,
     Bcast:1,                 % Broadcast flag
     0:4,                     % Return Code
     0:16,                    % Questions
     1:16,                    % Answer
     0:16,                    % Authority
     0:16,                    % Additional
     QName:34/binary,
     16#0020:16,              % Type: NB
     16#0001:16,              % Class: IN
     TTL:32,
     6:16,                    % RDLEN , length of following data
     %% G(0): QName is a unique Netbios name
     %% G(1): QName is a Group Netbios name
     G:1,                     
     ONT:2,                   % Owner Node Type, 0==B-node
     0:13,                    % Reserved 
     Ip:4/binary>>.
		

nbss_session_request_pdu(Called, Calling) ->
    CalledName = called_name(Called),
    CallingName = calling_name(Calling) ,
    Length = size(CalledName) + size(CallingName),
    <<?SESSION_REQUEST, 0, Length:16, CalledName/binary, CallingName/binary>>.

nbss_session_service_pdu(SMB_pdu) when binary(SMB_pdu) ->
    Length = size(SMB_pdu),
    <<?SESSION_SERVICE, 0, Length:16, SMB_pdu/binary>>.

%%%
%%% NetBIOS Datagram handling
%%%

-define(IP_HDR_SIZE,   20).
-define(UDP_HDR_SIZE,   8).
-define(DGM_MAX_SIZE, 576).

nbss_datagram_pdu(Dgm, SMB_pdu) when binary(SMB_pdu) ->
    MsgType = Dgm#netbios_dgm.msg_type,
    Flags   = Dgm#netbios_dgm.flags,
    DgmId   = Dgm#netbios_dgm.dgm_id,
    SrcIp   = ip2bin(Dgm#netbios_dgm.src_ip),
    SrcPort = Dgm#netbios_dgm.src_port,
    SrcName = Dgm#netbios_dgm.src_name,
    DstName = Dgm#netbios_dgm.dst_name,
    %% DgmLen = length of data + length of second level encoded
    %%          source and destination names;
    DgmLen  = size(SMB_pdu) + size(SrcName) + size(DstName),
    %% OFFSET in 2nd UDP = DGM_LENGTH - number of name and
    %%                     data bytes in 1st UDP;
    Offset  = 0,
    if ((?IP_HDR_SIZE + ?UDP_HDR_SIZE + DgmLen) > ?DGM_MAX_SIZE) ->
	    ?elog("Datagram too big, need to be fragmented!~n",[]),
	    exit("nbss_datagram_service_pdu");  % FIXME !!
       true ->
	    <<MsgType,
	     Flags,
	     DgmId:16,
	     SrcIp/binary,
	     SrcPort:16,
	     DgmLen:16,
	     Offset:16,
	     SrcName/binary,
	     DstName/binary,
	     SMB_pdu/binary>>
    end.
     
dec_nbss_datagram(<<MsgType,
		   Flags,
		   DgmId:16,
		   SrcIp:4/binary,
		   SrcPort:16,
		   DgmLen:16,
		   Offset:16,
		   SrcName:34/binary,
		   DstName:34/binary,
		   Data/binary>>) ->
    Dgm = #netbios_dgm{msg_type  = MsgType,
		       flags     = Flags,
		       dgm_id    = DgmId,
		       src_ip    = bin2ip(SrcIp),
		       src_port  = SrcPort,
		       src_name  = element(1, nb_dec_name(SrcName)),  % FIXME
		       dst_name  = element(1, nb_dec_name(DstName)),  % FIXME
		       data      = Data},
    {ok, Dgm};
dec_nbss_datagram(Bin) ->
    ?elog("dec_nbss_datagram failed: ~p~n", [Bin]),
    {error, "dec_nbss_datagram"}.


     

%%% The NetBIOS naming convention allows for 16 character in a 
%%% NetBIOS name. Microsoft, however, limits NetBIOS names to 15
%%% characters and uses the 16th character as a NetBIOS suffix in
%%% order to identify functionality installed on the registered device.


nbss_src_name({A,B,C,D} = IP) -> nbss_src_name(ip2str(IP));
nbss_src_name(Name) when length(Name) =< ?NETBIOS_NAME_LEN -> 
    nb_name(Name, ?NETBIOS_SX_WORKSTATION).

nbss_dst_name({A,B,C,D} = IP) -> nbss_dst_name(ip2str(IP));
nbss_dst_name(Name) when length(Name) =< ?NETBIOS_NAME_LEN -> 
    nb_name(Name, ?NETBIOS_LOCAL_MASTER_BROWSER).

%%% @private
called_name({A,B,C,D} = IP) -> called_name(ip2str(IP));
called_name(Name) when length(Name) =< ?NETBIOS_NAME_LEN -> 
    nb_name(Name, ?NETBIOS_SX_FILESERVER).

%%% @private
calling_name({A,B,C,D} = IP) -> calling_name(ip2str(IP));
calling_name(Name) when length(Name) =< ?NETBIOS_NAME_LEN -> 
    nb_name(Name, ?NETBIOS_SX_WORKSTATION).

%%% Level 1 encoding
nb_name(Name, Sx) ->
    Len = 32,
    list_to_binary([Len | l1enc(Name, Sx, 0)]).

%%% Level 1 decoding
nb_dec_name(<<Len, Name/binary>>) when binary(Name), size(Name)==33 ->
    [Sx|Rname] = lists:reverse(b2l(l1dec(Name))),
    {lists:reverse(rm_space(Rname)), Sx}.

rm_space([$\s|T]) -> rm_space(T);
rm_space(L)       -> L.

%%% test routine
tt_ename(Name) ->
    l1enc(Name, $A, 0).
tt_dname(Name) ->
    nb_dec_name(Name).

-define(SPACE, 16#20).

l1enc([H|T], Sx, N) when N < ?NETBIOS_NAME_LEN ->
    [l1msn(H),l1lsn(H)|l1enc(T, Sx, N+1)];
l1enc([], Sx, N) when N < ?NETBIOS_NAME_LEN ->
    [l1msn(?SPACE),l1lsn(?SPACE)|l1enc([], Sx, N+1)];
l1enc([], Sx, ?NETBIOS_NAME_LEN) ->
    [l1msn(Sx),l1lsn(Sx),0].

%%% Level 1 encoding, get most significant nibble
l1msn(B) -> (B bsr 4) + $A.

%%% Level 1 encoding, get least significant nibble
l1lsn(B) -> (B band 16#0F) + $A.



l1dec(<<M, L, B/binary>>) ->
    Dec = l1dec(B),
    <<(M-$A):4, (L-$A):4, Dec/binary>>;
l1dec(<<0>>) ->
    <<>>.


lcase_host(T) when tuple(T) -> T;
lcase_host(L) when list(L)  -> lcase(L).


send_recv(S, Packet) ->
    gen_tcp:send(S, [Packet]),
    recv(S).

recv(S) ->
    receive 
	{tcp,S,Bin} ->
	    case dec_msg(Bin) of
		{ok, ?SESSION_KEEP_ALIVE} ->
		    recv(S);
		Else ->
		    Else
	    end;

	{tcp,_,Bin} ->
	    %%?elog("recv: ignoring stale tcp msg~n",[]),
	    recv(S);
	    
	{tcp_closed,S} ->
	    %%?elog("recv: port closed~n",[]),
	    exit(closed);
	    
	{tcp_closed,_} ->
	    %%?elog("recv: ignoring stale port closed msg~n",[]),
	    recv(S);
	    
	Else ->
	    %%?elog("recv: got unexpected msg: ~p~n",[Else]),
	    gen_tcp:close(S),
	    exit(Else)
    end.

