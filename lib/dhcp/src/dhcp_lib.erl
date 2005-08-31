-module(dhcp_lib).
%%%-------------------------------------------------------------------
%%% Created : 18 May 2005 by Tobbe <tobbe@bluetail.com>
%%% Desc.   : Encode/Decode routines for DHCP.
%%%           See also: RFC 2132,1533
%%%
%%% $Id$
%%%-------------------------------------------------------------------
-export([enc/1, dec/1, get_opt/2, ip2str/1]).

-include("../include/dhcp_lib.hrl").

-define(BYTE, integer-unit:8).    % Nice syntactic sugar...

-define(elog(X,Y), error_logger:info_msg("*elog ~p:~p: " X,
					 [?MODULE, ?LINE | Y])).




dec(<<Op,
      Htype,
      Hlen,
      Hops,
      Xid:32,
      Secs:16,
      Flags:16,
      Ciaddr:4/binary,
      Yiaddr:4/binary,
      Siaddr:4/binary,
      Giaddr:4/binary,
      Bchaddr:16/binary,
      Bsname:64/binary,
      Bfile:128/binary,
      Bopts/binary>> ) ->
    Opts = dec_options(Bopts),
    #dhcp{op      = Op,
	  htype   = Htype,
	  hlen    = Hlen,
	  hops    = Hops,
	  xid     = Xid,
	  secs    = Secs,
	  flags   = Flags,
	  ciaddr  = bin2ip(Ciaddr),
	  yiaddr  = bin2ip(Yiaddr),
	  siaddr  = bin2ip(Siaddr),
	  giaddr  = bin2ip(Giaddr),
	  chaddr  = Bchaddr,
	  sname   = Bsname,
	  file    = Bfile,
	  options = Opts,
	  msg_type= get_msg_type(Opts)}.

get_msg_type(Opts) ->
    case get_opt(?DHCP_OP_MSGTYPE, Opts) of
	{ok, MsgType} -> MsgType;
	_             -> 0
    end.

get_opt(Tag, [{Tag, X}|_]) -> {ok, X};
get_opt(Tag, [_|T])        -> get_opt(Tag, T);
get_opt(_, [])             -> {error, "not_found"}. 

dec_options(<<99,130,83,99, Opts/binary>>) ->
    %%?elog("dec_options: ~p~n", [Opts]),
    case catch dec_opt(Opts) of
	{'EXIT', Reason}    -> 
	    ?elog("dec_options: Warning decoding crashed: ~p~n", [Reason]),
	    [];
	L when list(L) -> L
    end.


dec_opt(<<?DHCP_OP_END, _/binary>>) ->
    [];
%%
dec_opt(<<?DHCP_OP_MSGTYPE, 1, X, B/binary>>) ->
    [{?DHCP_OP_MSGTYPE, X} | dec_opt(B)];
%%
dec_opt(<<?DHCP_OP_VENDOR, N, X:N/binary, B/binary>>) ->
    [{?DHCP_OP_VENDOR, X} | dec_opt(B)];
%%
dec_opt(<<?DHCP_OP_SUBNET_MASK, 4, X:4/binary, B/binary>>) ->
    [{?DHCP_OP_SUBNET_MASK, bin2ip(X)} | dec_opt(B)];
%%
dec_opt(<<?DHCP_OP_BCAST_ADDR, 4, X:4/binary, B/binary>>) ->
    [{?DHCP_OP_BCAST_ADDR, bin2ip(X)} | dec_opt(B)];
%%
dec_opt(<<?DHCP_OP_SRV_ID, 4, X:4/binary, B/binary>>) ->
    [{?DHCP_OP_SRV_ID, bin2ip(X)} | dec_opt(B)];
%%
dec_opt(<<?DHCP_OP_RENEWAL_TIME, 4, X:32, B/binary>>) ->
    [{?DHCP_OP_RENEWAL_TIME, X} | dec_opt(B)];
%%
dec_opt(<<?DHCP_OP_REBINDING_TIME, 4, X:32, B/binary>>) ->
    [{?DHCP_OP_REBINDING_TIME, X} | dec_opt(B)];
%%
dec_opt(<<?DHCP_OP_LEASE_TIME, 4, X:32, B/binary>>) ->
    [{?DHCP_OP_LEASE_TIME, X} | dec_opt(B)];
%%
dec_opt(<<?DHCP_OP_DNS_SRVS, Sz, X:Sz/binary, B/binary>>) ->
    [{?DHCP_OP_DNS_SRVS, bin2ips(X)} | dec_opt(B)];
%%
dec_opt(<<?DHCP_OP_NBNS_SRVS, Sz, X:Sz/binary, B/binary>>) ->
    [{?DHCP_OP_NBNS_SRVS, bin2ips(X)} | dec_opt(B)];
%%
dec_opt(<<?DHCP_OP_ROUTERS, Sz, X:Sz/binary, B/binary>>) ->
    [{?DHCP_OP_ROUTERS, bin2ips(X)} | dec_opt(B)];
%%
dec_opt(<<?DHCP_OP_DOMAIN_NAME, Sz, Val:Sz/binary, B/binary>>) ->
    [{?DHCP_OP_DOMAIN_NAME, Val} | dec_opt(B)];
%%
dec_opt(<<Tag, Sz, Val:Sz/binary, B/binary>>) ->
    [{Tag, Val} | dec_opt(B)];
%%
dec_opt(<<>>) ->
    [].







enc(D) when record(D, dhcp) ->
    Op    = D#dhcp.op,
    Htype = D#dhcp.htype,
    Hlen  = D#dhcp.hlen,
    Hops  = D#dhcp.hops,
    Xid   = D#dhcp.xid,
    Secs  = D#dhcp.secs,
    Flags = D#dhcp.flags,
    Ciaddr  = ip2bin(D#dhcp.ciaddr),
    Yiaddr  = ip2bin(D#dhcp.yiaddr),
    Siaddr  = ip2bin(D#dhcp.siaddr),
    Giaddr  = ip2bin(D#dhcp.giaddr),
    Bchaddr = pad_bin(l2b(D#dhcp.chaddr), 16),
    Bsname  = pad_bin(l2b(D#dhcp.sname), 64),
    Bfile   = pad_bin(l2b(D#dhcp.file), 128),
    Bopts   = enc_options(D),
    <<Op,
      Htype,
      Hlen,
      Hops,
      Xid:32,
      Secs:16,
      Flags:16,
      Ciaddr/binary,
      Yiaddr/binary,
      Siaddr/binary,
      Giaddr/binary,
      Bchaddr/binary,
      Bsname/binary,
      Bfile/binary,
      Bopts/binary>>.


enc_options(D) ->
    %%?elog("+++++++++ D=~p~n", [D]),
    Opts = [99,130,83,99,  % magic cookie
	    enc_opt({?DHCP_OP_MSGTYPE, D#dhcp.msg_type}) | 
	    enc_opts(D#dhcp.options)],
    %%?elog("+++++++++ Opts=~p~n", [Opts]),
    list_to_binary(Opts).

enc_opts([H|T]) ->
    [enc_opt(H) | enc_opts(T)];
enc_opts([]) ->
    [].

enc_opt({?DHCP_OP_MSGTYPE, X}) -> 
    [?DHCP_OP_MSGTYPE, 1, X];
enc_opt({?DHCP_OP_SRV_ID, X}) -> 
    [?DHCP_OP_SRV_ID, 4, ip2bin(X)];
enc_opt({?DHCP_OP_VENDOR_CLASS, X}) -> 
    Len = sizeof(X),
    [?DHCP_OP_VENDOR_CLASS, Len, l2b(X)];
enc_opt({?DHCP_OP_CLIENT_ID, X}) -> 
    B = i2b(X),
    [?DHCP_OP_CLIENT_ID, 5, <<0, B/binary>>];
enc_opt({?DHCP_OP_REQUESTED_IP, X}) -> 
    [?DHCP_OP_REQUESTED_IP, 4, ip2bin(X)];
enc_opt({Option, X}) when binary(X);list(X) ->
    [Option, sizeof(X), X].

sizeof(L) when list(L)   -> length(L);
sizeof(B) when binary(B) -> size(B).


pad_bin(L, Size) ->
    Bin  = l2b(L),
    Len  = size(Bin),
    Plen = Size - Len,
    <<Bin/binary, 0:Plen/?BYTE>>.

     

ip2bin({A,B,C,D}) ->
    <<A,B,C,D>>;
ip2bin(0) ->
    <<0,0,0,0>>;
ip2bin(IP) when binary(IP) ->
    IP.

bin2ip(<<A,B,C,D>>) ->
    {A,B,C,D};
bin2ip(IP) when tuple(IP) ->
    IP.

ip2str({A,B,C,D}) ->
    i2l(A)++"."++i2l(B)++"."++i2l(C)++"."++i2l(D);
ip2str(<<A,B,C,D>>) ->
    i2l(A)++"."++i2l(B)++"."++i2l(C)++"."++i2l(D);
ip2str(0) ->
    "0.0.0.0".

tuple_to_u32({X3,X2,X1,X0}) ->
    (((X3) bsl 24) bor ((X2) bsl 16) bor ((X1) bsl 8) bor (X0)).

u32_to_tuple(X) ->
    {((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
     ((X) bsr 8) band 16#ff, (X) band 16#ff}.


bin2ips(<<A,B,C,D,T/binary>>) ->
    [{A,B,C,D} | bin2ips(T)];
bin2ips(<<>>) ->
    [].


l2b(L) when list(L)   -> list_to_binary(L);
l2b(B) when binary(B) -> B.

i2l(I) when integer(I) -> integer_to_list(I);
i2l(L) when list(L)    -> L.

i2b(I) when integer(I) -> ip2bin(u32_to_tuple(I));
i2b(B) when binary(B)  -> B.
