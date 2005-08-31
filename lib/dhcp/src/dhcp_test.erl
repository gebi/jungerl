%%%-------------------------------------------------------------------
%%% Created : 31 Aug 2005 by Tobbe <tobbe@bluetail.com>
%%% Desc.   : Test program for the DHCP allocator.
%%%
%%% $Id$
%%%-------------------------------------------------------------------
-module(dhcp_test).

%%% External export
-export([start/0, alloc/0, free/2]).

%%% Call-back export
-export([trace_info/3, release/2]).

-include("../include/dhcp_lib.hrl"). 



%%% Trace call-back function
trace_info(_CbData, Str, Args) -> 
    error_logger:info_msg(Str, Args).

%%% Release call-back function
release(_CbData, ClientIp) -> 
     error_logger:info_msg("Should release: ~p here!~n", [ClientIp]).



start() ->
    {ok, _} = fdsrv:start(),
    DbDir = "/tmp",
    dhcp_cli_srv:start(DbDir).

%%%
%%% Example, on configuration in the /etc/dhcpd.conf
%%% (using ICS dhcp-3.0.3)
%%%
%%% # Setting up my own DHCP attributes to be delivered
%%% option space TOBBE;
%%% option TOBBE.idle-timeout code 2 = unsigned integer 32; 
%%% option TOBBE.special-ip code 3 = ip-address;
%%% option TOBBE.funny-text code 4 = text; 
%%% class "vendor-classes" { 
%%%         match option vendor-class-identifier; 
%%% }
%%% 
%%% # Tobbes test network
%%% subnet 192.168.32.0 netmask 255.255.255.0 {
%%%         option domain-name-servers 192.168.32.100;
%%%         default-lease-time      60;
%%%         max-lease-time          60;
%%% 
%%%         option subnet-mask      255.255.255.0;
%%%         option broadcast-address 192.168.32.255;
%%%         option routers          192.168.32.99;
%%% 
%%%         # Use options from above
%%%         subclass "vendor-classes" "Tobbe Ltd." { 
%%%                   vendor-option-space TOBBE; 
%%%                   option TOBBE.idle-timeout 600; 
%%%                   option TOBBE.special-ip 192.168.128.1; 
%%%                   option TOBBE.funny-text "Hello World"; 
%%%         }
%%% 
%%%         range  dynamic-bootp    192.168.32.10 192.168.32.49;
%%% } 

alloc() ->
    D = #dhcp_alloc{srv_ips   = [{192,168,128,1}],
		    giaddr    = {192,168,32,1},
		    cb_mod    = ?MODULE,
		    cb_data   = none,
		    v_class   = "Tobbe Ltd.",
		    sock_opts = []},
    {ok, ClientIp, Opts} = dhcp_cli_srv:alloc(D),
    {D, ClientIp, parse_opts(Opts)}.


free(D, ClientIP) when record(D, dhcp_alloc) ->
    dhcp_cli_srv:free(D, ClientIP).



parse_opts(Opts) ->
    parse_opts(Opts, []).

parse_opts([{?DHCP_OP_SUBNET_MASK, NetMask}|T], Acc) ->
    parse_opts(T, [{netmask,NetMask} | Acc]);
%%
parse_opts([{?DHCP_OP_DNS_SRVS, Dns}|T], Acc) ->
    parse_opts(T, [{dns, Dns} | Acc]);
%%
parse_opts([{?DHCP_OP_DOMAIN_NAME, DomainName}|T], Acc) ->
    parse_opts(T, [{domain_name, b2l(DomainName)} | Acc]);
%%
parse_opts([{?DHCP_OP_NBNS_SRVS, Wins}|T], Acc) ->
    parse_opts(T, [{nbns, Wins} | Acc]);
%%
parse_opts([{?DHCP_OP_ROUTERS, Routers}|T], Acc) ->
    parse_opts(T, [{routers, Routers} | Acc]);
%%
parse_opts([{?DHCP_OP_VENDOR, Bin}|T], Acc) ->
    parse_opts(T, [{vendor_opts, parse_vendor_opts(Bin)} | Acc]);
%%
parse_opts([H|T], Acc) ->
    error_logger:info_msg("dhcp_test: Option not parsed: ~p~n", [H]),
    parse_opts(T, Acc);
%%
parse_opts([], Acc) ->
    Acc.

parse_vendor_opts(<<Id, Len, Vopt:Len/binary, Rest/binary>>) ->
    [{Id, Vopt} | parse_vendor_opts(Rest)];
parse_vendor_opts(_) ->
    [].

b2l(B) when binary(B) -> binary_to_list(B); 
b2l(L) when list(L)   -> L. 



