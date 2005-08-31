-module(dhcp_cli_srv).
%%%-------------------------------------------------------------------
%%% Created : 23 May 2005 by Tobbe <tobbe@bluetail.com>
%%% Desc.   : DHCP-client server. See also: RFC 2131.
%%%
%%%           This is a DHCP client server, which means that it can
%%%           be used just as a single client that allocates just one 
%%%           IP address and nothing more. Or it can be used as a sort 
%%%           of DHCP pool allocator, allocating lots of IP addresses.
%%%
%%%           To keep it independent of the surrounding system (so
%%%           that it can be plugged in and reused somewhere else),
%%%           it provide some hooks. First it need a directory at
%%%           startup where it can store the Dets file that holds
%%%           the lease information. Then, when allocating an IP
%%%           address, two callbacks exist. One for implementing a
%%%           trace function and one for informing the surrounding
%%%           system that an allocated IP address can't be renewed
%%%           and thus has to be released.
%%%
%%% $Id$
%%%-------------------------------------------------------------------
-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include_lib("stdlib/include/ms_transform.hrl").
-include("../include/dhcp_lib.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([start/1, start_link/1, alloc/1, free/2]).
%% Null callback functions
-export([trace_info/3, release/2]).
%% debug
-export([test/0, figure_out_our_ipaddr/2]).

%% gen_event callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).


-define(elog(X,Y), error_logger:info_msg("*elog ~p:~p: " X,
					 [?MODULE, ?LINE | Y])).


-define(SERVER,    ?MODULE).
-define(DB,        ?MODULE).

-define(DHCP_LEASES_FNAME, "dhcp_leases.dets").

-define(DHCP_SRV_PORT,   67).
-define(DHCP_CLI_PORT,   68).

%%% States
-define(ST_INIT,                init).
-define(ST_SELECTING,      selecting).
-define(ST_REQUESTING,    requesting).
-define(ST_BOUND,              bound).
-define(ST_RENEWING,        renewing).
-define(ST_REBINDING,      rebinding).
-define(ST_REBOOTING,      rebooting).
-define(ST_INIT_REBOOT,  init_reboot).


-define(SELECT_TIMER,        30*1000).  % Timeout in SELECTING state (30 sec)

%%% Calling the trace fun
-define(DHCP_TRACEFUN(X, Str, Args), 
	catch (X#lease.cb_mod):trace_info(X#lease.cb_data, Str, Args)).

%%% Calling the release fun
-define(DHCP_RELEASE_CLIENT(X), 
	spawn( fun() -> (X#lease.cb_mod):release(X#lease.cb_data, 
						 X#lease.cli_ip) end)).

%%% NOTE: We use one table to store two kinds of entries.
%%% The lease entries and the UDP portmap entries.
%%% The latter entries are used for the case where we
%%% are multi-homed. The sock_opts list should then probably
%%% contain a bindtodevice option (or something similar).
%%% This makes it possible to find the correct Port for
%%% an allocation request.

%%% Lease table entry
-record(lease, {
	  xid,                  % KEY
	  state   = ?ST_INIT,   % The DHCP state
	  pid,                  % client pid
	  from,                 % gen_server From
	  ref,                  % monitor reference
	  cli_ip,               % clients IP address
	  chaddr,               % clients ethernet address
	  giaddr = {0,0,0,0},   % our relay address
	  srv_ip = {0,0,0,0},   % DHCP server handing out lease
	  srv_ips = [],         % DHCP servers to try
	  t1,                   % renewal time (sec) or TimerRef
	  t2,                   % relative rebind time (sec) or TimerRef
	  itime,                % time recorded when sending first msg
	  cb_mod = ?MODULE,     % Callback module (for trace and release)
	  cb_data,              % Opaque callback data
	  fd,                   % Socket file descriptor.
	  sock_opts = [],       % List of socket options
	  v_class = "",
	  s_pdu,                % last sent PDU (#dhcp{})
	  r_pdu                 % last received PDU (#dhcp{})
	  }).

%%% Port mapper entry
-record(pmap, {
	  sock_opts = [],       % KEY
	  fd}).


-define(MAX_XID,   9753135).  % FIXME , should be 2**31 -1  ?

-define(XID(I), ((I) rem ?MAX_XID)).

%%% State record
-record(s, {
	  xid = 1,         % DHCP packet Id
	  db_dir           % Database directory
	 }).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start(DbDir) ->
    gen_server:start({local, ?SERVER}, ?MODULE, DbDir, []).

start_link(DbDir) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, DbDir, []).

-define(CHECK_DA(Da), 
	record(Da, dhcp_alloc), 
	list(Da#dhcp_alloc.srv_ips), 
	length(Da#dhcp_alloc.srv_ips)>=1,
	list(Da#dhcp_alloc.sock_opts)).

free(Da, CliIp) when ?CHECK_DA(Da) -> 
    gen_server:call(?SERVER, {free, Da, CliIp}, infinity).



alloc(Da) when ?CHECK_DA(Da) ->
    x_alloc(#dhcp{}, da2lease(Da)).

da2lease(Da) ->
    CbMod = if (Da#dhcp_alloc.cb_mod == #dhcp_alloc.cb_mod) ->
		    ?MODULE;
	       true ->
		    Da#dhcp_alloc.cb_mod
	    end,
    #lease{srv_ips   = Da#dhcp_alloc.srv_ips,
	   giaddr    = Da#dhcp_alloc.giaddr,
	   cb_mod    = CbMod,
	   cb_data   = Da#dhcp_alloc.cb_data,
	   v_class   = v_class(Da),
	   sock_opts = Da#dhcp_alloc.sock_opts}.

%%% Return Vendor-Class info or default value
v_class(Da) ->
    Za = #dhcp_alloc{},
    case {Da#dhcp_alloc.v_class, Za#dhcp_alloc.v_class} of
	{Vc, Vc} -> X = #lease{}, X#lease.v_class;
	{Vc, _}  -> Vc
    end.

x_alloc(D, X) ->
    gen_server:call(?SERVER, {alloc, D, X, self()}, infinity).

%%% Null trace function
trace_info(_CbData, _Str, _Args) ->
    %% true.
    error_logger:info_msg(_Str, _Args).  % FIXME for debugging

%%% Null trace function
release(_CbData, ClientIp) ->
    %% true.
    error_logger:info_msg("Should release: ~p here!~n", [ClientIp]).  % FIXME for debugging



test() ->
    %% Example usage:
    %% XId = 1,
    %% TF = fun(_D, Str, Args) ->
    %%          ?TRACE(ippool, XId, "vpn(~p): "++Str, [XId | Args])
    %%      end,
    %% alloc(#dhcp{tracefun = TF}).
    alloc(#dhcp_alloc{srv_ips = [{192,168,128,1}],     % albatross
		      %% ifconfig eth0:2 192.168.32.1 netmask 255.255.255.0
		      v_class = "Nortel-SSLVPN",
		      giaddr  = {192,168,32,1}}).  
%%			    sock_opts = [{bindtofwmark, 0}]}).


%%fixme(D) when record(D, dhcp) ->
%%    %% ifconfig eth0:2 192.168.32.1 netmask 255.255.255.0
%%    D#dhcp{giaddr = {192,168,32,1},                           % orre's IP
%%	   %%chaddr = <<16#00,16#11,16#11,16#C4,16#CE,16#6E>>}; % orre's eth0
%%	   chaddr = <<0,0,0,0,0,0>>}; % orre's eth0
%%fixme(L) when record(L, lease) ->
%%    %% ifconfig eth0:2 192.168.32.1 netmask 255.255.255.0
%%    L#lease{giaddr = {192,168,32,1},                           % orre's IP
%%	    %%chaddr = <<16#00,16#11,16#11,16#C4,16#CE,16#6E>>}. % orre's eth0
%%	    chaddr = <<0,0,0,0,0,0>>}. % orre's eth0






%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init(DbDir) ->
    process_flag(trap_exit, true),
    open_dets_file(DbDir),
    {ok, #s{db_dir = DbDir} }.

open_dets_file(DbDir) ->
    Fname = lease_fname(DbDir),
    case filelib:is_file(Fname) of
	true ->
	    %% At startup we need to release any previously held IP addresses.
	    %% We will then delete the file so that we can start with a clean DB.
	    dets:open_file(?DB, [{file, Fname}, {keypos, 2}]), 
	    Leases = get_bound_leases(),
	    free_leases(Leases),
	    dets:close(?DB),
	    ?elog("deleting old DHCP lease DB~n", []),
	    ok = file:delete(Fname);
	false ->
	    ok
    end,
    dets:open_file(?DB, [{file, Fname}, {keypos, 2}]).
    

lease_fname(DbDir) ->
    filename:join([DbDir, ?DHCP_LEASES_FNAME]).


%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({alloc, D, X, Pid}, From, State) ->
    Ref = erlang:monitor(process, Pid),
    Xid = ?XID(State#s.xid),
    Lease = X#lease{xid  = Xid, 
		    ref  = Ref, 
		    pid  = Pid,
		    from = From},
    case get_fd(Lease) of
	{ok, Fd} ->
	    do_alloc(State, Lease#lease{fd = Fd}, D#dhcp{xid = Xid}),
	    {noreply, State#s{xid = Xid + 1}};
	Error ->
	    {reply, Error, State}
    end;
%%
handle_call({free, Da, CliIp}, _From, State) ->
    do_free(Da, CliIp),
    {reply, ok, State};
%%
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(_Request, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({udp, _Sock, _Ip, ?DHCP_SRV_PORT, Pdu}, State) ->
    %%?elog("Got UDP packet on the SERVER-PORT~n", []),
    D = (catch dhcp_lib:dec(Pdu)),
    call_machine(State, D),
    {noreply, State};
%%
handle_info({udp, _Sock, _Ip, ?DHCP_CLI_PORT, Pdu}, State) ->
    %%?elog("Got UDP packet on the CLIENT-PORT~n", []),
    %%
    %% FIXME , must take care of the multi interface case !!
    %%
    D = (catch dhcp_lib:dec(Pdu)),
    call_machine(State, D),
    {noreply, State};
%%
handle_info({'DOWN', _Ref, _Type, Pid, _Reason}, State) ->
    %%?elog("cli_srv got DOWN msg: ~p~n", [_Reason]),
    release_leases(Pid),
    {noreply, State};
%%
handle_info({renew, Xid, T1}, State) ->
    %%?elog("cli_srv got RENEW: Xid=~p~n", [Xid]),
    do_renew(State, Xid, T1),
    {noreply, State};
%%
handle_info({rebind, Xid, T2}, State) ->
    %%?elog("cli_srv got RENEW: Xid=~p~n", [Xid]),
    do_rebind(State, Xid, T2),
    {noreply, State};
%%
handle_info({select_timer, Xid}, State) ->
    %%?elog("cli_srv got SelectTimer, Xid=: ~p~n", [Xid]),
    select_another_srv(State, Xid),
    {noreply, State};
%%
handle_info(Info, State) ->
    ?elog("cli_srv got: ~p~n", [Info]),
    {noreply, State}.


%%%
%%% Free all leases allocated by the given process
%%%
release_leases(Pid) when pid(Pid) ->
    Pattern = ets:fun2ms(fun(X = #lease{pid = C}) when C == Pid -> X end),
    F = fun(Lease) ->
		free_lease(Lease),
		dets:delete_object(?DB, Lease)
	end,
    lists:foreach(F, dets:select(?DB, Pattern)).


%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(shutdown, _State) ->  %% FIXME release all the leases
    Leases = get_bound_leases(),
    free_leases(Leases),
    dets:close(?DB),
    ok;
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%%%
%%% Machine State Tests
%%%
-define(IS_INIT(X), X#lease.state == ?ST_INIT).
-define(IS_SELECTING(X), X#lease.state == ?ST_SELECTING).
-define(IS_REQUESTING(X), X#lease.state == ?ST_REQUESTING).
-define(IS_BOUND(X), X#lease.state == ?ST_BOUND).
-define(IS_RENEWING(X), X#lease.state == ?ST_RENEWING).
-define(IS_REBINDING(X), X#lease.state == ?ST_REBINDING).

-define(IS_DHCPOFFER(D), D#dhcp.msg_type == ?DHCPOFFER).
-define(IS_DHCPACK(D), D#dhcp.msg_type == ?DHCPACK).
-define(IS_DHCPNACK(D), D#dhcp.msg_type == ?DHCPNACK).


%%%
%%% Client initiated release of IP address.
%%%
do_free(Da, CliIp) ->
    CbMod  = Da#dhcp_alloc.cb_mod,
    CbData = Da#dhcp_alloc.cb_data,
    Opts   = Da#dhcp_alloc.sock_opts,
    Pattern = ets:fun2ms(fun(X = #lease{cb_mod    = C0,
					cb_data   = C1,
					sock_opts = C2,
					cli_ip    = C3}) when C0 == CbMod,
							      C1 == CbData,
							      C2 == Opts,
							      C3 == CliIp -> 
				 X 
			 end),
    case dets:select(?DB, Pattern) of
	[Lease] ->
	    free_lease(Lease);
	Else ->
	    ?elog("<ERROR> do_free: Else=~p~n", [Else])
    end.


free_leases(Leases) ->
    lists:foreach(fun(X) -> free_lease(X) end, Leases).

free_lease(Lease) ->
    ?DHCP_TRACEFUN(Lease,"de-allocating IP address: ~s", 
		   [dhcp_lib:ip2str(Lease#lease.cli_ip)]),
    ?elog("DHCP de-allocating IP address: ~s~n", 
	  [dhcp_lib:ip2str(Lease#lease.cli_ip)]),
    Xid = Lease#lease.xid,
    D = #dhcp{xid      = Xid,
	      msg_type = ?DHCPRELEASE,
	      ciaddr   = Lease#lease.cli_ip,
	      giaddr   = Lease#lease.giaddr,
	      chaddr   = Lease#lease.chaddr,
	      options  = [{?DHCP_OP_CLIENT_ID, Xid} |
			  add_vendor_class(Lease)]},
    Pdu = dhcp_lib:enc(D),
    udp_send(Lease, Pdu, Lease#lease.srv_ip),
    dets:delete_object(?DB, Lease).


add_vendor_class(X) ->
    Z = #lease{},
    case {X#lease.v_class, Z#lease.v_class} of
	{Vc, Vc} -> [];
	{Vc, _}  -> [{?DHCP_OP_VENDOR_CLASS, Vc}]
    end.

%%% 
%%% We have not got any reply from our earlier DHCPDISCOVER.
%%% Let us try another server if possible, else fail.
%%%
select_another_srv(State, Xid) ->
    case dets:lookup(?DB, Xid) of
	[X] when ?IS_SELECTING(X), length(X#lease.srv_ips)>0 -> 
	    do_alloc(State, X, X#lease.s_pdu);
	[X] when ?IS_SELECTING(X), record(X, lease) ->
	    dets:delete(?DB, Xid),
	    gen_server:reply(X#lease.from, {error, "no contact with server(s)"});
	_ ->
	    false
    end.

    
do_renew(_State, Xid, T1) ->
    %% The client moves to RENEWING state and sends (via unicast)
    %% a DHCPREQUEST message to the server to extend its lease.  The client
    %% sets the 'ciaddr' field in the DHCPREQUEST to its current network
    %% address. The client MUST NOT include a 'server identifier' in 
    %% the DHCPREQUEST message.
    case dets:lookup(?DB, Xid) of
	[X] when ?IS_BOUND(X) -> 
	    ?DHCP_TRACEFUN(X,"timer T1 expired for ~s, in BOUND state", 
			   [dhcp_lib:ip2str(X#lease.cli_ip)]),
	    D = #dhcp{xid      = Xid,
		      msg_type = ?DHCPREQUEST,
		      ciaddr   = X#lease.cli_ip,
		      giaddr   = X#lease.giaddr,
		      chaddr   = X#lease.chaddr,
		      options  = [{?DHCP_OP_CLIENT_ID, Xid}|
				  add_vendor_class(X)]},
	    Pdu = dhcp_lib:enc(D),
	    udp_send(X, Pdu, X#lease.srv_ip),
	    T2 = X#lease.t2,
	    {ok, R2} = timer:send_after(T2 * 1000, ?SERVER, {rebind, Xid, T2}),
	    dets:insert(?DB, X#lease{state  = ?ST_RENEWING,
				    t1     = T1,
				    t2     = R2});
	_   -> 
	    ?elog("do_renew: Xid=~p not found~n",[Xid]),
	    ok
    end.

do_rebind(_State, Xid, _T2) ->
    %% When timer T2 expires we are supposed to broadcast
    %% a DHCPREQUEST message but since we don't do brodcasting (?)
    %% we ignore this state and goes to the INIT state.
    case dets:lookup(?DB, Xid) of
	[X] when ?IS_RENEWING(X) -> 
	    ?DHCP_TRACEFUN(X,"timer T2 expired for ~s, in RENEWING state", 
			   [dhcp_lib:ip2str(X#lease.srv_ip)]),
	    ?DHCP_RELEASE_CLIENT(X),
	    dets:insert(?DB, X#lease{state  = ?ST_INIT});
	_ ->
	    ?elog("do_rebind: Xid=~p not found~n",[Xid]),
	    ok
    end.

	    
    
    

call_machine(State, D) ->
    case dets:lookup(?DB, D#dhcp.xid) of
	[X] -> 
	    %% We may throw an exception here!
	    catch machine(State, X, D);
	_   -> 
	    %%?elog("call_machine: Xid=~p not found~n",[D#dhcp.xid]),
	    ok
    end.


%%%--------------------------------------------------------------------
%%%                T H E  S T A T E  M A C H I N E
%%%
%%% Take a look at the state diagram at p.34 in RFC 2131.
%%%--------------------------------------------------------------------
machine(_S,X,D) when ?IS_SELECTING(X), ?IS_DHCPOFFER(D) ->
    %% We select one DHCPOFFER message and extracts the 
    %% server address from the 'server identifier' option 
    %% in the DHCPOFFER message.
    timer:cancel(X#lease.t1), % cancel the select timer
    case dhcp_lib:get_opt(?DHCP_OP_SRV_ID, D#dhcp.options) of
	{ok, SrvId} ->
	    ?DHCP_TRACEFUN(X,"got DHCPOFFER from server: ~s, in SELECTING state", 
			   [dhcp_lib:ip2str(SrvId)]),
	    D1 = #dhcp{xid      = D#dhcp.xid,
		       msg_type = ?DHCPREQUEST,
		       giaddr   = X#lease.giaddr,
		       chaddr   = X#lease.chaddr,
		       options  = [{?DHCP_OP_SRV_ID, SrvId},
				   {?DHCP_OP_CLIENT_ID, D#dhcp.xid},
				   {?DHCP_OP_REQUESTED_IP, D#dhcp.yiaddr}|
				   add_vendor_class(X)]},
	    Pdu = dhcp_lib:enc(D1),
	    ?DHCP_TRACEFUN(X,"sending DHCPREQUEST to server: ~s", 
			   [dhcp_lib:ip2str(SrvId)]),
	    udp_send(X, Pdu, SrvId),
	    dets:insert(?DB, X#lease{state  = ?ST_REQUESTING,
				     cli_ip = D#dhcp.yiaddr,
				     srv_ip = SrvId,
				     r_pdu  = D, 
				     s_pdu  = D1});
	_ ->
	    %% FIXME , ignore this and await another offer.
	    %% We also have to use a timer here to do GC.
	    false
    end;
%%
machine(_S,X,D) when ?IS_REQUESTING(X), ?IS_DHCPACK(D) ->
    %% Once the DHCPACK message from the server arrives, 
    %% the client is initialized and moves to BOUND state.
    %% The client SHOULD perform a check on the suggested 
    %% address to ensure that the address is not already in use.    
    case is_addr_inuse(D) of
	false ->
	    ?DHCP_TRACEFUN(X,"got DHCPACK from server: ~s, in REQUESTING state",
			   [dhcp_lib:ip2str(X#lease.srv_ip)]),
	    Opts = D#dhcp.options,
	    {T1, T2} = get_t1_t2(Opts),
	    %% Reply to client
	    Yiaddr = D#dhcp.yiaddr,
	    gen_server:reply(X#lease.from, {ok, Yiaddr, Opts}),
	    %%?elog("machine: enter state BOUND , Xid=~p, Ip=~p, T1=~p~n", 
	    %%  [X#lease.xid, Yiaddr, T1]),

	    %% Set the Renewal timer
	    {ok, R1} = timer:send_after(T1 * 1000, ?SERVER, {renew, X#lease.xid, T1}),

	    dets:insert(?DB, X#lease{state  = ?ST_BOUND,
				     t1 = R1, t2 = T2 - T1,
				     r_pdu  = D});
	true ->
	    %% If the network address appears to be in use, the client 
	    %% MUST send a DHCPDECLINE message to the server. 
	    SrvId = X#lease.srv_ip,
	    ?DHCP_TRACEFUN(X,"got DHCPACK from server: ~s, address is in use already", 
			   [dhcp_lib:ip2str(SrvId)]),
	    D1 = D#dhcp{msg_type = ?DHCPDECLINE,
			options  = [{?DHCP_OP_CLIENT_ID, X#lease.xid}|
				    add_vendor_class(X)]},
	    Pdu = dhcp_lib:enc(D1),
	    ?DHCP_TRACEFUN(X,"sending DHCPDECLINE to server: ~s", 
			   [dhcp_lib:ip2str(SrvId)]),
	    udp_send(X, Pdu),
	    dets:insert(?DB, X#lease{state = ?ST_REQUESTING,
				     r_pdu = D, 
				     s_pdu = D1})
    end;
%%
machine(_S,X,D) when ?IS_RENEWING(X), ?IS_DHCPACK(D) ->
    %% Cancel Rebind timer
    timer:cancel(X#lease.t2),

    {T1, T2} = get_t1_t2(D#dhcp.options),
    ?DHCP_TRACEFUN(X,"got DHCPACK from server: ~s, in RENEWING state", 
		   [dhcp_lib:ip2str(X#lease.srv_ip)]),

    %% Set Renewal timer
    {ok, R1} = timer:send_after(T1 * 1000, ?SERVER, {renew, X#lease.xid, T1}),

    dets:insert(?DB, X#lease{state  = ?ST_BOUND,
			     t1 = R1, t2 = T2 - T1,
			     r_pdu  = D});
%%
machine(_,_X,_D) ->
    %% FIXME , ignore this.
    %% We also have to use a timer here to do GC.
    ?elog("machine: In State=~p got Message=~p~n",[_X#lease.state, _D#dhcp.msg_type]),
    false.

get_t1_t2(Opts) ->
    case dhcp_lib:get_opt(?DHCP_OP_RENEWAL_TIME, Opts) of
	{ok, T1} -> get_t2(Opts, T1);
	_ ->
	    case dhcp_lib:get_opt(?DHCP_OP_LEASE_TIME, Opts) of
		{ok, L} -> get_t2(Opts, L div 2);
		_       -> throw({error, "not timout info found"})
	    end
    end.
	    
get_t2(Opts, T1) ->
    case dhcp_lib:get_opt(?DHCP_OP_REBINDING_TIME, Opts) of
	{ok, T2} -> {T1, T2};
	_        -> {T1, trunc(T1 * 1.5)}
    end.

%%% When broadcasting an ARP request for the suggested address,
%%% the client must fill in its own hardware address as the sender's
%%% hardware address, and 0 as the sender's IP address, to avoid
%%% confusing ARP caches in other hosts on the same subnet.
is_addr_inuse(_D) ->
    false.                                   % FIXME

get_bound_leases() ->
    F = fun(X, Acc) when X#lease.state == ?ST_BOUND ->
		[X | Acc];
	   (_, Acc) ->
		Acc
	end,
    dets:foldl(F, [], ?DB).


do_alloc(_State, X0, D0) ->
    SrvIps = X0#lease.srv_ips,
    SrvIp = hd(SrvIps),          % FIXME take care of all servers
    case figure_out_our_ipaddr(SrvIp, X0) of
	{ok, OurIp} ->
	    X = X0#lease{giaddr = OurIp},
	    D = D0#dhcp{giaddr  = OurIp},
	    Opts = D#dhcp.options,
	    Pdu = dhcp_lib:enc(D#dhcp{msg_type = ?DHCPDISCOVER,
			      options  = [{?DHCP_OP_CLIENT_ID, X0#lease.xid}|
					  add_vendor_class(X)++Opts]}),
	    ?DHCP_TRACEFUN(X,"sending DHCPDISCOVER to server: ~s , giaddr: ~s",
		   [dhcp_lib:ip2str(SrvIp), dhcp_lib:ip2str(OurIp)]),
	    XXX = udp_send(X, Pdu, SrvIp),
	    ?elog("++++++++ ~p~n", [XXX]),
	    
	    %% Set a timer here so that we won't end up hanging 
	    %% in the SELECTING state.
	    {ok, R1} = timer:send_after(?SELECT_TIMER, ?SERVER, 
					{select_timer, X#lease.xid}),
	    
	    X1 = X#lease{state   = ?ST_SELECTING, 
			 t1      = R1,
			 chaddr  = D#dhcp.chaddr,
			 srv_ip  = SrvIp,
			 srv_ips = tl(SrvIps),
			 itime   = erlang:now()}, 
	    dets:insert(?DB, X1);

	{error, Emsg} ->
	    ?DHCP_TRACEFUN(X0,"failed to send DHCPDISCOVER, reason: ~s", [Emsg])
    end.

get_fd(X) ->
    SockOpts = X#lease.sock_opts,
    case dets:lookup(?DB, SockOpts) of
	[P] when record(P, pmap) -> 
	    {ok, P#pmap.fd};
	_    -> 
	    case open_socket(?DHCP_SRV_PORT, SockOpts) of
		{ok, Fd} -> 
		    dets:insert(?DB, #pmap{sock_opts = SockOpts, fd = Fd}),
		    {ok, Fd};
		_ ->
		    {error, "could't open dhcp socket"}
	    end
    end.


open_socket(Port, Opts) ->
    case fdsrv:bind_socket(udp, Port) of
	{ok, Fd} ->
	    %%gen_udp:open(Port, [{fd, Fd}, binary, {broadcast,true} | Opts]);
	    gen_udp:open(Port, [{fd, Fd}, binary | Opts]);
	Error ->
	    ?elog("Couldn't open socket, port=~p: ~p~n",[Port, Error])
    end. 

%%%
%%% On a multihomed system it can be non-obvious what IP address
%%% we are using. This code tries to evade this problem by setting
%%% up a socket towards the server in question, then checking what
%%% IP address we were assigned. 
%%%
%%% It is also possible to explicitly set the 'giaddr' in the
%%% input data to alloc/1. We will in that case just return that
%%% IP address from this function.
%%%
figure_out_our_ipaddr(ServIp, X) when X#lease.giaddr == {0,0,0,0} ->
    SockOpts = X#lease.sock_opts,
    case  gen_udp:open(0, SockOpts) of
	{ok, Fd} ->
	    gen_udp:connect(Fd, ServIp, ?DHCP_SRV_PORT),
	    case inet:sockname(Fd) of
		{ok, {OurIp, _OurPort}} ->
		    gen_udp:close(Fd),
		    {ok, OurIp};
		_ ->
		    gen_udp:close(Fd),
		    {error, "could not figure out our own IP address"}
	    end;
	_ ->
	    {error, "could not open UDP socket"}
    end;
figure_out_our_ipaddr(_ServIp, X) ->
    {ok, X#lease.giaddr}.


udp_send(X, Pdu) ->
    udp_send(X, Pdu, hd(X#lease.srv_ips)).  % FIXME, should try all IPs !!

udp_send(X, Pdu, SrvIp) ->
    gen_udp:send(X#lease.fd,
		 SrvIp,
		 ?DHCP_SRV_PORT,
		 Pdu).


