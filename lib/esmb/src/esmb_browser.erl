%%%-------------------------------------------------------------------
%%% File    : esmb_browser.erl
%%% Created : 18 Apr 2005 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Desc.   : Browser server for Windows Networks.
%%%
%%% $Id$
%%%-------------------------------------------------------------------
-module(esmb_browser).

-behaviour(gen_server).

%% External exports
-export([start/0, start_link/0, get_backup_list/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-import(esmb_netbios,[nbss_src_name/1, nbss_dst_name/1, nb_dec_name/1,
		      dec_nbns/2, positive_query_response_pdu/2,
		      dec_nbss_datagram/1]).

-import(esmb, [b2l/1, ip2bin/1]).

-include("esmb_lib.hrl").
-include("esmb_netbios.hrl").


-define(SERVER, ?MODULE).
-define(TAB,    ?MODULE).

-record(s, {
	  our_name,       % Our NetBIOS name
	  ns_sock,        % Name Service socket fd
	  dgm_sock        % Datagram socket fd
	 }).

%%%
%%% Microsoft Browser Protocol Operations
%%%
-define(HOST_ANNOUNCEMENT,     16#01).
-define(REQUEST_ANNOUNCEMENT,  16#02).
-define(ELECTION_REQUEST,      16#08).
-define(BACKUP_LIST_RESPONSE,  16#0a).
-define(DMB_ANNOUNCEMENT,      16#0c). % Domain Master
-define(LMB_ANNOUNCEMENT,      16#0f). % Local Master

%%% 
-record(browse_msg, {
	  cmd,            % Various Browser Protocol operations
	  name,           % Host name
	  ip,             % Ip address
	  comment,        % A host comment
	  workgroup,      % Workgroup name
	  backup_srvs     % A list of backup servers
	  }).

%%%
%%% Workgroup members should send out HOST_ANNOUNCEMENTs in
%%% intervals no longer than 12 minutes.
%%% Every time we see a HOST_ANNOUNCEMENT we update the member list
%%% to add new members and to reset the UpdateCounter of existing
%%% members. Every LMB_TIMEOUT we go through the list and removes
%%% all members which has reached MAX_UPDATE_CNT and increments
%%% the UpdateCounter for the others.
%%%

-define(LMB_TIMEOUT,     720).   % 12 min x 60 == 720 seconds
-define(MAX_UPDATE,        2).   % Start count from zero

%%% Guard test
-define(DEAD_HOST(BM), BM#browse_member.upcnt >= ?MAX_UPDATE).

-record(browse_list, {
	  workgroup,      % The workgroup name, string() , Ets-Key !!
	  ip,             % Local Master Browser, ip()
	  name,           % Local Master Browser name, string()
	  comment,        % Local Master Browser comment, string()
	  members=[],     % Workgroup members, list(#browse_member{})
	  backup_srvs=[], % A list of backup servers
	  dmb=false       % Domain Master, bool()
	  }).

-record(browse_member, {
	  name,           % NetBIOS name
	  ip,             % IP address
	  comment="",     % Member description
	  upcnt=0         % Update counter
	  }).


%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_backup_list() ->
    gen_server:cast(?SERVER, get_backup_list).

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
init([]) ->
    init_table(),
    {ok, Ns} = open_socket(?NETBIOS_NS_PORT, []),
    {ok, Ds} = open_socket(?NETBIOS_DGM_PORT, []),
    get_backup_list(Ds, 1, "192.168.128.32"),
    {ok, #s{
       our_name = "ORRE",   % FIXME , hardcoded for now...
       ns_sock  = Ns,
       dgm_sock = Ds
      }}.

init_table() ->
    ets:new(?TAB, [{keypos,2}, named_table, protected]).

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
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(get_backup_list, State) ->
    get_backup_list(State#s.dgm_sock, 1, "192.168.128.32"),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({udp, Sock, Ip, ?NETBIOS_NS_PORT, Data}, State) ->
    case dec_nbns(Data, #nbt{src_ip = Ip}) of
	{ok, NS} -> reply(NS, State);
	Else     -> ?elog("srv got data: ~p~n", [Else])
    end,
    {noreply, State};
%%
handle_info({udp, Sock, Ip, ?NETBIOS_DGM_PORT, Data}, State) ->
    decode_datagram(Data, #nbt{src_ip = Ip}, State),
    {noreply, State};
%%
handle_info(Info, State) ->
    ?elog("srv got: ~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


decode_datagram(Data, Ns, State) ->
    case catch dec_dgm(Data, Ns, State#s.our_name) of
	{Bm, Dgm} when Bm#browse_msg.cmd == ?BACKUP_LIST_RESPONSE -> 
	    Answer = ask_backup_srvs(Bm#browse_msg.backup_srvs),
	    add_backup_srv_info(State, Answer);
	%%
	{Bm, Dgm}  when Bm#browse_msg.cmd == ?REQUEST_ANNOUNCEMENT -> 
	    ?elog("Got Request-Announcement, Name=~p Dest=~p~n",
		  [Bm#browse_msg.name, Dgm#netbios_dgm.dst_name]),
	    State;
	%%
	{Bm, Dgm}  when Bm#browse_msg.cmd == ?HOST_ANNOUNCEMENT ->
	    ?elog("Got Host-Announcement, Name=~p Comment=~p Wgrp=~p~n",
		  [Bm#browse_msg.name, 
		   Bm#browse_msg.comment,
		   Bm#browse_msg.workgroup]),
	    update_host_info(Bm),
	    State;
	%%
	{Bm, Dgm}  when Bm#browse_msg.cmd == ?LMB_ANNOUNCEMENT ->
	    ?elog("Got LocalMaster-Announcement, Name=~p Comment=~p Wgrp=~p~n",
		  [Bm#browse_msg.name, 
		   Bm#browse_msg.comment,
		   Bm#browse_msg.workgroup]),
	    update_lmb_info(Bm),
	    State;
	%%
	{Bm, Dgm}  when Bm#browse_msg.cmd == ?DMB_ANNOUNCEMENT ->
	    ?elog("Got DomainMaster-Announcement, Name=~p Comment=~p Wgrp=~p~n",
		  [Bm#browse_msg.name, 
		   Bm#browse_msg.comment,
		   Bm#browse_msg.workgroup]),
	    update_dmb_info(Bm),
	    State;
	%%
	false ->
	    ?elog("decode_datagram: false~n", []),
	    State;
	Else -> 
	    ?elog("decode_datagram: ~p~n", [Else]),
	    State
    end.

update_dmb_info(Bm) ->
    update_lmb_info(Bm),
    [Bl] = ets:lookup(?TAB, Bm#browse_msg.workgroup),
    ets:insert(?TAB, Bl#browse_list{dmb = true}).

update_lmb_info(Bm) ->
    case ets:lookup(?TAB, Bm#browse_msg.workgroup) of
	[Bl] ->
	    ets:insert(?TAB, Bl#browse_list{ip      = Bm#browse_msg.ip,
					    name    = Bm#browse_msg.name,
					    comment = Bm#browse_msg.comment});
	[] ->
	    Bl = #browse_list{workgroup = Bm#browse_msg.workgroup,
			      ip        = Bm#browse_msg.ip,
			      name      = Bm#browse_msg.name,
			      comment   = Bm#browse_msg.comment},
	    ets:insert(?TAB, Bl)
    end.

update_host_info(Bm) ->
    case ets:lookup(?TAB, Bm#browse_msg.workgroup) of
	[Bl] ->
	    Ms = update_members(Bm, Bl#browse_list.members),
	    ets:insert(?TAB, Bl#browse_list{members = Ms});
	[] ->
	    Ms = update_members(Bm, []),
	    Bl = #browse_list{workgroup = Bm#browse_msg.workgroup,
			      members = Ms},
	    ets:insert(?TAB, Bl)
    end.

update_members(Bm, [H|T]) when Bm#browse_msg.name == H#browse_member.name ->
    [H#browse_member{ip      = Bm#browse_msg.ip,
		     comment = Bm#browse_msg.comment,
		     upcnt   = 0} | T];
update_members(Bm, [H|T]) ->
    [H | update_members(Bm, T)];
update_members(Bm, []) ->
    [#browse_member{name    = Bm#browse_msg.name,
		    comment = Bm#browse_msg.comment,
		    ip      = Bm#browse_msg.ip}].


ask_backup_srvs(Bs) ->
    ?elog("ask_backup_srvs: got ~p~n", [Bs]),
    tbd. % FIXME
		
add_backup_srv_info(State, Answer) ->
    tbd. % FIXME

reply(Ns, State) 
  when Ns#nbt.op == ?NS_QUERY_REQ,
       Ns#nbt.name == State#s.our_name ->
    ?elog("+++ GOT Name Query for us: ~p~n",[Ns#nbt.name]),
    Pdu = positive_query_response_pdu(Ns, State#s.our_name),
    Res = gen_udp:send(State#s.ns_sock,
		       Ns#nbt.src_ip,
		       ?NETBIOS_NS_PORT,
		       Pdu),
    ?elog("Sending UDP pos_q_resp: Res=~p~n",[Res]);
reply(Ns, State) when Ns#nbt.op == ?NS_QUERY_REQ ->
    ?elog("Name Query for: ~p~n",[Ns#nbt.name]),
    false.

dec_dgm(Bin, NS, OurName) ->
    case catch dec_nbss_datagram(Bin) of
	{ok, Dgm} when Dgm#netbios_dgm.src_name == OurName ->
	    %% This is our own broadcast we have received, i.e ignore it!
	    false;
	{ok, Dgm} ->
	    %% NB: The message we have received here is a
	    %% Transaction-Request (and not a Response) !!!
	    %% (took me a while to notice...sigh...)
	    case catch esmb:dec_transaction_req(#smbpdu{}, Dgm#netbios_dgm.data) of
		{'EXIT', Reason} ->
		    ?elog("dec_dgm failed reason = ~p~n", [Reason]),
		    {error, "esmb_browse:dec_dgm/2 internal error"};
		{error, Reason} ->
		    ?elog("esmb_browse:dec_dgm/2 failed reason2 = ~p~n", [Reason]),
		    {error, "internal error"};
		Pdu when record(Pdu, smbpdu) ->
		    case esmb:error_p(Pdu) of
			false -> 
			    decode_dgm(Pdu#smbpdu.bf, Dgm, NS);
			{true, _Ecode, Emsg} -> 
			    ?elog("dec_dgm failed reason = ~p~n", [Emsg]),
			    {error, Emsg}
		    end;
		Else ->
		    ?elog("esmb_browse:dec_dgm/2 failed 8888: ~p~n", [Else]),
		    {error, "internal error"}
	    end;
	Else ->
	    ?elog("esmb_browse:dec_dgm/2 failed 4: ~p~n", [Else]),
	    {error, "internal error"}
    end.
    
		  
decode_dgm(Data, Dgm, NS) ->
    case b2l(Data) of
	"\\MAILSLOT\\BROWSE" ++ _ ->
	    dec_mailslot_browse(Data, Dgm, NS);
	_ ->
	    {error, "internal error"} % FIXME , not really internal error...
    end.


dec_mailslot_browse(<<_:17/binary,          % \MAILSLOT\BROWSE<0>
		     ?BACKUP_LIST_RESPONSE,
		     Count,
		     Token:32/little,
		     BackupServers/binary>>,
		     Dgm, Ns) ->
    Bs = string:tokens(b2l(BackupServers), [0]),
    {#browse_msg{cmd         = ?BACKUP_LIST_RESPONSE,
		 backup_srvs = Bs}, 
     Dgm};
dec_mailslot_browse(<<_:17/binary,          % \MAILSLOT\BROWSE<0>
		     ?REQUEST_ANNOUNCEMENT,
		     _,                     % Unused flags
		     Name/binary>>,
		     Dgm, Ns) ->
    {#browse_msg{cmd  = ?REQUEST_ANNOUNCEMENT, 
		 name = bin2nulstr(Name)}, 
     Dgm};
dec_mailslot_browse(<<_:17/binary,          % \MAILSLOT\BROWSE<0>
		     ?HOST_ANNOUNCEMENT,
		     Count,
		     UpdatePeriod:32/little,
		     Name:16/binary,
		     OSmajorVer,
		     OSmminorVer,
		     SrvType:4/binary,
		     BrwsMajorVer,
		     BrwsMinorVer,
		     Signature:16/little,
		     Comment/binary>>,
		     Dgm, Ns) ->
    {#browse_msg{cmd       = ?HOST_ANNOUNCEMENT, 
		 name      = bin2nulstr(Name),
		 ip        = Dgm#netbios_dgm.src_ip,
		 workgroup = Dgm#netbios_dgm.dst_name,
		 comment   = bin2nulstr(Comment)}, 
     Dgm};
dec_mailslot_browse(<<_:17/binary,          % \MAILSLOT\BROWSE<0>
		     ?LMB_ANNOUNCEMENT,
		     Count,
		     UpdatePeriod:32/little,
		     Name:16/binary,
		     OSmajorVer,
		     OSmminorVer,
		     SrvType:4/binary,
		     BrwsMajorVer,
		     BrwsMinorVer,
		     Signature:16/little,
		     Comment/binary>>,
		     Dgm, Ns) ->
    {#browse_msg{cmd       = ?LMB_ANNOUNCEMENT, 
		 name      = bin2nulstr(Name),
		 ip        = Dgm#netbios_dgm.src_ip,
		 workgroup = Dgm#netbios_dgm.dst_name,
		 comment   = bin2nulstr(Comment)}, 
     Dgm};
dec_mailslot_browse(<<_:17/binary,          % \MAILSLOT\BROWSE<0>
		     ?DMB_ANNOUNCEMENT,
		     Count,
		     UpdatePeriod:32/little,
		     Workgroup:16/binary,
		     OSmajorVer,
		     OSmminorVer,
		     SrvType:4/binary,
		     BrwsMajorVer,
		     BrwsMinorVer,
		     Signature:16/little,
		     MasterBrowser/binary>>,
		     Dgm, Ns) ->
    {#browse_msg{cmd       = ?DMB_ANNOUNCEMENT, 
		 name      = bin2nulstr(MasterBrowser),
		 ip        = Dgm#netbios_dgm.src_ip,
		 workgroup = Workgroup},
     Dgm};
dec_mailslot_browse(Bin, Dgm, Ns) ->
    ?elog("dec_mailsot_browse: unable to decode: ~p~n",[Bin]),
    false.

bin2nulstr(Bin) ->
    case string:tokens(b2l(Bin), [0]) of
	[]    -> [];
	[H|_] -> H
    end.
    
get_backup_list(Sock, DgmId, SrcIp) ->
    Dgm = #netbios_dgm{sock     = Sock,
		       msg_type = ?DGM_MTYPE_DIRECT_GROUP,
		       flags    = ?DGM_FLAGS_FIRST_BNODE,
		       dgm_id   = DgmId,
		       src_ip   = SrcIp,
		       src_port = ?NETBIOS_DGM_PORT,
		       src_name = nbss_src_name("ORRE"),      % FIXME ??
		       dst_name = nbss_dst_name("BLUETAIL")
		      },
    Pdu = get_backup_list_req(),
    esmb:mailslot_browse_transaction(Dgm, Pdu).


get_backup_list_req() ->
    <<9,              % Cmd
      4,              % Count
      1:32/little>>.  % Token


open_socket(Port,Opts) ->
    case fdsrv:bind_socket(udp, Port) of
	{ok, Fd} ->
	    gen_udp:open(Port, [{fd, Fd}, binary, {broadcast,true} | Opts]);
	Error ->
	    ?elog("Couldn't open socket, port=~p: ~p~n",[Port, Error])
    end. 

close_socket(Fd) -> gen_udp:close(Fd).

set_options(Fd,Options) ->
    F = fun(Option) -> set_option(Fd,Option) end,
    lists:foreach(F,Options).

set_option(Fd,Option) ->
    case inet:setopts(Fd,Option) of
        ok             -> true;
        {error,Reason} -> ?elog("Couldn't set option ~p , reason ~p~n",
                                [Option,Reason])
    end.











