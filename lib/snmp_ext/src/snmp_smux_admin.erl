%%%-------------------------------------------------------------------
%%% File    : snmp_smux_admin.erl
%%% Author  : Martin Bjorklund <mbj@bluetail.com>
%%% Description : Holds the SMUX config, starts tcp listeners, implements
%%%               the SMUX MIB.
%%% Created : 31 Oct 2003 by Martin Bjorklund <mbj@bluetail.com>
%%%-------------------------------------------------------------------
-module(snmp_smux_admin).

-behaviour(gen_server).

-define(VMODULE, "SMUX").
-include_lib("snmp/src/misc/snmp_verbosity.hrl").
% PRE R10 -include_lib("snmp/src/snmp_verbosity.hrl").
-include("snmp_smux.hrl").

%% external exports
-export([start_link/0, start_link/5]).

%% called by smux_server
-export([add_peer/2, del_peer/1, add_registration/3, del_registration/3]).

%% internal exports
-export([accept/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-record(state, {agent, secret, accept_pid}).

-record(params, {agent, timeout}).

-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.

-define(SERVER, ?MODULE).

-define(SMUX_DEFAULT_TIMEOUT, 15000).
-define(SMUX_DEFAULT_PORT, 199).

%%====================================================================
%% External functions
%%====================================================================
start_link() ->
    start_link({127,0,0,1}, ?SMUX_DEFAULT_PORT,
	       snmp_master_agent, ?SMUX_DEFAULT_TIMEOUT, undefined).

start_link(IPAddress, Port, Agent, Timeout, Secret) ->
    Params = #params{agent = Agent, timeout = Timeout},
    gen_server:start_link({local, ?SERVER}, ?MODULE,
			  [IPAddress, Port, Secret, Params], []).

add_peer(SrvPid, SimplePdu) ->
    gen_server:call(?SERVER, {add_peer, SrvPid, SimplePdu}, infinity).

del_peer(SrvPid) ->
    gen_server:call(?SERVER, {del_peer, SrvPid}, infinity).

add_registration(SrvPid, SubTree, Priority) ->
    gen_server:call(?SERVER, {add_registration, SrvPid, SubTree, Priority}, 
		    infinity).
    
del_registration(SrvPid, SubTree, Priority) ->
    gen_server:call(?SERVER, {del_registration, SrvPid, SubTree, Priority},
		    infinity).
    

%%====================================================================
%% Server functions
%%====================================================================
init([IPAddress, Port, Secret, Params]) ->
    AcceptPid = proc_lib:spawn_link(?MODULE, accept,
				    [self(), IPAddress, Port, Params]),
    put(verbosity, log),
    {ok, #state{accept_pid = AcceptPid,
		agent = Params#params.agent,
		secret = Secret}}.


handle_call({check_passwd, Passwd}, _From, S) ->
    Reply = 
	case S#state.secret of
	    undefined ->
		ok;
	    Passwd ->
		ok;
	    _ ->
		authenticationFailure
	end,
    {reply, Reply, S};

handle_call({add_peer, SrvPid, SimplePdu}, _From, S) ->
    Reply = ok,
    {reply, Reply, S};

handle_call({del_peer, SrvPid, SimplePdu}, _From, S) ->
    Reply = ok,
    {reply, Reply, S};

handle_call({add_registration, SrvPid, SubTree, Priority}, _From, S) ->
    Reply = case snmp:register_subagent(S#state.agent, SubTree, SrvPid) of
		ok ->
		    ok;
		{error, Reason} ->
		    ?vlog("add_registration failed: ~p\n", [Reason]),
		    error
	    end,
    {reply, Reply, S};

handle_call({del_registration, SrvPid, SubTree, Priority}, _From, S) ->
    Reply = ok,
    {reply, Reply, S}.


handle_cast(Msg, S) ->
    {noreply, S}.

handle_info(Info, S) ->
    {noreply, S}.

terminate(Reason, S) ->
    ok.

code_change(OldVsn, S, Extra) ->
    {ok, S}.

%%====================================================================
%% Internal functions
%%====================================================================

%%----------------------------------------------------------------------
%% Accepting process. Sends connections to the server.
%%----------------------------------------------------------------------
accept(Parent, IPAddress, Port, Params) ->
    %% We don't want to die when a handler dies. We do want to tear
    %% all the handlers down if we crash, though, which is why we link
    %% with them.
%    process_flag(trap_exit, true),
    {ok, LSock} = gen_tcp:listen(Port, [binary,
					{reuseaddr, true},
					{packet, asn1},
					{active, false},
					{ip, IPAddress}]),
    accept_loop(Parent, LSock, Params).

accept_loop(Parent, LSock, Params) ->
    %% flush any exit messages from handlers
    flush_exits(Parent),
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn_handler(Sock, Params),
    accept_loop(Parent, LSock, Params).

flush_exits(Parent) ->
    receive
	{'EXIT', Parent, Reason} ->
	    exit(Reason);
	{'EXIT', _Pid, _Rsn} ->
	    flush_exits(Parent)
    after 0 ->
	    ok
    end.

%%----------------------------------------------------------------------
%% Request handler process
%%----------------------------------------------------------------------
spawn_handler(Socket, Params) ->
    case snmp_smux:get_pdu(Socket) of
	{ok, Open} when record(Open, smux_open),
	                Open#smux_open.version == ?smux_version_1  ->
	    case check_passwd(Open#smux_open.password) of
		ok ->
		    #params{agent = Agent, timeout = Timeout} = Params,
		    {ok, SrvPid} = 
			snmp_smux_server:start_link(Socket, Agent, Timeout),
		    gen_tcp:controlling_process(Socket, SrvPid),
		    add_peer(SrvPid, Open);
		authenticationFailure ->
		    snmp_smux:close(Socket, authenticationFailure, [])
	    end;
	{ok, Open} when record(Open, smux_open) ->
	    snmp_smux:close(Socket, unsupportedVersion,
			    Open#smux_open.version);
	{ok, OtherPDU} ->
	    snmp_smux:close(Socket, protocolError, OtherPDU);
	{error, Else} ->
	    snmp_smux:close(Socket, packetFormat, Else);
	Else ->
	    snmp_smux:close(Socket, internalError, Else)
    end.

check_passwd(Passwd) ->
    gen_server:call(?SERVER, {check_passwd, Passwd}, infinity).
