-module(eradius_acc).
%%%-------------------------------------------------------------------
%%% File    : eradius_acc.erl
%%% Author  : Torbjorn Tornkvist <tobbe@bluetail.com>
%%% Desc    : RADIUS accounting.
%%% Created :  9 Apr 2003 by Torbjorn Tornkvist <tobbe@bluetail.com>
%%%
%%% $Id$
%%%-------------------------------------------------------------------

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include("eradius_lib.hrl").
-include_lib("kernel/include/inet.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0, acc_on/1, acc_off/1, 
	 acc_start/1, acc_stop/1, 
	 validate_servers/1, start/0, 
	 set_user/2, set_nas_ip_address/1, set_nas_ip_address/2, 
 	 set_sockopts/2,
	 set_login_time/1, set_logout_time/1, set_session_id/2, new/0, 
	 set_radacct/1, set_attr/3, set_vend_attr/3, acc_update/1,
	 set_servers/2, set_timeout/2, set_login_time/2,  set_vendor_id/2,
	 set_logout_time/2, set_tc_ureq/1, 
	 set_tc_itimeout/1,set_tc_stimeout/1,
	 set_tc_areset/1, set_tc_areboot/1, 
	 set_tc_nasrequest/1, set_tc_nasreboot/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-ifdef(debug).
-export([test/0,test/1,test_stop/0]).
-endif.

%% The State record
-record(s, {
	  r         % #radacct{} record
	 }).   

-define(SERVER,     ?MODULE).
-define(TABLENAME,  ?MODULE).
-define(PORT,       1813).     % standard port for Radius Accounting
-define(TIMEOUT,    10).       


%%% ====================================================================
%%% External interface
%%% ====================================================================

%%% Create ADT
new() -> #rad_accreq{}.

%%% Set (any) Attribute
set_attr(R, Type, Bval) when record(R,rad_accreq),integer(Type),binary(Bval)->
    StdAttrs = R#rad_accreq.std_attrs,
    R#rad_accreq{std_attrs = [{Type, Bval} | StdAttrs]}.

%%% Vendor Attributes
set_vend_attr(R, Type, Bval) when record(R,rad_accreq),
				  integer(Type),binary(Bval)->
    VendAttrs = R#rad_accreq.vend_attrs,
    R#rad_accreq{vend_attrs = [{Type, Bval} | VendAttrs]}.

%%% Vendor Id
set_vendor_id(R, VendId) when record(R, rad_accreq),integer(VendId) ->
    R#rad_accreq{vend_id = VendId}.

%%% User
set_user(R, User) when record(R, rad_accreq) ->
    R#rad_accreq{user = any2bin(User)}.

%%% NAS-IP
set_nas_ip_address(R) when record(R, rad_accreq) ->
    R#rad_accreq{nas_ip = nas_ip_address()}.

set_nas_ip_address(R, Ip) when record(R, rad_accreq),tuple(Ip) ->
    R#rad_accreq{nas_ip = Ip}.

%%% Extra socket options
set_sockopts(R, SockOpts) when record(R, rad_accreq),list(SockOpts) ->
    R#rad_accreq{sockopts = SockOpts}.

%%% Login / Logout
set_login_time(R) ->
    set_login_time(R, erlang:now()).

set_login_time(R, Login) when record(R, rad_accreq) ->
    R#rad_accreq{login_time = Login}.

set_logout_time(R) ->
     set_logout_time(R, erlang:now()).

set_logout_time(R, Logout) when record(R, rad_accreq) ->
    SessTime = compute_session_time(R#rad_accreq.login_time, Logout),
    R#rad_accreq{session_time = SessTime,
		 logout_time = Logout}.

%%% Terminate Cause
set_tc_ureq(R) when record(R, rad_accreq) ->
    R#rad_accreq{term_cause = ?RTCUser_Request}.

set_tc_itimeout(R) when record(R, rad_accreq) ->
    R#rad_accreq{term_cause = ?RTCIdle_Timeout}.

set_tc_stimeout(R) when record(R, rad_accreq) ->
    R#rad_accreq{term_cause = ?RTCSession_Timeout}.

set_tc_areset(R) when record(R, rad_accreq) ->
    R#rad_accreq{term_cause = ?RTCAdmin_Reset}.

set_tc_areboot(R) when record(R, rad_accreq) ->
    R#rad_accreq{term_cause = ?RTCAdmin_Reboot}.

set_tc_nasrequest(R) when record(R, rad_accreq) ->
    R#rad_accreq{term_cause = ?RTCNAS_Request}.

set_tc_nasreboot(R) when record(R, rad_accreq) ->
    R#rad_accreq{term_cause = ?RTCNAS_Reboot}.

%%% Session ID
set_session_id(R, Id) when record(R, rad_accreq) ->
    R#rad_accreq{session_id = any2bin(Id)}.

%%% Server Info
set_servers(R, Srvs) when record(R, rad_accreq) ->
    R#rad_accreq{servers = Srvs}.

set_timeout(R, Timeout) when record(R, rad_accreq),integer(Timeout) ->
    R#rad_accreq{timeout = Timeout}.

set_radacct(Radacct) when record(Radacct,radacct) ->
    gen_server:call(?SERVER, {set_radacct, Radacct}).


%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).



%%-----------------------------------------------------------------
%% Func: auth(User, Passwd, AuthSpec)
%% Types: 
%% Purpose: 
%%-----------------------------------------------------------------

acc_on(Req) when record(Req,rad_accreq) ->
    gen_server:cast(?SERVER, {acc_on, Req}).

acc_off(Req) when record(Req,rad_accreq) ->
    gen_server:cast(?SERVER, {acc_off, Req}).

acc_start(Req) when record(Req,rad_accreq) ->
    gen_server:cast(?SERVER, {acc_start, Req}).

acc_stop(Req) when record(Req,rad_accreq) ->
    gen_server:cast(?SERVER, {acc_stop, Req}).

acc_update(Req) when record(Req,rad_accreq) ->
    gen_server:cast(?SERVER, {acc_update, Req}).


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
    create_ets_table(),
    {ok, #s{}}.

create_ets_table() ->
    ets:new(?TABLENAME, [named_table, public]),
    ets:insert(?TABLENAME, {id_counter, 0}).
 
get_id() -> 
    bump_id().
 
bump_id() ->
    ets:update_counter(?TABLENAME, id_counter, 1).


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
handle_call({set_radacct, R}, _From, State) when record(R, radacct) ->
    {reply, ok, State#s{r = R}}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({acc_on, Req}, State) ->
    punch_acc(Req, State, ?RStatus_Type_On),
    {noreply, State};
%%
handle_cast({acc_off, Req}, State) ->
    punch_acc(Req, State, ?RStatus_Type_Off),
    {noreply, State};
%%
handle_cast({acc_start, Req}, State) ->
    punch_acc(Req, State, ?RStatus_Type_Start),
    {noreply, State};
%%
handle_cast({acc_stop, Req}, State) ->
    punch_acc(Req, State, ?RStatus_Type_Stop),
    {noreply, State};
%%
handle_cast({acc_update, Req}, State) ->
    punch_acc(Req, State, ?RStatus_Type_Update),
    {noreply, State}.

punch_acc(Req, State, Stype) ->
    case get_servers(Req,State) of
	{Srvs,Timeout} ->
	    punch(Srvs, Timeout,
		 Req#rad_accreq{status_type = Stype});
	_ ->
	    false
    end.

%% Servers defined in the rad_accreq{} record
%% overrides the State info.
get_servers(Req,State) ->
    Def = #rad_accreq{},
    case {Req#rad_accreq.servers, Def#rad_accreq.servers} of
	{X,X} ->
	    %% Ok, Req hadn't set the servers so lets
	    %% use whatever we have in the State.
	    if record(State#s.r, radacct) -> 
		    R = State#s.r,
		    {R#radacct.servers, R#radacct.timeout};
	       true ->
		    false
	    end;
	{Srvs,_} ->
	    {Srvs, Req#rad_accreq.timeout}
    end.
	 
	    

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% -------------------------------------------------------------------
%%% Internal functions
%%% -------------------------------------------------------------------

punch(Srvs, Timeout, Req) ->
    spawn(fun() -> do_punch(Srvs, Timeout, Req) end).

do_punch([], _Timeout, _Req) ->
    %% FIXME some nice syslog message somewhere perhaps ?
    false;
do_punch([[Ip,Port,Shared] | Rest], Timeout, Req) ->
    Id = get_id(),
    PDU = eradius_lib:enc_accreq(Id, Shared, Req),
    case send_recv_msg(Ip, Port, Timeout, PDU) of
	timeout ->
	    %% NB: We could implement a re-send strategy here
	    %% along the lines of what the RFC proposes.
	    do_punch(Rest, Timeout, Req);
	Resp when record(Resp, rad_pdu) ->
	    %% Not really necessary...
	    if record(Resp#rad_pdu.cmd, rad_accresp) -> true;
	       true                                  -> false
	    end
    end.
	    
send_recv_msg(Ip, Port, Timeout, Req) ->
    {ok, S} = gen_udp:open(0, [binary]),
    gen_udp:send(S, Ip, Port, Req),
    Resp = recv_wait(S, Timeout),
    gen_udp:close(S),
    Resp.

recv_wait(S, Timeout) ->
    receive 
	{udp, S, _IP, _Port, Packet} -> 
	    eradius_lib:dec_packet(Packet)
    after Timeout ->
	    timeout 
    end.


%% Login = Logout = {MSec, Sec, uSec} | integer()
%% (In the second form it is erlang:now() in seconds)
compute_session_time(Login0, Logout0) ->
    Login = to_now(Login0),
    Logout = to_now(Logout0),
    calendar:datetime_to_gregorian_seconds(calendar:now_to_local_time(Logout)) -
	calendar:datetime_to_gregorian_seconds(calendar:now_to_local_time(Login)).

to_now(Now = {MSec, Sec, USec}) when is_integer(MSec),
				     is_integer(Sec), is_integer(USec) ->
    Now;
to_now(Now) when is_integer(Now) ->
    {Now div 1000000, Now rem 1000000, 0}.


any2bin(I) when integer(I) -> list_to_binary(integer_to_list(I));
any2bin(L) when list(L)    -> list_to_binary(L);
any2bin(B) when binary(B)  -> B.


%%%
%%% Registry validation and typecheck stuff
%%%

validate_servers(_X) ->
    true.


nas_ip_address() ->
    node2ip(node()).

node2ip(Node) ->
    host2ip(node2host(Node)).

node2host(Node) ->
    n2h(atom_to_list(Node)).

n2h([$@ | Host]) -> Host;
n2h([_H | T])    -> n2h(T);
n2h([])          -> [].

host2ip(Host) ->
    {ok, #hostent{h_addr_list = [Ip | _]}} = inet:gethostbyname(Host),
    Ip.
