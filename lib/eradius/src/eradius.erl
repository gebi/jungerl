-module(eradius).
%%%-------------------------------------------------------------------
%%% File        : eradius.erl
%%% Author      : {mbj,tobbe}@bluetail.com>
%%% Description : RADIUS Authentication
%%% Created     :  7 Oct 2002 by Martin Bjorklund <mbj@bluetail.com>
%%% 
%%% $Id$
%%%-------------------------------------------------------------------
-behaviour(gen_server).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("eradius.hrl").
-include("eradius_lib.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0, start/0, auth/1, auth/3, auth/4, default_port/0,
	 load_tables/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
	 code_change/3]).

%% Internal exports
-export([worker/5]).


-record(state, {}).

-define(SERVER    , ?MODULE).
-define(TABLENAME , ?MODULE).

-define(WORKER_TIMEOUT, 10000).  % serious error if no answer after 10 sec 

-define(PORT, 1812).

default_port() -> ?PORT.



%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    eradius_dict:start_link(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
start() ->
    eradius_dict:start(),
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%%-----------------------------------------------------------------
%% Authenticate a user
%%-----------------------------------------------------------------

auth(E) ->
    auth(E, E#eradius.user, E#eradius.passwd).

auth(E, User, Passwd) ->
    auth(E, User, Passwd, <<>>).

auth(E, User, Passwd, CallState) when record(E, eradius) ->
    gen_server:call(?SERVER, {auth, E, User, Passwd, CallState},
		   infinity).

load_tables(Tables) ->
    eradius_dict:load_tables(Tables).


%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function:    init/1
%%
%% Description: Setup a server which starts/controls the worker
%%              processes which talk SMB authentication. There
%%              is one worker per SMB-XNET "domain".
%%
%% Returns:     {ok, State}         
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    create_ets_table(),
    {ok, #state{}}.

create_ets_table() ->
    ets:new(?TABLENAME, [named_table, public]),
    ets:insert(?TABLENAME, {id_counter, 0}).

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
handle_call({auth, E, User, Passwd, CState}, From, State) ->
    start_worker(From, E, User, Passwd, CState),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(_Req, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State };
handle_info(_, State) ->
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

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%%
%%% Worker process, performing the Radius request.
%%%
start_worker(From, E, User, Passwd, CState) ->
    Args = [From, E, User, Passwd, CState],
    proc_lib:spawn(?MODULE, worker, Args).

get_id() -> 
    bump_id().

worker(From, E, User, Passwd, CState) ->
    process_flag(trap_exit, true),
    Servers = E#eradius.servers,
    State = if CState /= <<>> -> binary_to_term(CState);
	       true -> <<>>
	    end,
    case catch wloop(E, User, Passwd, Servers, State) of
	{'EXIT', R} ->
	    gen_server:reply(From, {reject, ?AL_Internal_Error}),
	    exit(R);
	R ->
	    gen_server:reply(From, R)
    end.

wloop(E, User, Passwd, _, {{Ip,Port,Secret}, State}) ->
    %% Make sure the challenge reply goes to the same Radius server.
    wloop(E, User, Passwd, [[Ip,Port,Secret]], State);
wloop(E, User0, Passwd0, [[Ip,Port,Secret0]|Srvs], State) ->
    Id = get_id(),
    Auth = eradius_lib:mk_authenticator(),
    Secret = list_to_binary(Secret0),
    Passwd = list_to_binary(Passwd0),
    User   = list_to_binary(User0),
    RPasswd = eradius_lib:mk_password(Secret, Auth, Passwd),
    Pdu = #rad_pdu{reqid = Id,
		   authenticator = Auth,
		   cmd = #rad_request{user = User,
				      passwd = RPasswd,
				      state = get_radius_state(State),
				      nas_ip = E#eradius.nas_ip_address}},
    ?TRACEFUN(E,"sending RADIUS request for ~s to ~p",
	      [binary_to_list(User), {Ip, Port}]),
    Req = eradius_lib:enc_pdu(Pdu),
    StatKey = [E, Ip, Port],
    case send_recv_msg(Ip, Port, Req, E) of
	timeout ->
	    ?STATFUN_TIMEDOUT(E, Ip, Port),
	    ?TRACEFUN(E,"RADIUS request for ~p timed out", [{Ip, Port}]),
	    wloop(E, User, Passwd, Srvs, State);
	{challenge, CState, Rmsg} ->
	    %% Wrap the call-state with the server
	    %% to be used in the next attempt.
	    WState = {{Ip,Port,Secret0}, CState},
	    {challenge, term_to_binary(WState), Rmsg};
	{accept, Attribs} ->
	    ?STATFUN_ACCEPTED(E, Ip, Port),
	    ?TRACEFUN(E,"got RADIUS reply Accept for ~s with attributes: ~p",
		      [binary_to_list(User), Attribs]),
	    {accept, Attribs};
	{reject, Resp} ->
	    ?STATFUN_REJECTED(E, Ip, Port),
	    ?TRACEFUN(E,"got RADIUS reply Reject for ~s",
		      [binary_to_list(User)]),
	    {reject, Resp};
	_Err ->
	    error_logger:format("~w: reject due to ~p\n", [?MODULE, _Err]),
	    ?STATFUN_REJECTED(E, Ip, Port), % correct to increment here ?
	    {reject, ?AL_Internal_Error}
    end;
wloop(E, User, _Passwd, [], _State) ->
    ?TRACEFUN(E,"no more RADIUS servers to try for ~s",[binary_to_list(User)]),
    {reject, ?AL_Backend_Unreachable}.


get_radius_state({_E, _User, State}) ->
    case catch binary_to_term(State) of
	{{_Ip,_Port,_Secret0}, RadiusState} -> 
	    RadiusState;
	_ -> 
	    State
    end;
get_radius_state(RadiusState) when binary(RadiusState) ->
    RadiusState;
get_radius_state(What) ->
    What.


send_recv_msg(Ip, Port, Req, E) ->
    {ok, S} = gen_udp:open(0, [binary]),
    gen_udp:send(S, Ip, Port, Req),
    Resp = recv_wait(S, E#eradius.timeout),
    gen_udp:close(S),
    decode_response(Resp, E).


recv_wait(S, Timeout) ->
    receive 
	{udp, S, _IP, _Port, Packet} ->
	    eradius_lib:dec_packet(Packet)
    after Timeout ->
	    timeout
    end.

decode_response(Resp, E) when record(Resp, rad_pdu) ->
    Resp#rad_pdu.cmd;
decode_response(timeout, _AuthSpec) ->
    timeout;
decode_response(Resp, _AuthSpec) ->
    Resp. % can't end up here really...
