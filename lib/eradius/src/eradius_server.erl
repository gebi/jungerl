%%%----------------------------------------------------------------------
%%% File    : radius_server.erl
%%% Author  : ottuser local account <otpuser@tiger>
%%% Purpose : Generic Radius server. Uses mnesia to store RAS details, 
%%%           and relies on external implementation modules for the 
%%%           authentication/accounting logic.
%%% Created :  8 Dec 1999 by ottuser local account <otpuser@tiger>
%%% Todo    : Add counters, duplicate packet handling.
%%% $Id$
%%%----------------------------------------------------------------------

-module(eradius_server).

-behaviour(gen_server).

%% External exports
-export([start_link/0, start_link/2]).

-export([create_tables/1, define_ras/4, trace_on/2, trace_off/2]).
-export([radius/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("eradius_lib.hrl").


-record(state, {socket,       % Socket Reference of opened UDP port
		interface,    % Interface to which this socket is bound
		port,         % Port number we are listening on
		transacts     % ETS table containing current transactions
               }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    start_link({127,0,0,1}, 1812).

start_link({A,B,C,D}=If, Port) ->
    Name = list_to_atom(lists:flatten(io_lib:format("rad_~p_~p_~p_~p_~p", [A,B,C,D,Port]))),
    gen_server:start_link({local, Name}, ?MODULE, {If, Port}, []).


create_tables(Nodes) ->
    mnesia:create_table(nas_prop, [{attributes, record_info(fields, nas_prop)},
                                   {disc_copies, Nodes}]).

%% All RAS devices must be known about otherwise packets from them
%% will be discarded. MF must be of the form {Mod, Func}.
define_ras(IP, Port, Secret, {Mod, Func}) ->
    mnesia:dirty_write(#nas_prop{ip = {IP, Port},
                                 secret = Secret,
                                 mf = {Mod, Func}}).

trace_on(IP, Port) ->
    set_trace(IP, Port, true).

trace_off(IP, Port) ->
    set_trace(IP, Port, false).

set_trace(IP, Port, Bool) ->
    F = fun() ->
                case mnesia:read({nas_prop, {IP, Port}}) of
                    [Nas] ->
                        mnesia:write(Nas#nas_prop{trace = Bool});
                    [] ->
                        {error, nas_not_defined}
                end
        end,
    mnesia:transaction(F).


%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init({Interface, Port}) ->
    process_flag(trap_exit, true),
    case gen_udp:open(Port, [{active, once}, {ip, Interface}, binary]) of
	{ok, Socket} ->
	    {ok, #state{socket = Socket,
                        interface = Interface,
			port = Port,
			transacts = ets:new(transacts, [])}};
	{error, Reason} ->
	    {stop, Reason}
    end.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({udp, Socket, IP, InPortNo, Packet} = Req, State) ->
    case dec_radius(IP, Packet, State#state.port) of
	{ok, Req_id, Nas_prop} ->
	    T = State#state.transacts,
	    case ets:lookup(T, {Req_id, IP}) of
		[] ->
		    Pid = proc_lib:spawn_link(?MODULE, radius, [self(), Req, Req_id, Nas_prop]),
		    ets:insert(T, {{Req_id, IP}, Pid}),
                    inet:setopts(Socket, [{active, once}]),
		    {noreply, State};
		[{{Req_id, IP}, Pid_old}] ->
                    %% Duplicate request.  We assume that the previous
                    %% request will still answer.  We should probably
                    %% also store old responses for some time so we
                    %% can return what was originally sent if the
                    %% duplicate request arrived after we had already
                    %% sent our answer. This is the only reason to
                    %% even store the transaction.
                    inet:setopts(Socket, [{active, once}]),
		    {noreply, State}
	    end;
	{discard, Reason, Counter} ->
	    error("Discarded request from ~1000.p, Reason: ~1000.p~n", [{IP,InPortNo}, Reason]),
            inet:setopts(Socket, [{active, once}]),
	    {noreply, State}
    end;

handle_info({reply, IP, Port, Req_id, Nas_prop, Reply}, State) ->
    dbg(Nas_prop, "sending auth response for ~1000.p~n", [{Req_id, IP, Port}]),
    ets:delete(State#state.transacts, {Req_id, IP}),
    gen_udp:send(State#state.socket, IP, Port, Reply),
    {noreply, State};

handle_info({discard, IP, Req_id, Nas_prop, Reason}, State) ->
    dbg(Nas_prop, "discarding response for ~1000.p~n", [{Req_id, IP, Reason}]),
    ets:delete(State#state.transacts, {Req_id, IP}),
    {noreply, State};

handle_info({'EXIT', Pid, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
    T = State#state.transacts,
    case ets:match_object(T, {'_', Pid}) of
	[{{Tr_id, IP}, Pid}] ->
	    ets:delete(T, {Tr_id, IP}),
	    {noreply, State};
	[] ->
	    {noreply, State}
    end;
handle_info(Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    gen_udp:close(State#state.socket),
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Deal with incoming radius packets
%%----------------------------------------------------------------------
dec_radius(IP, Packet, Port) ->
    case allowed_ip(IP, Port) of       	% Is request from a specified IP address?
	{ok, Nas_prop} ->		% Yes, and this is the shared secret etc for this
            case get_trid(Packet) of
                {ok, Req_id} ->
                    {ok, Req_id, Nas_prop};
		{discard, Reason} ->
		    {discard, Reason, dis_bad_packet}
	    end;
	not_allowed ->
	    {discard, "Radius Request from non allowed IP address.", dis_bad_ras}
    end.

get_trid(<<Code, Tr_id, _/binary>>) ->
    {ok, Tr_id};
get_trid(_) ->
    {discard, "Radius request packet size too small to start"}.

allowed_ip(IP, Port) ->
    case mnesia:dirty_read(nas_prop, {IP, Port}) of
	[] ->
	    not_allowed;
	[Nas_prop] ->
	    {ok, Nas_prop}
    end.


error(Fmt, Vals) ->
    error_logger:error_report([{application, radius},
			       lists:flatten(io_lib:format(Fmt, Vals))]).


dbg(on, Text, Vals) ->
    io:format("~s -- "++Text, [printable_date() | Vals]);
dbg(#nas_prop{trace = true}, Text, Vals) ->
    io:format("~s -- "++Text, [printable_date() | Vals]);
dbg(_, _, _) -> ok.


%%-----------------------------------------------------------------------
%% One of these is spawned for every radius request. This provides us
%% with nice fault isolation and allows remote LDAP servers to be as
%% slow as they like :)
%%-----------------------------------------------------------------------
radius(Server_pid, {udp, Socket, IP, InPortNo, Packet}, Req_id, Nas_prop) ->
    case catch eradius_lib:dec_packet(Packet) of
        #rad_pdu{} = Req_pdu ->
            {M, F} = Nas_prop#nas_prop.mf,
            Result = (catch M:F(Req_pdu, Nas_prop)),
            case encode_reply(Result, Req_pdu, Nas_prop#nas_prop.secret) of
                {reply, Reply_packet} ->
                    Server_pid ! {reply, IP, InPortNo, Req_id, Nas_prop, Reply_packet};
                {discard, Reason} ->
                    Server_pid ! {discard, IP, Req_id, Nas_prop, Reason}
            end;
        Else ->
            Server_pid ! {discard, IP, Req_id, Nas_prop, Else}
    end.

encode_reply({'EXIT', Reason}, _Pdu, _Secret) ->
    {discard, Reason};
encode_reply(Resp, Req_pdu, Secret) ->
    Reply = eradius_lib:enc_reply_pdu(Req_pdu#rad_pdu{cmd = Resp}, Secret),
    {reply, Reply}.

printable_date() ->
    {{Y,Mo,D},{H, M, S}} = calendar:local_time(),
    {_,_,MicroSecs} = now(),
    lists:flatten(io_lib:format("~p-~p-~p_~p:~p:~p:~p", [Y,Mo,D,H,M,S,MicroSecs div 1000])).
