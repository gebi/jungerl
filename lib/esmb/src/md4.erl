-module(md4).
%%%----------------------------------------------------------------------
%%% File    : md4.erl
%%% Author  : Torbjorn Tornkvist <tobbe@bluetail.com>
%%% Purpose : MD4 
%%% Created : 12 Mar 2004 by <tobbe@bluetail.com>
%%%----------------------------------------------------------------------
-behaviour(gen_server).
-export([start_link/0, digest/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {port}).

-define(SERVER, ?MODULE).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

digest(String) when length(String) =< 16 ->
    gen_server:call(?SERVER, {digest, String}, infinity);
digest(Bin) when binary(Bin) ->
    digest(binary_to_list(Bin)).

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
init([]) ->
%%    erl_ddll:load_driver(code:priv_dir(esmb), "md4_drv"),
    erl_ddll:load_driver("/home/tobbe/jungerl/lib/esmb/priv", "md4_drv"),
    Port = open_port({spawn, 'md4_drv'}, [binary]),
    {ok, #state{port = Port}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({digest, String}, From, S) ->
    Reply = call_drv(S#state.port, String),
    {reply, Reply, S}.

call_drv(Port, String) ->
    erlang:port_command(Port, [String]),
    recv(Port).

recv(Port) ->
    receive
	{Port, value, Bin} ->
	    {ok,Bin};
	{Port, error, ErrAtom} ->
	    unlink(Port),
	    erlang:port_close(Port),
	    {error, ErrAtom}
    end.



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
handle_info(Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

