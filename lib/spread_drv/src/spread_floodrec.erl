%%%----------------------------------------------------------------------
%%% File    : spread_floodrec.erl
%%% Author  : Scott Lystig Fritchie <slfritchie@snookles.com>
%%% Purpose : Spread flooder receiver
%%%
%%% usage: {ok, Pid} = spread_floodrec:start_link("flooder").
%%%
%%% Then run the "spflooder" application, which is distributed with 
%%% the Spread toolkit.  The spread_floodrec process will spit out 
%%% a message every time it receives 1,000 messages from the "flooder"
%%% group.  At any time, spread_floodrec:dump_state(Pid) can be used
%%% to dump the (simple) state of the gen_server.  Currently it just
%%% keeps track of how many regular messages and how many membership
%%% messages have been received.
%%%
%%%----------------------------------------------------------------------

-module(spread_floodrec).

%%-compile(export_all).
%%-export([Function/Arity, ...]).

-behaviour(gen_server).

-define(NAME, ?MODULE).
-define(Timeout, infinity).

%% External exports
-export([start_link/1]).
-export([dump_state/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
	  pid,					% Pid of spread
	  membs = 0,
	  msgs = 0
	 }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(Group) ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [Group], []).

dump_state(Pid) ->
    gen_server:call(Pid, {dump_state}, ?Timeout).

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
init([Group]) ->
    {ok, Pid} = spread:start_link(),
    spread:subscribe(Pid, Group, true),
    {ok, #state{pid = Pid}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({dump_state}, From, State) ->
    Reply = State,
    {reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    io:format("XXXYYYXXX ~w: ~s:handle_cast got ~w\n", [self(), ?MODULE, Msg]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({spread, Pid, membership, _, _}, State)
  when Pid == State#state.pid ->
    N = State#state.membs + 1,
    {noreply, State#state{membs = N}};
handle_info({spread, Pid, msg, _, _, _, _, _}, State)
  when Pid == State#state.pid ->
    N = State#state.msgs + 1,
    if N rem 1000 == 0 ->
	    io:format("Msg count = ~w\n", [N]);
       true -> ok
    end,
    {noreply, State#state{msgs = N}};
handle_info(Info, State) ->
    io:format("XXXYYYXXX ~w: ~s:handle_info got ~w\n", [self(), ?MODULE, Info]),
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
