-module(md4).
%%%----------------------------------------------------------------------
%%% File    : md4.erl
%%% Author  : Torbjorn Tornkvist <tobbe@bluetail.com>
%%% Purpose : MD4 
%%% Created : 12 Mar 2004 by <tobbe@bluetail.com>
%%%----------------------------------------------------------------------
-behaviour(gen_server).
-export([start/0, start_link/0, digest/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-record(state, {port}).

-define(DRV_NAME, "md4_drv").
-define(SERVER, ?MODULE).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

digest(String) ->
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
    erl_ddll:start(),
    {ok, Path} = load_path(?DRV_NAME ++ ".so"),
    erl_ddll:load_driver(Path, ?DRV_NAME),
    Port = open_port({spawn, ?DRV_NAME}, [binary]),
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
handle_call({digest, String}, _From, S) ->
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
handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


code_change(_, _, _) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

load_path(File) ->
    case lists:zf(fun(Ebin) ->
			  Priv = Ebin ++ "/../priv/",
			  case file:read_file_info(Priv ++ File) of
			      {ok, _} -> {true, Priv};
			      _ -> false
			  end
		  end, code:get_path()) of
        [Dir|_] ->
            {ok, Dir};
        [] ->
            error_logger:format("Error: ~s not found in code path\n", [File]),
            {error, enoent}
    end.
