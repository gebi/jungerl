%%%-------------------------------------------------------------------
%%% File    : memcached.erl
%%% Author  : David N. Welton <davidw@efoninc.com>
%%% Description : memcached client for Erlang
%%% Protocol described here: http://cvs.danga.com/browse.cgi/wcmtools/memcached/doc/protocol.txt?rev=HEAD
%%%
%%% Created : 20 Sep 2007 by David N. Welton <davidw@efoninc.com>
%%%-------------------------------------------------------------------
-module(memcached).

-behaviour(gen_server).

%% API
-export([start_link/2, mcset/3, mcset/5, mcget/2, mcquit/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {sock}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, Port) ->
    gen_server:start_link(?MODULE, [Host, Port], []).

%%--------------------------------------------------------------------
%% Function: mcset(Pid, Key, Bytes) -> 
%%
%% Description: Associates Bytes with Key in memcached.
%%--------------------------------------------------------------------

mcset(Pid, Key, Bytes) ->
    gen_server:call(Pid, {set, Key, Bytes}).

%%--------------------------------------------------------------------
%% Function: mcset(Pid, Key, Flags, Expire, Bytes) -> 
%%
%% Description: Assocates Bytes with Key, with flags, and a possible
%% expiration date.
%%--------------------------------------------------------------------

mcset(Pid, Key, Flags, Expire, Bytes) ->
    gen_server:call(Pid, {set, Key, Flags, Expire, Bytes}).

%%--------------------------------------------------------------------
%% Function: mcget(Pid, Key) -> 
%%
%% Description: Returns Bytes associated with Key.
%%--------------------------------------------------------------------

mcget(Pid, Key) when is_atom(Key) ->
    io:format("Keys2 ~p ~n", [Key]),
    gen_server:call(Pid, {get, [Key]});
%% This could be a list like [foo, bar] or ["foo", "bar"]
mcget(Pid, [Head|Tail]) when is_atom(Head) ->
    Keys = [Head] ++ Tail,
    io:format("Keys1 ~p ~n", [Keys]),
    gen_server:call(Pid, {get, Keys});
mcget(Pid, [Head|Tail]) when is_list(Head) ->
    Keys = [Head] ++ Tail,
    io:format("Keys1 ~p ~n", [Keys]),
    gen_server:call(Pid, {get, Keys});
mcget(Pid, Key) ->
    gen_server:call(Pid, {get, [Key]}).

%%--------------------------------------------------------------------
%% Function: mcquit(Pid) -> 
%%
%% Description: Terminate the connection.
%%--------------------------------------------------------------------
mcquit(Pid) ->
    gen_server:call(Pid, quit).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, Port]) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {active, false}]),
    {ok, #state{sock = Sock}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get, Keys}, _From, #state{sock = Sock} = S) ->
    Reply = process_get(Sock, Keys),
    {reply, Reply, S#state{sock = Sock}};
handle_call({set, Key, Bytes}, _From, #state{sock = Sock} = S) ->
    Reply = process_set(Sock, Key, 0, 0, Bytes),
    {reply, Reply, S#state{sock = Sock}};
handle_call({set, Key, Flags, Expire, Bytes}, _From, #state{sock = Sock} = S) ->
    Reply = process_set(Sock, Key, Flags, Expire, Bytes),
    {reply, Reply, S#state{sock = Sock}};
handle_call({delete, Key}, _From, #state{sock = Sock} = S) ->
    Reply = ok,
    {reply, Reply, #state{sock = Sock} = S};
handle_call(quit, _From, #state{sock = Sock} = S) ->
    gen_tcp:close(Sock),
    {reply, ok, {}}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% If it's an atom, transform it.  If it's a string, leave it alone.
to_list(Key) when is_atom(Key) ->
    atom_to_list(Key);
to_list(Key) when is_list(Key) ->
    Key.

%% Parse the get response.
parse_responses(<<"END\r\n", _/binary>>, Acc) ->
    Acc;
parse_responses(<<"\r\n", Data/binary>>, Acc) ->
    parse_responses(Data, Acc);
parse_responses(<<"VALUE ", Data/binary>>, Acc) ->
    {ok, [Key, Flags, Len], More} = io_lib:fread("~s ~u ~u\r\n", binary_to_list(Data)),
    <<Bytes:Len/binary, Rest/binary>> = list_to_binary(More),
    parse_responses(Rest, Acc ++ [binary_to_term(Bytes)]).

%% Send get and handle the response.
process_get(Sock, Keys) ->
    io:format("Keys ~p ~n", [Keys]),
    KeyList = [to_list(X) || X <- Keys],
    io:format("CHECK ~p ~n", [KeyList]),
    ok = gen_tcp:send(Sock, list_to_binary(["get ", string_join(KeyList), "\r\n"])),
    {ok, <<Data/binary>>} = gen_tcp:recv(Sock, 0),
    {ok, parse_responses(Data, [])}.

%% Set set and handle the response.
process_set(Sock, Key, Flags, Expire, Data) ->
    K = to_list(Key),
    Bytes = term_to_binary(Data),
    Len = size(Bytes),
    L = list_to_binary(
	  io_lib:format("set ~s ~p ~p ~p", [K, Flags, Expire, Len])),
    Line = <<L/binary, "\r\n">>,
    ok = gen_tcp:send(Sock, Line),
    ok = gen_tcp:send(Sock, <<Bytes/binary, "\r\n">>),
    {ok, Response} = gen_tcp:recv(Sock, 0),
    case Response of
	<<"STORED\r\n">> ->
	    ok;
	<<"NOT_STORED\r\n">> ->
	    not_stored
    end.

%% When oh when will we see this in Erlang proper.
string_join(Items) ->
    string_join(Items, " ").

string_join(Items, Sep) ->
    lists:flatten(lists:reverse(string_join1(Items, Sep, []))).

string_join1([Head | []], _Sep, Acc) ->
    [Head | Acc];
string_join1([Head | Tail], Sep, Acc) ->
    string_join1(Tail, Sep, [Sep, Head | Acc]).
