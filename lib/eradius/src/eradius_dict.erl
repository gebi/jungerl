-module(eradius_dict).
%%%----------------------------------------------------------------------
%%% File    : eradius_dict.erl
%%% Author  : Torbjorn Tornkvist <tobbe@bluetail.com>
%%% Purpose : Radius dictionary handling.
%%% Created : 25 Sep 2003 by Torbjorn Tornkvist <tobbe@bluetail.com>
%%%----------------------------------------------------------------------
-behaviour(gen_server).

%% External exports
-export([start/0,start_link/0, lookup/1]).
-export([load_tables/1, mk_dict/1, parse_dict/1, make/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).


-include("eradius_dict.hrl").


-define(SERVER    , ?MODULE).
-define(TABLENAME , ?MODULE).

-record(state, {}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

lookup(Id) -> 
    ets:lookup(?TABLENAME, Id).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).


load_tables(Tables) ->
    gen_server:call(?SERVER, {load_tables, Tables}, infinity).



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
    process_flag(trap_exit, true),
    create_table(),
    {ok, #state{}}.

create_table() ->
    ets:new(?TABLENAME, [named_table, {keypos, 2}, public]).


%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({load_tables, Tables}, From, State) ->
    Res = (catch lists:foreach(fun(Tab) -> load_table(Tab) end, Tables)),
    {reply, Res, State}.

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


%%% --------------------------------------------------------------------
%%% Load dictionary table
%%% --------------------------------------------------------------------

load_table(Table) ->
    Dir = priv_dir(),
    MapFile = Dir ++ "/" ++ Table ++ ".map",
    case file:consult(MapFile) of
	{ok, Res} ->
	    lists:foreach(fun(R) -> ets:insert(?TABLENAME, R) end, Res),
	    ok;
	Error ->
	    {error, load_table}
    end.
			  


%%% --------------------------------------------------------------------
%%% Dictionary making
%%% --------------------------------------------------------------------

%%% Assume this function is called from the Makefile in the priv dir.
make([File]) ->
    {ok, Dir} = file:get_cwd(),
    mk_dict(Dir ++ "/" ++ atom_to_list(File)),
    init:stop().
    

mk_dict(File) ->
    Res = parse_dict(File),
    Dir = dir(?MODULE),
    mk_outfiles(Res, Dir, o2u(File)).

mk_outfiles(Res, Dir, File) ->
    {Hrl, Map} = open_files(Dir, File),
    emit(Res, Hrl, Map),
    close_files(Hrl, Map).

emit([A|T], Hrl, Map) when record(A, attribute) ->
    io:format(Hrl, "-define( ~s , ~w ).~n", 
	      [A#attribute.name, A#attribute.id]),
    io:format(Map, "{attribute, ~w, ~w, \"~s\"}.~n", 
	      [A#attribute.id, A#attribute.type, A#attribute.name]),
    emit(T, Hrl, Map);
emit([_|T], Hrl, Map) ->
    emit(T, Hrl, Map);
emit([], _, _) ->
    true.

open_files(Dir, File) ->
    [Name|_] = lists:reverse(string:tokens(File, "/")),
    Hfile = Dir ++ "/include/" ++ Name ++ ".hrl",
    {ok,Hrl} = file:open(Hfile, [write]),
    Mfile = Dir ++ "/priv/" ++ Name ++ ".map",
    {ok,Map} = file:open(Mfile, [write]),
    io:format("Creating files: ~n  <~s>~n  <~s>~n", [Hfile, Mfile]),
    {Hrl, Map}.

close_files(Hrl, Map) ->
    file:close(Hrl),
    file:close(Map).

parse_dict(File) when list(File) ->
    {ok,B} = file:read_file(File),
    F = fun(Line,Acc) ->
		case pd(string:tokens(Line,"\s\t\r")) of
		    {ok,E} -> [E|Acc];
		    _      -> Acc
		end
	end,
    lists:foldl(F,[],string:tokens(b2l(B),"\n")).

pd(["VENDOR", Name, Id]) -> 
    put({vendor,Name}, l2i(Id));
pd(["ATTRIBUTE", Name, Id, Type]) -> 
    {ok,#attribute{name = d2u(Name), id = id2i(Id), type = l2a(Type)}};
pd(["ATTRIBUTE", Name, Id, Type, Vendor]) -> 
    VendId = get({vendor,Vendor}),
    {ok,#attribute{name = d2u(Name), id = {VendId,id2i(Id)},type = l2a(Type)}};
pd(X) -> 
    %%io:format("Skipping: ~p~n", [X]),
    false.

priv_dir() ->
    dir(?MODULE) ++ "/priv".
    
dir(Mod) ->
    P = code:which(Mod),
    [_,_|R] = lists:reverse(string:tokens(P,"/")),
    lists:foldl(fun(X,Acc) -> Acc ++ [$/|X] end, "", lists:reverse(R)).

id2i(Id) ->
    case catch l2i(Id) of
	I when integer(I) -> I;
	{'EXIT', _} ->
	    hex2i(Id)
    end.

%% FIXME , too ugly...
hex2i("0x" ++ [A]) -> x2i(A);
hex2i("0x" ++ [A,B]) -> x2i(A)*16 + x2i(B);
hex2i("0x" ++ [A,B,C]) -> x2i(A)*16*16 + x2i(B)*16 + x2i(C);
hex2i("0x" ++ [A,B,C,D]) -> x2i(A)*16*16*16 + x2i(B)*16*16 + x2i(C)*16 + x2i(D).

x2i(X) when X =< $9, X >= $0  -> X-48;
x2i(X) when X == $A ; X == $a -> 16#a;
x2i(X) when X == $B ; X == $b -> 16#b;
x2i(X) when X == $C ; X == $c -> 16#c;
x2i(X) when X == $D ; X == $d -> 16#d;
x2i(X) when X == $E ; X == $e -> 16#e;
x2i(X) when X == $F ; X == $f -> 16#f.

b2l(B) when binary(B) -> binary_to_list(B);
b2l(L) when list(L)   -> L.

l2i(L) when list(L)    -> list_to_integer(L);
l2i(I) when integer(I) -> I.

l2a(L) when list(L) -> list_to_atom(L);
l2a(A) when atom(A) -> A.

%%% Replace all dashes with underscore characters.
d2u(L) when list(L) ->
    repl(L, $-, $_).

%%% Replace all dots with underscore characters.
o2u(L) when list(L) ->
    repl(L, $., $_).

repl(L,X,Y) when list(L) ->
    F = fun(Z) when Z == X -> Y;
	   (C) -> C
	end,
    lists:map(F,L).

