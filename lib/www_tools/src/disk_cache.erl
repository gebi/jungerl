-module(disk_cache).

%IA Joe Armstrong
%ID 970212
%IK [disk,cache,key,value]
%IH A disk Key-Value cache which uses dets
%IT <p><b>disk_cache:start(File)</b> starts a disk cache with the
% dets file <b>File</b>. If the file does not exist a new cache is
% created. Note Key is single valued.
% <p><b>store(Key, Value)</b> inserts a value in the cache.
% <p><b>fetch(Key) -> ok{Val} | not_found</b> retrieves a value
% <p><b>set_max(int())</b> sets the maximum size of the cache.
% <p><b>flush(Key)</b> clears an individual cache item.
% <p><b>clear()</b> clers the entire cache.
% <p><b>info() -> {Max, [Key]}</b> provides information aboiut the cache.
% <p><b>stop()</b> stops the cache.

-behaviour(gen_server).

%% This is the interface

-export([start/1, store/2, fetch/1, flush/1, clear/0, set_max/1, info/0, 
	 stop/0]).

%% And these are cos we use gen_server
-export([init/1, handle_call/3, terminate/2]).

-import(lists, [member/2, reverse/1]).

-define(SERVER, disk_cache_server).
-define(RPC(X), gen_server:call(?SERVER, X, 10000)).
-define(DEFAULT_CACHE_ENTRIES, 100).

%% start(File) -> ok (creates file if non existent)
%% store(Key, Val) -> ok
%% fetch(Key) -> {ok, Val} | not_found
%% flush(Key) -> ok
%% clear()    -> ok
%% set_max(N) -> ok
%% info()     -> {File, Max, [Key]}
%% stop()     -> ok

start(F)             -> gen_server:start({local,?SERVER},?MODULE,F,[]).
store(Key, Val)      -> ?RPC({store, Key, Val}).
fetch(Key)           -> ?RPC({fetch, Key}).
flush(Key)           -> ?RPC(flush).
clear()              -> ?RPC(clear).
set_max(Max) when integer(Max), Max > 0  -> ?RPC({set_max, Max}).
info()               -> ?RPC(info).
stop() 	             -> ?RPC(stop).

%% End Interface


init(File) ->
    io:format("Starting disk cache in file:~p\n", [File]),
    case dets:open_file(myFile, [{type,set}, {file, File}]) of
	{ok, Tab} ->
	    Max = get_key_or_default(Tab, max, ?DEFAULT_CACHE_ENTRIES),
	    Keys = get_key_or_default(Tab, keys, []),
	    {ok, {Tab, Max, Keys}};
	_ ->
	    io:format("Cannot open cache - run make_cache\n",[]),
	    {stop, cannot_open_cache}
    end.
handle_call({store, Key, Val}, _, {Tab, Max, Keys}) ->
    case member(Key, Keys) of
	true ->
	    %% remove the old value
	    dets:delete(Tab, {key, Key}),
	    Keys1 = move_to_front(Key, Keys),
	    dets:insert(Tab, {{item,Key},Val}),
	    {reply, ok, {Tab, Max, Keys1}};
	false ->
	    dets:insert(Tab, {{item,Key},Val}),
	    Keys1 = trim(Tab, [Key|Keys], Max),
	    {reply, ok, {Tab, Max, Keys1}}
    end;
handle_call({fetch, Key}, _, {Tab, Max, Keys}) ->
    case member(Key, Keys) of
	true ->
	    Keys1 = move_to_front(Key, Keys),
	    [{_, Val}] = dets:lookup(Tab, {item, Key}),
	    {reply, {ok, Val}, {Tab, Max, Keys1}};
	false ->
	    {reply, not_found, {Tab, Max, Keys}}
    end;
handle_call({flush, Key}, _, {Tab, Max, Keys}) ->
    case member(Key, Keys) of
	true ->
	    Keys1 = remove(Key, Keys),
	    dets:delete(Tab, {item, Key}),
	    {reply, ok, {Tab, Max, Keys1}};
	false ->
	    {reply, ok, {Tab, Max, Keys}}
    end;
handle_call(clear, _, {Tab, Max, Keys}) ->
    Keys1 = trim(Tab, Keys, 0),
    {reply, ok, {Tab, Max, Keys}};
handle_call({set_max,N}, _, {Tab, Max, Keys}) ->
    Keys1 = trim(Tab, Keys, N),
    {reply, ok, {Tab, N, Keys1}};
handle_call(info, _, {Tab, Max, Keys}) ->
    {reply, {info, Max, Keys}, {Tab, Max, Keys}};
handle_call(stop, _, {Tab, Max, Keys}) ->
    dets:insert(Tab, {max, Max}),
    dets:insert(Tab, {keys, Keys}),
    {stop, normal, ok, []}.

terminate(normal, _) ->
    true.


get_key_or_default(Tab, Key, Default) ->
    case dets:lookup(Tab, Key) of
	[] ->
	    Default;
	[{_,Val}] ->
	    Val
    end.

%% trim(Tab, Keys, Max)
%% Keys is an ordered list of keys
%% If the length of Keys is > max then the list
%% is trimmed to Max elements
%% and the last elements are removed from the data base

trim(Tab, Keys, Max) ->
    case length(Keys) of
	N when N > Max ->
	    trim1(N-Max, Tab, reverse(Keys));
	_ ->
	    Keys
    end.

trim1(0, Tab, Keys) -> 
    reverse(Keys);
trim1(N, Tab, [H|T]) ->
    dets:delete(Tab, {item, H}),
    trim1(N-1, Tab, T).

%% moves H which is *known* to be a member of L to the front of L

%% -type move_to_front(A, [A]) -> [A].

move_to_front(H, L) -> [H|remove(H,L)].

remove(X, [X|T]) -> T;
remove(X, [H|T]) -> [H|remove(X, T)].







