%%%----------------------------------------------------------------------
%%% File    : edit_keymap.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Keymap implemented as an ETS table
%%% Created : 23 Sep 2000 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(edit_keymap).
-author('luke@bluetail.com').

-export([start_link_server/0, new/1, keymap_exists/1,
	 global_set_key/2, set_key/3,
	 bind/3, bind_each/2, delete/1, lookup/2, test/0]).

-export([server/0]).

start_link_server() ->
    case whereis(?MODULE) of
        Pid when pid(Pid) ->
            {error, {already_started, Pid}};
        undefined ->
            Pid = spawn_link(?MODULE, server, []),
            register(?MODULE, Pid),
            {ok, Pid}
    end.

server() ->
    receive
        {request, From, {create, Name}} ->
            Reply = case keymap_exists(Name) of
                        true ->
                            {error, {already_exists, Name}};
                        false ->
                            ets:new(Name, [named_table, set, public]),
                            Name
                    end,
            From ! {reply, Reply};
        _ ->
            true
    end,
    server().

new(Name) ->
    ?MODULE ! {request, self(), {create, Name}},
    receive {reply, R} -> R end.

keymap_exists(Name) ->
    case ets:info(Name) of
	undefined ->
	    false;
	_ ->
	    true
    end.

global_set_key(KeySeq, Binding) ->
    set_key(global_map, KeySeq, Binding).

%% e.g. set_key(edit_globalmap, "C-x C-f", {edit_file, find_file, []})
set_key(KeyMap, KeySeq, Binding) ->
    set_key_traverse(KeyMap, string:tokens(KeySeq, " "), Binding).

set_key_traverse(KeyMap, [Key], Binding) ->
    bind(KeyMap, Key, Binding),
    ok;
set_key_traverse(KeyMap, [MapKey | KeySeq], Binding) ->
    case lookup(KeyMap, MapKey) of
	{_, {keymap, SubMap}} ->
	    set_key_traverse(SubMap, KeySeq, Binding);
	_ ->
	    error
    end.

bind(Keymap, Key, Value) ->
    ets:insert(Keymap, {Key, Value}).

%% Bind each item of a key-value list.
bind_each(Name, KVList) ->
    lists:foreach(fun({Key, Value}) -> bind(Name, Key, Value) end,
		  KVList).

lookup(Name, Key) ->
    %% First lookup by key-code number
    case ets:lookup(Name, Key) of
	[{Key, Value}] ->
	    {ok, Value};
	[] ->
	    %% Next lookup by stringified name
	    String = edit_util:keyname(Key),
	    case ets:lookup(Name, String) of
		[{String, Value}] ->
		    {ok, Value};
		[] ->
		    unbound
	    end
    end.

delete(Name) ->
    ets:delete(Name).

test() ->
    Map = new(test),
    unbound = lookup(Map, key),
    bind(Map, key, value),
    {ok, value} = lookup(Map, key),
    bind(Map, key, newvalue),
    {ok, newvalue} = lookup(Map, key),
    delete(Map),
    ok.

