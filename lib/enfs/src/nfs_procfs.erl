%%%----------------------------------------------------------------------
%%% File    : nfs_procfs.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : 
%%% Created : 22 Jun 2001 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(nfs_procfs).
-author('luke@bluetail.com').

-export([start_link/0, root/0, getattr/1, lookup/2, dirlist/1, read/1]).

start_link() ->
    {ok, Pid} = nfs_server:start_link(),
    nfs_server:add_mountpoint("/procfs", ?MODULE),
    {ok, Pid}.

root() ->
    root.

getattr(root) ->
    %% Returns: {dir, Access}
    %%        | {file, Access, Timestamp, Size}
    %% Access = r | rw | rx | rwx | w | wx | x
    %% Timestamp = {Secs, MicroSecs} (used for atime/mtime/ctime)
    %% Size = integer()
    {dir, rx};
getattr({pid, P}) ->
    {dir, rx};
getattr({registered, P}) ->
    {dir, rx};
getattr(ID) ->
    {file, r, now_timestamp(), filesize(ID)}.

dirlist(root) ->
    Procs = [erlang:pid_to_list(P) -- "<>" || P <- erlang:processes()],
    Registered = [atom_to_list(R) || R <- erlang:registered()],
    {ok, Procs ++ Registered};
dirlist({registered, R}) ->
    case whereis(R) of
	undefined ->
	    {error, noent};
	Pid ->
	    dirlist({pid, Pid})
    end;
dirlist({pid, P}) ->
    case erlang:process_info(P) of
	undefined ->
	    {error, noent};
	Info ->
	    {ok, [atom_to_list(A) || {A, _} <- Info]}
    end.

lookup(root, Child) ->
    %% Child could be either a pid or a registered name
    case catch list_to_pid("<"++Child++">") of
	{'EXIT', _} ->
	    %% Not a pid - registered name
	    {ok, {registered, list_to_atom(Child)}};
	Pid ->
	    {ok, {pid, Pid}}
    end;
lookup({registered, R}, Child) ->
    {ok, {property, {registered, R}, list_to_atom(Child)}};
lookup({pid, P}, Child) ->
    {ok, {property, {pid, P}, list_to_atom(Child)}}.

read({property, P, Name}) ->
    case get_process_info(P, Name) of
	undefined ->
	    {error, noent};
	{_, Value} ->
	    {ok, io_lib:format("~p~n", [Value])}
    end.

get_process_info({pid, P}, Name) ->
    erlang:process_info(P, Name);
get_process_info({registered, R}, Name) ->
    case whereis(R) of
	undefined ->
	    undefined;
	Pid ->
	    get_process_info({pid, Pid}, Name)
    end.

filesize(ID = {property, P, Name}) ->
    case read(ID) of
	{ok, IOList} ->
	    size(list_to_binary([IOList]));
	_ ->
	    0
    end.

now_timestamp() ->
    {Mega, Sec, Micro} = now(),
    {((Mega * 1000000) + Sec) band 16#ffffffff, Micro}.

