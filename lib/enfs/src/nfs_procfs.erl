%%%----------------------------------------------------------------------
%%% File    : nfs_procfs.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : 
%%% Created : 22 Jun 2001 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(nfs_procfs).
-author('luke@bluetail.com').

-export([start_link/0, root/0, getattr/1, lookup/2, dirlist/1, read/1,
	 statfs/1]).

start_link() ->
    {ok, Pid} = nfs_server:start_link(),
    nfs_server:add_mountpoint("/procfs", ?MODULE),
    {ok, Pid}.

%% Returns: ID of root directory, any erlang term.
root() ->
    root.

getattr(root) ->
    %% Returns: {dir, Access}
    %%        | {file, Access, Timestamp, Size}
    %% Access = r | rw | rx | rwx | w | wx | x
    %% Timestamp = {Secs, MicroSecs} (used for atime/mtime/ctime)
    %% Size = integer()
    {dir, rx};
getattr({node, N}) ->
    {dir, rx};
getattr({pid, P}) ->
    {dir, rx};
getattr({registered, N, P}) ->
    {dir, rx};
getattr(ID) ->
    {file, r, now_timestamp(), filesize(ID)}.

dirlist(root) ->
    {ok, [atom_to_list(Node) || Node <- [node()] ++ nodes()]};

dirlist({node, Node}) ->
    case rpc:call(Node, erlang, processes, []) of
	{badrpc, _} ->
	    {error, io};
	Procs ->
	    case rpc:call(Node, erlang, registered, []) of
		{badrpc, _} ->
		    {error, io};
		Regs ->
		    ProcNms = [erlang:pid_to_list(P) -- "<>" || P <- Procs],
		    Registered = [atom_to_list(R) || R <- Regs],
		    {ok, ProcNms ++ Registered}
	    end
    end;

dirlist({registered, Node, R}) ->
    case rpc:call(Node, erlang, whereis, [R]) of
	undefined ->
	    {error, noent};
	{badrpc, _} ->
	    {error, io};
	Pid when pid(Pid) ->
	    dirlist({pid, Pid})
    end;
dirlist({pid, P}) ->
    case rpc:call(node(P), erlang, process_info, [P]) of
	{badrpc, _} ->
	    {error, io};
	Info ->
	    {ok, [atom_to_list(A) || {A, _} <- Info]}
    end.

lookup(root, Child) ->
    %% Child is a node name
    Node = list_to_atom(Child),
    if Node == node() ->
	    {ok, {node, node()}};
       true ->
	    case net_adm:ping(Node) of
		pang ->
		    {error, io};
		pong ->
		    {ok, {node, Node}}
	    end
    end;
lookup({node, Node}, Child) ->
    %% Child could be either a pid or a registered name
    case catch list_to_pid("<"++Child++">") of
	{'EXIT', _} ->
	    %% Not a pid - registered name
	    {ok, {registered, Node, list_to_atom(Child)}};
	Pid ->
	    {ok, {pid, Pid}}
    end;
lookup({registered, Node, R}, Child) ->
    {ok, {property, {registered, Node, R}, list_to_atom(Child)}};
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
    case rpc:call(node(P), erlang, process_info, [P, Name]) of
	{badrpc, _} ->
	    undefined;
	Info ->
	    Info
    end;
get_process_info({registered, Node, R}, Name) ->
    case rpc:call(Node, erlang, whereis, [R]) of
	undefined ->
	    undefined;
	{badrpc, _} ->
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

%% Callback: statfs(ID) -> {ok, {Tsize, Bsize, Blocks, Bfree, Bavail}} |
%%                         {error, Reason}
%% Return values:
%%       Tsize   The optimum transfer size of the server in bytes.  This is
%%               the number of bytes the server would like to have in the
%%               data part of READ and WRITE requests.
%%       Bsize   The block size in bytes of the filesystem.
%%       Blocks  The total number of "bsize" blocks on the filesystem.
%%       Bfree   The number of free "bsize" blocks on the filesystem.
%%       Bavail  The number of "bsize" blocks available to non-privileged
%%               users.

statfs(_) ->
    {ok, {65535, 1024, 1024, 0, 0}}.	% pulled out of the air

