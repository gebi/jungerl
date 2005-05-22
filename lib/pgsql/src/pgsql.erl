%%% File    : pgsql.erl
%%% Author  : Christian Sunesson <chrisu@kth.se>
%%% Description : PostgresQL interface
%%% Created : 11 May 2005

-module(pgsql).
-export([connect/1, close/1, exec/2]).

connect(Host, User, Database) ->
    Sock = pgsql_proto:start([{database, Database}, 
			      {host, Host},
			      {user, User}]),
    
    %% Make a oid to typename mapping.
    {ok, {select, Result}} = exec(Sock, "SELECT oid, typname FROM pg_type"),
    Map = lists:map(fun([{_, Oid, _}, {_, Name, _}]) ->
			    {list_to_integer(Oid), 
			     list_to_atom(Name)}
		    end, Result),
    Map1 = dict:from_list(Map),
    Sock ! {oid_typemap, Map1},
    
    {ok, Sock}.

close(Sock) ->
    Sock ! close.

exec(Sock, Query) ->
    Sock ! {exec, self(), Query},
    resultset(Sock, []).

resultset(Sock, Set) ->
    receive
	{datarow, Sock, Columns} ->
	    resultset(Sock, [Columns|Set]);
	{complete, Sock, Task} ->
	    Task1 = case Task of
			"SELECT" ++ _ ->
			    select;
			"INSERT" ++ _ ->
			    insert;
			"DELETE" ++ _ ->
			    delete;
			"UPDATE" ++ _ ->
			    update;
			"FETCH" ++ _ ->
			    fetch;
			"MOVE" ++ _ ->
			    move;
			Unknown ->
			    Unknown
		    end,
	    resultset(Sock, {Task1, lists:reverse(Set)});
	{ready, Sock} ->
	    case Set of
		{error, Reason} ->
		    {error, Reason};
		List ->
		    {ok, List}
	    end;
	{error, Sock, Reason} ->
	    resultset(Sock, {error, Reason})
    after 5000 ->
	    timeout
    end.




	    
	    
	

		    
		    
	
    
