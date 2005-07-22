%%% File    : pgsql.erl
%%% Author  : Christian Sunesson <chrisu@kth.se>
%%% Description : PostgresQL interface
%%% Created : 11 May 2005

%%
%% API for accessing the postgres driver.
%%

-module(pgsql).
-export([connect/4]).

-export([squery/2, 
	 pquery/3, 
	 terminate/1, 
	 prepare/3, unprepare/2, 
	 execute/3]).

%% Connect routine. Might be insufficient for your needs because I dont
%% know about your needs.
connect(Host, Database, User, Password) ->
    {ok, Db} = pgsql_proto:run([{database, Database},
				{host, Host},
				{user, User},
				{password, Password}]),
    %% Receive startup params or error.
    receive
	{pgsql_error, Reason} ->
	    exit(Reason);
	{pgsql_params, Params} ->
	    io:format("Params: ~p~n", [Params])
    end,
    %% Confirmation we're connected successfully.
    receive
	pgsql_connected ->
	    ok
    end,
    {ok, Db}.

%% Close a connection
terminate(Db) ->
    %%io:format("terminate:~n", []),
    Ref = make_ref(),
    Db ! {terminate, Ref, self()},
    receive
	{pgsql, Ref, terminated} ->
	    ok
    after 5000 ->
	    timeout
    end.

%%% In the "simple query" protocol, the frontend just sends a 
%%% textual query string, which is parsed and immediately 
%%% executed by the backend.  

%% A simple query can contain multiple statements (separated with a semi-colon),
%% and each statement's response.

%%% squery(Db, Query) -> {ok, Results} | ... no real error handling
%%% Query = string()
%%% Results = [Result]
%%% Result = {"SELECT", RowDesc, ResultSet} | ...
squery(Db, Query) ->
    %%io:format("squery: ~p~n", [Query]),
    Ref = make_ref(),
    Db ! {squery, Ref, self(), Query},
    receive
	{pgsql, Ref, Result} ->
	    %%io:format("Result: ~p~n", [Result]),
	    {ok, Result}
    end.

%%% In the "extended query" protocol, processing of queries is 
%%% separated into multiple steps: parsing, binding of parameter
%%% values, and execution. This offers flexibility and performance
%%% benefits, at the cost of extra complexity.

%%% pquery(Db, Query, Params) -> {ok, Command, Status, NameTypes, Rows} | timeout | ...
%%% Query = string()
%%% Params = [term()]
%%% Command = string()
%%% Status = idle | transaction | failed_transaction
%%% NameTypes = [{ColName, ColType}]
%%% Rows = [list()]
pquery(Db, Query, Params) ->
    %%io:format("equery: ~p~n", [Query]),
    Ref = make_ref(),
    Db ! {equery, Ref, self(), {Query, Params}},
    receive
	{pgsql, Ref, {Command, Status, NameTypes, Rows}} ->
	    {ok, Command, Status, NameTypes, Rows}
    after 5000 ->
	    timeout
    end.

%%% prepare(Db, Name, Query) -> {ok, Status, ParamTypes, ResultTypes}
%%% Status = idle | transaction | failed_transaction
%%% ParamTypes = [atom()]
%%% ResultTypes = [{ColName, ColType}]
prepare(Db, Name, Query) when is_atom(Name) ->
    Ref = make_ref(),
    Db ! {prepare, Ref, self(), {atom_to_list(Name), Query}},
    receive
	{pgsql, Ref, {prepared, State, ParamDesc, ResultDesc}} ->
	    {ok, State, ParamDesc, ResultDesc}
    after 5000 ->
	    timeout
    end.

%%% unprepare(Db, Name) -> ok | timeout | ...
%%% Name = atom()
unprepare(Db, Name) when is_atom(Name) ->
    Ref = make_ref(),
    Db ! {unprepare, Ref, self(), atom_to_list(Name)},
    receive
	{pgsql, Ref, unprepared} ->
	    ok
    after 5000 ->
	    timeout
    end.

%%% execute(Db, Name, Params) -> {ok, Result} | timeout | ...
%%% Result = {'INSERT', NRows} |
%%%          {'DELETE', NRows} |
%%%          {'SELECT', ResultSet} |
%%%          ...
%%% ResultSet = [Row]
%%% Row = list()
execute(Db, Name, Params) when is_atom(Name), is_list(Params) ->
    Ref = make_ref(),
    Db ! {execute, Ref, self(), {atom_to_list(Name), Params}},
    receive
	{pgsql, Ref, Result} ->
	    {ok, Result}
    after 5000 ->
	    timeout
    end.
