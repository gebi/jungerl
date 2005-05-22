%%% File    : pgsql_proto.erl
%%% Author  : Christian Sunesson <chrisu@kth.se>
%%% Description : PostgreSQL protocol driver
%%% Created :  9 May 2005

-module(pgsql_proto).

%%% Version 3.0 of the protocol. 
%%% Supported in postgres from version 7.4
-define(PROTOCOL_MAJOR, 3).
-define(PROTOCOL_MINOR, 0).

%%% PostgreSQL protocol message codes
-define(PG_EMPTY_RESPONSE, $I).
-define(PG_BACKEND_KEY_DATA, $K).
-define(PG_PARAMETER_STATUS, $S).
-define(PG_ERROR_MESSAGE, $E).
-define(PG_NOTICE_RESPONSE, $N).
-define(PG_COMMAND_COMPLETE, $C).
-define(PG_DATA_ROW, $D).
-define(PG_ROW_DESCRIPTION, $T).
-define(PG_READY_FOR_QUERY, $Z).
-define(PG_CLOSE, $X).

-export([start/1]).
-export([connected/2]).

-import(pgsql_util, [lookup/3]).
-import(pgsql_util, [socket/1]).
-import(pgsql_util, [send/2, send_int/2, send_msg/3]).
-import(pgsql_util, [recv_msg/2, recv_msg/1, recv_byte/2, recv_byte/1]).
-import(pgsql_util, [make_pair/2, split_pair/1]).
-import(pgsql_util, [count_string/1, to_string/1]).
-import(pgsql_util, [coldescs/2, datacoldescs/3]).

start(Options) ->
    Pid = spawn_link(fun() -> init(Options) end).

%% Start of state machine for postgres protocol.
init(Options) ->
    %%io:format("Init~n", []),
    Host = lookup(Options, host, "localhost"),
    Port = lookup(Options, port, 5432),
    case socket({tcp, Host, Port}) of 
	{ok, Sock } ->
	    connect(Sock, Options);
	Error ->
	    exit({init, Error})
    end.

connect(Sock, Options) ->
    %%io:format("Connect~n", []),
    %% Connection settings for database-login.
    UserName = lookup(Options, user, "cos"),
    DatabaseName = lookup(Options, database, "template1"),
    
    %% Make protocol startup packet.
    Version = <<?PROTOCOL_MAJOR:16/integer, ?PROTOCOL_MINOR:16/integer>>,
    User = make_pair(user, UserName),
    Database = make_pair(database, DatabaseName),
    StartupPacket = <<Version/binary, 
		     User/binary,
		     Database/binary,
		     0>>,

    PacketSize = 4 + size(StartupPacket),
    case gen_tcp:send(Sock, <<PacketSize:32/integer, StartupPacket/binary>>) of
	%% Backend will continue with authentication us
	ok ->
	    authenticate(Sock, Options);
	{error, Reason} ->
	    exit({connect, Reason})
    end.

authenticate(Sock, Options) ->
    %% Await authentication request from backend.
    case recv_msg(Sock, 20000) of
	%% Error response
	{ok, ?PG_ERROR_MESSAGE, Msg} -> 
	    exit({authentication, Msg});
	%% Request authentication method
	{ok, 82, Msg1= <<Code:32/integer, _Rest/binary>> } ->
	    %% 'Code' is the required authentication method.
	    case Code of
		%% Auth ok
		0 -> 
		    setup(Sock, Options);
		%% Kerberos 4
		1 -> 
		    exit({nyi, auth_kerberos4});
		%% Kerberos 5
		2 -> 
		    exit({nyi, auth_kerberos5});
		%% Password
		3 -> 
		    PassString = lookup(Options, password, ""),
		    PassWordLen = length(PassString) + 5,
		    Password = <<PassWordLen:32/integer,
				PassString, 0:32/integer>>,
		    ok = send(Sock, Password),
		    authenticate(Sock, Options);
		%% Hashed password
		4 -> 
		    {nyi, auth_crypt};
		%% MD5 password
		5 -> 
		    {nyi, auth_md5};
		_Any ->
		    exit({authentication, unknown})
	    end;
	%% Unknown message received
	Any ->
	    exit({protocol_error, Any})
    end.

setup(Sock, Options) ->
    %% Receive startup messages until ReadyForQuery
    case recv_msg(Sock, 5000) of
	%% BackendKeyData, necessary for issuing cancel requests
	{ok, ?PG_BACKEND_KEY_DATA, <<Pid:32/integer, Secret:32/integer>>} ->
	    Options1 = [{pid, Pid},{secret, Secret}|Options],
	    %%io:format("Pid: ~p~nSecret: ~p~n", [Pid, Secret]),
	    setup(Sock, Options1);
	%% ParameterStatus, a key-value pair.
	{ok, ?PG_PARAMETER_STATUS, Pair} ->
	    {Key, Value} = split_pair(Pair),
	    Options1 = [{list_to_atom(Key), Value}|Options],
	    %%io:format("Pair: ~p ~p ~n", [Key, Value]),
	    setup(Sock, Options1);
	%% Error message, with a sequence of <<Code:8/integer, String, 0>>
	%% of error descriptions. Code==0 terminates the Reason.
	{ok, ?PG_ERROR_MESSAGE, Reason} ->
	    Message = pgsql_util:errordesc(Reason),
	    %%io:format("Error: ~p~n", [Reason]),
	    gen_tcp:close(Sock),
	    exit({error_response, Message});
	%% Notice Response, with a sequence of <<Code:8/integer, String,0>>
	%% identified fields. Code==0 terminates the Notice.
	{ok, ?PG_NOTICE_RESPONSE, Notice} ->
	    %%io:format("Notice: ~p~n", [Notice]),
	    setup(Sock, Options);
	%% Ready for Query, backend is ready for a new query cycle
	{ok, ?PG_READY_FOR_QUERY, <<Status:8/integer>>} ->
	    %%io:format("Ready: ~p~n", [[Status]]),
	    connected(Sock, Options)
    end.

%% Connected state. Can now start to accept commands.
connected(Sock, Options) ->
    receive 
	%% Perform a query on behalf of 'Pid'
	{exec, Pid, Expr} ->
	    %%io:format("Exec start~n", []),
	    exec(Sock, Pid, Expr, Options),
	    %%io:format("Exec done~n", []),
	    connected(Sock, Options);
	{oid_typemap, Map} ->
	    Options1 = proplists:compact([{oid_typemap, Map}|Options]),
	    connected(Sock, Options1);
	{typelookup, Pid, Code} ->
	    Name = typelookup(Code, Options),
	    Pid ! {lookup, Code, Name},
	    connected(Sock, Options);
	close ->
	    ok = close(Sock),
	    exit(normal);
	codeswitch ->
	    ?MODULE:connected(Sock, Options);
	Any ->
	    connected(Sock, Options)
    end.

exec(Sock, Pid, Expr, Options) ->
    case send_msg(Sock, $Q, list_to_binary([Expr, 0])) of
	ok ->
	    %%io:format("Query sent~n", []),
	    response(Sock, Pid, Options);
	Err={error, closed} ->
	    exit(Err)
    end.

response(Sock, Pid, Options) ->
    %%io:format("Response~n", []),
    case recv_msg(Sock, 5000) of
	{ok, ?PG_ERROR_MESSAGE, Rest} ->
	    %%io:format("Error~n", []),
	    Message = pgsql_util:errordesc(Rest),
	    Pid ! {error, self(), Message},
	    response(Sock, Pid, Options);
	{ok, ?PG_EMPTY_RESPONSE, _Empty} ->
	    %%io:format("Empty response~n", []),
	    Pid ! {empty, self()},
	    response(Sock, Pid, Options);
	{ok, ?PG_ROW_DESCRIPTION, 
	 Resp= <<Columns:16/integer, ColDescs/binary>>} ->
	    %%io:format("Row descs~n", []),
	    Cols = coldescs(ColDescs, []),
	    RowInfo = [{list_to_atom(Name), typelookup(Type, Options)} 
		       || {Name, _Format, _Index, 
			   Type, Size, Mod, Table} <- Cols],
	    %%io:format("RowInfo ~p~n", [RowInfo]),
	    response_data(Sock, RowInfo, Pid),
	    response(Sock, Pid, Options);
	{ok, ?PG_READY_FOR_QUERY, <<State:8/integer>>} ->
	    %%io:format("Ready: ~p~n", [[$0+State]]),
	    Pid ! {ready, self()},
	    ok;
	{ok, ?PG_COMMAND_COMPLETE, Complete} ->
	    {Task, _} = to_string(Complete),
	    %%io:format("~p~n", [{complete, Task}]),
	    Pid ! {complete, self(), Task},
	    response(Sock, Pid, Options);
	Any ->
	    exit({unknown_message, Any})
    end.

response_data(Sock, RowInfo, Pid) ->
    case recv_msg(Sock, 5000) of
	%% DataRow
	{ok, ?PG_DATA_ROW, <<NumberCol:16/integer, RowData/binary>>} ->
	    ColData = datacoldescs(NumberCol, RowData, []),
	    ColData1 = pgsql_util:zip(ColData, RowInfo),
	    ColData2 = [{Name, Data, Type} || 
			   {Data, {Name, Type}} <- ColData1],
	    %%io:format("~p~n", [{datarow, ColData2}]),
	    Pid ! {datarow, self(), ColData2},
	    response_data(Sock, RowInfo, Pid);
	%% CommandComplete
	{ok, ?PG_COMMAND_COMPLETE, Complete} ->
	    {Task, _} = to_string(Complete),
	    %%io:format("~p~n", [{complete, Task}]),
	    Pid ! {complete, self(), Task},
	    ok;
	Any ->
	    exit({unknown_message, Any})
    end.

typelookup(TypeCode, Options) ->
    case proplists:lookup(oid_typemap, Options) of
	none ->
	    TypeCode;
	{oid_typemap, Typemap} ->
	    case dict:find(TypeCode, Typemap) of
		error ->
		    TypeCode;
		{ok, Name} ->
		    Name
	    end
    end.
    

close(Sock) ->	    
    case send_msg(Sock, ?PG_CLOSE, <<>>) of
	ok ->
	    gen_tcp:close(Sock),
	    ok;
	Err={error, closed} ->
	    Err
    end.
