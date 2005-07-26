%%%-------------------------------------------------------------------
%%% Created : 23 Jul 2005 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Desc.   : A naive SQL web interface.
%%%
%%% $Id$
%%%-------------------------------------------------------------------
-module(ysql).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include("../include/ysql.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0, start/0, odbc_connect/3, top/1, lk/2, lk/3, date/0,
	 use/2, desc_table/2, selected/1, where/1, select/3, select/4,
	 db/1, table/1, logout/1, sql_query/2, clock/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(s, {}).

-define(SERVER,  ?MODULE).

%%====================================================================
%% External functions
%%====================================================================

clock() ->
    js:periodically_call_remote_S([{update, "the_clock"}, 
				   {frequency, "1"},
				   {url, "/clock.yaws" }]).

selected(L) ->
    F = fun({"x_"++Selected, "on"}, Acc) -> [Selected | Acc];
	   (_, Acc)                      -> Acc
	end,
    lists:foldr(F, [], L).

where(L) ->
    F = fun({"w_"++Field, Value}, Acc) when Value =/= undefined -> 
		Condition = get_condtion(Field, L),
		[{Field, Condition, Value} | Acc];
	   (_, Acc) ->
		Acc
	end,
    lists:foldl(F, [], L).

db(Y) when Y#ysql.db == undefined -> "-";
db(Y)                             -> Y#ysql.db.

table(Y) when Y#ysql.table == undefined -> "-";
table(Y)                                -> Y#ysql.table.

get_condtion(Field, L) ->
    lk("c_"++Field, L, "=").

lk(Key, L) ->
    lk(Key, L, undefined).

lk(Key, L, Default) ->
    case lists:keysearch(Key,1,L) of
        {value, {_, Val}} -> Val;
        false             -> Default
    end.

date() ->
    {Y,M,D} = erlang:date(),
    integer_to_list(D)++" "++month(M)++" "++integer_to_list(Y).

month(1)  -> "Jan";
month(2)  -> "Feb";
month(3)  -> "Mar";
month(4)  -> "Apr";
month(5)  -> "May";
month(6)  -> "Jun";
month(7)  -> "Jul";
month(8)  -> "Aug";
month(9)  -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".


%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%% Setup an ODBC connection and return a handle.
odbc_connect(User, Passwd, Dsn) ->
    gen_server:call(?SERVER, {odbc_connect, User, Passwd, Dsn}, infinity).

top(Y) ->
    gen_server:call(?SERVER, {top, Y}, infinity).

use(Y, Db) ->
    gen_server:call(?SERVER, {use, Y, Db}, infinity).

desc_table(Y, Table) ->
    gen_server:call(?SERVER, {desc_table, Y, Table}, infinity).

sql_query(Y, Query) ->
    gen_server:call(?SERVER, {sql_query, Y, Query}, infinity).

select(Y, Selected, Where) ->
    select(Y, Selected, Where, "").

select(Y, Selected, Where, OrderBy) ->
    gen_server:call(?SERVER, {select, Y, Selected, Where, OrderBy}, infinity).

logout(Y) ->
    gen_server:call(?SERVER, {logout, Y}, infinity).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
    application:start(odbc),
    {ok, #s{}}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({odbc_connect, User, Passwd, Dsn}, _From, State) ->
    ConnStr = "DSN="++Dsn++";UID="++User++";PWD="++Passwd,
    Reply = case odbc:connect(ConnStr, []) of
		{ok,Ref} -> {ok, #ysql{odbc = Ref}};
		Else ->
		    ?elog("ysql: odbc_connect failed: ~p~n", [Else]),
		    {error, "odbc_connect failed"}
	    end,
    {reply, Reply, State};
%%
handle_call({top, Y}, _From, State) ->
    Reply = (catch show_databases(Y)),
    {reply, {ok, Y, Reply}, State};
%%
handle_call({use, Y, Db}, _From, State) ->
    Y2 = Y#ysql{db = Db},
    Reply = (catch use_db(Y2)),
    {reply, {ok, Y2, Reply}, State};
%%
handle_call({desc_table, Y, Table}, _From, State) ->
    Reply = (catch describe_table(Y, Table)),
    {reply, {ok, Y#ysql{table = Table}, Reply}, State};
%%
handle_call({select, Y, Selected, Where, OrderBy}, _From, State) ->
    Reply = (catch select_table(Y, Selected, Where, OrderBy, Y#ysql.table)),
    {reply, {ok, Y, Reply}, State};
%%
handle_call({sql_query, Y, Query}, _From, State) ->
    case catch do_sql_query(Y, Query) of
	{ok, Reply} ->
	    {reply, {ok, Y, Reply}, State};
	Else ->
	    {reply, Else, State}
    end;
%%
handle_call({logout, Y}, _From, State) ->
    odbc:disconnect(Y#ysql.odbc),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

do_sql_query(Y, Query) ->
    ?elog("do_sql_query: ~p~n", [Query]),
    case odbc:sql_query(Y#ysql.odbc, Query++";") of
	{selected, Headers, Tables} ->
	    {ok, {ehtml, mk_tab("tables", "", Headers, t2l(Tables))}};
	{updated, _} ->
	    {ok, {html, []} };
	Else ->
	    ?elog("do_sql_query: cmd failed: ~p~n", [Else]),
	    Else
    end.




select_table(Y, Selected, Where, OrderBy, Table) ->
    Q = mk_query(Selected, Where, OrderBy, Table),
    case odbc:sql_query(Y#ysql.odbc, Q) of
	{selected, Headers, Tables} ->
	    {ehtml,
	     [{'div', [{class, "sql_query"}],
	      "Query: "++Q},
	      {'div', [{class, "sql_table_name"}],
	       "Table: "++Table} |
	      mk_tab("tables", "", Headers, t2l(Tables))]};
	Else ->
	    ?elog("select_table: cmd failed: ~p~n", [Else]),
	    {html, []}
    end.

mk_query(Selected, Where, OrderBy, Table) ->
    "SELECT " ++ mk_selected(Selected) ++
	" FROM " ++ Table ++ 
	mk_where(Where) ++ 
	order_by(OrderBy) ++ ";".

order_by("o_" ++ OrderBy) -> " ORDER BY " ++ OrderBy;
order_by(_)               -> "".


mk_selected([]) -> "*";
mk_selected(Fs) -> 
    F = fun(Field, Acc) -> Acc++","++Field end,
    tl(lists:foldl(F, [], Fs)).

mk_where([])    -> [];
mk_where(Where) ->
    F = fun({Field, Cond, Value}) ->
		Field++" "++Cond++" "++where_value(Value)
	end,
    L = lists:map(F, Where),
    " WHERE " ++ add_and(L).

where_value(String) ->
    case catch list_to_integer(String) of
	I when integer(I) -> String;
	_                 -> "'"++String++"'"
    end.

add_and([A])     -> A;
add_and([A,B|T]) ->
    A++" AND "++add_and([B|T]).
	

describe_table(Y, Table) ->
    case odbc:describe_table(Y#ysql.odbc, Table) of
	{ok, L} ->
	    mk_sql_tab(Y, Table, t2l(L));
	Else ->
	    ?elog("describe_table: cmd failed: ~p~n", [Else]),
	    {html, []}
    end.

show_create_table(Y, Table) ->
    case odbc:sql_query(Y#ysql.odbc, "SHOW CREATE TABLE "++Table++";") of
	{selected, _, [{_,Cmd}]} ->
	    Cmd;
	Else ->
	    ?elog("show_create_table: cmd failed: ~p~n", [Else]),
	    []
    end.

mk_sql_tab(Y, Table, Rows) ->
    Cmd = show_create_table(Y, Table),
    Headers = ["Name", "Type"],
    {ehtml,
     [{form, [{method, post},
	      {action, "select.yaws"}],
       [{'div', [{class, "sql_table_submit"}],
	 {input, [{type, submit}, 
		  {value, "Submit"}]}},
	{'div', [{class, "sql_table_name"}],
	 "Table: "++Table},
	{'div', [{class, "sql_table"}],
	 [{table, [{class, "result_table"}],
	   [{tr, [],
	     [{td, [], "select"}] ++
	     [{td, [{class, "table_head"}], X} || X <- Headers] ++
	     [{td, [], "cond"}] ++
	     [{td, [], "where"}] ++
	     [{td, [], "sort"}]} |
	    lists:map(fun(Row) ->
			      {tr, [],
			       [{td, [],
				   {input, [{name, "x_"++hd(Row)},
					    {type, checkbox}]}}] ++
			       [{td, [], massage(W)} || W <- Row] ++
			       cond_select(hd(Row)) ++
			       [{td, [], 
				 {input, [{name, "w_"++hd(Row)},
					  {type, text},
					  {size, "10"}]}}] ++
			        [{td, [], 
				  {input, [{name, "orderby"},
					   {type, radio},
					   {value, "o_"++hd(Row)}]}}]
			      }
		      end, Rows)]}]},
	{'div', [{class, "table_create_cmd"}],
	 {pre_html, nl2br(Cmd)}}
       ]}]}.

nl2br([$\n|T]) -> [$<,$b,$r,$>|nl2br(T)];
nl2br([H|T])   -> [H|nl2br(T)];
nl2br([])      -> [].

cond_select(Field) ->
    [{td, [],
      [{select, [{name, "c_"++Field}, {size, 1}],
	options(["=",">",">=","<","<=","=/="])}]}].

options([H|T]) -> [{option,[], H} | options(T)];
options([])    -> [].


use_db(Y) ->
    case odbc:sql_query(Y#ysql.odbc, "use "++Y#ysql.db++";") of
	{updated, _} ->
	    show_tables(Y);
	Else ->
	    ?elog("use_db: cmd failed: ~p~n", [Else]),
	    {html, []}
    end.

show_tables(Y) ->
    case odbc:sql_query(Y#ysql.odbc, "show tables;") of
	{selected, Headers, Tables} ->
	    {ehtml, mk_tab("tables", "desc_table.yaws?table=", Headers, t2l(Tables))};
	Else ->
	    ?elog("show_databases: cmd failed: ~p~n", [Else]),
	    {html, []}
    end.

show_databases(Y) ->
    case odbc:sql_query(Y#ysql.odbc, "show databases;") of
	{selected, Headers, Databases} ->
	    %% {selected,["Database"],[{"mysql"},{"phpbb"},{"test"}]}
	    {ehtml, 
	     [{'div', [{class, "databases_title"}],
	       "Select a Database to be used!"} |
	      mk_tab("databases", "use.yaws?db=", Headers, t2l(Databases))
	     ]};
	Else ->
	    ?elog("show_databases: cmd failed: ~p~n", [Else]),
	    {html, []}
    end.

mk_tab(Class, Link, Headers, Rows) ->
    [{'div', [{class, Class}],
      [{table, [{class, "result_table"}],
	[{tr, [],
	  [{td, [{class, "table_head"}], X} || X <- Headers]} |
	 lists:map(fun(Row) ->
			   {tr, [],
			    [{td, [], lnk(Link,massage(W))} || W <- Row]}
		   end, Rows)]}]}].

lnk("", E)   -> E;
lnk(Link, E) -> {a, [{href, Link++E}], E}.

massage(L) when list(L) -> L;
massage(A) when atom(A) -> x(atom_to_list(A));
massage(I) when integer(I) -> integer_to_list(I);
massage({A,I}) when atom(A),integer(I) ->
    x(atom_to_list(A))++"("++integer_to_list(I)++")";
massage({A,I1,I2}) when atom(A),integer(I1),integer(I2) ->
    x(atom_to_list(A))++"("++integer_to_list(I1)++","++integer_to_list(I2)++")";
massage(Else) ->
    ?elog("massage: NYI: ~p~n", [Else]).

x("sql_" ++ Rest) -> Rest;
x(L)              -> L.

t2l(L) ->
    lists:map(fun(T) -> tuple_to_list(T) end, L).
