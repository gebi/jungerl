%%% BEGIN ce_db.erl %%%
%%%
%%% ce - Miscellaneous Programming Support Libraries for Erlang/OTP
%%% Copyright (c)2003 Cat's Eye Technologies.  All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%%   Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%%
%%%   Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in
%%%   the documentation and/or other materials provided with the
%%%   distribution.
%%%
%%%   Neither the name of Cat's Eye Technologies nor the names of its
%%%   contributors may be used to endorse or promote products derived
%%%   from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
%%% OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
%%% PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
%%% OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%%% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE. 

%% @doc Database library.
%%
%% <p>This library complements mnesia.  It uses mnesia to access the
%% databases, using a simpler (but less efficient) interface.</p>
%%
%% @end

-module(ce_db).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2002 Cat`s Eye Technologies. All rights reserved.').

-export([create/1, connect/3]).
-export([probe_db_nodes/1]).
-export([read/2, read/3, write/2, delete/2]).
-export([foreach/2, fold/2, fold/3, all_records/1]).
-export([update_counter/2]).

%%% DB MANAGEMENT %%%

%% @spec create(Tables::[tabledef()]) -> ok | {error, Reason}
%%         tabledef() = {table(), [option()]}
%% @doc Creates a schema with the given tables if one does not already exist.
%% This schema is created on the local node only.
%% If a schema already exists on the local node,
%% the given tables are added to it.  See the <code>mnesia</code> documentation
%% for an explanation of the <code>tabledef()</code> structure.

create(Tables) ->
  mnesia:stop(),
  Node = node(),
  (catch mnesia:create_schema([Node])),
  mnesia:start(),
  lists:foreach(fun({Table, Properties}) ->
    (catch mnesia:create_table(Table, Properties))
  end, Tables),
  ok.

%% @spec connect(Master::node(), RamTables::[table()], DiscTables::[table()]) -> ok | {error, Reason}
%% @doc Connects to a database hosted on a remote node.
%% This is useful for connecting slave nodes to a master after the
%% database has been created on the master node using <code>ce_db:create/1</code>.
%% This connection needs only be done once, at install time, not each time
%% the application is started.  If the connection is successful, local copies of
%% the given tables from the master node will be made in RAM and/or on
%% disc on the local node.

connect(MasterNode, RamTables, DiscTables) ->
  mnesia:start(),
  Node = node(),
  spawn(MasterNode, ?MODULE, probe_db_nodes, [self()]),
  receive
    {probe_db_nodes_reply, DBNodes} ->
      mnesia:change_config(extra_db_nodes, DBNodes),
      mnesia:change_table_copy_type(schema, Node, disc_copies),
      lists:foreach(fun(X) ->
                      mnesia:add_table_copy(X, Node, ram_copies)
		    end, RamTables),
      lists:foreach(fun(X) ->
                      mnesia:add_table_copy(X, Node, disc_copies)
		    end, DiscTables),
      ok;
    Else ->
      {error, Else}
  end.

%% @spec probe_db_nodes(pid()) -> ok
%% @doc Used by <code>connect/3</code> to probe the remote node.
%% This function is invoked on the remote (master) node, and sends a message
%% back to the local (slave) node.

probe_db_nodes(ForPid) ->
  ForPid ! {probe_db_nodes_reply, mnesia:system_info(db_nodes)},
  ok.

%% @spec read(table(), key()) -> record() | nil
%%         table() = atom()
%%         key() = term()
%% @doc Reads the record with a matching primary key from the table.
%% Returns nil if no record was found.

read(Table, Key) ->
  case mnesia:transaction(fun() ->
                            mnesia:read(Table, Key, read)
			  end) of
    {atomic, [H | T]} -> H;
                    _ -> nil
  end.

%% @spec read(table(), index(), key()) -> [record()]
%%         index() = atom()
%% @doc Reads all records with a matching secondary key from the table.
%% Returns an empty list if none were found.

read(Table, Index, Key) ->
  case mnesia:transaction(fun() ->
                            mnesia:index_read(Table, Key, Index)
			  end) of
    {atomic, List} -> List;
                 _ -> []
  end.

%% @spec write(table(), record()) -> true | false
%% @doc Inserts or updates a single record in a data table.

write(Table, Object) ->
  case mnesia:transaction(fun() ->
                            mnesia:write(Table, Object, write)
			  end) of
    {atomic, ok} -> true;
	    Else -> false
  end.

%% @spec delete(table(), key()) -> true | false
%% @doc Deletes a single record from a data table.

delete(Table, Key) ->
  case mnesia:transaction(fun() ->
                            mnesia:delete(Table, Key, write)
			  end) of
    {atomic, ok} -> true;
	    Else -> false
  end.

%% @spec foreach(table(), fun()) -> true | false
%% @doc Traverses an entire table, calling fun/2 for each record.
%% The result of the evaluation of the fun is discarded.

foreach(Table, Fun) ->
  case mnesia:transaction(fun() ->
                            mnesia:foldl(Fun, [], Table), ok
			  end) of
    {atomic, ok} -> true;
            Else -> false
  end.

%% @spec fold(table(), fun()) -> {ok, term()} | {error, Reason}
%% @equiv fold(Table, Fun, [])

fold(Table, Fun) ->
  fold(Table, Fun, []).

%% @spec fold(table(), fun(), term()) -> {ok, term()} | {error, Reason}
%% @doc Folds an entire table, calling fun/2 for each record.

fold(Table, Fun, Acc) ->
  case mnesia:transaction(fun() ->
                            mnesia:foldl(Fun, Acc, Table)
			  end) of
    {atomic, Acc0} -> {ok, Acc0};
              Else -> Else
  end.

%% @spec all_records(table()) -> [record()]
%% @doc Returns a list of all the records in the table.
%% Complements mnesia:all_keys/1.  Note that this is not a particularly
%% scaleable solution.

all_records(Table) ->
  Arity = mnesia:table_info(Table, arity),
  Template = list_to_tuple([Table | lists:duplicate(Arity - 1, '_')]),
  case mnesia:transaction(fun() ->
                            mnesia:match_object(Template)
			  end) of
    {atomic, List} -> List;
              Else -> []
  end.

%% @spec update_counter(table(), key()) -> {ok, integer()} | {error, Reason}
%% @doc Increments an integer value in a table.
%% Complements mnesia:dirty_update_counter/2.  This function is much less
%% efficient, but it is cleaner.

update_counter(Table, Key) ->
  case mnesia:transaction(fun() ->
                            [{T,K,Number}] = mnesia:read(Table, Key, read),
                            mnesia:write(Table, {T,K,Number + 1}, write),
			    Number + 1
			  end) of
    {atomic, N} -> {ok, N};
           Else -> Else
  end.

%%% END of ce_db.erl %%%
