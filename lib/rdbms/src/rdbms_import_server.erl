%%%
%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.0, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.erlang.org/license/EPL1_0.txt
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is rdbms_import_server-1.0.
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
 
%%%----------------------------------------------------------------------
%%% #0.    BASIC INFORMATION 
%%% ----------------------------------------------------------
%%% %CCaseFile:	rdbms_import_server.erl %
%%% Author:          Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description:     Imports a tab-delimited ASCII file.
%%%
%%% Modules used:    lists, file, string, rdbms, rdbms_types
%%% ----------------------------------------------------------
-module(rdbms_import_server).
-behaviour(gen_server).
-vsn('1.0').
-date('99-01-08').
-author('ulf.wiger@ericsson.com').

%%%----------------------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([start_link/0, start_link/1,
	 commit/1,
	 abort/1,
	 null_value/2,
	 type_check/2,
	 options/2,
	 header/2,
	 data/2
	]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

-ifdef(debug).
-define(dbg(Phase, Fmt, Args), debug_out(Phase, Fmt, Args)).
-define(dbg_opts, [trace]).
-else.
-define(dbg(Phase, Fmt, Args), no_debug).
-define(dbg_opts, []).
-endif.


-record(state, {data = [], 
		header = [], 
		tab,
		null_value = rdbms:null_value(),
		type_check = {rdbms_types, check},
		duplicates = overwrite}).


%%% Module description


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR THE EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------



%%% ----------------------------------------------------------
%%% #3.1.1           start_link()
%%% Input:           -
%%% Output:          {ok, Pid}.
%%% Exceptions:      -
%%% Description:     Starts an instance of the import server.
%%% ----------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], [{debug,?dbg_opts}]).

start_link(Options) ->
    gen_server:start_link(?MODULE, Options, [{debug,?dbg_opts}]).


%%% ----------------------------------------------------------
%%% #3.1.2           commit(ServerPid : pid())
%%% Input:           ServerPid
%%% Output:          ok | {aborted, Reason}
%%% Exceptions:      
%%% Description:     Sends a commit instruction to the import server
%%%                  process. The process terminates upon completing
%%%                  the operation.
%%% ----------------------------------------------------------

commit(S) ->
    gen_server:call(S, commit, 60000).

%%% ----------------------------------------------------------
%%% #3.1.3           abort(ServerPid : pid())
%%% Input:           ServerPid
%%% Output:          aborted
%%% Exceptions:      
%%% Description:     Sends an abort instruction to the import server
%%%                  process. The process terminates upon completing
%%%                  the operation.
%%% ----------------------------------------------------------

abort(S) ->
    gen_server:call(S, abort).

%%% ----------------------------------------------------------
%%% #3.1.4           header(ServerPid : pid(), Header : [FieldDef])
%%% Input:           ServerPid, Header
%%% Output:          ok
%%% Exceptions:      Exits if Header is incorrect
%%% Description:     Registers a header with an import server process.
%%%                  The header should look like
%%%                  [FieldDef] where
%%%                  FieldDef ::=  {TabName, AttrName} |
%%%                                [{TabName, AttrName}]
%%%                  The list should be ordered to match the following
%%%                  data tuples.
%%%                  Data ::= DataTuple | [DataTuple]
%%%                  DataTuple ::= tuple()
%%%                  where length(Header) == size(DataTuple)
%%%                  Metadata is verified against mnesia and rdbms.
%%% ----------------------------------------------------------

header(S, H) ->
    gen_server:call(S, {header, H}).

%%% ----------------------------------------------------------
%%% #3.1.5           data(ServerPid : pid(), Data : )
%%% Input:           Data
%%% Output:          ok
%%% Exceptions:      Exits if Data is incorrect
%%% Description:     Sends data to an import server process.
%%%                  Data must match the preceeding header def.
%%%                  Data ::= DataTuple | [DataTuple]
%%%                  DataTuple ::= tuple()
%%%                  where length(Header) == size(DataTuple)
%%%                  Data is verified against mnesia and rdbms.
%%% ----------------------------------------------------------

data(S, D) ->
    gen_server:call(S, {data, D}).


%%% ----------------------------------------------------------
%%% #3.1.6           null_value(Value : term())
%%% Input:           The current null value
%%% Output:          ok.
%%% Exceptions:      
%%% Description:     Sends an instruction to the import server to
%%%                  any occurence of Value in a data tuple as null.
%%% ----------------------------------------------------------

null_value(S, V) ->
    gen_server:call(S, {null_value, V}).


%%% ----------------------------------------------------------
%%% #3.1.7           type_check(F : function/2)
%%% Input:           Type check/conversion function F(Value, Attr)
%%% Output:          ok.
%%% Exceptions:      
%%% Description:     Sends an instruction to the import server to
%%%                  use another type checker than bbs_types:check/2.
%%% ----------------------------------------------------------

type_check(S, F) ->
    gen_server:call(S, {type_check, F}).



%%% ----------------------------------------------------------
%%% #3.1.8           options(ServerPid : pid(), Options : [{atom(), term()}])
%%% Input:           ServerPid, Options
%%% Output:          ok.
%%% Exceptions:      
%%% Description:     Sends user options to the import server.
%%%                  Valid options are:
%%%                  - {null_value, term()}
%%%                  - {duplicates, skip | overwrite}
%%% ----------------------------------------------------------

options(S, O) when list(O) ->
    gen_server:call(S, {options, O}).


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

init(Options) ->
    ?dbg(init, "Starting import server (~p).~n", [self()]),
    Tab = ets:new(keys, [set, protected]),
    State = handle_options(Options, #state{tab = Tab}),
    ?dbg(init, "State = ~p.~n", [State]),
    {ok, State}.

handle_call({header, H}, From, S) ->
    Hdr = check_hdr(H),
    ?dbg(import, "header(~p) -> ~p.~n", [H, Hdr]),
    {reply, ok, S#state{header = Hdr}};
handle_call({data, D}, From, S) ->
    ?dbg(import, "data(~p).~n", [D]),
    S1 = process_data(D, S),
    {reply, ok, S1};
handle_call({null_value, V}, From, S) ->
    ?dbg(import, "null_value(~p).~n", [V]),
    {reply, ok, S#state{null_value = V}};
handle_call({type_check, F}, From, S) ->
    ?dbg(import, "type_check(~p).~n", [F]),
    {reply, ok, S#state{type_check = F}};
handle_call({options, O}, From, S) ->
    ?dbg(import, "options(~p).~n", [O]),
    S1 = handle_options(O, S),
    {reply, ok, S1};
handle_call(commit, From, S) ->
    case catch do_commit(S) of
	{'EXIT', {aborted, Reason}} ->
	    {stop, normal, {aborted, Reason}, S};
	{'EXIT', Reason} ->
	    %% this shouldn't happen...
	    {stop, normal, {aborted, Reason}, S};
	Res ->
	    ?dbg(import, "commit() -> ~p.~n", [Res]),
	    {stop, normal, Res, S}
    end;
handle_call(abort, From, S) ->
    ?dbg(import, "commit().~n", []),
    {stop, normal, aborted, S}.

handle_cast(Msg, S) ->
    {noreply, S}.

handle_info(Msg, S) ->
    {noreply, S}.

terminate(Reason, S) ->
    ok.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

do_commit(S) ->
    %% must reverse the data list to process in order of input
    Data1 = lists:reverse(S#state.data),
    ?dbg(commit, "commit. Data = ~n~p~n", [Data1]),
    rdbms:activity(fun() ->
			   store(Data1)
		   end).



%%% ----------------------------------------------------------
%%% #3.3.2           store(Rec_Id, Ets)
%%% Input:           Record ID and corresponding ETS table.
%%% Output:          ok.
%%% Exceptions:      No error checking at this level.
%%% Description:     store/2 goes through the Ets table and
%%%                  writes to Mnesia.
%%%                  Must be called from within a transaction
%%%                  as store(ets:first(Ets), Ets).
%%% ----------------------------------------------------------
store([]) ->
    ok;
store([Rec|Data]) ->
    ?dbg(commit, "mnesia:write(~p)~n", [Rec]),
    mnesia:write(Rec),
    store(Data).


handle_options([{null_value, V}|T], S) ->
    handle_options(T, S#state{null_value = V});
handle_options([{duplicates, Mode}|T], S) ->
    validate_option(duplicates, Mode, [skip, overwrite]),
    handle_options(T, S#state{duplicates = Mode});
handle_options([Other|T], S) ->
    exit({invalid_option, Other});
handle_options([], S) ->
    S.

validate_option(Key, Value, Valid) ->
    case lists:member(Value, Valid) of
	true ->
	    ok;
	false ->
	    exit({invalid_option, Value})
    end.

%%% ----------------------------------------------------------
%%% #3.3.12          process_data(Data, State).
%%% Input:           All data needed to process input, State
%%% Output:          A list of records, one for each row.
%%% Exceptions:      Exit if type checks fail.
%%% Description:     The function uses rdbms_types:check() to
%%%                  verify that indata is correct.
%%% ----------------------------------------------------------

process_data([Data|T], S) ->
    S1 = process_data1(S#state.header, Data, S),
    process_data(T, S1);
process_data([], S) ->
    S;
process_data(Data, S) when tuple(Data) ->
    process_data1(S#state.header, Data, S).


process_data1([{Tab, KeyPos, PosL, DefRec, Names}|T], Data, S) ->
    S1 = make_rec(PosL, Data, Tab, KeyPos, DefRec, Names, S),
    process_data1(T, Data, S1);
process_data1([], _, S) ->
    S.


%% when parsing the key pos (default 2), skip rows with null keys
make_rec([{Pos, KeyPos}|T], Cols, Tab, KeyPos, NewRec, Names, S) ->
    %% key attribute position
    case element(Pos, Cols) of
	Value when Value == S#state.null_value ->
	    ?dbg(process, "Key pos has null value (~p).~n", [Value]),
	    S;   % nil key element -- don't process, return here
	Value ->
	    Attr =  element(KeyPos, Names),
	    Value1 = check_type(Value, Tab, Attr, S),
	    ?dbg(process, "Tab = ~p; Key pos value = ~p.~n", [Tab, Value1]),
            case S#state.duplicates of
                skip ->
		    case ets:lookup(S#state.tab, {Tab, Value1}) of
			[] ->
			    make_rec(T, Cols, Tab, KeyPos,
				     setelement(2, NewRec, Value1),
				     Names, S);
			_ ->
			    ?dbg(process, "Exists - skip (~p).~n", 
				 [{Tab, Value1}]),
			    skip
		    end;
                _ ->
                    make_rec(T, Cols, Tab, KeyPos,
			     setelement(2, NewRec, Value1), Names, S)
            end
    end;
make_rec([{Pos, Rec_pos}|T], Cols,
	 Tab, KeyPos, NewRec, Names, S) when Pos > size(Cols) ->
    % nil in last column(s)
    make_rec(T, Cols, Tab, KeyPos, NewRec, Names, S);
make_rec([{Pos, Rec_pos}|T], Cols, Tab, KeyPos, NewRec, Names, S) ->
    case element(Pos, Cols) of
	Value when Value == S#state.null_value ->
	    make_rec(T, Cols, Tab, KeyPos, NewRec, Names, S);
	Value ->
	    Attr =  element(Rec_pos, Names),
	    Value1 = check_type(Value, Tab, Attr, S),
	    make_rec(T, Cols, Tab, KeyPos, setelement(Rec_pos, NewRec, Value1),
		     Names, S)
    end;
make_rec([], _, Tab, KeyPos, Record, _, S) ->
    S#state{data = [Record|S#state.data]}.
	    
    

%%% ----------------------------------------------------------
%%% #3.3.4           check_hdr(Hdr, Record_name)
%%% Input:           Header of import file and name of record to match
%%% Output:          Field positions of import file columns,
%%%                  the definition record,
%%%                  and a record with default values.
%%% Exceptions:      
%%% Description:     This function analyses the data header.
%%% ----------------------------------------------------------
check_hdr(Hdr) when list(Hdr) ->
    rec_of(wrap_header(Hdr), 1);
check_hdr(Hdr) when tuple(Hdr) ->
    rec_of(wrap_header(tuple_to_list(Hdr)), 1).

wrap_header([{R,F}|T]) ->
    [[{R, F}]|wrap_header(T)];
wrap_header([H|T]) when list(H) ->
    [H|wrap_header(T)];
wrap_header([]) ->
    [].


rec_of([[{Rec, Attr}|T1]|T], Cur_pos) ->
    case rdbms:attributes(Rec) of
	undefined ->
	    exit({bad_record, Rec});
	Attrs ->
	    {Cols, T2} = match_cols([[{Rec, Attr}|T1]|T], Rec, Attrs,
				    Cur_pos, [], []),
	    R1 = {Rec, keypos(Rec), Cols, rdbms:default_record(Rec),
		 list_to_tuple([Rec|Attrs])},
	    [R1|rec_of(T2, Cur_pos)]
    end;
rec_of([[]|T], Cur_pos) ->
    rec_of(T, Cur_pos + 1);
rec_of([], _) ->
    [].

keypos(Tab) ->
    case catch mnesia:table_info(Tab, keypos) of
	{'EXIT',_} -> 2;
	Pos -> Pos
    end.


match_cols([ [] | Cols], Rec, Attrs, Cur_pos, PosL, Rest_cols) ->
    match_cols(Cols, Rec, Attrs, Cur_pos+1, PosL, [ [] | Rest_cols]);
match_cols([L|Cols], Rec, Attrs, Cur_pos, PosL, Rest_cols) ->
    case lists:keysearch(Rec, 1, L) of
	{value, {Rec, Attr}} ->
	    case pos(Attrs, Attr) of
		0 ->
		    exit({error, {bad_field, {Rec, Attr}}});
		N ->
		    L2 = lists:keydelete(Rec, 1, L),
		    match_cols([L2|Cols], Rec, Attrs, Cur_pos,
			       [{Cur_pos, N+1}|PosL], Rest_cols)
	    end;
	Other ->
	    match_cols(Cols, Rec, Attrs, Cur_pos+1, PosL, [L|Rest_cols])
    end;
match_cols([], Rec, _, _, PosL, Rest_cols) ->
    case lists:keymember(2, 2, PosL) of
        false ->
           exit({error, {key_missing, Rec}});
        true ->
           {lists:keysort(2, PosL), lists:reverse(Rest_cols)}
    end.

pos(Attrs, Name) ->
    pos(Attrs, Name, 1).

pos([Name|T], Name, N) ->
    N;
pos([_|T], Name, N) ->
    pos(T, Name, N+1);
pos([], _, _) ->
    0.
	 

check_type(Value, Tab, Attr, State) ->
    case State#state.type_check of
	[] ->
	    %% do not perform type check/conversion
	    %% (types are still verified by the dictionary)
	    Value;
	F ->
	    F(Value, {Tab, Attr})
    end.



debug_out(Phase, Fmt, Args) ->
    io:format("~p-~p: " ++ Fmt, [?MODULE,?LINE|Args]).
