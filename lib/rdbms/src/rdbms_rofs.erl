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
%%% The Original Code is rdbms-1.2.
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.

%%%----------------------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%%----------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%% File    : rdbms_rofs.erl
%%% Author  : Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description : Read-only file system external table type for mnesia
%%%
%%% Created :  9 Jan 2006 by Ulf Wiger
%%%-------------------------------------------------------------------
-module(rdbms_rofs).
-compile(export_all).

-export([create_external_table/1,
	 delete_external_table/1,
	 is_ram_only/1,
	 verify_cstruct/1,
	 load_table/5]).
-export([table_info/2]).

%% low-level accessor callbacks.
-export([db_chunk/1,
	 db_erase/2,
	 db_erase_tab/1,
	 db_first/1,
	 db_get/2,
	 db_init_chunk/2,
	 db_first/1,
	 db_last/1,
	 db_next_key/2,
	 db_prev_key/2,
	 db_put/2,
	 db_slot/2,
	 db_update_counter/3,
	 match_erase/2,
	 match_object/2,
	 repair_continuation/2,
	 safe_fixtable/2,
	 select/1,
	 select/2,
	 select_init/3]).

-include("mnesia.hrl").
-include_lib("kernel/include/file.hrl").
-record(info, {tab,
	       key,
	       fullname,
	       recname}).


create_external_table(_Cs) ->
    ok.

delete_external_table(_Cs) ->
    ok.

is_ram_only(_Tab) ->
    true.

verify_cstruct(Cs) ->
    #cstruct{ram_copies = Rc, disc_copies = Dc, disc_only_copies = DOc} = Cs,
    case {Rc,Dc,DOc} of
	{[],[],[]} ->
	    case Cs#cstruct.type of 
		ordered_set -> ok;
		Type -> mnesia:abort({combine_error, Cs#cstruct.name,
				      [{external_copies,
					Cs#cstruct.external_copies},
				       {type, Type}]})
	    end,
	    NoMount = fun(Props) ->
			      mnesia:abort(
				{no_mount_point, Cs#cstruct.name, Props})
		      end,
	    Props = Cs#cstruct.user_properties,
	    case Props of
		[] ->
		    NoMount([]);
		[_|_] ->
		    case lists:keysearch(rofs_mount, 1, Props) of
			{value, {_, MP}} ->
			    verify_mountpoint(MP, Cs#cstruct.name),
			    ok;
			false ->
			    NoMount(Props)
		    end
	    end,
	    ValidAttrs = [path,name,data|record_info(fields, file_info)],
	    Attrs = Cs#cstruct.attributes,
	    if hd(Attrs) =/= path ->
		    mnesia:abort({invalid_key, Cs#cstruct.name, hd(Attrs)});
	       true ->
		    ok
	    end,
	    AttrMap = lists:map(
			fun(A) ->
				case pos(A, ValidAttrs) of
				    0 ->
					mnesia:abort({invalid_attribute,
						      Cs#cstruct.name, A});
				    P ->
					P
				end
			end, Attrs),
	    Props1 = orddict:store(rofs_attr_map, AttrMap,
				   orddict:from_list(Props)),
	    Cs#cstruct{user_properties = Props1};

	    %% We could possibly do something clever with frag_properties,
	    %% but then we'd probably have to keep a static table with 
	    %% root-level directories on each node...
%%% 	    case Cs#cstruct.frag_properties of
%%% 		[] ->
%%% 		    Cs;
%%% 		[_|_] = Props ->
%%% 		    case lists:keysearch(hash_module, 1, Props) of
%%% 			{value, {_, rdbms_log_hash}} ->
%%% 			    Cs;
%%% 			{value, {_, mnesia_frag_hash}} ->
%%% 			    mnesia:abort(
%%% 			      [{combine_error, Cs#cstruct.name,
%%% 				[{external_copies, Cs#cstruct.external_copies},
%%% 				 {hash_module, mnesia_frag_hash}]}]);
%%% 			{value, _Other} ->
%%% 			    %% hope for the best...
%%% 			    Cs;
%%% 			false ->
%%% 			    Cs#cstruct{frag_properties = 
%%% 				       [{hash_module, rdbms_log_hash}|
%%% 					Cs#cstruct.frag_properties]}
%%% 		    end
%%% 	    end;
	_ ->
	    X = fun([], _) -> [];
		   ([_|_]=Ns, T) -> [{T, Ns}]
		end,
	    mnesia:abort({combine_error, Cs#cstruct.name,
			  [{external_copies, Cs#cstruct.external_copies}|
			   X(ram_copies,Rc) ++ X(disc_copies,Dc) ++
			   X(disc_only_copies,DOc)]})
    end.

pos(A, Attrs) ->  pos(A, Attrs, 1).

pos(A, [A|_], P) ->  P;
pos(A, [_|T], P) ->  pos(A, T, P+1);
pos(_, [], _) ->     0.


verify_mountpoint(Dir, Tab) ->
    case file:read_link_info(Dir) of
	{ok, #file_info{type = symlink}} ->
	    case file:read_link(Dir) of 
		{ok, NewDir} ->
		    verify_mountpoint(NewDir, Tab);
		Error ->
		    mnesia:abort({bad_mountpoint, Tab, [Dir, Error]})
	    end;
	{ok, #file_info{type = directory}} ->
	    case file:list_dir(Dir) of
		{ok, _} ->
		    ok;
		Error ->
		    mnesia:abort({bad_mountpoint, Tab, [Dir, Error]})
	    end;
	{error, Reason} ->
	    mnesia:abort({bad_mountpoint, Tab, [Dir, Reason]})
    end.
	    


load_table(Tab, _LoadReason, Repair, _Type, _OnLoadFun) ->
    {_, Dir} = mnesia:read_table_property(Tab, rofs_mount),
    verify_mountpoint(Dir, Tab).


fullname(Tab, Key0) ->
    Key = if is_binary(Key0) ->
		  list_to_binary(Key0);
	     is_list(Key0) ->
		  lists:flatten(Key0)
	  end,
    {_, MP} = mnesia:read_table_property(Tab, rofs_mount),
    filename:join(MP, Key).


table_info(Tab, Item) ->
    {info, Tab, Item}.

read_file_info(Tab, Key) ->
    Fname = fullname(Tab, Key),
    Info = #info{tab = Tab,
		 key = Key,
		 fullname = Fname,
		 recname = mnesia:table_info(Tab, record_name)},
    try read_file_info1(Fname, Info, 1)
    catch
	error:Reason ->
	    mnesia:abort({Reason, erlang:get_stacktrace()})
    end.

read_file_info1(Fname, Info, X) when X < 100 ->
    case file:read_link_info(Fname) of
	{ok, #file_info{type = symlink}} ->
	    case file:read_link(Fname) of
		{ok, NewLink} ->
		    read_file_info1(Fname, X+1, Info);
		Error ->
		    mnesia:abort({Error, Info})
	    end;
	{ok, #file_info{} = FI} ->
	    Attrs = tl(tuple_to_list(FI)),
	    Read = fun() ->
			   {ok, Bin} = file:read_file(Fname),
			   Bin
		   end,
	    Tup = list_to_tuple([Fname, Info#info.key, data | Attrs]),
	    {_, Map} = mnesia:read_table_property(
			 Info#info.tab, rofs_attr_map),
	    Rec = list_to_tuple([Info#info.recname |
				 lists:map(
				   fun(3) ->
					   Read();  % fetch the data
				      (P) ->
					   element(P, Tup)
				   end, Map)]),
	    [Rec];	    
	{error, enoent} ->
	    []
    end.


db_get(Tab, Key) ->
    read_file_info(Tab, Key).

db_put(Tab, Val) ->
    erlang:error({not_allowed, [{?MODULE,db_put,[Tab,'...']}]}).

db_init_chunk(_Tab, _N) ->
    [].

db_chunk(_State) ->
    [].

match_object(Tab, Pat) ->
    select(Tab, [{Pat,[],['$_']}]).

select(Tab, Ms) ->
    [].

is_key_bound([{'_',_,_}]) ->
    false;
is_key_bound([{Pat,_,_}]) when is_atom(Pat) -> false;
is_key_bound([{Pat,_,_}]) when is_tuple(Pat) ->
    KeyPat = element(2,Pat),
    case regexp:match(atom_to_list(KeyPat), "^\\$[0-9]+$") of
	{match,_,_} ->
	    false;
	nomatch ->
	    true
    end;
is_key_bound([{Pat,_,_}]) when is_list(Pat) ->
    true.

remove_first_slash("/" ++ Str) ->
    Str;
remove_first_slash(Str) ->
    Str.

keypat_to_match(Str) ->
    keypat_to_match(Str, [], []).

keypat_to_match("/" ++ Str, DirAcc, Dirs) ->
    keypat_to_match(Str, [], [lists:reverse(DirAcc)|Dirs]);
keypat_to_match("." ++ T, DirAcc, Dirs) ->
    keypat_to_match(T, ".\\" ++ DirAcc, Dirs);
keypat_to_match([H|T], DirAcc, Dirs) when is_integer(H) ->
    keypat_to_match(T, [H|DirAcc], Dirs);
keypat_to_match([H|T], DirAcc, Dirs) when is_atom(H) ->
    keypat_to_match(T, "." ++ DirAcc, Dirs);
keypat_to_match('_', DirAcc, Dirs) ->
    {open, lists:reverse(Dirs), lists:reverse("*." ++ DirAcc)};
keypat_to_match([], DirAcc, Dirs) ->
    {closed, lists:reverse(Dirs), lists:reverse(DirAcc)}.


select_init(Tab, Ms, Limit) ->
    [].


repair_continuation(Cont, _Ms) ->
    Cont.

select(Cont) ->
    [].
%%%     case cont_log(Cont) of
%%% 	{_Res, _Cont} = Intermediate -> Intermediate;
%%% 	{[], _, 0} ->
%%% 	    '$end_of_table';
%%% 	{Result, _, _} ->
%%% 	    {lists:reverse(Result), '$end_of_table'}
%%%     end.

safe_fixtable(_Tab, _Bool) ->
    true.

db_erase(_Tab, _Key) ->
    erlang:error({not_allowed, [{?MODULE,db_erase,[_Tab,_Key]}]}).

match_erase(_Tab, _Pat) ->
    erlang:error({not_allowed, [{?MODULE,match_erase,[_Tab, _Pat]}]}).


db_first(Tab) ->
    Root = fullname(Tab, []),
    case file:list_dir(Root) of
	{ok, []} ->
	    '$end_of_table';
	{ok, [_|_] = Fs} ->
	    hd(lists:sort(Fs))
    end.

db_next_key(Tab, Key) ->
    '$end_of_table'.

db_last(_Tab) ->
    ok.

db_prev_key(_Tab, _Key) ->
    ok.

db_slot(_Tab, _Pos) ->
    ok.

db_update_counter(_Tab, _C, _Val) ->
    ok.

db_erase_tab(_Tab) ->
    ok.
