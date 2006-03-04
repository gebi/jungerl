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
%%% File    : rdbms_frag.erl
%%% Author  : Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description : Code snipped from mnesia_frag
%%%
%%% Created : 21 Dec 2005 by Ulf Wiger
%%%-------------------------------------------------------------------
-module(rdbms_frag).

-export([key_to_frag_name/2]).


%%% Code snipped from mnesia_frag, since the needed functions
%%% are not exported.

-define(OLD_HASH_MOD, mnesia_frag_old_hash).
-define(DEFAULT_HASH_MOD, mnesia_frag_hash).
%%-define(DEFAULT_HASH_MOD, ?OLD_HASH_MOD). %%  BUGBUG: New should be default

-record(frag_state,
	{foreign_key,
	 n_fragments,
	 hash_module,
	 hash_state}).

-include_lib("mnesia/src/mnesia.hrl").
-import(mnesia_lib, [val/1]).

%% Returns name of fragment table
key_to_frag_name({BaseTab, _} = Tab, Key) ->
    N = key_to_frag_number(Tab, Key),
    n_to_frag_name(BaseTab, N);
key_to_frag_name(Tab, Key) ->
    N = key_to_frag_number(Tab, Key),
    n_to_frag_name(Tab, N).

%% Returns name of fragment table
n_to_frag_name(Tab, 1) ->
    Tab;
n_to_frag_name(Tab, N) when atom(Tab), integer(N) ->
    list_to_atom(atom_to_list(Tab) ++ "_frag" ++ integer_to_list(N));
n_to_frag_name(Tab, N) ->
    mnesia:abort({bad_type, Tab, N}).

%% Returns name of fragment table
key_to_frag_number({Tab, ForeignKey}, _Key) ->
    FH = val({Tab, frag_hash}),
    case FH#frag_state.foreign_key of
	{_ForeignTab, _Pos} ->
	    key_to_n(FH, ForeignKey);
	undefined ->
	    mnesia:abort({combine_error, Tab, frag_properties,
			  {foreign_key, undefined}})
    end;
key_to_frag_number(Tab, Key) ->
    case ?catch_val({Tab, frag_hash}) of
	{'EXIT', _} ->
	    1;
	FH ->
	    key_to_n(FH, Key)
    end.

%% Returns fragment number
key_to_n(FH, Key) ->
    HashState = FH#frag_state.hash_state,
    N = 
	case FH#frag_state.hash_module of
	    HashMod when HashMod == ?DEFAULT_HASH_MOD ->
		?DEFAULT_HASH_MOD:key_to_frag_number(HashState, Key);
	    HashMod ->
		HashMod:key_to_frag_number(HashState, Key)
	end,
    if
	integer(N), N >= 1, N =< FH#frag_state.n_fragments ->
	    N;
	true ->
	    mnesia:abort({"key_to_frag_number: Fragment number out of range",
			  N, {range, 1, FH#frag_state.n_fragments}})
    end.
