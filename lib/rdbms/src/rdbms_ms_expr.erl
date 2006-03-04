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

%%%-------------------------------------------------------------------
%%% File    : rdbms_ms_expr.erl
%%% Author  : Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description : match expression emulator
%%%
%%% Created : 17 Jan 2006 by Ulf Wiger
%%%-------------------------------------------------------------------
-module(rdbms_ms_expr).

-compile(export_all).



parse(Ms, KeyPos) ->
    lists:foldr(fun(MF, Acc) ->
			[match_function(MF, KeyPos)|Acc]
		end, [], Ms).

match_function({MatchHead, MatchConds, MatchBody}, KeyPos) ->
    IsBound = is_bound(MatchHead, MatchConds, KeyPos).


is_bound('_', _, _) ->
    false;
is_bound(Var, _, _) when is_atom(Var) ->
    false;
is_bound(Pat, MatchConds, KeyPos) when is_tuple(Pat) ->
    KeyPat = element(KeyPos, Pat),
    if is_atom(KeyPat) ->
	    not(is_var(KeyPat));
       is_tuple(KeyPat) ->
	    case element(KeyPos, Pat) of
		'_' ->
		    false;
		A when is_atom(A) ->
		    is_var(A);
		L when is_list(L) ->
		    has_wild(L)
	    end
    end.


is_var(A) when is_atom(A) ->
    case regexp:match(atom_to_list(A), "^\\$[0-9]+$") of
	{match,_,_} ->
	    %% should really also check MatchConds...
	    false;
	nomatch ->
	    true
    end;
is_var(_) ->
    false.

has_wild([]) -> false;
has_wild([I]) when is_integer(I) ->
    false;
has_wild(L) ->
    has_wild(L, []).

has_wild([H|T], Acc) when is_integer(H) ->
    has_wild(T, [H|Acc]);
has_wild([H|T], Acc) when is_atom(T) ->
    {partial, lists:reverse([H|Acc])};
has_wild([H|_], Acc) when is_atom(H) ->
    {partial, lists:reverse(Acc)};
has_wild([], _) ->
    false.
