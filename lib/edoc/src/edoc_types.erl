%% =====================================================================
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% $Id$
%%
%% @private
%% @copyright 2001-2003 Richard Carlsson
%% @author Richard Carlsson <richardc@csd.uu.se>
%% @see edoc
%% @end
%% =====================================================================

%% @doc Datatype representation for EDoc.

-module(edoc_types).

-export([is_predefined/1, to_ref/1, to_xml/2, to_label/1, arg_names/1,
	 set_arg_names/2]).

-include("edoc.hrl").
-include("xmerl.hrl").


is_predefined(any) -> true;
is_predefined(atom) -> true;
is_predefined(binary) -> true;
is_predefined(bool) -> true;
is_predefined(char) -> true;
is_predefined(cons) -> true;
is_predefined(deep_string) -> true;
is_predefined(float) -> true;
is_predefined(function) -> true;
is_predefined(integer) -> true;
is_predefined(list) -> true;
is_predefined(nil) -> true;
is_predefined(none) -> true;
is_predefined(number) -> true;
is_predefined(pid) -> true;
is_predefined(port) -> true;
is_predefined(reference) -> true;
is_predefined(string) -> true;
is_predefined(term) -> true;
is_predefined(tuple) -> true;
is_predefined(_) -> false.

to_ref(#t_typedef{name = N}) ->
    to_ref(N);
to_ref(#t_def{name = N}) ->
    to_ref(N);
to_ref(#t_type{name = N}) ->
    to_ref(N);
to_ref(#t_name{module = [], name = N}) ->
    edoc_refs:type(N);
to_ref(#t_name{app = [], module = M, name = N}) ->
    edoc_refs:type(M, N);
to_ref(#t_name{app = A, module = M, name = N}) ->
    edoc_refs:type(A, M, N).

to_label(N) ->
    edoc_refs:to_label(to_ref(N)).

get_uri(Name, Env) ->
    edoc_refs:get_uri(to_ref(Name), Env).

to_xml(#t_var{name = N}, _Env) ->
    {typevar, [{name, atom_to_list(N)}], []};
to_xml(#t_name{module = [], name = N}, _Env) ->
    {erlangName, [{name, atom_to_list(N)}], []};
to_xml(#t_name{app = [], module = M, name = N}, _Env) ->
    {erlangName, [{module, atom_to_list(M)},
		  {name, atom_to_list(N)}], []};
to_xml(#t_name{app = A, module = M, name = N}, _Env) ->
    {erlangName, [{app, atom_to_list(A)},
		  {module, atom_to_list(M)},
		  {name, atom_to_list(N)}], []};
to_xml(#t_type{name = N, args = As}, Env) ->
    Predef = case N of
		 #t_name{module = [], name = T} ->
		     is_predefined(T);
		 _ ->
		     false
	     end,
    HRef = case Predef of
	       true -> [];
	       false -> [{href, get_uri(N, Env)}]
	   end,
    {abstype, HRef, [to_xml(N, Env) | map(fun wrap_utype/2, As, Env)]};
to_xml(#t_fun{args = As, range = T}, Env) ->
    {'fun', [{argtypes, map(fun wrap_utype/2, As, Env)},
	     wrap_utype(T, Env)]};
to_xml(#t_tuple{types = Ts}, Env) ->
    {tuple, map(fun wrap_utype/2, Ts, Env)};
to_xml(#t_list{type = T}, Env) ->
    {list, [wrap_utype(T, Env)]};
to_xml(#t_nil{}, _Env) ->
    nil;
to_xml(#t_atom{val = V}, _Env) ->
    {atom, [{value, io_lib:write(V)}], []};
to_xml(#t_integer{val = V}, _Env) ->
    {integer, [{value, integer_to_list(V)}], []};
to_xml(#t_float{val = V}, _Env) ->
    {float, [{value, io_lib:write(V)}], []};
to_xml(#t_union{types = Ts}, Env) ->
    {union, map(fun wrap_type/2, Ts, Env)};
to_xml(#t_def{name = N = #t_var{}, type = T}, Env) ->
    {localdef, [to_xml(N, Env), wrap_type(T, Env)]};
to_xml(#t_def{name = N, type = T}, Env) ->
    {localdef, [{label, to_label(N)}],
     [to_xml(N, Env), wrap_type(T, Env)]};
to_xml(#t_spec{name = N, type = T, defs = Ds}, Env) ->
    {typespec, [to_xml(N, Env), wrap_utype(T, Env)
		| map(fun to_xml/2, Ds, Env)]};
to_xml(#t_typedef{name = N, args = As, type = undefined, defs = Ds},
	 Env) ->
    {typedef, [to_xml(N, Env),
	       {argtypes, map(fun wrap_utype/2, As, Env)}
	       | map(fun to_xml/2, Ds, Env)]};
to_xml(#t_typedef{name = N, args = As, type = T, defs = Ds}, Env) ->
    {typedef, [to_xml(N, Env),
	       {argtypes, map(fun wrap_utype/2, As, Env)},
	       wrap_type(T, Env)
	       | map(fun to_xml/2, Ds, Env)]}.

wrap_type(T, Env) ->
    {type, [to_xml(T, Env)]}.

wrap_utype(T, Env) ->
    E = to_xml(T, Env),
    case ?t_ann(T) of
	[] -> {type, [E]};
	A -> {type, [{name, atom_to_list(A)}], [E]}
    end.

map(F, Xs, Env) ->
    [F(X, Env) || X <- Xs].

arg_names(#t_spec{type = #t_fun{args = As}}) ->
    [arg_name(A) || A <- As].

arg_name(T) ->
    case ?t_ann(T) of
	[] -> '_';
	N -> N
    end.

set_arg_names(#t_spec{type = #t_fun{args = As}=F}=S, As1) ->
    S#t_spec{type = F#t_fun{args = set_arg_names_1(As, As1)}}.

set_arg_names_1([A | As], [A1 | As1]) ->
    [?set_t_ann(A, A1) | set_arg_names_1(As, As1)];
set_arg_names_1([], []) ->
    [].
