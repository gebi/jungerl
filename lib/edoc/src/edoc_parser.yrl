%% ========================== -*-Erlang-*- =============================
%% EDoc function specification grammar for the Yecc parser generator,
%% adapted from Sven-Olof Nyström's type specification parser.
%%
%% Copyright (C) 2002 Richard Carlsson
%%
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
%% Author contact: richardc@csd.uu.se
%%
%% $Id$
%%
%% ====================================================================

Nonterminals
start spec fun_type parameter_list parameters parameter utype_list
utype_tuple utypes utype ptypes ptype function_name defs def typedef.

Terminals
atom float integer var start_spec start_typedef

'(' ')' ',' '->' '{' '}' '[' ']' '|' '+' ':' '::' '='.

Rootsymbol start.

start -> start_spec spec: '$2'.
start -> start_typedef typedef: '$2'.

spec -> function_name fun_type defs:
    {spec, '$1', '$2', lists:reverse('$3')}.

function_name -> '$empty': none.
function_name -> atom: {name, tok_val('$1')}.

fun_type -> parameter_list '->' utype: {'fun', '$1', '$3'}.

parameter_list -> '(' ')' : [].
parameter_list -> '(' parameters ')': lists:reverse('$2').

%% Produced in reverse order.

parameters -> parameter: ['$1'].
parameters -> parameters ',' parameter: ['$3' | '$1'].
	
parameter -> utype: {par, none, '$1'}.
parameter -> var '::' utype: {par, {name, tok_val('$1')}, '$3'}	.

utype_list -> '(' ')' : [].
utype_list -> '(' utypes ')' : lists:reverse('$2').

utype_tuple -> '{' '}' : [].
utype_tuple -> '{' utypes '}' : lists:reverse('$2').

%% Produced in reverse order.

utypes -> utype : ['$1'].
utypes -> utypes ',' utype : ['$3' | '$1'].

utype -> ptypes: {union, lists:reverse('$1')}.

%% Produced in reverse order.
	
ptypes -> ptype : ['$1'].
ptypes -> ptypes '|' ptype : ['$3' | '$1'].
ptypes -> ptypes '+' ptype : ['$3' | '$1'].

ptype -> var : {var, tok_val('$1')}.
ptype -> fun_type: '$1'.
ptype -> utype_tuple : {tuple, '$1'}.
ptype -> '[' utype ']' : {list, '$2'}.
ptype -> '[' ']' : nil.
ptype -> atom utype_list: {type, tok_val('$1'), '$2'}.
ptype -> atom ':' atom utype_list : 
	{type, tok_val('$1'), tok_val('$3'), '$4'}.
ptype -> atom : {atom, tok_val('$1')}.
ptype -> integer: {integer, tok_val('$1')}.
ptype -> float: {float, tok_val('$1')}.

%% Produced in reverse order.

defs -> '$empty' : [].
defs -> defs def : ['$2' | '$1'].

def -> var '=' utype:
       {def, {var, tok_val('$1')}, '$3'}.
def -> atom parameter_list '=' utype:
       {def, {type, tok_val('$1'), '$2'}, '$4'}.

typedef -> atom parameter_list defs:
       {typedef, {type, tok_val('$1'), '$2'}, '$3'}.
typedef ->  atom parameter_list '=' utype defs:
       {typedef, {def, {type, tok_val('$1'), '$2'}, '$4'}, '$5'}.

Erlang code.

-export([parse_spec/1, parse_typedef/1, format/1]).

%% Multiple entry point hack:

parse_spec(Ts) -> parse([{start_spec, 0} | Ts]).

parse_typedef(Ts) -> parse([{start_typedef, 0} | Ts]).

format({spec, none, T, Ds}) ->
    format(T) ++ format_defs(Ds);
format({spec, {name, F}, T, Ds}) ->
    atom_to_list(F) ++ format(T) ++ format_defs(Ds);
format({typedef, T, Ds}) ->
    format(T) ++ format_defs(Ds);
format({'fun', As, T}) ->
    "(" ++ format_seq(As) ++ ") -> " ++ format(T);
format({par, none, T}) ->
    format(T);
format({par, {name, N}, T}) ->
    atom_to_list(N) ++ "::" ++ format(T);
format({def, V, T}) ->
    format(V) ++ " = " ++ format(T);
format({union, As}) ->
    format_union(As);
format({type, N, As}) ->
    atom_to_list(N) ++ "(" ++ format_seq(As) ++ ")";
format({type, M, N, As}) ->
    atom_to_list(M) ++ ":" ++ atom_to_list(N)
	++ "(" ++ format_seq(As) ++ ")";
format({tuple, As}) ->
    "{" ++ format_seq(As) ++ "}";
format({list, T}) ->
    "[" ++ format(T) ++ "]";
format({var, V}) ->
    atom_to_list(V);
format(nil) ->
    "[]";
format({atom, V}) ->
    atom_to_list(V);
format({integer, V}) ->
    integer_to_list(V);
format({float, V}) ->
    float_to_list(V).

format_union(Ts) -> format_seq(Ts, "| ").

format_seq(Ts) -> format_seq(Ts, ", ").

format_seq([T], S) -> format(T);
format_seq([T | Ts], S) -> format(T) ++ S ++ format_seq(Ts, S);
format_seq([], _) -> "".

format_defs([D | Ds]) -> "\n\t" ++ format(D) ++ format_defs(Ds);
format_defs([]) -> "".

%% Utility functions:

tok_val(T) -> element(3, T).
