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
start spec fun_type utype_list utype_tuple utypes utype ptypes ptype
function_name defs def typedef qname ref aref mref lref pref var_list
vars.

Terminals
atom float integer var start_spec start_typedef start_ref

'(' ')' ',' '.' '->' '{' '}' '[' ']' '|' '+' ':' '::' '=' '/' '//' '*'.

Rootsymbol start.

start -> start_spec spec: '$2'.
start -> start_typedef typedef: '$2'.
start -> start_ref ref: '$2'.

%% Produced in reverse order.
qname -> atom: [tok_val('$1')].
qname -> qname '.' atom: [tok_val('$3') | '$1'].

spec -> fun_type defs:
    #t_spec{type = '$1', defs = lists:reverse('$2')}.
spec -> function_name fun_type defs:
    #t_spec{name = '$1', type = '$2', defs = lists:reverse('$3')}.

function_name -> atom: #t_name{name = tok_val('$1')}.

fun_type -> utype_list '->' utype:
    #t_fun{args = '$1', range = '$3'}.


utype_list -> '(' ')' : [].
utype_list -> '(' utypes ')' : lists:reverse('$2').

utype_tuple -> '{' '}' : [].
utype_tuple -> '{' utypes '}' : lists:reverse('$2').

%% Produced in reverse order.
utypes -> utype : ['$1'].
utypes -> utypes ',' utype : ['$3' | '$1'].

utype -> var '::' ptypes: ann(union('$3'), tok_val('$1')).
utype -> ptypes: union('$1').

%% Produced in reverse order.
ptypes -> ptype : ['$1'].
ptypes -> ptypes '+' ptype : ['$3' | '$1'].
ptypes -> ptypes '|' ptype : ['$3' | '$1'].

ptype -> var : #t_var{name = tok_val('$1')}.
ptype -> atom : #t_atom{val = tok_val('$1')}.
ptype -> integer: #t_integer{val = tok_val('$1')}.
ptype -> float: #t_float{val = tok_val('$1')}.
ptype -> fun_type: '$1'.
ptype -> utype_tuple : #t_tuple{types = '$1'}.
ptype -> '[' ']' : #t_nil{}.
ptype -> '[' utype ']' : #t_list{type = '$2'}.
ptype -> atom utype_list:
	#t_type{name = #t_name{name = tok_val('$1')},
		args = '$2'}.
ptype -> qname ':' atom utype_list : 
	#t_type{name = #t_name{module = qname('$1'),
			       name = tok_val('$3')},
		args = '$4'}.
ptype -> '//' atom '/' qname ':' atom utype_list : 
	#t_type{name = #t_name{app = tok_val('$2'),
			       module = qname('$4'),
			       name = tok_val('$6')},
		args = '$7'}.

%% Produced in reverse order.
defs -> '$empty' : [].
defs -> defs def : ['$2' | '$1'].

def -> var '=' utype:
       #t_def{name =  #t_var{name = tok_val('$1')},
	      type = '$3'}.
def -> atom var_list '=' utype:
       #t_def{name = #t_type{name = #t_name{name = tok_val('$1')},
			     args = '$2'},
	      type = '$4'}.

var_list -> '(' ')' : [].
var_list -> '(' vars ')' : lists:reverse('$2').

%% Produced in reverse order.
vars -> var : [#t_var{name = tok_val('$1')}].
vars -> vars ',' var : [#t_var{name = tok_val('$3')} | '$1'].

typedef -> atom var_list defs:
       #t_typedef{name = #t_name{name = tok_val('$1')},
		  args = '$2',
		  defs = lists:reverse('$3')}.
typedef -> atom var_list '=' utype defs:
       #t_typedef{name = #t_name{name = tok_val('$1')},
		  args = '$2',
		  type = '$4',
		  defs = lists:reverse('$5')}.

%% References

ref -> aref: '$1'.
ref -> mref: '$1'.
ref -> lref: '$1'.
ref -> pref: '$1'.

aref -> '//' atom:
    edoc_refs:app(tok_val('$2')).
aref -> '//' atom '/' mref:
    edoc_refs:app(tok_val('$2'), '$4').
aref -> '//' atom '/' pref:
    edoc_refs:app(tok_val('$2'), '$4').

mref -> qname ':' atom '/' integer:
    edoc_refs:function(qname('$1'), tok_val('$3'), tok_val('$5')).
mref -> qname ':' atom '(' ')':
    edoc_refs:type(qname('$1'), tok_val('$3')).
mref -> qname:
    edoc_refs:module(qname('$1')).

pref -> qname '.' '*':
    edoc_refs:package(qname('$1')).

lref -> atom '/' integer:
    edoc_refs:function(tok_val('$1'), tok_val('$3')).
lref -> atom '(' ')':
    edoc_refs:type(tok_val('$1')).

Erlang code.

%% ========================== -*-Erlang-*- =============================
%% EDoc function specification parser, generated from the file
%% "edoc_parser.yrl" by the Yecc parser generator.
%%
%% Copyright (C) 2002-2004 Richard Carlsson
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
%% ====================================================================

-export([parse_spec/2, parse_typedef/2, parse_ref/2]).

-include("edoc.hrl").

%% Multiple entry point hack:

parse_spec(Ts, L) -> run_parser(Ts, L, start_spec).

parse_typedef(Ts, L) -> run_parser(Ts, L, start_typedef).

parse_ref(Ts, L) -> run_parser(Ts, L, start_ref).

%% Error reporting fix

run_parser(Ts, L, Start) ->
    case parse([{Start,L} | Ts]) of
	{error, {999999,?MODULE,_}} ->
	    What = case Start of
		       start_spec -> "specification";
		       start_typedef -> "type definition";
		       start_ref -> "reference"
		   end,
	    {error, {L,?MODULE,["unexpected end of ", What]}};
	Other -> Other
    end.

%% Utility functions:

tok_val(T) -> element(3, T).

qname([A]) ->
    A;    % avoid unnecessary call to packages:concat/1.
qname(List) ->
    list_to_atom(packages:concat(lists:reverse(List))).

union(Ts) ->
    case Ts of
	[T] -> T;
	_ -> #t_union{types = lists:reverse(Ts)}
    end.

ann(T, A) -> ?set_t_ann(T, A).
