%% =====================================================================
%% Support functions for property lists
%%
%% Property lists are ordinary lists containing entries in the form of
%% tuples whose first elements are keys used for lookup and insertion,
%% and atoms, which work as shorthand for tuples `{Atom, true}'. (Terms
%% not matching this description are allowed as elements in the lists,
%% but are ignored by this module.) If there is more than one entry in a
%% list for a certain key, the first occurrence normally overrides any
%% later (irrespective of the arity of the tuples). Property lists are
%% useful for representing inherited properties, such as options passed
%% to a function where a user may specify options overriding the default
%% settings, object properties, annotations, etc.
%%
%% Copyright (C) 2000 Richard Carlsson
%%
%% $Id$
%% ---------------------------------------------------------------------
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
%% =====================================================================

-module(proplists).

-export([property/1, property/2, unfold/1, compact/1, lookup/2,
	 lookup_all/2, is_defined/2, get_value/2, get_value/3,
	 get_all_values/2, append_values/2, get_bool/2, get_keys/1,
	 delete/2, substitute_aliases/2, substitute_negations/2,
	 expand/2, normalize/2]).


%% =====================================================================
%% property(Property) -> Property1
%% property(Key, Value) -> Property
%%
%%	    Property = Property1 = Name | {Key, ...} | term()
%%	    Name = atom()
%%	    Key = Name | term()
%%	    Value = term()
%%
%%	These are simple utility functions for creating a minimal
%%	(normal form) representation of a property:
%%
%%	`property(Property)' returns `Key' if `Property' is `{Key,
%%	true}' and `Key' is an atom; otherwise it returns `Property'.
%%
%%	`property(Key, Value)' returns `Key' if `Value' is the atom
%%	`true' and `Key' is an atom; otherwise it returns `{Key,
%%	Value}'.

property({Key, true}) when atom(Key) ->
    Key;
property(Property) ->
    Property.

property(Key, true) when atom(Key) ->
    Key;
property(Key, Value) ->
    {Key, Value}.


%% =====================================================================
%% unfold(List) -> [{Key, ...} | term()]
%%
%%	    List = [Name | {Key, ...} | term()]
%%	    Name = atom()
%%	    Key = Name | term()
%%
%%	Unfolds all occurences of atoms A in `List' to corresponding
%%	2-tuples `{A, true}'.
%%
%% compact(List) -> [{Key, ...} | term()]
%%
%%	    List = [Name | {Key, ...} | term()]
%%	    Name = atom()
%%	    Key = Name | term()
%%
%%	This is equivalent to `[property(P) || P <- List]', minimizing
%%	the representation of all entries in the list.

unfold([P | Ps]) ->
    if atom(P) ->
	    [{P, true} | unfold(Ps)];
       true ->
	    [P | unfold(Ps)]
    end;
unfold([]) ->
    [].

compact(List) ->
    [property(P) || P <- List].


%% =====================================================================
%% lookup(Key, List) -> none | {Key, ...}
%%
%%	    Key = Name | term()
%%	    Name = atom()
%%	    List = [Name | {Key, ...} | term()]
%%
%%	Returns the first entry associated with the given key `Key' in
%%	the property list `List', if one exists, or otherwise `none'.
%%	For a tuple T = `{Key, ...}' in the list, T itself is the entry
%%	associated with `Key'. For an atom A in the list, the tuple `{A,
%%	true}' is the entry associated with `Key' if A = `Key'.
%%
%% lookup_all(Key, List) -> [{Key, ...}]
%%
%%	    Key = Name | term()
%%	    Name = atom()
%%	    List = [Name | {Key, ...} | term()]
%%
%%	Like `lookup', but returns the list of *all* entries associated
%%	with `Key' in `List'. If no such entry exists, the result is the
%%	empty list.

lookup(Key, [P | Ps]) ->
    if atom(P), P =:= Key ->
	    {Key, true};
       tuple(P), size(P) >= 1, element(1, P) =:= Key ->
	    %% Note that `Key' does not have to be an atom in this case.
	    P;
       true ->
	    lookup(Key, Ps)
    end;
lookup(Key, []) ->
    none.

lookup_all(Key, [P | Ps]) ->
    if atom(P), P =:= Key ->
	    [{Key, true} | lookup_all(Key, Ps)];
       tuple(P), size(P) >= 1, element(1, P) =:= Key ->
	    [P | lookup_all(Key, Ps)];
       true ->
	    lookup_all(Key, Ps)
    end;
lookup_all(Key, []) ->
    [].


%% =====================================================================
%% is_defined(Key, List) -> bool()
%%
%%	    Key = Name | term()
%%	    Name = atom()
%%	    List = [Name | {Key, ...} | term()]
%%
%%	Returns `true' if `List' contains an element `{Key, ...}', or if
%%	`Key' is an atom, an element `Key' or `{Key, ...}', otherwise
%%	returns `false'.

is_defined(Key, [P | Ps]) ->
    if atom(P), P =:= Key ->
	    true;
       tuple(P), size(P) >= 1, element(1, P) =:= Key ->
	    true;
       true ->
	    is_defined(Key, Ps)
    end;
is_defined(Key, []) ->
    false.


%% =====================================================================
%% get_value(Key, List) -> term()
%% get_value(Key, List, Default) -> term()
%%
%%	    Key = Name | term()
%%	    Name = atom()
%%	    List = [Name | {Key, ...} | term()]
%%	    Default = term()
%%
%%	If `lookup(Key, List)' would yield `{Key, Value}', this function
%%	returns the corresponding `Value'; otherwise the value `Default'
%%	is returned. Thus, the entry associated with `Key' is assumed to
%%	be a 2-tuple, and if it is not, or if there is no entry
%%	associated with `Key' in `List', the value `Default' is used. By
%%	default, `Default' is the atom `undefined'.
%%
%% get_all_values(Key, List) -> [term()]
%%
%%	    Key = Name | term()
%%	    Name = atom()
%%	    List = [Name | {Key, ...} | term()]
%%
%%	Like `get_value', but returns the list of corresponding values
%%	for *all* 2-tuple entries associated with `Key' in `List'. If no
%%	such entry exists, the result is the empty list.
%%
%% append_values(Key, List) -> [term()]
%%
%%	    Key = Name | term()
%%	    Name = atom()
%%	    List = [Name | {Key, ...} | term()]
%%
%%	Like `get_all_values', but treating any value that is not a list
%%	as representing a singleton list, and appending all the
%%	resulting lists. This is often useful for "incremental" options;
%%	e.g., `append_values(a,[{a,[1,2]},{b,0},{a,3},{c,-1},{a,[4]}])'
%%	will return the list `[1,2,3,4].

get_value(Key, List) ->
    get_value(Key, List, undefined).

get_value(Key, [P | Ps], Default) ->
    if atom(P), P =:= Key ->
	    true;
       tuple(P), size(P) >= 1, element(1, P) =:= Key ->
	    case P of
		{_, Value} ->
		    Value;
		_ ->
		    %% Don't continue the search!
		    Default
	    end;
       true ->
	    get_value(Key, Ps, Default)
    end;
get_value(Key, [], Default) ->
    Default.

get_all_values(Key, [P | Ps]) ->
    if atom(P), P =:= Key ->
	    [true | get_all_values(Key, Ps)];
       tuple(P), size(P) >= 1, element(1, P) =:= Key ->
	    case P of
		{_, Value} ->
		    [Value | get_all_values(Key, Ps)];
		_ ->
		    get_all_values(Key, Ps)
	    end;
       true ->
	    get_all_values(Key, Ps)
    end;
get_all_values(Key, []) ->
    [].

append_values(Key, [P | Ps]) ->
    if atom(P), P =:= Key ->
	    [true | append_values(Key, Ps)];
       tuple(P), size(P) >= 1, element(1, P) =:= Key ->
	    case P of
		{_, Value} when list(Value) ->
		    Value ++ append_values(Key, Ps);
		{_, Value} ->
		    [Value | append_values(Key, Ps)];
		_ ->
		    append_values(Key, Ps)
	    end;
       true ->
	    append_values(Key, Ps)
    end;
append_values(Key, []) ->
    [].


%% =====================================================================
%% get_bool(Key, List) -> bool()
%%
%%	    Key = Name | term()
%%	    Name = atom()
%%	    List = [Name | {Key, ...} | term()]
%%
%%	If `lookup(Key, List)' would yield `{Key, true}', this function
%%	returns `true'; otherwise `false' is returned. Thus, the entry
%%	associated with `Key' is assumed to be a 2-tuple whose second
%%	element is a boolean value (`true' or `false'), and if it is
%%	not, or if there is no entry associated with `Key' in `List',
%%	the value `false' is used.

get_bool(Key, [P | Ps]) ->
    if atom(P), P =:= Key ->
	    true;
       tuple(P), size(P) >= 1, element(1, P) =:= Key ->
	    case P of
		{_, true} ->
		    true;
		_ ->
		    %% Don't continue the search!
		    false
	    end;
       true ->
	    get_bool(Key, Ps)
    end;
get_bool(Key, []) ->
    false.


%% =====================================================================
%% get_keys(List) -> [Key]
%%
%%	    Expansions = [{Property, Expansion}]
%%	    List = [Property]
%%	    Property = Name | {Key ...} | term()
%%	    Name = atom()
%%	    Key = Name | term()
%%
%%	Returns a list of the keys used in `List', without duplicates.

get_keys(Ps) ->
    sets:to_list(get_keys(Ps, sets:new())).

get_keys([P | Ps], Keys) ->
    if atom(P) ->
	    get_keys(Ps, sets:add_element(P, Keys));
       tuple(P), size(P) >= 1 ->
	    get_keys(Ps, sets:add_element(element(1, P), Keys));
       true ->
	    get_keys(Ps, Keys)
    end;
get_keys([], Keys) ->
    Keys.


%% =====================================================================
%% delete(Key, List) -> List1
%%
%%	    Key = Name | term()
%%	    Name = atom()
%%	    List = List1 = [Name | {Key, ...} | term()]
%%
%%	Deletes all occurrences of `{Key, ...}', and if `Key' is an
%%	atom, also all occurrences of `Key', from `List'

delete(Key, [P | Ps]) ->
    if atom(P), P =:= Key ->
	    delete(Key, Ps);
       tuple(P), size(P) >= 1, element(1, P) =:= Key ->
	    delete(Key, Ps);
       true ->
	    [P | delete(Key, Ps)]
    end;
delete(_, []) ->
    [].


%% =====================================================================
%% substitute_aliases(Aliases, List) -> List1
%%
%%	    Aliases = [{Key1, Key2}]
%%	    List = List1 = [Name | {Key, ...} | term()]
%%	    Name = atom()
%%	    Key1 = Key2 = Key = Name | term()
%%
%%	Replaces each entry in `List' associated with some key `Key1' in
%%	`Aliases' with a corresponding entry associated with `Key2'. If
%%	the same value of `Key1' occurs more than once in `Aliases',
%%	only the first occurrence is used. For example,
%%	`substitute_aliases([{colour, color}], L)' will replace any
%%	tuple `{colour, ...}' in `L' with `{color, ...}', and any atom
%%	`colour' with `color'.

substitute_aliases(As, [P | Ps]) ->
    [substitute_aliases_1(As, P) | substitute_aliases(As, Ps)];
substitute_aliases(As, []) ->
    [].

substitute_aliases_1([{Key, Key1} | As], P) ->
    if atom(P), P =:= Key ->
	    property(Key1, true);
       tuple(P), size(P) >= 1, element(1, P) =:= Key ->
	    property(setelement(1, P, Key1));
       true ->
	    substitute_aliases_1(As, P)
    end;
substitute_aliases_1([], P) ->
    P.


%% =====================================================================
%% substitute_negations(Aliases, List) -> List1
%%
%%	    Aliases = [{Key1, Key2}]
%%	    List = List1 = [Name | {Key, ...} | term()]
%%	    Name = atom()
%%	    Key1 = Key2 = Key = Name | term()
%%
%%	Replaces each entry in `List' associated with some key `Key1' in
%%	`Aliases', which is assumed to be a boolean property, with a
%%	corresponding *negated* entry associated with `Key2'. If the
%%	same value of `Key1' occurs more than once in `Aliases', only
%%	the first occurrence is used. For example,
%%	`substitute_negations([{no_foo, foo}], L)' will replace any
%%	tuple `{no_foo, X}' in `L' with the value of `{foo, not X}', and
%%	any atom `no_foo' with `{foo, false}'.

substitute_negations(As, [P | Ps]) ->
    [substitute_negations_1(As, P) | substitute_negations(As, Ps)];
substitute_negations(As, []) ->
    [].

substitute_negations_1([{Key, Key1} | As], P) ->
    if atom(P), P =:= Key ->
	    property(Key1, false);
       tuple(P), size(P) >= 1, element(1, P) =:= Key ->
	    case P of
		{_, true} ->
		    property(Key1, false);
		{_, false} ->
		    property(Key1, true);
		_ ->
		    %% The property is supposed to be a boolean, so any
		    %% other tuple is interpreted as `false', as done in
		    %% `get_bool'.
		    property(Key1, true)
	    end;		    
       true ->
	    substitute_negations_1(As, P)
    end;
substitute_negations_1([], P) ->
    P.


%% =====================================================================
%% expand(Expansions, List) -> List1
%%
%%	    Expansions = [{Property, Expansion}]
%%	    List = List1 = Expansion = [Property]
%%	    Property = Name | {Key ...} | term()
%%	    Name = atom()
%%	    Key = Name | term()
%%
%%	For each element `{Property_i, Expansion_i}' in `Expansions', in
%%	order, let `E_i' be the first entry in `List' with the same key
%%	as `Property_i' (if one exists). `List1' is then `List' with the
%%	elements of each `Expansion_i' inserted in place of the
%%	corresponding `E_i', whenever `E_i' and `Property_i' have
%%	equivalent normal forms. If an entry is expanded, then any
%%	following entries with the same key are deleted from the list.
%%
%%	For example, all of `expand([{foo, [bar, baz]}], [fie, foo,
%%	fum])', `expand([{{foo, true}, [bar, baz]}], [fie, foo, fum]'
%%	and `expand([{{foo, false}, [bar, baz]}], [fie, {foo, false},
%%	fum]' return `[fie, bar, baz, fum]'. However, no expansion is
%%	done in the call `expand([{{foo, true}, [bar, baz]}], [fie,
%%	{foo, false}, foo, fum])', because `{foo, false}' shadows `foo'.
%%
%%	Note that if the existing property is to be preserved when
%%	expanded, it must be included in the expansion list. The
%%	inserted expansions are not expanded recursively.

expand(Es, Ps) ->
    flatten(expand_0(key_uniq(normalize(Es)), Ps)).

%% Here, all key properties are normalized and there are no multiple
%% entries in the list of expansions for any specific key property. We
%% insert the expansions one at a time - this is quadratic, but gives
%% the desired behaviour in a simple way.

expand_0([{P, L} | Es], Ps) ->
    expand_0(Es, expand_1(P, L, Ps));
expand_0([], Ps) ->
    Ps.

expand_1(P, L, Ps) ->
    %% First, we must find out what key to look for.
    %% P has a minimal representation here.
    if atom(P) ->
	    expand_2(P, P, L, Ps);
       tuple(P), size(P) >= 1 ->
	    expand_2(element(1, P), P, L, Ps);
       true ->
	    Ps    % refuse to expand non-property
    end.

expand_2(Key, P1, L, [P | Ps]) ->
    if atom(P), P =:= Key ->
	    expand_3(Key, P1, P, L, Ps);
       tuple(P), size(P) >= 1, element(1, P) =:= Key ->
	    expand_3(Key, P1, property(P), L, Ps);
       true ->
	    %% This case handles non-property entries, and thus
	    %% any already inserted expansions (lists), by simply
	    %% ignoring them.
	    [P | expand_2(Key, P1, L, Ps)]
    end;
expand_2(_, _, _, []) ->
    [].

expand_3(Key, P1, P, L, Ps) ->
    %% Here, we have found the first entry with a matching key. Both P
    %% and P1 have minimal representations here. The inserted list will
    %% be flattened afterwards. If the expansion is done, we drop the
    %% found entry and alao delete any later entries with the same key.
    if P1 =:= P ->
	    [L | delete(Key, Ps)];
       true ->
	    %% The existing entry does not match - keep it.
	    [P | Ps]
    end.

normalize([{P, V} | Ps]) ->
    [{property(P), V} | normalize(Ps)];
normalize([]) ->
    [].

key_uniq([{K, V} | Ps]) ->
    [{K, V} | key_uniq_1(K, Ps)];
key_uniq([]) ->
    [].

key_uniq_1(K, [{K1, V} | Ps]) ->
    if K =:= K1 ->
	    key_uniq_1(K, Ps);
       true ->
	    [{K1, V} | key_uniq_1(K1, Ps)]
    end;
key_uniq_1(K, []) ->
    [].

%% This does top-level flattening only.

flatten([E | Es]) when list(E) ->
    E ++ flatten(Es);
flatten([E | Es]) ->
    [E | flatten(Es)];
flatten([]) ->
    [].


%% =====================================================================
%% normalize(List, Stages) -> List1
%%
%%	    Stages = [Stage]
%%	    Stage = {aliases, Aliases} | {expansions, ExpansionList}
%%	          | {negations, Negations}
%%	    Aliases = Negations = [{Key1, Key2}]
%%	    Key1 = Key2 = Key = Name | term()
%%	    ExpansionList = [{Property, Expansion}]
%%	    Property = Name | {Key ...} | term()
%%	    Name = atom()
%%	    List = List1 = Expansion = [Property]
%%
%%	This passes `List' through a sequence of processing stages, as
%%	defined by `Stages'. For an `aliases' stage,
%%	`substitute_aliases' is applied using the `Aliases' field; for
%%	an `expansions' stage, the `expand' function is applied using
%%	the `ExpansionList' field; for a `negations' stage,
%%	`substitute_negations' is applied using the `Negations' field.
%%	The final result is automatically compacted (cf. `compact').
%%
%%	In the normal case, you want to substitute aliases first, then
%%	perform one or more expansions (sometimes you want to pre-expand
%%	particular entries before doing the main expansion), and lastly,
%%	you substitute negations (allowing negative forms to be used in
%%	expansion lists).

normalize(L, [{aliases, As} | Xs]) ->
    normalize(substitute_aliases(As, L), Xs);
normalize(L, [{expand, Es} | Xs]) ->
    normalize(expand(Es, L), Xs);
normalize(L, [{negations, Ns} | Xs]) ->
    normalize(substitute_negations(Ns, L), Xs);
normalize(L, []) ->
    compact(L).


%% =====================================================================
