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

%% @doc EDoc tag scanning.

%% TODO: tag (or macro) for including the code of a function as <pre>-text.

-module(edoc_tags).

-export([tags/0, tags/1, tag_names/0, tag_parsers/0, scan_lines/2,
	 filter_tags/3, check_tags/4, parse_tags/3]).

-import(edoc_report, [report/4, warning/4, error/3]).

-include("edoc.hrl").


%% Tags are described by {Name, Parser, Flags}.
%%   Name = atom()
%%   Parser = text | xml | (Text,Line,Where) -> term()
%%   Flags = [Flag]
%%   Flag = module | function | package | overview | single
%%
%% Note that the pseudo-tag '@clear' is not listed here.
%% (Cf. the function 'filter_tags'.)

tags() ->
    [{author, fun parse_contact/3, [module,package,overview]},
     {copyright, text, [module,package,overview,single]},
     {deprecated, xml, [module,function,package,single]},
     {doc, xml,	[module,function,package,overview,single]},
     {'end', text, [module,footer,function,package,overview]},
     {equiv, fun parse_expr/3, [function,single]},
     {hidden, text, [module,function,single]},
     {private, text, [module,function,single]},
     {reference, xml, [module,footer,package,overview]},
     {see, fun parse_see/3, [module,function,package,overview]},
     {since, text, [module,function,package,overview,single]},
     {spec, fun parse_spec/3, [function,single]},
     {title, text, [overview,single]},
     {type, fun parse_type/3, [module,footer,function]},
     {version, text, [module,package,overview,single]}].

%% Selecting tags based on flags.
tags(Flag) ->
    [T || {T,_,Fs} <- tags(), lists:member(Flag, Fs)].

%% The set of known tags.
tag_names() ->
    [T || {T,_,_} <- tags()].

%% The pairs of tags and their parsers.
tag_parsers() ->
    [{T,F} || {T,F,_} <- tags()].


%% Scanning lines of comment text.

scan_lines(Ss, L) ->
    lists:reverse(scan_lines(Ss, L, [])).

scan_lines([S | Ss], L, As) ->
    scan_lines(S, Ss, L, As);
scan_lines([], _L, As) ->
    As.

%% Looking for a leading '@', skipping whitespace.

scan_lines([$\s | Cs], Ss, L, As) -> scan_lines(Cs, Ss, L, As);
scan_lines([$\t | Cs], Ss, L, As) -> scan_lines(Cs, Ss, L, As);
scan_lines([$@ | Cs], Ss, L, As) -> scan_tag(Cs, Ss, L, As, []);
scan_lines(_, Ss, L, As) -> scan_lines(Ss, L + 1, As).

%% Scanning chars following '@', accepting only nonempty valid names.
%% See edoc_lib:is_name/1 for details on what is a valid name. In tags
%% we also allow the initial letter to be uppercase or underscore.

scan_tag([C | Cs], Ss, L, As, Ts) when C >= $a, C =< $z ->
    scan_tag_1(Cs, Ss, L, As, [C | Ts]);
scan_tag([C | Cs], Ss, L, As, Ts) when C >= $A, C =< $Z ->
    scan_tag_1(Cs, Ss, L, As, [C | Ts]);
scan_tag([C | Cs], Ss, L, As, Ts) when C >= $\300, C =< $\377,
					 C =/= $\327, C =/= $\367 ->
    scan_tag_1(Cs, Ss, L, As, [C | Ts]);
scan_tag([$_ | Cs], Ss, L, As, Ts) ->
    scan_tag_1(Cs, Ss, L, As, [$_ | Ts]);
scan_tag(_Cs, Ss, L, As, _Ts) ->
    scan_lines(Ss, L + 1, As).    % not a valid name

scan_tag_1([C | Cs], Ss, L, As, Ts) when C >= $a, C =< $z ->
    scan_tag_1(Cs, Ss, L, As, [C | Ts]);
scan_tag_1([C | Cs], Ss, L, As, Ts) when C >= $A, C =< $Z ->
    scan_tag_1(Cs, Ss, L, As, [C | Ts]);
scan_tag_1([C | Cs], Ss, L, As, Ts) when C >= $0, C =< $9 ->
    scan_tag_1(Cs, Ss, L, As, [C | Ts]);
scan_tag_1([C | Cs], Ss, L, As, Ts) when C >= $\300, C =< $\377,
					 C =/= $\327, C =/= $\367 ->
    scan_tag_1(Cs, Ss, L, As, [C | Ts]);
scan_tag_1([$_ | Cs], Ss, L, As, Ts) ->
    scan_tag_1(Cs, Ss, L, As, [$_ | Ts]);
scan_tag_1(Cs, Ss, L, As, Ts) ->
    scan_tag_2(Cs, Ss, L, As, {Ts, L}).

%% Check that the tag is followed by whitespace or linebreak.

scan_tag_2([$\s | Cs], Ss, L, As, T) ->
    scan_tag_lines(Ss, T, [Cs], L + 1, As);
scan_tag_2([$\t | Cs], Ss, L, As, T) ->
    scan_tag_lines(Ss, T, [Cs], L + 1, As);
scan_tag_2([], Ss, L, As, T) ->
    scan_tag_lines(Ss, T, [[]], L + 1, As);
scan_tag_2(_, Ss, L, As, _T) ->
    scan_lines(Ss, L + 1, As).

%% Scanning lines after a tag is found.

scan_tag_lines([S | Ss], T, Ss1, L, As) ->
    scan_tag_lines(S, S, Ss, T, Ss1, L, As);
scan_tag_lines([], {Ts, L1}, Ss1, _L, As) ->
    [make_tag(Ts, L1, Ss1) | As].

%% Collecting tag text lines until end of comment or next tagged line.

scan_tag_lines([$\s | Cs], S, Ss, T, Ss1, L, As) ->
    scan_tag_lines(Cs, S, Ss, T, Ss1, L, As);
scan_tag_lines([$\t | Cs], S, Ss, T, Ss1, L, As) ->
    scan_tag_lines(Cs, S, Ss, T, Ss1, L, As);
scan_tag_lines([$@, C | _Cs], S, Ss, {Ts, L1}, Ss1, L, As)
  when C >= $a, C =< $z ->
    scan_lines(S, Ss, L, [make_tag(Ts, L1, Ss1) | As]);
scan_tag_lines([$@, C | _Cs], S, Ss, {Ts, L1}, Ss1, L, As)
  when C >= $A, C =< $Z ->
    scan_lines(S, Ss, L, [make_tag(Ts, L1, Ss1) | As]);
scan_tag_lines([$@, C | _Cs], S, Ss, {Ts, L1}, Ss1, L, As)
  when C >= $\300, C =< $\377, C =/= $\327, C =/= $\367 ->
    scan_lines(S, Ss, L, [make_tag(Ts, L1, Ss1) | As]);
scan_tag_lines(_Cs, S, Ss, T, Ss1, L, As) ->
    scan_tag_lines(Ss, T, [S | Ss1], L + 1, As).

make_tag(Cs, L, Ss) ->
    #tag{name = list_to_atom(lists:reverse(Cs)),
	 line = L,
	 data = append_lines(lists:reverse(Ss))}.

%% Flattening lines of text and inserting line breaks.

append_lines([L]) -> L;
append_lines([L | Ls]) -> L ++ [$\n | append_lines(Ls)];
append_lines([]) -> [].

%% Filtering out unknown tags.

filter_tags(Ts, Tags, Where) ->
    filter_tags(Ts, Tags, Where, []).

filter_tags([#tag{name = clear} | Ts], Tags, Where, _Ts1) ->
    filter_tags(Ts, Tags, Where);
filter_tags([#tag{name = N, line = L} = T | Ts], Tags, Where, Ts1) ->
    case sets:is_element(N, Tags) of
	true ->
	    filter_tags(Ts, Tags, Where, [T | Ts1]);
	false ->
	    warning(L, Where, "tag @~s not recognized.", [N]),
	    filter_tags(Ts, Tags, Where, Ts1)
    end;
filter_tags([], _, _, Ts) ->
    lists:reverse(Ts).

%% Check occurrances of tags.

check_tags(Ts, Allow, Single, Where) ->
    check_tags(Ts, Allow, Single, Where, false, sets:new()).

check_tags([#tag{name = T, line = L} | Ts], Allow, Single, Where, Error, Seen) ->
    case sets:is_element(T, Seen) of
	true ->
	    case sets:is_element(T, Single) of
		false ->
		    check_tags(Ts, Allow, Single, Where, Error, Seen);
		true ->
		    report(L, Where, "multiple @~s tag.", [T]),
		    check_tags(Ts, Allow, Single, Where, true, Seen)
	    end;
	false ->
	    Seen1 = sets:add_element(T, Seen),
	    case sets:is_element(T, Allow) of
		true ->
		    check_tags(Ts, Allow, Single, Where, Error, Seen1);
		false ->
		    report(L, Where, "tag @~s not allowed here.", [T]),
		    check_tags(Ts, Allow, Single, Where, true, Seen1)
	    end
    end;
check_tags([], _, _, _, Error, _) ->
    Error.


%% Parses tag contents for specific tags.

parse_tags(Ts, How, Where) ->
    parse_tags(Ts, How, Where, []).

parse_tags([#tag{name = Name} = T | Ts], How, Where, Ts1) ->
    case dict:fetch(Name, How) of
	F when function(F) ->
	    T1 = parse_tag(T, F, Where),
	    parse_tags(Ts, How, Where, [T1 | Ts1]);
	text ->
	    parse_tags(Ts, How, Where, [T | Ts1]);
	xml ->
	    T1 = parse_tag(T, fun parse_xml/3, Where),
	    parse_tags(Ts, How, Where, [T1 | Ts1])
    end;
parse_tags([], _How, _Where, Ts) ->
    lists:reverse(Ts).

parse_tag(T, F, Where) ->
    case catch {ok, F(T#tag.data, T#tag.line, Where)} of
	{ok, Data} ->
	    T#tag{data = Data};
	{error, L, Error} ->
	    error(L, Where, Error),
	    exit(error);
	{'EXIT', R} -> exit(R);
	Other -> throw(Other)
    end.

%% parser functions for the built-in content types. They also perform
%% some sanity checks on the results.

parse_xml(Data, Line, _Where) ->
    edoc_parse_xml:parse(edoc_wiki:expand(Data, Line), Line).

parse_see(Data, Line, _Where) ->
    edoc_parse_ref:parse_see(Data, Line).

parse_expr(Data, Line, _Where) ->
    edoc_parse_expr:parse(Data, Line).

parse_spec(Data, Line, {_, {F, A}} = _Where) ->
    Spec = edoc_parse_spec:parse(Data, Line),
    #t_spec{name = N, type = #t_fun{args = As}} = Spec,
    if length(As) /= A ->
	    throw_error(Line, "@spec arity does not match.");
       true ->
	    case N of
		undefined ->
		    Spec#t_spec{name = #t_name{module = [], name = F}};
		#t_name{module = [], name = F} ->
		    Spec;
		_ ->
		    throw_error(Line, "@spec name does not match.")
	    end
    end.

parse_contact(Data, Line, _Where) ->
    case edoc_parse_contact:parse(Data, Line) of
	{"", "", _URI} ->
	    throw_error(Line, "must specify name or e-mail.");
	Info ->
	    Info
    end.

parse_type(Data, Line, _Where) ->
    Def = edoc_parse_typedef:parse(Data, Line),
    {#t_typedef{name = #t_name{name = T}}, _} = Def,
    case edoc_types:is_predefined(T) of
	true ->
	    throw_error(Line, {"redefining built-in type '~w'.", [T]});
	false ->
	    Def
    end.

throw_error(L, D) ->
    throw({error, L, D}).
