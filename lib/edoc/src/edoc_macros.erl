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

%% @doc EDoc macro expansion

-module(edoc_macros).

-export([expand_tags/4, std_macros/1, check_defs/1]).

-import(edoc_report, [report/2, error/3]).

-include("edoc.hrl").

-define(DEFAULT_XML_EXPORT, xmerl_html).


std_macros(Env) ->
    (if Env#env.module == [] -> [];
	true -> [{module, atom_to_list(Env#env.module)}]
     end
     ++
     if Env#env.package == [] -> [];
	true -> [{package, atom_to_list(Env#env.package)}]
     end
     ++
     [{docRoot, Env#env.root},
      {date, fun date_macro/3},
      {link, fun link_macro/3},
      {time, fun time_macro/3},
      {type, fun type_macro/3}]).


%% Check well-formedness of list of macro definitions.

check_defs([{K, D} | Ds]) when atom(K), list(D) ->
    check_defs(Ds);
check_defs([X | _Ds]) ->
    report("bad macro definition: ~P.", [X, 10]),
    exit(error);
check_defs([]) ->
    ok.

%% Code for special macros should throw {error, Line, Reason} for error
%% reporting, where Reason and Line are passed to edoc_report:error(...)
%% together with the file name etc. The expanded text must be flat!

date_macro(_S, _Line, _Env) ->
    {Y,M,D} = date(),
    Ms = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
	  "Oct", "Nov", "Dec"],
    io_lib:fwrite("~s ~w ~w",[lists:nth(M, Ms),D,Y]).

time_macro(_S, _Line, _Env) ->
    {H,M,Sec} = time(),
    io_lib:fwrite("~w:~w:~w",[H,M,Sec]).

link_macro(S, Line, Env) ->
    {S1, S2} = edoc_lib:split_at_stop(S),
    Ref = edoc_parse_ref:parse(S1, Line),
    URI = edoc_refs:get_uri(Ref, Env),
    Txt = if S2 == [] -> "<code>" ++ S1 ++ "</code>";
	     true -> S2
	  end,
    Target = case edoc_refs:is_top(Ref, Env) of
		 true -> " target=\"_top\""; % note the initial space
		 false -> ""
	     end,
    lists:flatten(io_lib:fwrite("<a href=\"~s\"~s>~s</a>",
				[URI, Target, Txt])).

type_macro(S, Line, Env) ->
    S1 = "t()=" ++ S,
    Def = edoc_parse_typedef:parse(S1, Line),
    {#t_typedef{type = T}, _} = Def,
    Txt = edoc_layout:type(edoc_data:type(T, Env)),
    lists:flatten(io_lib:fwrite("<code>~s</code>", [Txt])).


%% Expand inline macros in tag content.

expand_tags([#tag{data = Cs, line = L} = T | Ts], Defs, Env, Where) ->
    [T#tag{data = expand_tag(Cs, L, Defs, Env, Where)}
     | expand_tags(Ts, Defs, Env, Where)];
expand_tags([T | Ts], Defs, Env, Where) ->
    [T | expand_tags(Ts, Defs, Env, Where)];
expand_tags([], _, _, _) ->
    [].

expand_tag(Cs, L, Defs, Env, Where) ->
    case catch {ok, expand(Cs, L, Defs, Env)} of
 	{ok, Cs1} ->
	    lists:reverse(Cs1);
 	{'EXIT', R} ->
	    exit(R);
	{error, L1, Error} ->
	    error(L1, Where, Error),
	    exit(error);
	Other ->
	    throw(Other)
    end.

%% Expand macros in arbitrary lines of text.
%% The result is in reverse order.

expand(Cs, L, Defs, Env) ->
    expand(Cs, L, Defs, Env, sets:new(), []).

%% Inline macro syntax: "{@name content}"
%%   where 'content' is optional, and separated from 'name' by one or
%%   more whitespace characters. The content is bound to the '{@?}'
%%   parameter variable, and the macro definition is expanded and
%%   substituted for the call. Recursion is detected and reported as an
%%   error, since there are (currently) no control flow operators.
%% Escape sequences:
%%   "@{" -> "{"
%%   "@}" -> "}"
%%   "@@" -> "@"

expand([$@, $@ | Cs], L, Defs, Env, Set, As) ->
    expand(Cs, L, Defs, Env, Set, [$@ | As]);
expand([$@, ${ | Cs], L, Defs, Env, Set, As) ->
    expand(Cs, L, Defs, Env, Set, [${ | As]);
expand([$@, $} | Cs], L, Defs, Env, Set, As) ->
    expand(Cs, L, Defs, Env, Set, [$} | As]);
expand([${, $@ | Cs], L, Defs, Env, Set, As) ->
    expand_macro(Cs, L, Defs, Env, Set, As);
expand([$\n = C | Cs], L, Defs, Env, Set, As) ->
    expand(Cs, L + 1, Defs, Env, Set, [C | As]);
expand([C | Cs], L, Defs, Env, Set, As) ->
    expand(Cs, L, Defs, Env, Set, [C | As]);
expand([], _, _, _, _, As) ->
    As.

expand_macro(Cs, L, Defs, Env, Set, As) ->
    {M, Cs1} = macro_name(Cs, L),
    {Arg, Cs2, L1} = macro_content(Cs1, L, Defs),
    As1 = expand_macro_def(M, Arg, L, Defs, Env, Set, As),
    expand(Cs2, L1, Defs, Env, Set, As1).

%% The macro argument (the "content") is expanded in the environment of
%% the call, and the result is bound to the '{@?}' parameter. The result
%% of the macro expansion is then expanded again. This allows macro
%% definitions to contain calls to other macros, avoids name capture of
%% '{@?}', and makes it easier to write handler functions for special
%% macros such as '{@link ...}', since the argument is already expanded.

expand_macro_def(M, Arg, L, Defs, Env, Set, As) ->
    case sets:is_element(M, Set) of
	true ->
	    throw_error(L, {"recursive macro expansion of {@~s}.",
			    [M]});
	false ->
	    Set1 = sets:add_element(M, Set),
	    Arg1 = lists:reverse(expand(Arg, L, Defs, Env, Set, [])),
	    Defs1 = dict:store('?', Arg1, Defs),
	    case dict:find(M, Defs) of
		{ok, Def} ->
		    Txt = if function(Def) ->
				  Def(Arg1, L, Env);
			     list(Def) ->
				  Def
			  end,
		    expand(Txt, L, Defs1, Env, Set1, As);
		error ->
		    throw_error(L, {"undefined macro {@~s}.", [M]})
	    end
    end.

%% The macro name ends at the first whitespace or '}' character.  The
%% content, if any, starts at the next non-whitespace character.

%% See edoc_tags:scan_tag/is_name/1 for details on what is a valid
%% name. In macro names we also allow '?' as the initial character.

macro_name(Cs, L) ->
    macro_name(Cs, [], L).

macro_name([C | Cs], As, L) when C >= $a, C =< $z ->
    macro_name_1(Cs, [C | As], L);
macro_name([C | Cs], As, L) when C >= $A, C =< $Z ->
    macro_name_1(Cs, [C | As], L);
macro_name([C | Cs], As, L) when C >= $\300, C =< $\377,
				 C =/= $\327, C =/= $\367 ->
    macro_name_1(Cs, [C | As], L);
macro_name([$_ | Cs], As, L) ->
    macro_name_1(Cs, [$_ | As], L);
macro_name([$? | Cs], As, L) ->
    macro_name_1(Cs, [$? | As], L);
macro_name([$\s | _Cs], _As, L) ->
    throw_error(L, macro_name);
macro_name([$\t | _Cs], _As, L) ->
    throw_error(L, macro_name);
macro_name([C | _Cs], As, L) ->
    throw_error(L, {macro_name, [C | As]});
macro_name([], _As, L) ->
    throw_error(L, macro_name).

macro_name_1([C | Cs], As, L) when C >= $a, C =< $z ->
    macro_name_1(Cs, [C | As], L);
macro_name_1([C | Cs], As, L) when C >= $A, C =< $Z ->
    macro_name_1(Cs, [C | As], L);
macro_name_1([C | Cs], As, L) when C >= $0, C =< $9 ->
    macro_name_1(Cs, [C | As], L);
macro_name_1([C | Cs], As, L) when C >= $\300, C =< $\377,
				   C =/= $\327, C =/= $\367 ->
    macro_name_1(Cs, [C | As], L);
macro_name_1([$_ | Cs], As, L) ->
    macro_name_1(Cs, [$_ | As], L);
macro_name_1([$\s | _] = Cs, As, _L) ->
    macro_name_2(Cs, As);
macro_name_1([$\t | _] = Cs, As, _L) ->
    macro_name_2(Cs, As);
macro_name_1([$\n | _] = Cs, As, _L) ->
    macro_name_2(Cs, As);
macro_name_1([$} | _] = Cs, As, _L) ->
    macro_name_2(Cs, As);
macro_name_1([C | _Cs], As, L) ->
    throw_error(L, {macro_name, [C | As]});
macro_name_1([], _As, L) ->
    throw_error(L, unterminated_macro).

macro_name_2(Cs, As) ->
    {list_to_atom(lists:reverse(As)), edoc_lib:strip_space(Cs)}.

%% The macro content ends at the first non-escaped '}' character that is
%% not balanced by a corresponding non-escaped '{@' sequence.
%% Escape sequences are those defined above.

macro_content(Cs, L, Defs) ->
    %% If there is an error, we report the start line, not the end line.
    case catch {ok, macro_content(Cs, [], L, 0, Defs)} of
	{ok, X} ->
	    X;
	{'EXIT', R} ->
	    exit(R);
	'end' ->
	    throw_error(L, unterminated_macro);
	Other ->
	    throw(Other)
    end.

macro_content([$@, $@ | Cs], As, L, N, Defs) ->
    macro_content(Cs, [$@, $@ | As], L, N, Defs);  % escaped '@'
macro_content([$@, $} | Cs], As, L, N, Defs) ->
    macro_content(Cs, [$}, $@ | As], L, N, Defs);  % escaped '}'
macro_content([$@, ${ | Cs], As, L, N, Defs) ->
    macro_content(Cs, [${, $@ | As], L, N, Defs);  % escaped '{'
macro_content([${, $@ | Cs], As, L, N, Defs) ->
    macro_content(Cs, [$@, ${ | As], L, N + 1, Defs);
macro_content([$} | Cs], As, L, 0, _Defs) ->
    {lists:reverse(As), Cs, L};
macro_content([$} | Cs], As, L, N, Defs) ->
    macro_content(Cs, [$} | As], L, N - 1, Defs);
macro_content([$\n = C | Cs], As, L, N, Defs) ->
    macro_content(Cs, [C | As], L + 1, N, Defs);
macro_content([C | Cs], As, L, N, Defs) ->
    macro_content(Cs, [C | As], L, N, Defs);
macro_content([], _As, _L, _N, _Defs) ->
    throw('end').

throw_error(L, unterminated_macro) ->
    throw_error(L, {"unexpected end of macro.", []});
throw_error(L, macro_name) ->
    throw_error(L, {"missing macro name.", []});
throw_error(L, {macro_name, S}) ->
    throw_error(L, {"bad macro name: '@~s...'.", [lists:reverse(S)]});
throw_error(L, D) ->
    throw({error, L, D}).
