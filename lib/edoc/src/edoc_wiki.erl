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

%% @doc EDoc wiki expansion.

%% Whatever happens in this module, it must interact nicely with the
%% following XML-parsing. E.g. auto-detecting paragraph breaks would be
%% nice, but could be tricky to get right.
%%
%% The central reasoning for the code-quoting goes like this: I don't
%% want to have special escape characters within the quotes (like
%% backslash in C), to allow quoting of the quote characters themselves.
%% I also don't want to use the "`" character both for opening and
%% closing quotes. Therefore, you can either use `...' - and then you
%% cannot use the "'" character without ending the quote - or you can
%% use ``...'' - which allows single but not double "'" characters
%% within the quote. Whitespace is automatically removed from the
%% beginning and the end of the quoted strings; this allows you to write
%% things like "`` 'foo@bar' ''". Text that contains "''" has to be
%% written within <code>...</code>.
%%
%% To produce a single "`" character without starting a quote, write
%% "`'" (no space between "`" and "'").
%%
%% For verbatim/preformatted text, the ```...'''-quotes expand to
%% "<pre><![CDATA[...]]></pre>". The indentation at the start of the
%% quoted string is preserved; whitespace is stripped only at the end.
%% Whole leading lines of whitespace are however skipped.

-module(edoc_wiki).

-export([expand/2]).

-include("edoc.hrl").


%% Expand wiki stuff in arbitrary text.

expand(Cs, L) ->
    lists:reverse(expand(Cs, L, [])).

%% Interestingly, the reverse of "code" is "edoc". :-)

expand([$`, $' | Cs], L, As) ->
    expand(Cs, L, [$` | As]);    % produce "`" - don't start a new quote
expand([$`, $`, $` | Cs], L, As) ->
    %% If this is the first thing on the line, compensate for the
    %% indentation, unless we had to skip one or more empty lines.
    {Cs1, Skipped} = strip_empty_lines(Cs),    % avoid vertical space
    N = case Skipped of
	    true -> 0;
	    false ->
		{As1, _} = edoc_lib:split_at(As, $\n),
		case edoc_lib:is_space(As1) of
		    true -> 3 + length(As1);
		    false -> 2    % nice default - usually right.
		end
	end,
    Ss = lists:duplicate(N, $\s),
    expand_triple(Cs1, L, Ss ++ "[ATADC[!<>erp<" ++ As);
expand([$`, $` | Cs], L, As) ->
    expand_double(edoc_lib:strip_space(Cs), L, ">edoc<" ++ As);
expand([$` | Cs], L, As) ->
    expand_single(edoc_lib:strip_space(Cs), L, ">edoc<" ++ As);
expand([$\n = C | Cs], L, As) ->
    expand(Cs, L + 1, [C | As]);
expand([C | Cs], L, As) ->
    expand(Cs, L, [C | As]);
expand([], _, As) ->
    As.

expand_single(Cs, L, As) ->
    expand_single(Cs, L, As, L).

expand_single([$' | Cs], L, As, _L0) ->
    expand(Cs, L, ">edoc/<" ++ edoc_lib:strip_space(As));
expand_single([$< | Cs], L, As, L0) ->
    expand_single(Cs, L, ";tl&" ++ As, L0);
expand_single([$> | Cs], L, As, L0) ->
    expand_single(Cs, L, ";tg&" ++ As, L0);
expand_single([$& | Cs], L, As, L0) ->
    expand_single(Cs, L, ";pma&" ++ As, L0);
expand_single([$\n = C | Cs], L, As, L0) ->
    expand_single(Cs, L + 1, [C | As], L0);
expand_single([C | Cs], L, As, L0) ->
    expand_single(Cs, L, [C | As], L0);
expand_single([], L, _, L0) ->
    throw_error(L0, {"`-quote ended unexpectedly at line ~w.", [L]}).


expand_double(Cs, L, As) ->
    expand_double(Cs, L, As, L).

expand_double([$', $' | Cs], L, As, _L0) ->
    expand(Cs, L, ">edoc/<" ++ edoc_lib:strip_space(As));
expand_double([$< | Cs], L, As, L0) ->
    expand_double(Cs, L, ";tl&" ++ As, L0);
expand_double([$> | Cs], L, As, L0) ->
    expand_double(Cs, L, ";tg&" ++ As, L0);
expand_double([$& | Cs], L, As, L0) ->
    expand_double(Cs, L, ";pma&" ++ As, L0);
expand_double([$\n = C | Cs], L, As, L0) ->
    expand_double(Cs, L + 1, [C | As], L0);
expand_double([C | Cs], L, As, L0) ->
    expand_double(Cs, L, [C | As], L0);
expand_double([], L, _, L0) ->
    throw_error(L0, {"``-quote ended unexpectedly at line ~w.", [L]}).


expand_triple(Cs, L, As) ->
    expand_triple(Cs, L, As, L).

expand_triple([$', $', $' | Cs], L, As, _L0) ->
    expand(Cs, L, ">erp/<>]]" ++ edoc_lib:strip_space(As));
expand_triple([$], $], $> | Cs], L, As, L0) ->
    expand_triple(Cs, L, ";tg&]]" ++ As, L0);
expand_triple([$\n = C | Cs], L, As, L0) ->
    expand_triple(Cs, L + 1, [C | As], L0);
expand_triple([C | Cs], L, As, L0) ->
    expand_triple(Cs, L, [C | As], L0);
expand_triple([], L, _, L0) ->
    throw_error(L0, {"```-quote ended unexpectedly at line ~w.", [L]}).


strip_empty_lines(Cs) ->
    strip_empty_lines(Cs, false).

strip_empty_lines(Cs, Skipped) ->
    {Cs1, Cs2} = edoc_lib:split_at(Cs, $\n),
    case edoc_lib:is_space(Cs1) of
	true ->
	    strip_empty_lines(Cs2, true);
	false ->
	    {Cs, Skipped}
    end.


throw_error(L, D) ->
    throw({error, L, D}).
