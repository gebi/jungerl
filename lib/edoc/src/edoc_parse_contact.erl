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
%% @copyright 2003 Richard Carlsson
%% @author Richard Carlsson <richardc@csd.uu.se>
%% @see edoc
%% @end 
%% =====================================================================

%% @doc EDoc "contact information" parsing. This is the type of the
%% content in e.g.
%% <a href="overview-summary.html#mtag-author">`@author'</a> tags.

-module(edoc_parse_contact).

-export([parse/2]).

-record(info, {name = "", mail = "", uri = ""}).

parse(S, L) ->
    parse_name(S, L, #info{}, []).

%% The name is taken as the first non-whitespace-only string before,
%% between, or following the e-mail/URI sections. Subsequent text that
%% is not e/mail or URI is ignored.

parse_name([$< | Cs], L, I, As) ->
    case I#info.mail of
	"" ->
	    {Cs1, I1} = parse_mail(Cs, L, set_name(I, As), []),
	    parse_name(Cs1, L, I1, []);
	_ ->
	    error(L, {"multiple '<...>' sections."})
    end;
parse_name([$[ | Cs], L, I, As) ->
    case I#info.uri of
	"" ->
	    {Cs1, I1} = parse_uri(Cs, L, set_name(I, As), []),
	    parse_name(Cs1, L, I1, []);
	_ ->
	    error(L, {"multiple '[...]' sections."})
    end;
parse_name([$\n | Cs], L, I, As) ->
    parse_name(Cs, L + 1, I, [$\n | As]);
parse_name([C | Cs], L, I, As) ->
    parse_name(Cs, L, I, [C | As]);
parse_name([], _L, I, As) ->
    return(set_name(I, As)).

parse_uri([$] | Cs], _L, I, As) ->
    {Cs, I#info{uri = finalize(As)}};
parse_uri([$\n | Cs], L, I, As) ->
    parse_uri(Cs, L + 1, I, [$\n | As]);
parse_uri([C | Cs], L, I, As) ->
    parse_uri(Cs, L, I, [C | As]);
parse_uri([], L, _I, _As) ->
    error(L, {missing, $]}).

parse_mail([$> | Cs], _L, I, As) ->
    {Cs, I#info{mail = finalize(As)}};
parse_mail([$\n | Cs], L, I, As) ->
    parse_mail(Cs, L + 1, I, [$\n | As]);
parse_mail([C | Cs], L, I, As) ->
    parse_mail(Cs, L, I, [C | As]);
parse_mail([], L, _I, _As) ->
    error(L, {missing, $>}).

return(#info{name = Name, mail = Mail, uri = URI}) ->
    {Name, Mail, URI}.

set_name(I, As) ->
    case I#info.name of
	"" -> I#info{name = finalize(As)};
	_ -> I
    end.

finalize(As) ->
    edoc_lib:strip_space(lists:reverse(edoc_lib:strip_space(As))).

error(L, {missing, C}) ->
    error(L, {"missing '~c'.", [C]});
error(L, D) ->
    throw({error, L, D}).
