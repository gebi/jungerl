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

%% @doc EDoc XML parsing. Uses {@link //xmerl. XMerL}.

-module(edoc_parse_xml).

-export([parse/2]).

-include("xmerl.hrl").


%% Parsing a piece of text as XML. Note that the parent and position
%% information in the returned tree will not be correct.

parse(Text, Line) ->
    Text1 = "<doc>" ++ Text ++ "</doc>",
    case catch {ok, xmerl_scan:string(Text1, [{line, Line}])} of
	{ok, {E, _}} ->
	    E#xmlElement.content;
	{'EXIT', {fatal, {Reason, L, _C}}} ->
	    error(L, {"XML parse error: ~p.", [Reason]});
	{'EXIT', Reason} ->
	    error(Line, {"error in XML parser: ~P.", [Reason, 10]});
	Other ->
	    error(Line, {"nocatch in XML parser: ~P.", [Other, 10]})
    end.

error(L, D) ->
    throw({error, L, D}).
