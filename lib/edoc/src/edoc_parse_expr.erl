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

%% @doc EDoc Erlang expression parsing. For parsing things like the
%% content of <a href="overview-summary.html#ftag-equiv">`@equiv'</a>
%% tags.

-module(edoc_parse_expr).

-export([parse/2]).


%% @doc Parses an Erlang expression.

parse(S, L) ->
    case erl_scan:string(S ++ ".", L) of
	{ok, Ts, _} ->
	    case erl_parse:parse_exprs(Ts) of
		{ok, [Expr]} ->
		    Expr;
		{error, E} ->
		    error(E, L)
	    end;
	{error, E, _} ->
	    error(E, L)
    end.

error({L, M, D}, _L0) ->
    throw({error,L,{format_error,M,D}});
error(E, L) ->
    %% Just in case.
    throw({error,L,{"unknown error parsing expression: ~P.",[E,15]}}).
