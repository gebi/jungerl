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

%% @doc EDoc object-reference parsing. Parses references to modules,
%% functions, types, etc.

-module(edoc_parse_ref).

-export([parse/2, parse_see/2]).

%% @doc Parses a reference to a module, function or type.

%% Valid forms are:
%%	atom/integer		    a local function
%%	atom()			    a local data type
%%	module			    a module
%%	module:atom/integer	    a function in a given module
%%	module:atom()		    a data type in a given module
%%	/app/			    an application
%%	/app/module		    a module in a given application
%%	/app/module:atom/integer    a function in a given app/module
%%	/app/module:atom()	    a data type in a given app/module
%% Module names can contain periods, i.e., be package-qualified.

parse(S, L) ->
    case edoc_scanner:string(S, L) of
	{ok, Ts, _} ->
	    case edoc_parser:parse_ref(Ts, L) of
		{ok, T} ->
		    T;
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
    throw({error,L,{"unknown error parsing reference: ~P.",[E,15]}}).

%% @doc Parses a @see-tag. These contain a reference optionally followed
%% by a period and an XHTML comment.

parse_see(S, L) ->
    {S1, S2} = edoc_lib:split_at_stop(S),
    N = edoc_lib:count($\n, S1),
    L1 = L + N,
    Text = edoc_wiki:expand(edoc_lib:strip_space(S2), L1),
    {parse(S1, L), edoc_parse_xml:parse(Text, L1)}.
