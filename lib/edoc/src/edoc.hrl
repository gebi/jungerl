%% =====================================================================
%% Header file for EDoc
%% 
%% Copyright (C) 2001-2004 Richard Carlsson
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
%% =====================================================================

-define(APPLICATION, edoc).
-define(INFO_FILE, "edoc-info").
-define(PACKAGE_FILE, "package.edoc").
-define(OVERVIEW_FILE, "overview.edoc").
-define(PACKAGE_SUMMARY, "package-summary").
-define(DEFAULT_SOURCE_SUFFIX, ".erl").
-define(DEFAULT_FILE_SUFFIX, ".html").
-define(DEFAULT_DOCLET, edoc_doclet).
-define(DEFAULT_LAYOUT, edoc_layout).
-define(APP_DEFAULT, "http://www.erlang.org/edoc/doc").
-define(CURRENT_DIR, ".").
-define(SOURCE_DIR, "src").
-define(EBIN_DIR, "ebin").
-define(EDOC_DIR, "doc").

-include("../include/edoc_doclet.hrl").

%% Module information

-record(module, {name,		% = atom()
		 functions,	% = ordset({atom(), int()})
		 exports,	% = ordset({atom(), int()})
		 attributes,	% = ordset({atom(), term()})
		 records	% = [{atom(), [{atom(), term()}]}]
		}).

%% Environment for generating documentation data

-record(env, {module = [],
	      package = [],
	      root = "",
	      file_suffix,
	      package_summary,
	      apps,
	      modules,
	      packages,
	      app_default
	     }).

%% Simplified comment data

-record(comment, {line = 0, text}).

%% Module Entries (one per function, plus one for the module header)

-record(entry, {name, args = [], line = 0, export, data}).

%% Generic tag information

-record(tag, {name, line = 0, data}).

%% Type specification data structures

-record(t_spec, {name, type, defs=[]}).		% function specification

-record(t_typedef, {name, args, type,
		    defs=[]}).			% type declaration/definition

-record(t_def, {name, type}).			% local definition 'name = type'
-record(t_name, {app = [],			% app = [] if module = []
		 module=[],			% unqualified if module = []
		 name=[]}).

%% The following records all have 'a=[]' as their first field.
%% This is used for name annotations; in particular, the fun-argument
%% types of a function specification (t_spec) are often annotated with
%% the names of the corresponding formal parameters.

-define(t_ann(X), element(2, X)).
-define(set_t_ann(X, Y), setelement(2, X, Y)).

-record(t_var, {a=[], name=[]}).	% type variable

-record(t_type, {a=[], name, args = []}).	% abstract type 'name(...)'

-record(t_union, {a=[], types = []}).	% union type 't1|...|tN'

-record(t_fun, {a=[], args, range}).	% function '(t1,...,tN) -> range'

-record(t_tuple, {a=[], types = []}).	% tuple type '{t1,...,tN}'

-record(t_list, {a=[], type}).		% list type '[type]'

-record(t_nil, {a=[]}).			% empty-list constant '[]'

-record(t_atom, {a=[], val}).		% atom constant

-record(t_integer, {a=[], val}).	% integer constant

-record(t_float, {a=[], val}).		% floating-point constant
