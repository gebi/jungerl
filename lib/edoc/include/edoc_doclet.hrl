%% =====================================================================
%% Header file for EDoc doclet modules.
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

-define(NO_APP, []).

%% Context for doclets

-record(context, {dir = "",
		  env,
		  opts = []}).

%% Doclet commands

-record(doclet_gen, {sources = [],
		     app = ?NO_APP,
		     packages = [],
		     modules = [],
		     filemap
		    }).

-record(doclet_toc, {paths,
		     indir
		    }).
