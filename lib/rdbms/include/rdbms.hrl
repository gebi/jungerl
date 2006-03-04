%%%
%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.0, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.erlang.org/license/EPL1_0.txt
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is rdbms-1.1.
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.

%%%----------------------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%%----------------------------------------------------------------------
%%% File:	rdbms.hrl
%%% Author       : Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description  : Header file definitions for RBMS
%%%----------------------------------------------------------------------

%% rdbms_obj a slightly more powerful data structure than record
%% Example: 
%%	#rdbms_obj{name = person,
%%		   attributes = [{name, string, "Ulf Wiger"},
%%				 {title, atom, 'designer'},
%%				 {tfn, {tuples,2,1}, [{wk,"+46-8-719 8195"},
%%						      {mob,"+46-8-519 8195"}
%%						     ]},
%%				 ...]}.
%%
-record(rdbms_obj, {name,		% name of the struct
		    attributes = []}).	% [{AttrName, Type, Value}]



%%% Index record, unordered index
-record(ix, {key,
             oid}).

%%% Index record, ordered index
-record(ord_ix, {key,         % {IxValue, Oid}
                 dummy = 1}).

%%% Index record, weighted (ordered) index
-record(w_ix, {key,           % {IxValue, Weight, Oid}
	       dummy = 1}).

%% Index meta-data
-record(index, {pos,   % {Attr, Tag} | Attr::integer()
                m_f,   % {Module, Function}
                arg,   % term()
                %% is_ordered = false,
		type = bag,   % ordered | bag | weighted | set
                table_name,
                tab_opts = [],
                options = []}).

-record(verify, {is_schema_trans,
		 tab_property,
		 attr_property,
		 global_property,
		 table_info}).

-record(attribute, {name,
		    tab,
		    value,
		    type}).

%%%-define(NULL, '#.[].#').  % this is my definition of a null value.
-define(NULL, undefined).  % this is my bowing to reality


-ifdef(debug).
-define(dbg(Fmt, Args), io:format("~p-~p: " ++ Fmt, [?MODULE,?LINE|Args])).
-else.
-define(dbg(Fmt,Args), no_debug).
-endif.

