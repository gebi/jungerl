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
