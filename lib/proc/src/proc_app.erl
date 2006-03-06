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
%%% The Original Code is proc-1.0
%%%
%%% The Initial Developer of the Original Code is Ericsson AB
%%% Portions created by Ericsson are Copyright (C), 2006, Ericsson AB
%%% All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.

%%%-------------------------------------------------------------------
%%% File    : proc_app.erl
%%% Author  : Ulf Wiger <etxuwig@ws12858>
%%% Description : Application callback
%%%
%%% Created :  6 Mar 2006 by Ulf Wiger <etxuwig@ws12858>
%%%-------------------------------------------------------------------
-module(proc_app).

-export([start/2,
	 start_phase/3,
	 prep_stop/1,
	 stop/1,
	 config_change/3
	]).


start(_Type, []) ->
    proc_super:start_link(top).

start_phase(_Phase, _StartType, _PhaseArgs) ->
    {error, not_supported}.

prep_stop(State) ->
    State.

stop(_State) ->
    ok.

config_change(_Changed, _New, _Removed) ->
    ok.
