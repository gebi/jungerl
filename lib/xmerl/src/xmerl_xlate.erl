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
%%% The Original Code is xmerl-0.9
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%
%%%----------------------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%%----------------------------------------------------------------------
%%% File:       xmerl.erl
%%% Author       : Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description  : Wrapper to xmerl_scan, which reads an XML document
%%%                from disk (alt. consumes a string), processes it, and
%%%                exports is using the specified Callback module.
%%% 
%%% Modules used : file, xmerl, xmerl_scan
%%% 
%%%----------------------------------------------------------------------
-module(xmerl_xlate).
-vsn('0.6').
-date('00-09-22').
-author('ulf.wiger@ericsson.com').


-export([file/3,
	 string/3]).


-include("xmerl.hrl").

file(F, Title, Callback) ->
    case file:read_file(F) of
	{ok, Bin} ->
	    string(binary_to_list(Bin), Title, Callback);
	Error ->
	    Error
    end.

string(Str, Title, Callback) ->
    xmerl_scan:string(Str, [{hook_fun, fun hook/2, {Title, Callback}}]).


hook(E = #xmlElement{parents = []}, S) ->
    {Title, Callback} = xmerl_scan:hook_state(S),
    Data = xmerl:export(E, Callback, [{title, Title}]),
    {Data, S};
hook(X, S) ->
    {X, S}.

