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
%%% The Original Code is xmerl-0.6
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
%%% @private
%%% File:       xmerl_xml.erl
%%% Author       : Ulf Wiger <ulf.wiger@ericsson.com>
%%%                Richard Carlsson <richardc@csd.uu.se>
%%% Description  : Callback module for exporting 
%%%		   complete or simple forms to XML.
%%% 
%%% Modules used : xmerl_lib
%%%----------------------------------------------------------------------

-module(xmerl_xml).

-export(['#xml-inheritance#'/0]).

-export(['#root#'/4,
	 '#element#'/5,
	 '#text#'/1]).

-import(xmerl_lib, [markup/3, empty_tag/2, export_text/1]).

-include("xmerl.hrl").


'#xml-inheritance#'() -> [].


%% The '#text#' function is called for every text segment.

'#text#'(Text) ->
io:format("Text=~p~n",[Text]),
    export_text(Text).


%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.

'#root#'(Data, _Attrs, [], _E) ->
    ["<?xml version=\"1.0\"?>", Data].


%% The '#element#' function is the default handler for XML elements.

'#element#'(Tag, [], Attrs, _Parents, _E) ->
io:format("Empty Tag=~p~n",[Tag]),
    empty_tag(Tag, Attrs);
'#element#'(Tag, Data, Attrs, _Parents, _E) ->
io:format("Tag=~p~n",[Tag]),
    markup(Tag, Attrs, Data).
