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
%%% The Original Code is xmerl-0.7
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
%%% File:       xmerl_sgml.erl
%%% Author       : Ulf Wiger <ulf.wiger@ericsson.com>
%%%                Richard Carlsson <richardc@csd.uu.se>
%%% Description  : Callback module for exporting XML to SGML.
%%% 
%%% Modules used : xmerl_lib
%%%----------------------------------------------------------------------

-module(xmerl_sgml).

-export(['#xml-inheritance#'/0]).

%% Note: we assume XML data, so all tags are lowercase!

-export(['#root#'/4,
	 '#element#'/5,
	 '#text#'/1]).

-import(xmerl_lib, [markup/3, find_attribute/2, export_text/1]).

-include("xmerl.hrl").


'#xml-inheritance#'() -> [].


%% The '#text#' function is called for every text segment.

'#text#'(Text) ->
    export_text(Text).


%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.

'#root#'(Data, Attrs, [], _E) -> 
    case find_attribute(header, Attrs) of
	{value, Hdr} ->
	    [Hdr, Data];
	false ->
	    Data
    end.


%% Note that SGML does not have the <Tag/> empty-element form.
%% Furthermore, for some element types, the end tag may be forbidden -
%% this can be handled by extending this module - see xmerl_otpsgml.erl
%% for an example. (By default, we always generate the end tag, to make
%% sure that the scope of a markup is not extended by mistake.)

'#element#'(Tag, Data, Attrs, _Parents, _E) ->
    markup(Tag, Attrs, Data).
