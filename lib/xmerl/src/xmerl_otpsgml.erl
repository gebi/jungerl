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
%%% File:       xmerl_otpsgml.erl
%%% Author       : Ulf Wiger <ulf.wiger@ericsson.com>
%%%                Richard Carlsson <richardc@csd.uu.se>
%%% Description  : Callback module for exporting XHTML to OTP-SGML.
%%% 
%%% Modules used : xmerl_lib
%%%----------------------------------------------------------------------

-module(xmerl_otpsgml).

-export(['#xml-inheritance#'/0]).

%% Note: we assume XML data, so all tags are lowercase!

-export(['#root#'/4,
	 '#element#'/5,
	 '#text#'/1,
	 p/4]).

-import(xmerl_lib, [markup/3, start_tag/2, is_empty_data/1,
		    export_text/1]).

-include("xmerl.hrl").


'#xml-inheritance#'() -> [xmerl_sgml].


%% The '#text#' function is called for every text segment.

'#text#'(Text) ->
    export_text(Text).


%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.

'#root#'(Data, _Attrs, [], _E) -> 
    ["<!doctype erlref PUBLIC \"-//Stork//DTD erlref//EN\">\n",Data].


%% Note that SGML does not have the <Tag/> empty-element form.
%% Furthermore, for some element types, the end tag is forbidden. (In
%% all other cases, we always generate the end tag, to make sure that
%% the scope of a markup is not extended by mistake.)

'#element#'(Tag, Data, Attrs, _Parents, _E) ->
%    io:format("parents:\n~p\n",[_Parents]),
    case convert_tag(Tag,Attrs) of
	{false,NewTag,NewAttrs} ->
	    markup(NewTag, NewAttrs, Data);
	{true,NewTag,NewAttrs} ->
	    [start_tag(NewTag, NewAttrs), Data]
    end.


%% HTML tags with special handling

p(Data, Attrs, _Parents, _E) ->
    %% In general, we cannot drop the end tag for paragraph elements;
    %% that is only allowed if we know that it is immediately followed
    %% by some other block-level tag.
    case is_empty_data(Data) of
	true ->
	    %% Paragraph elements should never be completely empty.
	    markup(p, Attrs, "\s");
	false ->
	    markup(p, Attrs, Data)
    end.


%% Utility functions

convert_tag(code,Attrs) -> convert_tag(c,Attrs);
convert_tag(strong,Attrs) -> convert_tag(em,Attrs);
convert_tag(b,Attrs) -> convert_tag(em,Attrs);
convert_tag(underline,Attrs) -> convert_tag(em,Attrs); % what is underline in sgml???
convert_tag(dl,Attrs) -> convert_tag(taglist,Attrs);
convert_tag(dt,Attrs) -> convert_tag(tag,Attrs);
convert_tag(dd,Attrs) -> convert_tag(item,Attrs);
convert_tag(ul,Attrs) -> convert_tag(list,Attrs);
convert_tag(li,Attrs) -> convert_tag(item,Attrs);
convert_tag(tt,Attrs) -> convert_tag(c,Attrs);
convert_tag(a, Attrs) -> convert_tag(seealso,convert_seealso_attrs(Attrs));
convert_tag(Tag,Attrs) -> {forbid_end(Tag),Tag,Attrs}.

convert_seealso_attrs([#xmlAttribute{name = href, value = V} = A|Rest]) ->
    [A#xmlAttribute{name=marker,value=V}|convert_seealso_attrs(Rest)];
convert_seealso_attrs([#xmlAttribute{name = K}|Rest]) ->
    io:format("Warning: ignoring attribute \'~p\' for tag \'a\'\n",[K]),
    convert_seealso_attrs(Rest);
convert_seealso_attrs([]) ->
    [].

forbid_end(area) -> true; 
forbid_end(base) -> true; 
forbid_end(basefont) -> true; 
forbid_end(br) -> true; 
forbid_end(col) -> true; 
forbid_end(frame) -> true; 
forbid_end(hr) -> true; 
forbid_end(img) -> true; 
forbid_end(input) -> true; 
forbid_end(isindex) -> true; 
forbid_end(link) -> true; 
forbid_end(marker) -> true; 
forbid_end(meta) -> true; 
forbid_end(param) -> true; 
forbid_end(_) -> false.
