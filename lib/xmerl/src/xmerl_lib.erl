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
%%% File:       xmerl_lib.erl
%%% Author       : Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description  : Utility module for handling XML trees.
%%%----------------------------------------------------------------------

-module(xmerl_lib).

-export([export_text/1, export_attribute/1, markup/2, markup/3,
	 start_tag/1, start_tag/2, end_tag/1, empty_tag/1,
	 empty_tag/2,is_empty_data/1, find_attribute/2,
	 remove_whitespace/1]).

-include("xmerl.hrl").


%% Flatten text and escape special characters `<' and `&'.

export_text(T) ->
    export_text(T, []).

export_text([$< | T], Cont) ->
    "&lt;" ++ export_text(T, Cont);
export_text([$& | T], Cont) ->
    "&amp;" ++ export_text(T, Cont);
export_text([C | T], Cont) when integer(C) ->
    [C | export_text(T, Cont)];
export_text([T | T1], Cont) when list(T) ->
    export_text(T, [T1 | Cont]);
export_text([], [T | Cont]) ->
    export_text(T, Cont);
export_text([], []) ->
    [].


%% Flatten attribute text and escape characters `"', `<' and `&'. (Note
%% that single-quote characters are not escaped; the markup-generating
%% functions (`start_tag', `end_tag', ...) always use `"' to delimit the
%% attribute values.)

export_attribute(T) ->
    export_attribute(T, []).

export_attribute([$< | T], Cont) ->
    "&lt;" ++ export_attribute(T, Cont);
export_attribute([$& | T], Cont) ->
    "&amp;" ++ export_attribute(T, Cont);
export_attribute([$" | T], Cont) ->
    "&quot;" ++ export_attribute(T, Cont);
export_attribute([C | T], Cont) when integer(C) ->
    [C | export_attribute(T, Cont)];
export_attribute([T | T1], Cont) when list(T) ->
    export_attribute(T, [T1 | Cont]);
export_attribute([], [T | Cont]) ->
    export_attribute(T, Cont);
export_attribute([], []) ->
    [].


%% Looking up an attribute value

find_attribute(Name, Attrs) ->
    case lists:keysearch(Name, #xmlAttribute.name, Attrs) of
	{value, #xmlAttribute{value = V}} ->
	    {value, V};
	false ->
	    false
    end.


markup(Tag, Data) ->
    markup(Tag, [], Data).

markup(Tag, Attrs, []) ->
    empty_tag(Tag, Attrs);
markup(Tag, Attrs, Data) ->
    [start_tag(Tag, Attrs), Data, end_tag(Tag)].

start_tag(TagStr) ->
    start_tag(TagStr, []).

start_tag(Tag, Attrs) when atom(Tag) ->
    start_tag(atom_to_list(Tag), Attrs);
start_tag(TagStr, []) ->
    ["<", TagStr, ">"];
start_tag(TagStr, Attrs) ->
    ["<", TagStr, attributes(Attrs), ">"].

empty_tag(Tag) ->
    empty_tag(Tag, []).

empty_tag(Tag, Attrs) when atom(Tag) ->
    empty_tag(atom_to_list(Tag), Attrs);
empty_tag(TagStr, []) ->
    ["<", TagStr, "/>"];
empty_tag(TagStr, Attrs) ->
    ["<", TagStr, attributes(Attrs), "/>"].

end_tag(Tag) when atom(Tag) ->
    end_tag(atom_to_list(Tag));
end_tag(TagStr) ->
    ["</", TagStr, ">"].

attributes(Attrs) ->
    [attr_string(A) || A <- Attrs].

attr_string(#xmlAttribute{name = K, value = V}) ->
    [" ", atom_to_list(K), "=\"", value_string(V), "\""].

value_string(I) when integer(I) ->
    integer_to_list(I);
value_string(A) when atom(A) ->
    atom_to_list(A);
value_string(Term) ->
    case io_lib:deep_char_list(Term) of
	true ->
	    export_attribute(Term);
	false ->
	    io_lib:format("~p", [Term])
    end.


is_empty_data([]) ->
    true;
is_empty_data([X | Xs]) ->
    case is_empty_data(X) of
	false ->
	    false;
	true ->
	    is_empty_data(Xs)
    end;
is_empty_data(_) ->
    false.


%% Removing normalised whitespace-only text segments.

remove_whitespace([#xmlText{value = " "} | Data]) ->
    remove_whitespace(Data);
remove_whitespace([E = #xmlElement{content = Content} | Data]) ->
    [E#xmlElement{content = remove_whitespace(Content)}
     | remove_whitespace(Data)];
remove_whitespace([Other | Data]) ->
    [Other | remove_whitespace(Data)];
remove_whitespace([]) ->
    [].
