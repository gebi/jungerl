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
%%% The Original Code is xmerl-0.14
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
%%% Description  : Functions to export simple and complete XML forms
%%% 
%%% Modules used : lists, <callbacks specified via arguments>
%%% 
%%%----------------------------------------------------------------------

-module(xmerl).

-export([export/2, 
	 export/3,
	 export_simple/2, 
	 export_simple/3]).

-export([callbacks/1,
	 export_element/3]).

-include("xmerl.hrl").


%% export(Data, Callback) -> ExportedFormat.
%%
%% This function operates on #xmlElement{} records.
%%
export(Data, Callback) ->
    export(Data, Callback, []).

export(Data, Callback, RootAttributes) when atom(Callback) ->
    export1(Data, callbacks(Callback), RootAttributes);
export(Data, Callbacks, RootAttributes) when list(Callbacks) ->
    export1(Data, Callbacks, RootAttributes).

export1(Data, Callbacks, RootAttributes) ->
    Result = do_export(Data, Callbacks),
    Root = #xmlElement{name = '#root#',
		       pos = 1,
		       parents = [],
		       attributes = full_attributes(RootAttributes, 
						    [{'#root#',1}])},
    Args = [Result,Root#xmlElement.attributes, [], Root],
    tagdef('#root#',1,[],Args,Callbacks,Result).

%% export_simple(Data, Callback) -> ExportedFormat.
%%
%% This function operates on simple XML structures - {Tag, Attr, Content}.
%%
export_simple(Data, Callback) ->
    export_simple(Data, Callback, []).

export_simple(Data, Callback, RootAttributes) when list(Data)  ->
    ?dbg("Data = ~p~n", [Data]),
    Expanded = expand_content(Data, []),
    export(Expanded, Callback, RootAttributes);
export_simple(Data, Callback, RootAttributes)  ->
    ?dbg("Data = ~p~n", [Data]),
    Expanded = expand_element(Data, 1, []),
    export(Expanded, Callback, RootAttributes).

export_element(#xmlText{value = Text},CBs,CBArgs) ->
    apply_text_cb(CBs, Text);
export_element(#xmlPI{},CBs,CBArgs) ->
    [];
export_element(#xmlComment{},CBs,CBArgs) ->
    [];
export_element(#xmlDecl{},CBs,CBArgs) ->
    [];
export_element(#xmlElement{name = Tag,
			   pos = Pos,
			   parents = Parents,
			   attributes = Attributes,
			   content = Content},CBs,CBArgs) ->
    Args = [Content, Attributes]++CBArgs,
    tagdef(Tag,Pos,Parents,Args,CBs,Content).

do_export(#xmlText{value = Text}, CBs) ->
    apply_text_cb(CBs, Text);
do_export(#xmlPI{}, CBs) ->
    [];
do_export(#xmlComment{}, CBs) ->
    [];
do_export(E = #xmlElement{name = Tag,
			  pos = Pos,
			  attributes = Attributes,
			  parents = Parents,
			  content = Content}, CBs) ->
    Data = export_content(Content, CBs),
    Args = [Data, Attributes, Parents, E],
    tagdef(Tag,Pos,Parents,Args,CBs,Data);
do_export(Content, CBs) when list(Content) ->
    export_content(Content, CBs).

export_content([#xmlPI{}|T], CBs) ->
    export_content(T, CBs);
export_content([#xmlComment{}|T], CBs) ->
    export_content(T, CBs);
export_content([E = #xmlText{}|T], CBs) ->
    [do_export(E, CBs)|export_content(T, CBs)];
export_content([E = #xmlElement{}|T], CBs) ->
    [do_export(E, CBs)|export_content(T, CBs)];
export_content([], CBs) ->
    [].

tagdef(Tag,Pos,Parents,Args,CBs,Data) ->
    case apply_tag_cb(CBs, Tag, Args) of
	{'#xml-alias#', NewTag} ->
	    tagdef(NewTag,Pos,Parents,Args,CBs,Data);
	{'#xml-redefine#', NewData} when tuple(NewData) ->
	    do_export(maybe_expand(NewData, Pos, Parents), CBs);
	Other ->
	    Other
    end.

maybe_expand(X = [#xmlElement{}|_], Pos, Parents) ->	X;
maybe_expand(X = #xmlElement{}, Pos, Parents) ->	X;
maybe_expand(X = #xmlPI{}, Pos, Parents) ->		X;
maybe_expand(X = #xmlComment{}, Pos, Parents) ->	X;
maybe_expand(X = #xmlText{}, Pos, Parents) ->		X;
maybe_expand(X, Pos, Parents) when list(X) ->
    expand_content(X, Parents);
maybe_expand(X, Pos, Parents) ->
    expand_element(X, Pos, Parents).

expand_element(Tag, Pos, Parents) when atom(Tag) ->
    NewParents = [{Tag, Pos}|Parents],
    #xmlElement{name = Tag,
		pos = Pos,
		parents = Parents,
		attributes = [],
		content = []};
expand_element({Data}, Pos, Parents) ->
    #xmlText{pos = Pos,
	     parents = Parents,
	     value = Data};
expand_element(Bin, Pos, Parents) when binary(Bin) ->
    #xmlText{pos = Pos,
	     parents = Parents,
	     value = Bin};
expand_element({Tag, Content}, Pos, Parents) ->
    NewParents = [{Tag, Pos}|Parents],
    #xmlElement{name = Tag,
		pos = Pos,
		parents = Parents,
		attributes = [],
		content = expand_content(Content, NewParents)};
expand_element({Tag, Attrs, Content}, Pos, Parents) ->
    NewParents = [{Tag, Pos}|Parents],
    FullAttrs = full_attributes(Attrs, NewParents),
    #xmlElement{name = Tag,
		pos = Pos,
		parents = Parents,
		attributes = FullAttrs,
		content = expand_content(Content, NewParents)}.

expand_content(Content = [#xmlElement{}|_], Parents) ->
    Content;
expand_content(Content, Parents) ->
    expand_content(Content, 1, Parents).

expand_content(Content = [H|_], Pos, Parents) when integer(H) ->
    case catch list_to_binary(Content) of
	{'EXIT', _} ->
	    %% mixed content
	    {Text, Rest} = collect_text(lists:flatten(Content)),
	    [#xmlText{pos = Pos,
		      parents = Parents,
		      value = Text}|expand_content(Rest, Pos+1, Parents)];
	Bin ->
	    [#xmlText{pos = Pos,
		      parents = Parents,
		      value = binary_to_list(Bin)}]
    end;
expand_content([H|T], Pos, Parents) ->
    [expand_element(H, Pos, Parents)|expand_content(T, Pos+1, Parents)];
expand_content([], Pos, Parents) ->
    [].

collect_text(Str) ->
    collect_text(Str, []).

collect_text([H|T], Acc) when integer(H) ->
    collect_text(T, [H|Acc]);
collect_text(Str, Acc) ->
    {lists:reverse(Acc), Str}.


full_attributes(Attrs, Parents) ->
    full_attributes(Attrs, 1, Parents).

full_attributes([{K, V}|T], Pos, Parents) ->
    [#xmlAttribute{name = K,
		   pos = Pos,
		   parents = Parents,
		   value = V}|full_attributes(T, Pos+1, Parents)];
full_attributes([], Pos, Parents) ->
    [].
		   
callbacks(M) ->
    Result = check_inheritance(M, []),
%%%     io:format("callbacks = ~p~n", [lists:reverse(Result)]),
    lists:reverse(Result).

callbacks([M|Mods], Visited) ->
    case lists:member(M, Visited) of
	false ->
	    NewVisited = check_inheritance(M, Visited),
	    callbacks(Mods, NewVisited);
	true ->
	    exit({cyclic_inheritance, {M, hd(Visited)}})
    end;
callbacks([], Visited) ->
    Visited.

check_inheritance(M, Visited) ->
%%%     io:format("calling ~p:'#xml-inheritance#'()~n", [M]),
    case M:'#xml-inheritance#'() of
	[] ->
	    [M|Visited];
	Mods ->
	    callbacks(Mods, [M|Visited])
    end.

apply_text_cb(Ms, Text) ->
    apply_cb(Ms, '#text#', '#text#', [Text]).

apply_tag_cb(Ms, F, Args) ->
    apply_cb(Ms, F, '#element#', Args).

apply_cb(Ms, F, Df, Args) ->
    apply_cb(Ms, F, Df, Args, Ms).

apply_cb([M|Ms], F, Df, Args, Ms0) ->
    case catch apply(M, F, Args) of
	{'EXIT', {undef, _}} ->
	    apply_cb(Ms, F, Df, Args, Ms0);
	{'EXIT', Reason} ->
	    exit(Reason);
	Res ->
	    Res
    end;
apply_cb([], Df, Df, Args, Ms0) ->
    exit({unknown_tag, {Df, Args}});
apply_cb([], F, Df, Args, Ms0) ->
    apply_cb(Ms0, Df, Df, [F|Args]).
