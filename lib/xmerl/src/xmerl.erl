%% The contents of this file are subject to the Erlang Public License,
%% Version 1.0, (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% http://www.erlang.org/license/EPL1_0.txt
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is xmerl-0.14
%%
%% The Initial Developer of the Original Code is Ericsson Telecom
%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%% Telecom AB. All Rights Reserved.
%%
%% Contributor(s): ______________________________________.
%%
%%----------------------------------------------------------------------
%% #0.    BASIC INFORMATION
%%----------------------------------------------------------------------
%% File:       xmerl.erl
%% Author       : Ulf Wiger <ulf.wiger@ericsson.com>
%% Description  : Functions to export simple and complete XML forms
%% 
%% Modules used : lists, <callbacks specified via arguments>
%% 
%%----------------------------------------------------------------------

%% @doc Functions for exporting XML data to an external format.
%%

-module(xmerl).

-compile(export_all).

-export([export/2, 
	 export/3,
	 export_content/2,
	 export_element/2,
	 export_element/3,
	 export_simple/2,
	 export_simple/3,
	 export_simple_element/2,
	 export_simple_content/2,
	 callbacks/1]).

-include("xmerl.hrl").


%% @spec export(Content, Callback) -> ExportedFormat
%% @equiv export(Data, Callback, [])

export(Data, Callback) ->
    export(Data, Callback, []).

%% @spec export(Content, Callback, RootAttributes) -> ExportedFormat
%%	Content = [Element]
%%	Callback = atom()
%% @doc Exports normal, well-formed XML content, using the specified
%% callback-module.
%% <code>Element</code> is any of:
%% <ul>
%% 	<li><code>#xmlText{}</code></li>
%%	<li><code>#xmlElement{}</code></li>
%%	<li><code>#xmlPI{}</code></li>
%%	<li><code>#xmlComment{}</code></li>
%%	<li><code>#xmlDecl{}</code></li>
%% </ul>
%% (See <tt>xmerl.hrl</tt> for the record definitions.)
%% Text in <code>#xmlText{}</code> elements can be deep lists of
%% characters and/or binaries.
%%
%% <p><code>RootAttributes</code> is a list of
%% <code>[#xmlAttribute{}]</code> attributes for the <code>#root#</code>
%% element, which implicitly becomes the parent of the given
%% <code>Content</code>. The tag-handler function for
%% <code>#root#</code> is thus called with the complete exported data of
%% <code>Content</code>. Root attributes can be used to specify
%% e.g. encoding or other metadata of an XML or HTML document.</p>
%%
%% <p>The <code>Callback</code> module should contain hook functions for
%% all tags present in the data structure. A hook function must have the
%% following format:
%% <pre>    Tag(Data, Attributes, Parents, E)</pre>
%% where <code>E</code> is the corresponding <code>#xmlElement{}</code>,
%% <code>Data</code> is the already-exported contents of <code>E</code>
%% and <code>Attributes</code> is the list of
%% <code>#xmlAttribute{}</code> records of <code>E</code>. Finally,
%% <code>Parents</code> is the list of parent nodes of <code>E</code>,
%% on the form <code>[{ParentTag::atom(),
%% ParentPosition::integer()}]</code>.</p>
%%
%% <p>The hook function should return either the data to be exported, or
%% a tuple <code>{'#xml-alias#', NewTag::atom()}</code>, or a tuple
%% <code>{'#xml-redefine#', Content}</code>, where <code>Content</code>
%% is a content list (which can be on simple-form; see
%% <code>export_simple/2</code> for details).</p>
%%
%% <p>A callback module can inherit definitions from other callback
%% modules, through the required function <code>'#xml-interitance#() ->
%% [ModuleName::atom()]</code>.</p>
%%
%% @see export/2
%% @see export_simple/3

export(Data, Callback, RootAttrs) when atom(Callback) ->
    export1(Data, callbacks(Callback), RootAttrs);
export(Data, Callbacks, RootAttrs) when list(Callbacks) ->
    export1(Data, Callbacks, RootAttrs).

%% @spec export_simple(Content, Callback) -> ExportedFormat
%% @equiv export_simple(Data, Callback, [])

export_simple(Data, Callback) ->
    export_simple(Data, Callback, []).

%% @spec export_simple(Content, Callback, RootAttributes) -> ExportedFormat
%%	Content = [Element]
%%	Callback = atom()
%% @doc Exports "simple-form" XML content, using the specified
%% callback-module.
%% <code>Element</code> is any of:
%% <ul>
%%	<li><code>{Tag, Attributes, Content}</code></li>
%%	<li><code>{Tag, Content}</code></li>
%%	<li><code>Tag</code></li>
%%	<li><code>IOString</code></li>
%% 	<li><code>#xmlText{}</code></li>
%%	<li><code>#xmlElement{}</code></li>
%%	<li><code>#xmlPI{}</code></li>
%%	<li><code>#xmlComment{}</code></li>
%%	<li><code>#xmlDecl{}</code></li>
%% </ul>
%% where
%% <ul>
%%	<li><code>Tag = atom()</code></li>
%%	<li><code>Attributes = [{Name, Value}]</code></li>
%%	<li><code>Name = atom()</code></li>
%%	<li><code>Value = IOString | atom() | integer()</code></li>
%% </ul>
%% Normal-form XML elements can thus be included in the simple-form
%% representation. Note that content lists must be flat. An
%% <code>IOString</code> is a (possibly deep) list of characters and/or
%% binaries.
%%
%% <p>See <code>export/3</code> for details on the callback module and
%% the root attributes. The XML-data is always converted to normal form
%% before being passed to the callback module.</p>
%%
%% @see export/3
%% @see export_simple/2

export_simple(Data, Callback, RootAttrs) when atom(Callback) ->
    export_simple1(Data, callbacks(Callback), RootAttrs);
export_simple(Data, Callbacks, RootAttrs) when list(Callbacks) ->
    export_simple1(Data, Callbacks, RootAttrs).

export_simple1(Data, Callback, RootAttrs) ->
    export1(xmerl_lib:expand_content(Data), Callback, RootAttrs).

%% This exports proper XML content in root context.

export1(Data, Callbacks, RootAttrs) when is_list(Data) ->
    Result = export_content(Data, Callbacks),
    Attrs = xmerl_lib:expand_attributes(RootAttrs, 1, [{'#root#',1}]),
    Root = #xmlElement{name = '#root#',
		       pos = 1,
		       parents = [],
		       attributes = Attrs},
    Args = [Result, Root#xmlElement.attributes, [], Root],
    tagdef('#root#',1,[],Args,Callbacks).

%% @doc Exports simple XML content directly, without further context.

export_simple_content(Data, Callback) when atom(Callback) ->
    export_content(xmerl_lib:expand_content(Data),
		   callbacks(Callback));
export_simple_content(Data, Callbacks) when list(Callbacks) ->
    export_content(xmerl_lib:expand_element(Data), Callbacks).

%% @doc Exports normal XML content directly, without further context.

export_content([#xmlText{value = Text} | Es], CBs) ->
    [apply_text_cb(CBs, Text) | export_content(Es, CBs)];
export_content([#xmlPI{} | Es], CBs) ->
    export_content(Es, CBs);
export_content([#xmlComment{} | Es], CBs) ->
    export_content(Es, CBs);
export_content([#xmlDecl{} | Es], CBs) ->
    export_content(Es, CBs);
export_content([E | Es], CBs) ->
    [export_element(E, CBs) | export_content(Es, CBs)];
export_content([], _CBs) ->
    [].

%% @doc Exports a simple XML element directly, without further context.

export_simple_element(Data, Callback) when atom(Callback) ->
    export_element(xmerl_lib:expand_element(Data),
		   callbacks(Callback));
export_simple_element(Data, Callbacks) when list(Callbacks) ->
    export_element(xmerl_lib:expand_element(Data), Callbacks).

%% @doc Exports a normal XML element directly, without further context.

%% This is the usual DOM style parsing.

export_element(E, CB) when atom(CB) ->
    export_element(E, callbacks(CB));
export_element(#xmlText{value = Text}, CBs) ->
    apply_text_cb(CBs, Text);
export_element(E = #xmlElement{name = Tag,
			       pos = Pos,
			       attributes = Attributes,
			       parents = Parents,
			       content = Content}, CBs) ->
    Data = export_content(Content, CBs),
    Args = [Data, Attributes, Parents, E],
    tagdef(Tag,Pos,Parents,Args,CBs);
export_element(#xmlPI{}, _CBs) ->
    [];
export_element(#xmlComment{}, _CBs) ->
    [];
export_element(#xmlDecl{}, _CBs) ->
    [].


%% @spec export_element(E,CBs,UserState) -> ExportedFormat
%% @doc For on-the-fly exporting during parsing (SAX style) of the XML
%% document. 
export_element(E, CB,CBstate) when atom(CB) ->
    export_element(E, callbacks(CB), CBstate);
export_element(#xmlText{value = Text},CBs,CBstate) ->
    apply_cb(CBs, '#text#', '#text#', [Text,CBstate]);
export_element(#xmlElement{name = Tag,
			   pos = Pos,
			   parents = Parents,
			   attributes = Attributes,
			   content = Content},CBs,CBstate) ->
    Args = [Content, Attributes,CBstate],
    tagdef(Tag,Pos,Parents,Args,CBs);
export_element(#xmlPI{},_CBs,CBstate) ->
    CBstate;
export_element(#xmlComment{},_CBs,CBstate) ->
    CBstate;
export_element(#xmlDecl{},_CBs,CBstate) ->
    CBstate.

%% A thing returned with #xml-redefine is assumed to be a content list
%% The data may be on "simple" format.

tagdef(Tag,Pos,Parents,Args,CBs) ->
    case apply_tag_cb(CBs, Tag, Args) of
	{'#xml-alias#', NewTag} ->
	    tagdef(NewTag,Pos,Parents,Args,CBs);
	{'#xml-redefine#', Data} ->
	    export_content(xmerl_lib:expand_content(Data, Pos, Parents),
			   CBs);
	Other ->
	    Other
    end.

%% @spec callbacks(atom()) -> [atom()]
%% @doc Find the list of inherited callback modules for a given module.

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
apply_cb([], Df, Df, Args, _Ms0) ->
    exit({unknown_tag, {Df, Args}});
apply_cb([], F, Df, Args, Ms0) ->
    apply_cb(Ms0, Df, Df, [F|Args]).
