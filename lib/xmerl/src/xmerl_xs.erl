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
%%% The Original Code is xmerl-0.19
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): "Vlad Dumitrescu" <Vlad.Dumitrescu@erv.ericsson.se>.
%%%
%%%----------------------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%%----------------------------------------------------------------------
%%% File         : xmerl_xs.erl
%%% Author       : Mikael Karlsson <mikael.karlsson@creado.com>
%%%                Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description  : Implements XSLT like transformations in Erlang
%%% 
%%% Modules used : lists, xmerl_xpath, 
%%%		   
%%%----------------------------------------------------------------------
%% @doc
%     <p>Erlang has similarities to XSLT since both languages
% 	have a functional programming approach. Using <code>xmerl_xpath</code>
% 	it is possible to write XSLT like transforms in Erlang.
%     </p>
%     <p>XSLT stylesheets are often used when transforming XML
%       documents, to other XML documents or (X)HTML for presentation.
%       There are a number of brick-sized books written on the
%       topic. XSLT contains quite many
%       functions and learning them all may take some effort, which
%       could be a reason why the author only has reached a basic level of
%       understanding. This document assumes a basic level of
%       understanding of XSLT.
%     </p>
%     <p>Since XSLT is based on a functional programming approach
%       with pattern matching and recursion it is possible to write
%       similar style sheets in Erlang. At least for basic
%       transforms. This
%       document describes how to use the XPath implementation together
%       with Erlangs pattern matching and a couple of functions to write
%       XSLT like transforms.</p>
%     <p>This approach is probably easier for an Erlanger but
%       if you need to use real XSLT stylesheets in order to "comply to
%       the standard" there is an adapter available to the Sablotron
%       XSLT package which is written i C++.
% See also the <a href="xmerl_xs_examples.html">Tutorial</a>.
%     </p>

-module(xmerl_xs).
-vsn('0.19').
-date('03-02-03').
-author('mikael.karlsson@creado.com').

-export([xslapply/2, value_of/1, select/2, built_in_rules/2 ]).
-include("xmerl.hrl").


%% @spec xslapply(Function, EList::list()) -> List
%%   Function = () -> list()
%% @doc xslapply is a wrapper to make things look similar to
%% xsl:apply-templates.
%%
%% Example, original XSLT:<br/><pre>
%% &lt;xsl:template match="doc/title">
%%   &lt;h1>
%%     &lt;xsl:apply-templates/>
%%   &lt;/h1>
%% &lt;/xsl:template>
%% </pre>
%%
%% becomes in Erlang:<br/><pre>
%% template(E = #xmlElement{ parents=[{'doc',_}|_], name='title'}) ->
%%   ["&lt;h1>",
%%    xslapply(fun template/1, E),
%%    "&lt;/h1>"];
%% </pre>

xslapply(Fun, EList) when list(EList) ->
    lists:map( Fun, EList);
xslapply(Fun, E = #xmlElement{})->
    lists:map( Fun, E#xmlElement.content).


%% @spec value_of(E) -> List
%%   E = unknown()
%%
%% @doc Concatenates all text nodes within the tree.
%%
%% Example:<br/><pre>
%% &lt;xsl:template match="title">
%%   &lt;div align="center">
%%     &lt;h1>&lt;xsl:value-of select="." />&lt;/h1>
%%   &lt;/div>
%% &lt;/xsl:template>
%% </pre>
%%
%%  becomes:<br/> <pre>
%%  template(E = #xmlElement{name='title'}) ->
%%    ["&lt;div align="center">&lt;h1>", value_of(select(".", E)), "&lt;/h1>&lt;/div>"]<br/>
%% </pre>
value_of(E)->
    lists:reverse(xmerl_lib:foldxml(fun value_of1/2, [], E)).

value_of1(#xmlText{}=T1, Accu)->
    [xmerl_lib:export_text(T1#xmlText.value)|Accu];
value_of1(_, Accu) ->
    Accu.

%% @spec select(String::string(),E)-> E
%%
%% @doc Extracts the nodes from the xml tree according to XPath.
%% @see value_of/1
select(Str,E)->
    xmerl_xpath:string(Str,E).

%% @spec built_in_rules(Fun, E) -> List
%%
%% @doc The default fallback behaviour. Template funs should end with:
%% <br/><code>template(E) -> built_in_rules(fun template/1, E)</code>.
built_in_rules(Fun, E = #xmlElement{})->
    lists:map(Fun, E#xmlElement.content);
built_in_rules(_Fun, E = #xmlText{}) ->
    xmerl_lib:export_text(E#xmlText.value);
built_in_rules(_Fun, E = #xmlAttribute{}) ->
    E#xmlAttribute.value;
built_in_rules(_Fun, _E) ->[].
