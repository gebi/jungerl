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
%%% The Original Code is xmerl-0.13
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
%%% File:       xmerl_xpath.erl
%%% Author       : Ulf Wiger <ulf.wiger@ericsson.com>
%%% Description  : Implements a search engine based on XPath
%%% 
%%% Modules used : lists, xmerl_xpath_parse, xmerl_xpath_pred, 
%%%		   xmerl_xpath_scan
%%% 
%%%----------------------------------------------------------------------
%% @doc The <code>xmerl_xpath</code> module handles the entire XPath 1.0 spec
%% XPath expressions typically occurs in XML attributes and are used to addres
%% parts of an XML document.
%     The grammar is defined in <code>xmerl_xpath_parse.yrl</code>.
%     The core functions are defined in <code>xmerl_xpath_pred.erl</code>.
%
%     <h3>Some useful shell commands for debugging the XPath parser</h3>
% <p><pre>
% c(xmerl_xpath_scan).
% yecc:yecc("xmerl_xpath_parse.yrl", "xmerl_xpath_parse", true, []).
% c(xmerl_xpath_parse).
%
% xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens("position() > -1")).
% xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens("5 * 6 div 2")).
% xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens("5 + 6 mod 2")).
% xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens("5 * 6")).
% xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens("-----6")).
% xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens("parent::node()")).
% xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens("descendant-or-self::node()")).
% xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens("parent::processing-instruction('foo')")).
%% </pre></p>
%%
%% @type docEntity() = 
%%      xmlElement()
%%    | xmlAttribute()
%%    | xmlText() 
%%    | xmlPI()
%%    | xmlComment()
%% @type nodeEntity() = 
%%      xmlElement()
%%    | xmlAttribute()
%%    | xmlText() 
%%    | xmlPI()
%%    | xmlNamespace()
%%    | xmlDocument()
%% @type option_list(). <p>Options allows to customize the behaviour of the
%%     XPath scanner.
%% </p>
%% Possible options are:
%% <dl>
%%  <dt><code>{namespace, #xmlNamespace}</code></dt>
%%    <dd>Set namespace nodes, from XmlNamspace, in xmlContext</dd>
%%  <dt><code>{namespace, Nodes}</code></dt>
%%    <dd>Set namespace nodes in xmlContext.</dd>
%% </dl>

%%  <dt><code>{bindings, Bs}</code></dt>
%%   <dd></dd>
%% <dt><code>{functions, Fs}</code></dt>
%%   <dd></dd>
-module(xmerl_xpath).
-vsn('0.13').
-date('01-02-21').
-author('ulf.wiger@ericsson.com').


%% main API
-export([string/2,
	 string/3,
	 string/5]).

%% exported helper functions, internal for the XPath support
-export([eval_path/3,
	 axis/3, axis/4]).

%% debug function
-export([write_node/1]).


-include("xmerl.hrl").


-record(state, {context = #xmlContext{},
		acc = []}).
-record(node, {node,
	       pos,
	       parents}).

-define(nodeset(NS), #state{context = #xmlContext{nodeset = NS}}).
-define(context(C), #state{context = C}).




%% @spec string(Str, Doc) -> docEntity()
%% @equiv string(Str,Doc, [])
string(Str, Doc) ->
    string(Str, Doc, []).

%% @spec string(Str,Doc,Options) -> 
%%      docEntity()
%% @equiv string(Str,Doc, [],Doc,Options)
string(Str, Doc, Options) ->
    string(Str, Doc, [], Doc, Options).

%% @spec string(Str,Node,Parents,Doc,Options) ->
%%      docEntity()
%%   Str     = xPathString()
%%   Node    = nodeEntity()
%%   Parents = parentList()
%%   Doc     = nodeEntity()
%%   Options = option_list()
%% @doc Extracts the nodes from the parsed XML tree according to XPath.
string(Str, Node, Parents, Doc, Options) ->
    FullParents = 
	case Parents of
	    [] ->
		[];
	    [{H, P}|_] when atom(H), integer(P) ->
		full_parents(Parents, Doc)
	end,
%io:format("string FullParents=~p~n",[FullParents]),
    ContextNode=#xmlNode{type = node_type(Node),
			 node = Node,
			 parents = FullParents},
%io:format("string ContextNode=~p~n",[ContextNode]),
    WholeDoc = whole_document(Doc),
%io:format("string WholeDoc=~p~n",[WholeDoc]),
    Context=(new_context(Options))#xmlContext{context_node = ContextNode,
					      whole_document = WholeDoc},
%io:format("string Context=~p~n",[Context]),
    #state{context =  NewContext} = match(Str, #state{context = Context}),
%io:format("string NewContext=~p~n",[NewContext]),
    [N || #xmlNode{node = N} <- NewContext#xmlContext.nodeset].


whole_document(#xmlDocument{} = Doc) ->
    #xmlNode{type = root_node,
	     node = Doc,
	     parents = []};
whole_document(Other) ->
    #xmlNode{type = root_node,
	     node = #xmlDocument{content = Other},
	     parents = []}.


new_context(Options) ->
    new_context(Options, #xmlContext{}).

new_context([{namespace, #xmlNamespace{nodes = Nodes}}|T], C) ->
    new_context(T, C#xmlContext{namespace = ns_nodes(Nodes)});
new_context([{namespace, Nodes}|T], C) ->
    new_context(T, C#xmlContext{namespace = ns_nodes(Nodes)});
new_context([{bindings, Bs}|T], C) ->
    new_context(T, C#xmlContext{bindings = Bs});
new_context([{functions, Fs}|T], C) ->
    new_context(T, C#xmlContext{functions = Fs});
new_context([], C) ->
    C.


ns_nodes([{Prefix, URI}|T]) ->
    [{to_string(Prefix), to_atom(URI)}|ns_nodes(T)];
ns_nodes([]) ->
    [].

full_parents(Ps, Doc) ->
    full_parents1(lists:reverse(Ps), [Doc], []).

full_parents1([{Name, Pos}|Ns], Content, Parents) ->
    E = locate_element(Name, Pos, Content),
    PN = #xmlNode{type = element,
		  node = E,
		  parents = Parents},
    full_parents1(Ns, get_content(E), [PN|Parents]);
full_parents1([], _E, Parents) ->
    Parents.


locate_element(Name, Pos, [E = #xmlElement{name = Name, pos = Pos}|_]) ->
    E;
locate_element(_Name, Pos, [#xmlElement{pos = P}|_]) when P >= Pos ->
    %% we've passed Pos (P > Pos) or the name is wrong (P == Pos)
    exit(invalid_parents);
locate_element(_Name, _Pos, []) ->
    exit(invalid_parents);
locate_element(Name, Pos, [_|T]) ->
    locate_element(Name, Pos, T).


match(Str, S = #state{}) ->
    Tokens = xmerl_xpath_scan:tokens(Str),
    case xmerl_xpath_parse:parse(Tokens) of
	{ok, Expr} ->
	    match_expr(Expr, S);
	Error ->
	    Error
    end.


match_expr({path, Type, Arg}, S) ->
    eval_path(Type, Arg, S#state.context).





path_expr({refine, StepExpr1, StepExpr2}, S) ->
    ?dbg("StepExpr1=~p StepExpr2=~p~n", [StepExpr1,StepExpr2]),
    ?dbg("length(nodeset) = ~p~n", 
	 [length((S#state.context)#xmlContext.nodeset)]),
    S1 = path_expr(StepExpr1, S),
    ?dbg("length(nodeset1) = ~p~n", 
	 [length((S1#state.context)#xmlContext.nodeset)]),
    path_expr(StepExpr2, S1);
path_expr({step, {Axis, NodeTest, PredExpr}}, S = #state{context = C,
							 acc = Acc}) ->
    ?dbg("PredExpr = ~p~n", [PredExpr]),
    NewContext = axis(Axis, NodeTest, C, Acc),
    pred_expr(PredExpr, S#state{context = NewContext}).



pred_expr([], S) ->
    S;
pred_expr([{pred, Pred}|Preds], S = #state{}) ->
    ?dbg("Pred = ~p~n", [Pred]),
    NewS = eval_pred(Pred, S),
    pred_expr(Preds, NewS).

%% simple case: the predicate is a number, e.g. para[5].
%% No need to iterate over all nodes in the nodeset; we know what to do.
%%
eval_pred({number, N}, S = #state{context = C = #xmlContext{nodeset = NS}}) ->
    NewNodeSet = [lists:nth(N, NS)],
    NewContext = C#xmlContext{nodeset = NewNodeSet},
    S#state{context = NewContext};
eval_pred(Predicate, S = #state{context = C = 
				#xmlContext{nodeset = NodeSet}}) ->
    NewNodeSet = 
	lists:filter(
	  fun(Node) ->
		  %io:format("current node: ~p~n", [write_node(Node)]),
		  ThisContext = C#xmlContext{context_node = Node},
		  xmerl_xpath_pred:eval(Predicate, ThisContext)
	  end, NodeSet),
    NewContext = C#xmlContext{nodeset = NewNodeSet},
    S#state{context = NewContext}.    
    


%% write_node(Node::xmlNode()) -> {Type,Pos,Name,Parents}
%% Helper function to access essential information from the xmlNode record.
%% @hidden
write_node(#xmlNode{pos = Pos,
		    node = #xmlAttribute{name = Name,
					 parents = Ps}}) ->
    {attribute, Pos, Name, Ps};
write_node(#xmlNode{pos = Pos,
		    node = #xmlElement{name = Name,
				       parents = Ps}}) ->
    {element, Pos, Name, Ps};
write_node(#xmlNode{pos = Pos,
		    node = #xmlText{value = Txt,
				    parents = Ps}}) ->
    {text, Pos, Txt, Ps};
write_node(_) ->
    other.


%% eval_path(Type,Arg,S::state()) -> state()
%% Eval path
%% @hidden
eval_path(union, {PathExpr1, PathExpr2}, C = #xmlContext{}) ->
    S = #state{context = C},
    S1 = match_expr(PathExpr1, S),
    NewNodeSet = (S#state.context)#xmlContext.nodeset,
    match_expr(PathExpr2, S1#state{acc = NewNodeSet});
eval_path(abs, PathExpr, C = #xmlContext{}) ->
    NodeSet = [C#xmlContext.whole_document],
    Context = C#xmlContext{nodeset = NodeSet},
    S = #state{context = Context},
    path_expr(PathExpr, S);
eval_path(rel, PathExpr, C = #xmlContext{}) ->
    NodeSet = [C#xmlContext.context_node],
    Context = C#xmlContext{nodeset = NodeSet},
    S = #state{context = Context},
    path_expr(PathExpr, S);
eval_path(filter, {PathExpr, PredExpr}, C = #xmlContext{}) ->
    S = #state{context = C},
    S1 = path_expr(PathExpr, S),
    pred_expr(PredExpr, S1).


%% axis(Axis,NodeTest,Context::xmlContext()) -> xmlContext()
%% axis(Axis,NodeTest,Context,[])
%% @hidden
axis(Axis, NodeTest, Context) ->
    axis(Axis, NodeTest, Context, []).


%% axis(Axis,NodeTest,Context::xmlContext(),Acc) -> xmlContext()
%%  
%% An axis specifies the tree relationship between the nodes selected by
%% the location step and the context node.
%% @hidden
axis(Axis, NodeTest, Context = #xmlContext{nodeset = NS0}, Acc) ->
    NewNodeSet=lists:foldr(
		 fun(N, AccX) ->
			 axis1(Axis, NodeTest, N, AccX, Context)
		 end, Acc, NS0),
    update_nodeset(fwd_or_reverse(Axis, Context), NewNodeSet).


axis1(self, Tok, N, Acc, Context) ->
    match_self(Tok, N, Acc, Context);
axis1(descendant, Tok, N, Acc, Context) ->
    match_descendant(Tok, N, Acc, Context);
axis1(child, Tok, N, Acc, Context) ->
    match_child(Tok, N, Acc, Context);
axis1(parent, Tok, N, Acc, Context) ->
    match_parent(Tok, N, Acc, Context);
axis1(ancestor, Tok, N, Acc, Context) ->
    match_ancestor(Tok, N, Acc, Context);
axis1(following_sibling, Tok, N, Acc, Context) ->
    match_following_sibling(Tok, N, Acc, Context);
axis1(preceding_sibling, Tok, N, Acc, Context) ->
    match_preceding_sibling(Tok, N, Acc, Context);
axis1(following, Tok, N, Acc, Context) ->
    match_following(Tok, N, Acc, Context);
axis1(preceding, Tok, N, Acc, Context) ->
    match_preceding(Tok, N, Acc, Context);
axis1(attribute, Tok, N, Acc, Context) ->
    match_attribute(Tok, N, Acc, Context);
axis1(namespace, Tok, N, Acc, Context) ->
    match_namespace(Tok, N, Acc, Context);
axis1(ancestor_or_self, Tok, N, Acc, Context) ->
    match_ancestor_or_self(Tok, N, Acc, Context);
axis1(descendant_or_self, Tok, N, Acc, Context) ->
    match_descendant_or_self(Tok, N, Acc, Context).


fwd_or_reverse(ancestor, Context) ->
    reverse_axis(Context);
fwd_or_reverse(preceding_sibling, Context) ->
    reverse_axis(Context);
fwd_or_reverse(preceding, Context) ->
    reverse_axis(Context);
fwd_or_reverse(_, Context) ->
    forward_axis(Context).

reverse_axis(Context) ->
    Context#xmlContext{axis_type = reverse}.
forward_axis(Context) ->
    Context#xmlContext{axis_type = forward}.



match_self(Tok, N, Acc, Context) ->
    case node_test(Tok, N, Context) of
	true ->
	    %io:format("node_test -> true.~n", []),
	    [N|Acc];
	false ->
	    Acc
    end.


match_descendant(Tok, N, Acc, Context) ->
    #xmlNode{parents = Ps, node = Node, type = Type} = N,
    case Type of
	element ->
	    NewPs = [N|Ps],
	    match_desc(get_content(Node), NewPs, Tok, Acc, Context);
	_Other ->
	    Acc
    end.

%match_desc(Content, Parents, Tok, Context) ->
%    match_desc(Content, Parents, Tok, [], Context).

match_desc([E = #xmlElement{}|T], Parents, Tok, Acc, Context) ->
    N = #xmlNode{type = node_type(E),
		 node = E,
		 parents = Parents},
    NewParents = [N|Parents],
    Acc1 = case node_test(Tok, N, Context) of
	       true ->
		   [N|Acc];
	       false ->
		   Acc
	   end,
    Acc2 = match_desc(get_content(E), NewParents, Tok, Acc1, Context),
    match_desc(T, Parents, Tok, Acc2, Context);
match_desc([E|T], Parents, Tok, Acc, Context) ->
    N = #xmlNode{node = E,
		 type = node_type(E),
		 parents = Parents},
    Acc1 = case node_test(Tok, N, Context) of
	       true ->
		   [N|Acc];
	       false ->
		   Acc
	   end,
    match_desc(T, Parents, Tok, Acc1, Context);
match_desc([], _Parents, _Tok, Acc, _Context) ->
    Acc.
			  


%% "The 'descendant-or-self' axis contains the context node and the 
%% descendants of the context node."
match_descendant_or_self(Tok, N, Acc, Context) ->
    Acc1 = case node_test(Tok, N, Context) of
	       true ->
		   [N|Acc];
	       false ->
		   Acc
	   end,
    match_descendant(Tok, N, Acc1, Context).


match_child(Tok, N, Acc, Context) ->
    %io:format("match_child(~p)~n", [write_node(N)]),
    #xmlNode{parents = Ps, node = Node, type = Type} = N,
    case Type of
	element ->
	    NewPs = [N|Ps],
	    lists:foldr(
	      fun(E, AccX) ->
		      ThisN = #xmlNode{type = node_type(E),
				       node = E,
				       parents = NewPs},
		      case node_test(Tok, ThisN, Context) of
			  true ->
			      [ThisN|AccX];
			  false ->
			      AccX
		      end
	      end, Acc, get_content(Node));
	_Other ->
	    Acc
    end.


%% "The 'parent' axis contains the parent of the context node, 
%% if there is one."
match_parent(Tok, N, Acc, Context) ->
    case N#xmlNode.parents of
	[] ->
	    Acc;
	[PN|_] ->
	    case node_test(Tok, PN, Context) of
		true ->
		    [PN|Acc];
		false ->
		    Acc
	    end
    end.


%% "The 'ancestor' axis contains the ancestors of the context node;
%% the ancestors of the context node consists of the parent of the context
%% node and the parent's parent and so on; thus, the ancestor axis will 
%% always include the root node, unless the context node is the root node."
match_ancestor(Tok, N, Acc, Context) ->
    Parents = N#xmlNode.parents,
    lists:foldr(
      fun(PN, AccX) ->
	      case node_test(Tok, PN, Context) of
		  true ->
		      [PN|AccX];
		  false ->
		      AccX
	      end
      end, Acc, Parents).




%% "The 'ancestor-or-self' axis contains the context node and the ancestors
%% of the context node; thus, the acestor axis will always include the
%% root node."
match_ancestor_or_self(Tok, N, Acc, Context) ->
    Acc1 = case node_test(Tok, N, Context) of
	       true ->
		   [N|Acc];
	       false ->
		   Acc
	   end,
    match_ancestor(Tok, N, Acc1, Context).


match_following_sibling(_Tok, #xmlAttribute{}, Acc, _Context) ->
    Acc;
match_following_sibling(_Tok, #xmlNamespace{}, Acc, _Context) ->
    Acc;

match_following_sibling(Tok, N, Acc, Context) ->
    #xmlNode{parents = Ps, node = Node} = N,
    case Ps of
	[#xmlNode{type = element,
		  node = #xmlElement{} = PNode}|_] ->
	    FollowingSiblings = lists:nthtail(Node#xmlElement.pos, 
					      get_content(PNode)),
	    lists:foldr(
	      fun(E, AccX) ->
		      ThisN = #xmlNode{type = node_type(E),
				       node = E,
				       parents = Ps},
		      case node_test(Tok, ThisN, Context) of
			  true ->
			      [ThisN|AccX];
			  false ->
			      AccX
		      end
	      end, Acc, FollowingSiblings);
	_Other ->
	    Acc
    end.


%% "The 'following' axis contains all nodes in the same document as the
%% context node that are after the context node in document order, excluding
%% any descendants and excluding attribute nodes and namespace nodes."
%% (UW: I interpret this as "following siblings and their descendants")
match_following(Tok, N, Acc, Context) ->
    #xmlNode{parents = Ps, node = Node} = N,
    case Ps of
	[#xmlNode{type = element,
		  node = #xmlElement{} = PNode}|_] ->
	    FollowingSiblings = lists:nthtail(Node#xmlElement.pos, 
					      get_content(PNode)),
	    lists:foldr(
	      fun(E, AccX) ->
		      ThisN = #xmlNode{type = node_type(E),
				       node = E,
				       parents = Ps},
		      Acc1 =
			  case node_test(Tok, ThisN, Context) of
			      true ->
				  [ThisN|AccX];
			      false ->
				  AccX
			  end,
		      match_desc(get_content(E), Tok, Ps, Acc1, Context)
	      end, Acc, FollowingSiblings);
	_Other ->
	    Acc
    end.


%% "The preceding-sibling axis contains all the preceding siblings of the 
%% context node; if the context node is an attribute node or namespace node,
%% the preceding-sibling axis is empty."
match_preceding_sibling(_Tok, #xmlAttribute{}, Acc, _Context) ->
    Acc;
match_preceding_sibling(_Tok, #xmlNamespace{}, Acc, _Context) ->
    Acc;

match_preceding_sibling(Tok, N, Acc, Context) ->
    #xmlNode{parents = Ps, node = Node} = N,
    case Ps of
	[#xmlNode{type = element,
		  node = #xmlElement{} = PNode}|_] ->
	    PrecedingSiblings = lists:sublist(get_content(PNode), 1,
					      Node#xmlElement.pos-1), 
	    lists:foldr(
	      fun(E, AccX) ->
		      ThisN = #xmlNode{type = node_type(E),
				       node = E,
				       parents = Ps},
		      case node_test(Tok, ThisN, Context) of
			  true ->
			      [ThisN|AccX];
			  false ->
			      AccX
		      end
	      end, Acc, PrecedingSiblings);
	_Other ->
	    []
    end.


%% "The 'preceding' axis contains all nodes in the same document as the context
%% node that are before the context node in document order, exluding any
%% ancestors and excluding attribute nodes and namespace nodes."
%% (UW: I interpret this as "preceding siblings and their descendants".)
match_preceding(Tok, N, Acc, Context) ->
    #xmlNode{parents = Ps, node = Node} = N,
    case Ps of
	[#xmlNode{type = element,
		  node = #xmlElement{} = PNode}|_] ->
	    PrecedingSiblings = lists:sublist(get_content(PNode), 1,
					      Node#xmlElement.pos-1), 
	    lists:foldr(
	      fun(E, AccX) ->
		      ThisN = #xmlNode{type = node_type(E),
				       node = E,
				       parents = Ps},
		      Acc1 =
			  case node_test(Tok, ThisN, Context) of
			      true ->
				  [ThisN|AccX];
				  false ->
				      AccX
			  end,
		      match_desc(get_content(E), Tok, Ps, Acc1, Context)
	      end, Acc, PrecedingSiblings);
	_Other ->
	    []
    end.


%% "The 'attribute' axis contains the attributes of the context node; the
%% axis will be empty unless the context node is an element."
match_attribute(Tok, N, Acc, Context) ->
    case N#xmlNode.type of
	element ->
	    #xmlNode{parents = Ps, node = E} = N,
	    lists:foldl(
	      fun(A, AccX) ->
		      ThisN = #xmlNode{type = attribute,
				       node = A,
				       parents = [N|Ps]},
		      case node_test(Tok, ThisN, Context) of
			  true ->
			      [ThisN|AccX];
			  false ->
			      AccX
		      end
	      end, Acc, E#xmlElement.attributes);
	_Other ->
	    []
    end.

node_type(#xmlAttribute{}) ->	attribute;
node_type(#xmlElement{}) ->	element;
node_type(#xmlText{}) ->	text;
node_type(#xmlPI{}) ->		processing_instruction;
node_type(#xmlNamespace{}) ->	namespace;
node_type(#xmlDocument{}) ->	root_node.

%% "The namespace axis contains the namespace nodes of the context node;
%% the axis will be empty unless the context node is an element."
match_namespace(_Tok, _N, _Acc, _Context) ->
    %% TODO: IMPLEMENT NAMESPACE AXIS
    erlang:fault(not_yet_implemented).


update_nodeset(Context = #xmlContext{axis_type = reverse}, NodeSet) ->
    Context#xmlContext{nodeset = reverse(NodeSet)};
update_nodeset(Context, NodeSet) ->
    Context#xmlContext{nodeset = forward(NodeSet)}.

reverse(NodeSet) ->
    reverse(NodeSet, 1, []).

reverse([H|T], Pos, Acc) ->
    reverse(T, Pos+1, [H#xmlNode{pos = Pos}|Acc]);
reverse([], _Pos, Acc) ->
    Acc.

forward(NodeSet) ->
    forward(NodeSet, 1).

forward([H|T], Pos) ->
    [H#xmlNode{pos = Pos}|forward(T, Pos+1)];
forward([], _Pos) ->
    [].



node_test(F, N, Context) when function(F) ->
    F(N, Context);
node_test({wildcard, _}, _N, _Context) -> 
    true;
node_test({prefix_test, Prefix}, #xmlNode{node = N}, _Context) ->
    case N of
	#xmlElement{nsinfo = {Prefix, _}} -> true;
	#xmlAttribute{nsinfo = {Prefix, _}} -> true;
	_ ->
	    false
    end;
node_test({name, {Tag, _Prefix, _Local}}, 
	  #xmlNode{node = #xmlElement{name = Tag}}=_N, _Context) -> 
    %io:format("node_test({tag, ~p}, ~p) -> true.~n", [Tag, write_node(_N)]),
    true;
node_test({name, {Tag, Prefix, Local}}, 
	  #xmlNode{node = #xmlElement{name = Name,
				      expanded_name = EExpName,
				      nsinfo = {_Prefix1, _}
				     }}, Context) -> 
    case expanded_name(Prefix, Local, Context) of
	[] ->
	    Res = (Tag == Name),
	    ?dbg("node_test(~p, ~p) -> ~p.~n", 
		 [{Tag, Prefix, Local}, write_node(Name), Res]),
	    Res;
	ExpName ->
	    Res = (ExpName == EExpName),
	    ?dbg("node_test(~p, ~p) -> ~p.~n", 
		 [{Tag, Prefix, Local}, write_node(Name), Res]),
	    Res
    end;
node_test({name, {Tag,_Prefix,_Local}}, 
	  #xmlNode{node = #xmlAttribute{name = Tag}}, _Context) -> 
    true;
node_test({name, {_Tag, Prefix, Local}}, 
	  #xmlNode{node = #xmlAttribute{expanded_name = {URI, Local},
					nsinfo = {_Prefix1, _},
					namespace = NS}}, _Context) -> 
    NSNodes = NS#xmlNamespace.nodes,
    case lists:keysearch(Prefix, 1, NSNodes) of
	{value, {_, URI}} ->
	    ?dbg("node_test(~, ~p) -> true.~n", 
		 [{_Tag, Prefix, Local}, write_node(NSNodes)]),
	    true;
	false ->
	    ?dbg("node_test(~, ~p) -> false.~n", 
		 [{_Tag, Prefix, Local}, write_node(NSNodes)]),
	    false
    end;
node_test({node_type, NT}, #xmlNode{node = N}, _Context) ->
    case {NT, N} of
	{text, {_Data}} ->
	    true;
	{node, _} ->
	    true;
	{attribute, #xmlAttribute{}} ->
	    true;
	{namespace, #xmlNamespace{}} ->
	    true;
	_ ->
	    false
    end;
node_test({processing_instruction, {literal, _, Name}}, 
	  #xmlNode{node = {processing_instruction, Name, _Data}}, _Context) ->
    true;
node_test(_Other, _N, _Context) ->
    %io:format("node_test(~p, ~p) -> false.~n", [_Other, write_node(_N)]),
    false.


expanded_name(Prefix, Local, #xmlContext{namespace = NS}) ->
    case lists:keysearch(Prefix, 1, NS) of
	{value, {_, URI}} ->
	    {URI, list_to_atom(Local)};
	false ->
	    []
    end.


to_atom(A) when atom(A) -> A;
to_atom(S) when list(S) -> list_to_atom(S).

to_string(A) when atom(A) -> atom_to_list(A);
to_string(S) when list(S) -> S.


get_content(#xmlElement{content = C}) when list(C) ->
    C;
get_content(#xmlElement{content = F} = E) when function(F) ->
    case F() of
	C when list(C) ->
	    C;
	_Other ->
	    exit({bad_content, E})
    end.
