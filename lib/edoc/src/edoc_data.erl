%% =====================================================================
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% $Id$
%%
%% @private
%% @copyright 2003 Richard Carlsson
%% @author Richard Carlsson <richardc@csd.uu.se>
%% @see edoc
%% @end 
%% =====================================================================

%% @doc Building the EDoc external data structure. See the file
%% <a href="../priv/edoc.dtd">`edoc.dtd'</a> for details.

-module(edoc_data).

-export([module/4, package/4, overview/4, type/2]).

-include("edoc.hrl").

%% TODO: report multiple definitions of the same type in the same module.
%% TODO: check that variables in @equiv are found in the signature
%% TODO: copy types from target (if missing) when using @equiv
%% TODO: recognize 'behaviour_info/1' functions?

%% <!ELEMENT module (behaviour*, description?, author*, copyright?,
%%                   version?, since?, deprecated?, see*, reference*,
%%                   typedecls?, functions)>
%% <!ATTLIST module
%%   name CDATA #REQUIRED
%%   private NMTOKEN(yes | no) #IMPLIED
%%   hidden NMTOKEN(yes | no) #IMPLIED
%%   root CDATA #IMPLIED>
%% <!ELEMENT behaviour (#PCDATA)>
%% <!ATTLIST behaviour
%%   href CDATA #IMPLIED>
%% <!ELEMENT description (briefDescription, fullDescription?)>
%% <!ELEMENT briefDescription (#PCDATA)>
%% <!ELEMENT fullDescription (#PCDATA)>
%% <!ELEMENT author EMPTY>
%% <!ATTLIST author
%%   name CDATA #REQUIRED
%%   email CDATA #IMPLIED
%%   website CDATA #IMPLIED>
%% <!ELEMENT version (#PCDATA)>
%% <!ELEMENT since (#PCDATA)>
%% <!ELEMENT copyright (#PCDATA)>
%% <!ELEMENT deprecated (description)>
%% <!ELEMENT see (#PCDATA)>
%% <!ATTLIST see
%%   name CDATA #REQUIRED
%%   href CDATA #IMPLIED>
%% <!ELEMENT reference (#PCDATA)>
%% <!ELEMENT typedecls (typedecl+)>
%% <!ELEMENT typedecl (typedef, description?)>
%% <!ELEMENT functions (function+)>

%% NEW-OPTIONS: private, hidden
%% DEFER-OPTIONS: edoc_extract:source/4

module(Module, Entries, Env, Opts) ->
    Name = atom_to_list(Module#module.name),
    HeaderEntry = get_entry(module, Entries),
    HeaderTags = HeaderEntry#entry.data,
    AllTags = get_all_tags(Entries),
    Out = {module, ([{name, Name},
		     {root, Env#env.root}]
		    ++ case is_private(HeaderTags) of
			   true -> [{private, "yes"}];
			   false -> []
		       end
		    ++ case is_hidden(HeaderTags) of
			   true -> [{hidden, "yes"}];
			   false -> []
		       end),
	   (behaviours(Module#module.attributes, Env)
	    ++ get_doc(HeaderTags)
	    ++ authors(HeaderTags)
	    ++ get_version(HeaderTags)
	    ++ get_since(HeaderTags)
	    ++ get_copyright(HeaderTags)
	    ++ get_deprecated(HeaderTags)
	    ++ sees(HeaderTags, Env)
	    ++ references(HeaderTags)
	    ++ [{typedecls, types(AllTags, Env)},
		{functions, functions(Entries, Env, Opts)}])
	  },
    xmerl_lib:expand_element(Out).

get_all_tags(Es) ->
    lists:flatmap(fun (#entry{data = Ts}) -> Ts end, Es).

is_private(Ts) ->
    get_tags(private, Ts) =/= [].

description([]) ->
    [];
description(Desc) ->
    ShortDesc = edoc_lib:get_first_sentence(Desc),
    [{description,
      [{briefDescription, ShortDesc},
       {fullDescription, Desc}]}].

types(Tags, Env) ->
    [{typedecl, [{label, edoc_types:to_label(Def)}],
      [edoc_types:to_xml(Def, Env)] ++ description(Doc)}
     || #tag{name = type, data = {Def, Doc}} <- Tags].

functions(Es, Env, Opts) ->
    Private = proplists:get_bool(private, Opts),
    Hidden = proplists:get_bool(hidden, Opts),
    [function(N, As, Export, Ts, Env)
     || #entry{name = {_,_}=N, args = As, export = Export, data = Ts}
	    <- [E || E <- Es, function_filter(E, Private, Hidden)]].

%% Note that only entries whose names have the form {_,_} are functions.
function_filter(#entry{name = {_,_}, export = Export, data = Ts},
		Private, Hidden) ->
    ((Export andalso not is_private(Ts)) orelse Private)
	andalso ((not is_hidden(Ts)) orelse Hidden);
function_filter(_, _, _) ->
    false.

is_hidden(Ts) ->
    get_tags(hidden, Ts) =/= [].

%% <!ELEMENT function (args, typespec?, equiv?, description?, since?,
%%                     deprecated?, see*)>
%% <!ATTLIST function
%%   name CDATA #REQUIRED
%%   arity CDATA #REQUIRED
%%   exported NMTOKEN(yes | no) #REQUIRED
%%   label CDATA #IMPLIED>
%% <!ELEMENT args (arg*)>
%% <!ELEMENT arg description?>
%% <!ATTLIST arg name CDATA #REQUIRED>
%% <!ELEMENT equiv (expr, see?)>
%% <!ELEMENT expr (#PCDATA)>

function({N, A}, As, Export, Ts, Env) ->
    {As1, Spec} = signature(Ts, As, Env),
    {function, [{name, atom_to_list(N)},
		{arity, integer_to_list(A)},
      		{exported, case Export of
			       true -> "yes";
			       false -> "no"
			   end},
		{label, edoc_refs:to_label(edoc_refs:function(N, A))}],
     [{args, [{arg, [{argName, [atom_to_list(A)]}]} || A <- As1]}]
     ++ Spec
     ++ get_equiv(Ts, Env)
     ++ get_doc(Ts)
     ++ get_since(Ts)
     ++ get_deprecated(Ts, N, A, Env)
     ++ sees(Ts, Env)
     ++ references(Ts)
    }.

get_equiv(Ts, Env) ->
    case get_tags(equiv, Ts) of
	[Equiv] ->
	    Expr = Equiv#tag.data,
	    See = case get_expr_ref(Equiv#tag.data) of
		      none -> [];
		      Ref ->
			  [see(Ref, [edoc_refs:to_string(Ref)], Env)]
		  end,
	    [{equiv, [{expr, [erl_prettypr:format(Expr)]} | See]}];
	[] ->
	    []
    end.

get_doc(Ts) ->
    case get_tags(doc, Ts) of
	[T] ->
	    description(T#tag.data);
	[] ->
	    []
    end.

get_copyright(Ts) ->
    get_pcdata_tag(copyright, Ts).

get_version(Ts) ->
    get_pcdata_tag(version, Ts).

get_since(Ts) ->
    get_pcdata_tag(since, Ts).

get_pcdata_tag(Tag, Ts) ->
    case get_tags(Tag, Ts) of
	[T] ->
	    [{Tag, [T#tag.data]}];
	[] ->
	    []
    end.

%% TODO: use info from '-deprecated(...)' (xref-)declarations.
%% Deprecation declarations for xref:
%%
%%   -deprecated(Info).
%%       Info = Spec | [Spec]
%%       Spec = module | {F,A} | {F,A,Details}}
%%       Details = next_version | next_major_release | eventually
%%                 (EXTENSION: | string() | {M1,F1,A1}}

get_deprecated(Ts) ->
    case get_tags(deprecated, Ts) of
	[T] ->
	    [{deprecated, description(T#tag.data)}];
	[] ->
	    []
    end.

get_deprecated(Ts, F, A, Env) ->
    case get_deprecated(Ts) of
	[] ->
	    M = Env#env.module,
	    case erl_internal:obsolete(M, F, A) of
		{true, {M1, F1, A1}} ->
		    Text = if M == M1 ->
				   io_lib:fwrite("~w/~w", [F1, A1]);
			      true ->
				   io_lib:fwrite("~w:~w/~w",
						 [M1, F1, A1])
			   end,
		    Ref = if M == M1 ->
				  edoc_refs:function(F1, A1);
			     true ->
				  edoc_refs:function(M1, F1, A1)
			  end,
		    Desc = ["Use ",
			    {a, href(Ref, Env), [{code, [Text]}]},
			    " instead."],
		    [{deprecated, description(Desc)}];
		{true, Text} ->
		    [{deprecated, description([Text])}];
		_ ->
		    []
	    end;
	Es ->
	    Es
    end.

get_expr_ref(Expr) ->
    case catch {ok, erl_syntax_lib:analyze_application(Expr)} of
	{ok, {F, A}} when atom(F), integer(A) ->
	    edoc_refs:function(F, A);
 	{ok, {M, {F, A}}} when atom(M), atom(F), integer(A) ->
 	    edoc_refs:function(M, F, A);
	_ ->
	    none
    end.

authors(Ts) ->
    [author(Info) || #tag{data = Info} <- get_tags(author, Ts)].

%% <!ATTLIST author
%%   name CDATA #REQUIRED
%%   email CDATA #IMPLIED
%%   website CDATA #IMPLIED>

author({Name, Mail, URI}) ->
    %% At least one of Name and Mail must be nonempty in the tag.
    {author, ([{name, if Name == "" -> Mail;
			 true -> Name
		      end}]
	      ++ if Mail == "" ->
			 case lists:member($@, Name) of
			     true -> [{email, Name}];
			     false -> []
			 end;
		    true -> [{email, Mail}]
		 end
	      ++ if URI == "" -> [];
		    true -> [{website, URI}]
		 end), []}.

behaviours(As, Env) ->
    [{behaviour, href(edoc_refs:module(B), Env), [atom_to_list(B)]}
     || {behaviour, B} <- As, is_atom(B)].

sees(Tags, Env) ->
    Ts = get_tags(see, Tags),
    Rs = lists:keysort(1, [Data || #tag{data = Data} <- Ts]),
    [see(Ref, XML, Env) || {Ref, XML} <- Rs].

see(Ref, [], Env) ->
    see(Ref, [edoc_refs:to_string(Ref)], Env);
see(Ref, XML, Env) ->
    {see, [{name, edoc_refs:to_string(Ref)}] ++ href(Ref, Env), XML}.

href(Ref, Env) ->
    [{href, edoc_refs:get_uri(Ref, Env)}]
	++ case edoc_refs:is_top(Ref, Env) of
	       true ->
		   [{target, "_top"}];
	       false ->
		   []
	   end.

references(Tags) ->
    [{reference, XML} || #tag{data = XML} <- get_tags(reference, Tags)].

signature(Ts, As, Env) ->
    case get_tags(spec, Ts) of
	[T] ->
	    Spec = T#tag.data,
	    As0 = edoc_types:arg_names(Spec),
	    As1 = merge_argnames(As0, As),  % choose spec before code
	    Spec1 = edoc_types:set_arg_names(Spec, As1),
	    {As1, [edoc_types:to_xml(Spec1, Env)]};
	[] ->
	    S = sets:new(),
	    {fix_argnames(As, S, 1), []}
    end.

%% Names are chosen from the first list if possible.

merge_argnames(As, As1) ->
    merge_argnames(As, As1, sets:new(), 1).

merge_argnames(['_' | As], ['_' | As1], S, N) ->
    A = make_name(N, S),
    [A | merge_argnames(As, As1, sets:add_element(A, S), N + 1)];
merge_argnames(['_' | As], [A | As1], S, N) ->
    [A | merge_argnames(As, As1, sets:add_element(A, S), N + 1)];
merge_argnames([A | As], [_ | As1], S, N) ->
    [A | merge_argnames(As, As1, sets:add_element(A, S), N + 1)];
merge_argnames([], [], _S, _N) ->
    [].

fix_argnames(['_' | As], S, N) ->
    A = make_name(N, S),
    [A | fix_argnames(As, sets:add_element(A, S), N + 1)];
fix_argnames([A | As], S, N) ->
    [A | fix_argnames(As, sets:add_element(A, S), N + 1)];
fix_argnames([], _S, _N) ->
    [].

make_name(N, S) ->
    make_name(N, S, "X").

make_name(N, S, Base) ->
    A = list_to_atom(Base ++ integer_to_list(N)),
    case sets:is_element(A, S) of
 	true ->
 	    make_name(N, S, Base ++ "x");
 	false ->
 	    A
    end.

get_entry(Name, [#entry{name = Name} = E | _Es]) -> E;
get_entry(Name, [_ | Es]) -> get_entry(Name, Es).

get_tags(Tag, [#tag{name = Tag} = T | Ts]) -> [T | get_tags(Tag, Ts)];
get_tags(Tag, [_ | Ts]) -> get_tags(Tag, Ts);
get_tags(_, []) -> [].

%% ---------------------------------------------------------------------

type(T, Env) ->
    xmerl_lib:expand_element({type, [edoc_types:to_xml(T, Env)]}).

package(Package, Tags, Env, _Opts) ->
    Env1 = Env#env{package = Package,
		   root = edoc_refs:relative_package_path('', Package)},
    xmerl_lib:expand_element(package_1(Package, Tags, Env1)).

package_1(Package, Tags, Env) ->
    {package, [{root, Env#env.root}],
     ([{packageName, [atom_to_list(Package)]}]
      ++ get_doc(Tags)
      ++ authors(Tags)
      ++ get_version(Tags)
      ++ get_since(Tags)
      ++ get_copyright(Tags)
      ++ get_deprecated(Tags)
      ++ sees(Tags, Env)
      ++ references(Tags))
    }.

overview(Title, Tags, Env, _Opts) ->
    Env1 = Env#env{package = '',
		   root = ""},
    xmerl_lib:expand_element(overview_1(Title, Tags, Env1)).

overview_1(Title, Tags, Env) ->
    {overview, [{root, Env#env.root}],
     ([{title, [get_title(Tags, Title)]}]
      ++ get_doc(Tags)
      ++ authors(Tags)
      ++ get_version(Tags)
      ++ get_since(Tags)
      ++ get_copyright(Tags)
      ++ sees(Tags, Env)
      ++ references(Tags))
    }.

get_title(Ts, Default) ->
    case get_tags(title, Ts) of
	[T] ->
	    T#tag.data;
	[] ->
	    Default
    end.
