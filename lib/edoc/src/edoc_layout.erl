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
%% @author Richard Carlsson <richardc@csd.uu.se>
%% @copyright 2001-2002 Richard Carlsson
%% @see edoc
%% @end
%% =====================================================================

%% TODO: generate navigation links, at least back to index.html
%% TODO: mark deprecated functions in function index.

%% @doc The standard HTML layout module for EDoc. See the {@link edoc}
%% module for details on usage.

%% Note that this is written so that it is *not* depending on edoc.hrl!

-module(edoc_layout).

-export([module/2, package/2, overview/2, type/1]).

-import(edoc_report, [report/2]).

-include("xmerl.hrl").

-define(HTML_EXPORT, xmerl_html).
-define(DEFAULT_XML_EXPORT, ?HTML_EXPORT).
-define(STYLESHEET, "stylesheet.css").
-define(NL, "\n").
-define(DESCRIPTION_TITLE, "Description").
-define(DESCRIPTION_LABEL, "description").
-define(DATA_TYPES_TITLE, "Data Types").
-define(DATA_TYPES_LABEL, "types").
-define(FUNCTION_INDEX_TITLE, "Function Index").
-define(FUNCTION_INDEX_LABEL, "index").
-define(FUNCTIONS_TITLE, "Function Details").
-define(FUNCTIONS_LABEL, "functions").


%% @doc The layout function.
%%
%% <p>Options to the standard layout:
%% <dl>
%%  <dt>{@type {index_columns, integer()@}}
%%  </dt>
%%  <dd>Specifies the number of column pairs used for the function
%%      index tables. The default value is 1.
%%  </dd>
%%  <dt>{@type {stylesheet, string()@}}
%%  </dt>
%%  <dd>Specifies the URI used for referencing the stylesheet. The
%%      default value is `"stylesheet.css"'. If an empty string is
%%      specified, no stylesheet reference will be generated.
%%  </dd>
%%  <dt>{@type {xml_export, Module::atom()@}}
%%  </dt>
%%  <dd>Specifies an {@link //xmerl. `xmerl'} callback module to be
%%      used for exporting the documentation. See {@link
%%      //xmerl/xmerl:export_simple/3} for details.
%%  </dd>
%% </dl></p>
%%
%% @see edoc:layout/2

-record(opts, {root, stylesheet, index_columns}).

%% NEW-OPTIONS: xml_export, index_columns, stylesheet

module(Element, Options) ->
    XML = layout_module(Element, init_opts(Element, Options)),
    Export = proplists:get_value(xml_export, Options,
				 ?DEFAULT_XML_EXPORT),
    xmerl:export_simple([XML], Export, []).

% Put layout options in a data structure for easier access.

init_opts(Element, Options) ->
    R = #opts{root = get_attrval(root, Element),
	      index_columns = proplists:get_value(index_columns,
						  Options, 1)
	     },
    case proplists:get_value(stylesheet, Options) of
	undefined ->
	    S = edoc_lib:join_uri(R#opts.root, ?STYLESHEET),
	    R#opts{stylesheet = S};
	"" ->
	    R;  % don't use any stylesheet
	S when list(S) ->
	    R#opts{stylesheet = S}; 
	_ ->
	    report("bad value for option `stylesheet'.", []),
	    exit(error)
    end.


%% =====================================================================
%% XML-BASED LAYOUT ENGINE
%% =====================================================================

%% We assume that we have expanded XML data.

%% <!ELEMENT module (behaviour*, description?, author*, copyright?,
%%                   version?, since?, deprecated?, see*, reference*,
%%                   typedecls?, functions)>
%% <!ATTLIST module
%%   name CDATA #REQUIRED
%%   private NMTOKEN(yes | no) #IMPLIED
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
%% <!ELEMENT functions (function+)>

layout_module(#xmlElement{name = module, content = Es}=E, Opts) ->
    Name = get_attrval(name, E),
    Title = io_lib:format("Module ~s", [Name]),
    Desc = get_content(description, Es),
    ShortDesc = get_content(briefDescription, Desc),
    FullDesc = get_content(fullDescription, Desc),
    Functions = [E || E <- get_content(functions, Es)],
    SortedFs = lists:sort([{function_name(E), E} || E <- Functions]),
    Types = get_content(typedecls, Es),
    SortedTs = lists:sort([{type_name(E), E} || E <- Types]),
    Body = ([?NL, {h1, [Title]}, ?NL]
	    ++ ShortDesc
	    ++ copyright(Es)
	    ++ deprecated(Es, "module")
	    ++ doc_index(FullDesc, Functions, Types)
	    ++ version(Es)
	    ++ since(Es)
	    ++ behaviours(Es)
	    ++ authors(Es)
	    ++ references(Es)
	    ++ sees(Es)
	    ++ if FullDesc == [] -> [];
		  true -> [{h2, [{a, [{name, "description"}],
				  ["Description"]}]}
			   | FullDesc]
	       end
	    ++ types(SortedTs)
	    ++ function_index(SortedFs, Opts#opts.index_columns)
	    ++ functions(SortedFs)),
    xhtml(Title, stylesheet(Opts), Body).

stylesheet(Opts) ->
    case Opts#opts.stylesheet of
	undefined ->
	    [];
	CSS ->
	    [{link, [{rel, "stylesheet"},
		     {type, "text/css"},
		     {href, CSS}], []},
	     ?NL]
    end.

doc_index(FullDesc, Functions, Types) ->
    case doc_index_rows(FullDesc, Functions, Types) of
	[] -> [];
	Rs ->
	    [{ul, [{li, [{a, [{href, local_label(R)}], [T]}]}
		   || {T, R} <- Rs]}]
    end.

doc_index_rows(FullDesc, Functions, Types) ->
    (if FullDesc == [] -> [];
	true -> [{?DESCRIPTION_TITLE, ?DESCRIPTION_LABEL}]
     end
     ++ if Types == [] -> [];
	   true -> [{?DATA_TYPES_TITLE, ?DATA_TYPES_LABEL}]
	end
     ++ if Functions == [] -> [];
	   true -> [{?FUNCTION_INDEX_TITLE, ?FUNCTION_INDEX_LABEL},
		    {?FUNCTIONS_TITLE, ?FUNCTIONS_LABEL}]
	end).

function_index(Fs, Cols) ->
    case function_index_rows(Fs, Cols, []) of
	[] -> [];
	Rows ->
	    [?NL,
	     {h2, [{a, [{name, ?FUNCTION_INDEX_LABEL}],
		    [?FUNCTION_INDEX_TITLE]}]},
	     ?NL,
	     {table, [{width, "100%"}, {border, 1}], Rows},
	     ?NL]
    end.

function_index_rows(Fs, Cols, Title) ->
    Rows = (length(Fs) + (Cols - 1)) div Cols,
    (if Title == [] -> [];
	true -> [{tr, [{th, [{colspan, Cols * 2}, {align, left}],
			[Title]}]},
		 ?NL]
     end
     ++ lists:flatmap(fun index_row/1,
		      edoc_lib:transpose(edoc_lib:segment(Fs, Rows)))).

index_row(Fs) ->
    [{tr, lists:flatmap(fun index_col/1, Fs)}, ?NL].

index_col({Name, F=#xmlElement{content = Es}}) ->
    [{td, [{valign, "top"}],
      label_href(function_header(Name, F, "*"), F)},
     {td, index_desc(Es)}].

index_desc(Es) ->
    Desc = get_content(description, Es),
    case get_content(briefDescription, Desc) of
	[] ->
	    equiv(Es);    % no description at all if no equiv
	ShortDesc ->
	    ShortDesc
    end.

label_href(Content, F) ->
    case get_attrval(label, F) of
	"" -> Content;
	Ref -> [{a, [{href, local_label(Ref)}], Content}]
    end.

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

functions(Fs) ->
    Es = lists:flatmap(fun ({Name, E}) -> function(Name, E) end, Fs),
    if Es == [] -> [];
       true ->
	    [?NL,
	     {h2, [{a, [{name, ?FUNCTIONS_LABEL}], [?FUNCTIONS_TITLE]}]},
	     ?NL | Es]
    end.

function(Name, E=#xmlElement{content = Es}) ->
    ([?NL,
      {h3, label_anchor(function_header(Name, E, " (private)"), E)},
      ?NL]
     ++ case typespec(get_content(typespec, Es)) of
	    [] ->
		signature(get_content(args, Es),
			  get_attrval(name, E));
	    Spec -> Spec
	end
     ++ equiv(Es)
     ++ deprecated(Es, "function")
     ++ fulldesc(Es)
     ++ since(Es)
     ++ sees(Es)).

function_name(E) ->
    get_attrval(name, E) ++ "/" ++ get_attrval(arity, E).

function_header(Name, E, Private) ->
    case is_exported(E) of
	true -> [Name];
	false -> [Name, Private]
    end.

is_exported(E) ->
    case get_attrval(exported, E) of
 	"yes" -> true;
 	_ -> false
    end.

label_anchor(Content, E) ->
    case get_attrval(label, E) of
	"" -> Content;
	Ref -> [{a, [{name, Ref}], Content}]
    end.

%% <!ELEMENT args (arg*)>
%% <!ELEMENT arg (argName, description?)>
%% <!ELEMENT argName (#PCDATA)>

%% This is currently only done for functions without type spec.

signature(Es, Name) -> 
    [{tt, [Name, "("] ++ seq(fun arg/1, Es) ++ [") -> term()", ?NL]}].

arg(#xmlElement{content = Es}) ->
    [get_text(argName, Es)].

%% <!ELEMENT typespec (erlangName, type, localdef*)>

typespec([]) -> [];
typespec(Es) ->
    [{p, ([{tt, ([t_name(get_elem(erlangName, Es))]
		 ++ t_utype(get_elem(type, Es)))}]
	  ++ local_defs(get_elem(localdef, Es)))},
     ?NL].

%% <!ELEMENT typedecl (typedef, description?)>
%% <!ELEMENT typedef (erlangName, argtypes, type?, localdef*)>

types([]) -> [];
types(Ts) ->
    Es = lists:flatmap(fun ({Name, E}) -> typedecl(Name, E) end, Ts),
    if Es == [] -> [];
       true ->
	    [?NL,
	     {h2, [{a, [{name, ?DATA_TYPES_LABEL}],
		    [?DATA_TYPES_TITLE]}]},
	     ?NL | Es]
    end.

typedecl(Name, E=#xmlElement{content = Es}) ->
    ([?NL, {h3, label_anchor([Name, "()"], E)}, ?NL]
     ++ [{p, typedef(get_content(typedef, Es))}, ?NL]
     ++ fulldesc(Es)).

type_name(#xmlElement{content = Es}) ->
    t_name(get_elem(erlangName, get_content(typedef, Es))).

typedef(Es) ->
    Name = ([t_name(get_elem(erlangName, Es)), "("]
  	    ++ seq(fun t_utype_elem/1, get_content(argtypes, Es), [")"])),
    (case get_elem(type, Es) of
 	 [] -> [{b, ["abstract datatype"]}, ": ", {tt, Name}];
 	 Type ->
	     [{tt, Name ++ [" = "] ++ t_utype(Type)}]
     end
     ++ local_defs(get_elem(localdef, Es))).

local_defs([]) -> [];
local_defs(Es) ->
    [?NL, {ul, [{li, [{tt, localdef(E)}]} || E <- Es]}].

localdef(E = #xmlElement{content = Es}) ->
    (case get_elem(typevar, Es) of
	 [] -> 
	     label_anchor(t_abstype(get_content(abstype, Es)), E);
	 [V] ->
	     t_var(V)
     end
     ++ [" = "] ++ t_utype(get_elem(type, Es))).

fulldesc(Es) ->
    case get_content(fullDescription, get_content(description, Es)) of
	[] -> [?NL];
	Desc -> [{p, Desc}, ?NL]
    end.

sees(Es) ->
    case get_elem(see, Es) of
	[] -> [];
	Es1 ->
	    [{p, [{b, ["See also:"]}, " "] ++ seq(fun see/1, Es1, ["."])},
	     ?NL]
    end.

see(E=#xmlElement{content = Es}) ->
    see(E, Es).

see(E, Es) ->
    case href(E) of
	[] -> Es;
	Ref ->
	    [{a, Ref, Es}]
    end.

href(E) ->
    case get_attrval(href, E) of
	"" -> [];
	URI ->
	    T = case get_attrval(target, E) of
		    "" -> [];
		    S -> [{target, S}]
		end,
	    [{href, URI} | T]
    end.

equiv(Es) ->
    case get_content(equiv, Es) of
	[] -> [];
	Es1 ->
	    case get_content(expr, Es1) of
		[] -> [];
		[Expr] ->
		    Expr1 = [{tt, [Expr]}],
		    Expr2 = case get_elem(see, Es1) of
				[] ->
				    Expr1;
				[E=#xmlElement{}] ->
				    see(E, Expr1)
			    end,
		    [{p, ["Equivalent to "] ++ Expr2 ++ ["."]}, ?NL]
	    end
    end.

copyright(Es) ->
    case get_content(copyright, Es) of
	[] -> [];
	Es1 ->
	    [{p, ["Copyright \251 " | Es1]}, ?NL]
    end.

version(Es) ->
    case get_content(version, Es) of
	[] -> [];
	Es1 ->
	    [{p, [{b, ["Version:"]}, " " | Es1]}, ?NL]
    end.

since(Es) ->
    case get_content(since, Es) of
	[] -> [];
	Es1 ->
	    [{p, [{b, ["Introduced in:"]}, " " | Es1]}, ?NL]
    end.

deprecated(Es, S) ->
    Es1 = get_content(description, get_content(deprecated, Es)),
    case get_content(fullDescription, Es1) of
	[] -> [];
	Es2 ->
	    [{p, [{b, ["This " ++ S ++ " is deprecated:"]}, " " | Es2]},
	     ?NL]
    end.

behaviours(Es) ->
    case get_elem(behaviour, Es) of
	[] -> [];
	Es1 ->
	    [{p, [{b, ["Behaviour:"]}, " "]
	      ++ seq(fun behaviour/1, Es1, ["."])},
	     ?NL]
    end.

behaviour(E=#xmlElement{content = Es}) ->
    see(E, [{tt, Es}]).

authors(Es) ->
    case get_elem(author, Es) of
	[] -> [];
	Es1 ->
	    [{p, [{b, ["Authors:"]}, " "] ++ seq(fun author/1, Es1, ["."])},
	     ?NL]
    end.

%% <!ATTLIST author
%%   name CDATA #REQUIRED
%%   email CDATA #IMPLIED
%%   website CDATA #IMPLIED>

author(E=#xmlElement{}) ->
    Name = get_attrval(name, E),
    Mail = get_attrval(email, E),
    URI = get_attrval(website, E),
    (if Name == Mail ->
	     [{a, [{href, "mailto:" ++ Mail}],[{tt, [Mail]}]}];
	true ->
	     if Mail == "" -> [Name];
		true -> [Name, " (", {a, [{href, "mailto:" ++ Mail}],
				      [{tt, [Mail]}]}, ")"]
	     end
     end
     ++ if URI == "" ->
		[];
	   true ->
		[" [", {em, ["web site:"]}, " ",
		 {tt, [{a, [{href, URI}, {target, "_top"}], [URI]}]},
		 "]"]
	end).

references(Es) ->
    case get_elem(reference, Es) of
	[] -> [];
	Es1 ->
	    [{p, [{b, ["References"]},
		  {ul, [{li, C} || #xmlElement{content = C} <- Es1]}]},
	     ?NL]
    end.

t_name([E]) ->
    N = get_attrval(name, E),
    case get_attrval(module, E) of
	"" -> N;
	M ->
	    S = M ++ ":" ++ N,
	    case get_attrval(app, E) of
		"" -> S;
		A -> "//" ++ A ++ "/" ++ S
	    end
    end.

t_utype([E]) ->
    t_utype_elem(E).

t_utype_elem(E=#xmlElement{content = Es}) ->
    case get_attrval(name, E) of
	"" -> t_type(Es);
	Name ->
	    T = t_type(Es),
	    case T of
		[Name] -> T;    % avoid generating "Foo::Foo"
		T -> [Name] ++ ["::"] ++ T
	    end
    end.

t_type([E=#xmlElement{name = typevar}]) ->
    t_var(E);
t_type([E=#xmlElement{name = atom}]) ->
    t_atom(E);
t_type([E=#xmlElement{name = integer}]) ->
    t_integer(E);
t_type([E=#xmlElement{name = float}]) ->
    t_float(E);
t_type([#xmlElement{name = nil}]) ->
    t_nil();
t_type([#xmlElement{name = list, content = Es}]) ->
    t_list(Es);
t_type([#xmlElement{name = tuple, content = Es}]) ->
    t_tuple(Es);
t_type([#xmlElement{name = 'fun', content = Es}]) ->
    t_fun(Es);
t_type([E = #xmlElement{name = abstype, content = Es}]) ->
    T = t_abstype(Es),
    see(E, T);
t_type([#xmlElement{name = union, content = Es}]) ->
    t_union(Es).

t_var(E) ->
    [get_attrval(name, E)].

t_atom(E) ->
    [get_attrval(value, E)].

t_integer(E) ->
    [get_attrval(value, E)].

t_float(E) ->
    [get_attrval(value, E)].

t_nil() ->
    ["[]"].

t_list(Es) ->
    ["["] ++ t_utype(get_elem(type, Es)) ++ ["]"].

t_tuple(Es) ->
    ["{"] ++ seq(fun t_utype_elem/1, Es, ["}"]).

t_fun(Es) ->
    ["("] ++ seq(fun t_utype_elem/1, get_content(argtypes, Es),
		 [") -> "] ++ t_utype(get_elem(type, Es))).

t_abstype(Es) ->
    ([t_name(get_elem(erlangName, Es)), "("]
     ++ seq(fun t_utype_elem/1, get_elem(type, Es), [")"])).

t_union(Es) ->
    seq(fun t_utype_elem/1, Es, " | ", []).

seq(F, Es) ->
    seq(F, Es, []).

seq(F, Es, Tail) ->
    seq(F, Es, ", ", Tail).

seq(F, [E], _Sep, Tail) ->
    F(E) ++ Tail;
seq(F, [E | Es], Sep, Tail) ->
    F(E) ++ [Sep] ++ seq(F, Es, Sep, Tail);
seq(_F, [], _Sep, Tail) ->
    Tail.

get_elem(Name, [#xmlElement{name = Name} = E | Es]) ->
    [E | get_elem(Name, Es)];
get_elem(Name, [_ | Es]) ->
    get_elem(Name, Es);
get_elem(_, []) ->
    [].

get_attr(Name, [#xmlAttribute{name = Name} = A | As]) ->
    [A | get_attr(Name, As)];
get_attr(Name, [_ | As]) ->
    get_attr(Name, As);
get_attr(_, []) ->
    [].

get_attrval(Name, #xmlElement{attributes = As}) ->
    case get_attr(Name, As) of
	[#xmlAttribute{value = V}] ->
	    V;
	[] -> ""
    end.

get_content(Name, Es) ->
    case get_elem(Name, Es) of
	[#xmlElement{content = Es1}] ->
	    Es1;
	[] -> []
    end.

get_text(Name, Es) ->
    case get_content(Name, Es) of
	[#xmlText{value = Text}] ->
	    Text;
	[] -> ""
    end.

local_label(R) ->
    "#" ++ R.

xhtml(Title, CSS, Body) ->
    {html, [?NL,
	    {head, [?NL,
		    {title, [Title]},
		    ?NL] ++ CSS},
	    ?NL,
	    {body, [{bgcolor, "white"}], Body},
	    ?NL]
    }.

%% ---------------------------------------------------------------------

type(E) ->
    type(E, []).

type(E, Ds) ->
    xmerl:export_simple_content(t_utype_elem(E) ++ local_defs(Ds),
				?HTML_EXPORT).

package(E=#xmlElement{name = package, content = Es}, Options) ->
    Opts = init_opts(E, Options),
    Name = get_text(packageName, Es),
    Title = io_lib:fwrite("Package ~s", [Name]),
    Desc = get_content(description, Es),
%    ShortDesc = get_content(briefDescription, Desc),
    FullDesc = get_content(fullDescription, Desc),
    Body = ([?NL, {h1, [Title]}, ?NL]
%	    ++ ShortDesc
	    ++ copyright(Es)
	    ++ deprecated(Es, "package")
	    ++ version(Es)
	    ++ since(Es)
	    ++ authors(Es)
	    ++ references(Es)
	    ++ sees(Es)
	    ++ FullDesc),
    XML = xhtml(Title, stylesheet(Opts), Body),
    xmerl:export_simple([XML], ?HTML_EXPORT, []).

overview(E=#xmlElement{name = overview, content = Es}, Options) ->
    Opts = init_opts(E, Options),
    Title = get_text(title, Es),
    Desc = get_content(description, Es),
%    ShortDesc = get_content(briefDescription, Desc),
    FullDesc = get_content(fullDescription, Desc),
    Body = ([?NL, {h1, [Title]}, ?NL]
%	    ++ ShortDesc
	    ++ copyright(Es)
	    ++ version(Es)
	    ++ since(Es)
	    ++ authors(Es)
	    ++ references(Es)
	    ++ sees(Es)
	    ++ FullDesc),
    XML = xhtml(Title, stylesheet(Opts), Body),
    xmerl:export_simple([XML], ?HTML_EXPORT, []).
