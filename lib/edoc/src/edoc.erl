%% =====================================================================
%% EDoc - Erlang program documentation generator
%% 
%% Copyright (C) 2001-2002 Richard Carlsson
%%
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
%% Author contact: richardc@csd.uu.se
%% 
%% $Id$
%%
%% @doc Erlang program documentation generator.
%%
%% <p>This program allows you to write the documentation of an Erlang
%% program as comments in the source code itself, using tags on the form
%% "<code>@Name ...</code>".</p>
%%
%% <p>A tag must be the first thing on a comment line, apart from
%% leading <code>%</code> characters and whitespace. The comment must be
%% between program declarations, and not on the same line as any program
%% text. All following comment text, up to the end of the comment or the
%% next tagged line, is taken as the value of the tag.</p>
%%
%% <p>Tags are associated with the nearest following program construct
%% "of significance" (module name declarations and function
%% definitions). Other constructs are ignored, e.g.:
%% <pre>
%%   %% @doc Prints X.
%%
%%   -record(foo, {x, y, z}).
%%
%%   print(X) -> ...</pre>
%%
%% associates the <code>@doc</code> tag with the function
%% <code>print/1</code>.</p>
%%
%% <p>Note that in a comment such as:
%% <pre>
%%   % % @doc ...</pre>
%% 
%% the tag is ignored, because only the first <code>%</code> character
%% is considered "leading". This allowes tags to be "commented out".</p>
%%
%% <h3>Module tags</h3>
%%
%% <p>The following tags can be used before a module declaration:
%% <dl>
%%   <dt><code>@doc</code></dt>
%%       <dd>Describes the module, using well-formed XHTML text. The
%%       first sentence is used as a summary (see <code>@doc</code>
%%       function tags below for details). E.g.:
%% <pre>
%%    %% @doc This is a very useful module. It is a ...
%%    -module(fred).</pre></dd>
%%
%%   <dt><code>@type</code></dt>
%%       <dd>Documents an abstract data type or type alias. The text
%%       consists of a type declaration or definition, optionally
%%       followed by a period ("<code>.</code>") separator and XHTML
%%       text describing the type (i.e., its purpose, use, etc.). The
%%       first part has a form such as e.g.
%%       "<code>myList(X::integer())</code>" or "<code>mytype() = foo |
%%       bar</code>" (for a truly abstract data type, no equivalence is
%%       specified), and may be followed by further definitions
%%       (generally used for local type variables). Examples:
%% <pre>    %% @type myList(X). A special kind of lists ...</pre>
%% <pre>    %% @type filename() = string(). Atoms not allowed!</pre>
%% <pre>
%%   %% @type thing(A) = {thong, A}
%%   %%           A = term().
%%   %%   A kind of wrapper type thingy.</pre>
%%
%%      All data type descriptions are placed in a separate section of
%%      the document, regardless of where the tags are put.</dd>
%%
%%   <dt><code>@end</code></dt>
%%       <dd>The text following this tag is always ignored. Use this to
%%       mark the end of the previous tag, when necessary, as e.g. in:
%% <pre>
%%    %% ----------------------------------
%%    %% ...
%%    %% @doc ...
%%    %% ...
%%    %% @end
%%    %% ----------------------------------</pre>
%% 
%%       to avoid including the last "ruler" line in the
%%       <code>@doc</code> tag. <em>Note: using some other "dummy"
%%       <code>@</code>-tag for the same purpose might work in a
%%       particular implementation of EDoc, but is not guaranteed to.
%%       Always use <code>@end</code> to ensure portability.</em></dd>
%%
%% </dl>
%% </p>
%%
%% <h3>Function tags</h3>
%%
%% <p>The following tags can be used before a function definition:
%% <dl>
%%   <dt><code>@spec</code></dt>
%%       <dd>Used to specify the function type; see below for syntax. If
%%       the function name is included in the specification, it must
%%       match the name in the actual code.</dd>
%%
%%   <dt><code>@doc</code></dt>
%%       <dd>XHTML text describing a function or module. The first
%%       sentence of the text is used as a quick summary; this ends at
%%       the first period character ("<code>.</code>") that is followed
%%       by a whitespace character, a line break, or the end of the tag
%%       text, and is not within XML markup.</dd>
%%
%%   <dt><code>@see</code></dt>
%%       <dd>Make a reference to another function or module. Allowed
%%       forms are "<code>Module</code>", "<code>Function/Arity</code>"
%%       and "<code>Module:Function/Arity</code>".</dd>
%%
%%   <dt><code>@equiv</code></dt>
%%       <dd>Specify equivalence to another function call/expression.
%%       Must be a proper Erlang expression. (Replaces
%%       <code>@doc</code>.)</dd>
%%
%%   <dt><code>@type</code></dt>
%%       <dd>See module tags above. Placing a <code>@type</code> tag by
%%       a function definition may be convenient, but does not affect
%%       where the description is placed in the document.</dd>
%%
%%   <dt><code>@end</code></dt>
%%       <dd>See module tags above.</dd>
%% </dl>
%% </p>
%%
%% <h3>Function type specification syntax</h3>
%%
%% <table valign="top">
%%   <tr valign="top">
%%     <td><code>Spec</code></td>
%%     <td>::=</td>
%%     <td><code>Atom FunSpec Defs
%% 	   <br/>| FunSpec Defs</code></td>
%%   </tr>
%%   <tr valign="top">
%%     <td><code>FunSpec</code></td>
%%     <td>::=</td>
%%     <td><code>"(" ParamSpecs ? ")" "->" TypeSpec</code></td>
%%   </tr>
%%   <tr valign="top">
%%     <td><code>ParamSpecs</code></td>
%%     <td>::=</td>
%%     <td><code>ParamSpec
%% 	   <br/>| ParamSpec "," ParamSpecs</code></td>
%%   </tr>
%%   <tr valign="top">
%%     <td><code>ParamSpec</code></td>
%%     <td>::=</td>
%%     <td><code>TypeSpec
%% 	   <br/>| Variable "::" TypeSpec</code></td>
%%   </tr>
%%   <tr valign="top">
%%     <td><code>TypeSpec</code></td>
%%     <td>::=</td>
%%     <td><code>Type
%% 	   <br/>| Type "|" TypeSpec
%% 	   <br/>| Type "+" TypeSpec</code></td>
%%   </tr>
%%   <tr valign="top">
%%     <td><code>Type</code></td>
%%     <td>::=</td>
%%     <td><code>Variable
%% 	   <br/>| Atom
%% 	   <br/>| Integer
%% 	   <br/>| Float
%% 	   <br/>| "{" TypeSpecs ? "}"
%% 	   <br/>| "[" "]"
%% 	   <br/>| "[" TypeSpec "]"
%% 	   <br/>| FunSpec
%% 	   <br/>| Atom "(" TypeSpecs ? ")"
%% 	   <br/>| Atom ":" Atom "(" TypeSpecs ? ")"</code></td>
%%   </tr>
%%   <tr valign="top">
%%     <td><code>TypeSpecs</code></td>
%%     <td>::=</td>
%%     <td><code>TypeSpec
%% 	   <br/>| TypeSpec "," TypeSpecs</code></td>
%%   </tr>
%%   <tr valign="top">
%%     <td><code>Defs</code></td>
%%     <td>::=</td>
%%     <td><code>""
%% 	   <br/>| Def Defs</code></td>
%%   </tr>
%%   <tr valign="top">
%%     <td><code>Def</code></td>
%%     <td>::=</td>
%%     <td><code>Variable "=" TypeSpec
%% 	   <br/>| Atom "(" TypeSpecs ? ")" "=" TypeSpec</code></td>
%%   </tr>
%% </table>
%%
%% <p>Examples:
%% <pre>    %% @spec my_function(X::integer()) -> integer()</pre>
%% <pre>    %% @spec (X::integer()) -> integer()</pre>
%% <pre>    %% @spec sqrt(float()) -> float()</pre>
%% <pre>    %% @spec pair(S, T) -> {S, T}</pre>
%% <pre>
%%   %% @spec append(List, List) -> List
%%   %%           List = [term()]</pre>
%% <pre>
%%   %% @spec append(A::List, B::List) -> List
%%   %%           List = [term()]</pre>
%% <pre>
%%   %% @spec open(File::filename()) -> file_descriptor()
%%   %%           filename() = string() | atom()</pre>
%% <pre>
%%   %% @spec close(graphics:window()) -> ok</pre>
%%
%% In the above examples, <code>X</code>, <code>A</code>, <code>B</code>
%% and <code>File</code> are parameter names, used for referring to the
%% parameters from the documentation text. The <em>type variables</em>
%% <code>S</code>, <code>T</code> and <code>List</code> are used to
%% simplify the type specifications, and may be supplied with
%% definitions. It is also possible to give definitions for named types,
%% which means that the name is simply an alias. (Use the
%% <code>@type</code> tag to document abstract data types.) If a named
%% type is defined in another module, it can be referred to as
%% <code>Module:TypeName(...)</code>.</p>
%%
%% <p>If only a type variable is given for a parameter, as in
%% "<code>pair(S, T) -> ...</code>", the same variable name may
%% implicitly be used as the parameter name; there is no need to write
%% "<code>pair(S::S, T::T) -> ...</code>".</p>
%%
%% <p>Both the "<code>|</code>" and the "<code>+</code>" character may
%% be used to separate alternatives in union types; there is no semantic
%% difference. </p>
%% @end
%% =====================================================================

%% Disclaimer: This code is pretty much a hack. I have not had the time
%% to make it pretty or well structured. Please do something about this
%% if you have the time and energy.

%% TODO: add @deprecated tag (place at top of function documentations)
%% TODO: process @doc XML and handle in-line <see>...</see>-markup.
%% TODO: output internal function documentation to separate file?
%% TODO: report collisions between predefined/userdefined types.
%% TODO: for better efficiency, don't do full comment insertion?
%% TODO: make error messages more uniform; always report tag line number.
%% TODO: use "doc-files" subdirectory convention, like Javadoc.
%% TODO: output file suffix option (note that this also affects links)

-module(edoc).

-export([file/1, file/2, read_comments/1, read_comments/2,
	 read_module/1, read_module/2, forms/3, forms/4]).

-import(lists, [sort/1, reverse/1, reverse/2, append/1, keysearch/3,
		concat/1, flatten/1, member/2, foreach/2]).

-include("xmerl.hrl").


%% Data structure for module information

-record(module, {name,		% = atom()
		 functions,	% = ordset({atom(), int()})
		 exports,	% = ordset({atom(), int()})
				% | ordset({{atom(), int()}, term()})
		 attributes,	% = ordset({atom(), term()})
		 records	% = [{atom(), [{atom(), term()}]}]
		}).


%% @spec file(Name) -> ok
%% @equiv file(Name, [])

file(Name) ->
    file(Name, []).

%% @spec file(File::filename(), Options::option_list()) -> ok 
%%		filename() = file:filename()
%%		option_list() = [term()]
%%
%% @doc Reads a source code file and outputs documentation text to a
%% corresponding <code>.html</code>-file. Possible options are:
%% <dl>
%%  <dt><code>{dir, filename()}</code></dt>
%%    <dd>Specifies the output directory for the created file. (By
%%    default, the output is written to the directory of the source
%%    file.)</dd>
%%  <dt><code>{xml_export, atom()}</code></dt>
%%    <dd>Specifies the callback module used for formatting the internal
%%    XML representation as text; see <code>xmerl</code> for
%%    details. The default is <code>xmerl_html</code>.</dd>
%%  <dt><code>{index_columns, integer()}</code></dt>
%%    <dd>Specifies the number of column pairs used for the function
%%    index tables. The default value is 1.</dd>
%%  <dt><code>{stylesheet, string() | none}</code></dt>
%%    <dd>Specifies the name of the stylesheet file used. If the value
%%    is <code>none</code>, no stylesheet link will be generated.</dd>
%%  <dt><code>{preprocess, bool()}</code></dt>
%%    <dd>If the value is <code>true</code>, the source file will be
%%    read via the Erlang preprocessor (<code>epp</code>). The default
%%    value is <code>false</code>. Normally, preprocessing is not
%%    necessary for EDoc to work, but if a file contains too exotic
%%    definitions or uses of macros, it will not be possible to read it
%%    without preprocessing. <em>Note: comments in included files will
%%    not be available to EDoc.</em></dd>
%%  <dt><code>{includes, [string()]}</code></dt>
%%    <dd>Specifies a list of directory names to be searched for include
%%    files, if the <code>preprocess</code> option is turned on. The
%%    default value is the empty list. The directory of the source file
%%    is always automatically appended to the search path.</dd>
%%  <dt><code>{macros, [{atom(), term()}]}</code></dt>
%%    <dd>Specifies a list of pre-defined preprocessor macro
%%    definitions, used if the <code>preprocess</code> option is turned
%%    on. The default value is the empty list.</dd>
%% </dl>
%% @see xmerl
%% @see epp
%% @see forms/3

file(Name, Opts) ->
    Forms = read_module(Name, Opts),
    Comments = read_comments(Name, Opts),
    Text = forms(Forms, Comments, Name, Opts),
    Dir = proplists:get_value(dir, Opts, filename:dirname(Name)),
    Out = filename:join(Dir,
			filename:basename(Name, ".erl") ++ ".html"),
    {ok, FD} = file:open(Out, [write]),
    io:put_chars(FD, Text),
    file:close(FD),
    ok.


%% @spec (File) ->  [comment()]
%% @equiv read_comments(File, [])

read_comments(File) ->
    read_comments(File, []).

%% @spec read_comments(File::filename(), Options::option_list()) ->
%%           [comment()]
%%
%%	    comment() = {Line, Column, Indentation, Text}
%%	    Line = integer()
%%          Column = integer()
%%          Indentation = integer()
%%          Text = [string()]
%%
%% @doc Extracts comments from an Erlang source code file. See the
%% module <code>erl_comment_scan</code> for details on the
%% representation of comments.
%%
%% <p>Currently, no options are avaliable.</p>
%%
%% @see erl_comment_scan

read_comments(File, Opts) ->
    erl_comment_scan:file(File).


%% @spec (File) -> [syntaxTree()]
%% @equiv read_module(File, [])

read_module(Name) ->
    read_module(Name, []).

%% @spec (File::filename(), Options::option_list()) -> [syntaxTree()]
%%      syntaxTree() = erl_syntax:syntaxTree()
%%
%% @doc Reads an Erlang source file and returns the list of "source code
%% form" syntax trees. See <code>file/2</code> for options pertaining to
%% reading source code files.
%%
%% @see file/2
%% @see erl_syntax

read_module(Name, Opts) ->
    verbose("reading module `~s'.", [filename(Name)], Opts),
    case read_module_1(Name, Opts) of
	{ok, Forms} ->
	    check_forms(Forms, Name),
	    Forms;
	{error, R} ->
	    error_read_file(Name),
	    exit({error, R})
    end.

read_module_1(Name, Opts) ->
    case proplists:get_bool(preprocess, Opts) of
	true ->
	    read_module_2(Name, Opts);
	false ->
	    epp_dodger:parse_file(Name)
    end.

read_module_2(Name, Opts) ->
    Includes = proplists:append_values(includes, Opts)
	++ [filename:dirname(Name)],
    Macros = proplists:append_values(macros, Opts),
    epp:parse_file(Name, Includes, Macros).

check_forms(Fs, Name) ->
    Fun = fun (F) ->
		  case erl_syntax:type(F) of
		      error_marker ->
			  case erl_syntax:error_marker_info(F) of
			      {L, M, D} ->
				  report_error("~w: ~s",
					       [L, M:format_error(D)]);
			      _ ->
				  report_error("unknown error")
			  end,
			  exit(error);
		      _ ->
			  ok
		  end
	  end,
    foreach(Fun, Fs).


%% @spec forms(Forms, Comments::[comment()], File::filename(),
%%             Options::option_list()) -> string()
%%
%%	    Forms = syntaxTree() | [syntaxTree()]
%%
%% @doc Like <code>forms/3</code>, but first inserts the given comments
%% in the syntax trees. <code>Forms</code> must be a single syntax tree
%% of type <code>form_list</code>, or a list of syntax trees
%% representing "program forms". The syntax trees must contain valid
%% position information. (Cf. <code>read_comments/2</code>.)
%%
%% @see read_comments/2
%% @see read_module/2
%% @see forms/3

forms(Forms, Comments, File, Opts) when list(Forms) ->
    forms(erl_syntax:form_list(Forms), Comments, File, Opts);
forms(Forms, Comments, File, Opts) ->
    Tree = erl_recomment:recomment_forms(Forms, Comments),
    forms(Tree, File, Opts).

%% @spec forms(Forms, File::filename(), Options::option_list()) ->
%%           string()
%%
%%	    Forms = syntaxTree() | [syntaxTree()]
%%
%% @doc Like <code>file/2</code>, but operates directly on source code
%% syntax trees, and returns the resulting text instead of writing to a
%% file. The given <code>Forms</code> must be a single syntax tree of
%% type <code>form_list</code>, or a list of syntax trees representing
%% "program forms". The syntax trees are assumed to be already annotated
%% with comments (cf. <code>forms/4</code>). The <code>File</code>
%% argument is used for error reporting and output file name generation
%% only.
%%
%% @see file/2
%% @see forms/4
%% @see erl_recomment

forms(Forms, File, Opts) when list(Forms) ->
    forms(erl_syntax:form_list(Forms), File, Opts);
forms(Tree, File, Opts) ->
    Module = get_module_info(Tree),
    Forms = preprocess_forms(erl_syntax:form_list_elements(
			       erl_syntax:flatten_form_list(Tree))),
    {Header, Entries} = gather(Forms),
    Tags0 = tags([{module, Header} | Entries]),
    [{module, HTags} | Tags] = parse(Tags0, filename(File)),
    {Exports, Locals} = split_functions(Tags, Module),
    Data = make_layout(Module#module.name, HTags,
		       sort(Exports), sort(Locals), Opts),
    Export = proplists:get_value(xml_export, Opts, xmerl_html),
    lists:flatten(xmerl:export(Data, Export, [])).


%% This copies out any form precomments to standalone comments, and
%% strips "invisible" forms.

preprocess_forms([F | Fs]) ->
    case erl_syntax:get_precomments(F) of
	[] ->
	    filter_forms(F, Fs);
	Cs ->
	    Cs ++ filter_forms(F, Fs)
    end;
preprocess_forms([]) ->
    [].

filter_forms(F, Fs) ->
    case erl_syntax_lib:analyze_form(F) of
	comment ->
	    [F | preprocess_forms(Fs)];
	{function, _} ->
	    [F | preprocess_forms(Fs)];
	{rule, _} ->
	    [F | preprocess_forms(Fs)];
	{attribute, {module, _}} ->
	    [F | preprocess_forms(Fs)];
	_ ->
	    preprocess_forms(Fs)
    end.

%% Splitting the entries into {Exported, Local} and getting rid of local
%% functions without tags.

split_functions(Es, Module) ->
    split_functions(Es, [], [], Module).

split_functions([{Name, Ts}=E | Es], Xs, Ls, Module) ->
    case ordsets:is_element(Name, Module#module.exports) of
	true ->
	    split_functions(Es, [E | Xs], Ls, Module);
	false when Ts == [] ->
	    split_functions(Es, Xs, Ls, Module);
	false ->
	    split_functions(Es, Xs, [E | Ls], Module)
    end;
split_functions([], Xs, Ls, _) ->
    {reverse(Xs), reverse(Ls)}.

%% This parses tag contents for specific tags: '@doc' as XML, '@spec' as
%% a function specification, '@type' as an ADT descripton, and '@see' as
%% Erlang tokens.

parse([{Name, Ts} | Es], File) ->
    [{Name, parse_1(Ts, Name, File)} | parse(Es, File)];
parse([], _) ->
    [].

parse_1([{spec, L, S} | Ts], Name, File) ->
    [{spec, L, parse_spec(S, L, Name)} | parse_1(Ts, Name, File)];
parse_1([{doc, L, S} | Ts], Name, File) ->
    [{doc, L, parse_xml(S, L, File)} | parse_1(Ts, Name, File)];
parse_1([{see, L, S} | Ts], Name, File) ->
    [{see, L, parse_ref(S, L)} | parse_1(Ts, Name, File)];
parse_1([{equiv, L, S} | Ts], Name, File) ->
    [{equiv, L, parse_expr(S, L)} | parse_1(Ts, Name, File)];
parse_1([{type, L, S} | Ts], Name, File) ->
    [{type, L, parse_typedoc(S, L)} | parse_1(Ts, Name, File)];
parse_1([T | Ts], Name, File) ->
    [T | parse_1(Ts, Name, File)];
parse_1([], _, _) ->
    [].

parse_spec(S, L, {F, A}) ->
    case edoc_scanner:string(S, L) of
	{ok, Ts, _} ->
	    case edoc_parser:parse_spec(Ts) of
		{ok, Spec} ->
		    check_spec(Spec, F, A),
		    Spec;
		{error, E} ->
		    E1 = case E of
			     {L1, M, D} ->
				 io_lib:format("~w: ~s",
					       [L1, M:format_error(D)]);
			     _ ->
				 "unknown error"
			 end,
		    report_error(E1),
		    exit(error)
	    end;
	{error, E, _} ->
	    E1 = case E of
		     {_, M, D} -> M:format_error(D);
		     _ -> "unknown error"
		 end,
	    report_error(E1),
	    exit(error)
    end.

check_spec({spec, Name, Type, Ds}, F, A) ->
    case Name of
	{name, F} -> ok;
	none -> ok;
	_ ->
	    report_error("at function ~w/~w: "
			 "@spec name does not match.", [F, A]),
	    exit(error)
    end,
    {'fun', As, T} = Type,
    if length(As) /= A ->
	    report_error("at function ~w/~w: "
			 "@spec arity does not match.", [F, A]),
	    exit(error);
       true ->
	    ok
    end.

%% Parsing a piece of text as XML. Note that the parent and position
%% information in the returned tree will not be correct.

parse_xml(Text, Line, File) ->
    Text1 = "<doc>" ++ Text ++ "</doc>",
    case catch {ok, xmerl_scan:string(Text1, [{line, Line}])} of
	{ok, {E, _}} ->
	    E#xmlElement.content;
	{'EXIT', {fatal, {Reason, L, C}}} ->
	    report_error("~s:~w:~w: XML parse error: ~w.",
			 [File, L, C, Reason]),
	    exit(error);
	{'EXIT', Reason} ->
	    report_error("error in XML parser: ~W.", [Reason, 10]),
	    exit(Reason);
	Other ->
	    report_error("nocatch in XML parser: ~W.", [Other, 10]),
	    throw(Other)
    end.

%% Parsing a @see reference. Valid forms are:
%%	atom			a module name
%%	atom/integer		a local function
%%	atom:atom/integer	a function in a named module

parse_ref(S, L) ->
    case edoc_scanner:string(S, L) of
	{ok, Ts, _} ->
	    case parse_ref(Ts) of
		{ok, R} -> R;
		error ->
		    report_error("at line ~w: malformed @see reference.",
				 [L]),
		    exit(error)
	    end;
	{error, E, _} ->
	    E1 = case E of
		     {_, M, D} -> M:format_error(D);
		     _ -> "unknown error"
		 end,
	    report_error(E1),
	    exit(error)
    end.

parse_ref([{atom,_,M}, {':',_}, {atom,_,F}, {'/',_}, {integer,_,A}]) ->
    {ok, {function, M, F, A}};
parse_ref([{atom,_,F}, {'/',_}, {integer,_,A}]) ->
    {ok, {function, F, A}};
parse_ref([{atom,_,M}]) ->
    {ok, {module, M}};
parse_ref(_) ->
    error.

%% Parsing an Erlang expression.

parse_expr(S, L) ->
    case erl_scan:string(S ++ ".", L) of
	{ok, Ts, _} ->
	    case erl_parse:parse_exprs(Ts) of
		{ok, [Expr]} ->
		    Expr;
		{error, E} ->
		    E1 = case E of
			     {L1, M, D} ->
				 io_lib:format("~w: ~s",
					       [L1, M:format_error(D)]);
			     _ ->
				 "unknown error"
			 end,
		    report_error(E1),
		    exit(error)
	    end;
	{error, E, _} ->
	    E1 = case E of
		     {_, M, D} -> M:format_error(D);
		     _ -> "unknown error"
		 end,
	    report_error(E1),
	    exit(error)
    end.

parse_typedoc(S, L) ->
    {S1, S2} = split_at($., S),
    %% Note that L is not always correct for scanning S2.
    {parse_typename(S1, L), parse_xml(S2, L, "")}.

parse_typename(S, L) ->
    case edoc_scanner:string(S, L) of
	{ok, Ts, _} ->
	    case edoc_parser:parse_typedef(Ts) of
		{ok, D} ->
		    %% Keep only the LHS of the definition text
		    parse_xml(format_typedef(D), 0, "");
		{error, E} ->
		    E1 = case E of
			     {L1, M, D} ->
				 io_lib:format("~w: ~s",
					       [L1, M:format_error(D)]);
			     _ ->
				 "unknown error"
			 end,
		    report_error(E1),
		    exit(error)
	    end;
	{error, E, _} ->
	    E1 = case E of
		     {_, M, D} -> M:format_error(D);
		     _ -> "unknown error"
		 end,
	    report_error(E1),
	    exit(error)
    end.

%% This collects the tags belonging to each entry, discarding all other
%% comment text. Returns [{Name, [{{Tag, Line}, Text}]}]

tags([{Name, Es} | Fs]) ->
    Ts = scan_tags(Es),
    check_tags(Ts, Name),
    [{Name, Ts} | tags(Fs)];
tags([]) ->
    [].

%% Scanning individual comments.

scan_tags(Es) ->
    scan_tags(Es, []).

scan_tags([{L, Ss} | Es], As) ->
    scan_tags(Ss, L, Es, As);
scan_tags([], As) ->
    reverse(As).

%% Scanning the lines of a comment.

scan_tags([S | Ss], L, Es, As) ->
    scan_tags(S, Ss, L, Es, As);
scan_tags([], L, Es, As) ->
    scan_tags(Es, As).

%% Looking for a leading '@', skipping whitespace.

scan_tags([$@ | Cs], Ss, L, Es, As) -> scan_tags_0(Cs, Ss, L, Es, As);
scan_tags([$\s | Cs], Ss, L, Es, As) -> scan_tags(Cs, Ss, L, Es, As);
scan_tags([$\t | Cs], Ss, L, Es, As) -> scan_tags(Cs, Ss, L, Es, As);
scan_tags(_, Ss, L, Es, As) -> scan_tags(Ss, L + 1, Es, As).

%% Scanning text following '@', recognising valid tags.

scan_tags_0("spec" ++ Cs, Ss, L, Es, As) ->
    scan_tags_1(Cs, Ss, {spec, L}, L + 1, Es, As);
scan_tags_0("doc" ++ Cs, Ss, L, Es, As) ->
    scan_tags_1(Cs, Ss, {doc, L}, L + 1, Es, As);
scan_tags_0("see" ++ Cs, Ss, L, Es, As) ->
    scan_tags_1(Cs, Ss, {see, L}, L + 1, Es, As);
scan_tags_0("equiv" ++ Cs, Ss, L, Es, As) ->
    scan_tags_1(Cs, Ss, {equiv, L}, L + 1, Es, As);
scan_tags_0("type" ++ Cs, Ss, L, Es, As) ->
    scan_tags_1(Cs, Ss, {type, L}, L + 1, Es, As);
scan_tags_0("end" ++ Cs, Ss, L, Es, As) ->
    scan_tags_1(Cs, Ss, {'end', L}, L + 1, Es, As);
scan_tags_0(Cs, Ss, L, Es, As) ->
    report_warning("at line ~w: '@'-tag not recognized.", [L]),
    scan_tags(Ss, L + 1, Es, As).

%% Assuring tag is followed by some form of whitespace.

scan_tags_1([$\s | Cs], Ss, T, L, Es, As) ->
    scan_tags_2(Ss, T, [Cs], L + 1, Es, As);
scan_tags_1([$\t | Cs], Ss, T, L, Es, As) ->
    scan_tags_2(Ss, T, [Cs], L + 1, Es, As);
scan_tags_1([], Ss, T, L, Es, As) ->
    scan_tags_2(Ss, T, [[]], L + 1, Es, As);
scan_tags_1(_, Ss, T, L, Es, As) ->
    scan_tags(Ss, L + 1, Es, As).

%% Scanning text after a recognised tag.

scan_tags_2([S | Ss], T, Ss1, L, Es, As) ->
    scan_tags_2(S, [], Ss, T, Ss1, L, Es, As);
scan_tags_2([], {T, L1}, Ss1, L, Es, As) ->
    scan_tags(Es, [{T, L1, append_lines(reverse(Ss1))} | As]).

%% Scanning tag text until end of comment or next tag line.

scan_tags_2([$\s | Cs], Cs1, Ss, T, Ss1, L, Es, As) ->
    scan_tags_2(Cs, [$\s | Cs1], Ss, T, Ss1, L, Es, As);
scan_tags_2([$\t | Cs], Cs1, Ss, T, Ss1, L, Es, As) ->
    scan_tags_2(Cs, [$\t | Cs1], Ss, T, Ss1, L, Es, As);
scan_tags_2([$@ | Cs] = Cs0, Cs1, Ss, {T, L1}, Ss1, L, Es, As) ->
    scan_tags(reverse(Cs1, Cs0), Ss, L, Es,
	      [{T, L1, append_lines(reverse(Ss1))} | As]);
scan_tags_2(Cs0, Cs1, Ss, T, Ss1, L, Es, As) ->
    scan_tags_2(Ss, T, [reverse(Cs1, Cs0) | Ss1], L + 1, Es, As).

check_tags(Ts, module) ->
    check_allowed_tags([doc, type, 'end'], Ts, module);
check_tags(Ts, Name) ->
    check_allowed_tags([spec, doc, equiv, see, type, 'end'], Ts, Name),
    check_single_tag(spec, Ts, Name),
    check_single_tag(doc, Ts, Name),
    check_single_tag(equiv, Ts, Name),
    check_mutex_tags([doc, equiv], Ts, Name).

check_single_tag(Tag, Ts, Name) ->
    case length([L || {T, L, _} <- Ts, T == Tag]) of
	N when N > 1 ->
	    error_multiple_tag(Tag, Name),
	    exit(error);
	_ -> ok
    end.

check_mutex_tags(Tags, Ts, Name) ->
    case length([L || {T, L, _} <- Ts, member(T, Tags)]) of
	N when N > 1 ->
	    error_mutex_tag(Tags, Name),
	    exit(error);
	_ -> ok
    end.

check_allowed_tags(Tags, Ts, Name) ->
    case [T || {T, L, _} <- Ts, not member(T, Tags)] of
	[] -> ok;
	Ts1 -> error_forbidden_tags(Ts1, Name),
	       exit(error)
    end.

%% This gathers pairs of function names and corresponding comments. Each
%% entry has the form {{Name,Arity},[{StartLine,Strings}]}.

gather(Fs) ->
    gather(Fs, [], [], none).

gather([F | Fs], Cs, As, Module) ->
    case erl_syntax_lib:analyze_form(F) of
	comment ->
	    gather(Fs, [F | Cs], As, Module);
	{function, Name} ->
	    gather(Fs, [], [{Name, text(Cs)} | As], Module);
	{rule, Name} ->
	    gather(Fs, [], [{Name, text(Cs)} | As], Module);
	{attribute, {module, _}} ->
	    gather(Fs, [], As, text(Cs));
	_ ->
	    gather(Fs, [], As, Module)    %% Drop current comments.
    end;
gather([], _, As, Module) ->
    {Module, reverse(As)}.

text(Cs) ->
    text(Cs, []).

text([C | Cs], Ss) ->
    L = erl_syntax:get_pos(C),
    text(Cs, [{L, [strip(S) || S <- erl_syntax:comment_text(C)]} | Ss]);
text([], Ss) ->
    Ss.

% %% @spec (string()) -> string()
% %%
% %% @doc Replaces leading <code>%</code> characters by spaces. For
% %% example, <code>"%%% foo"</code> -> <code>"\s\s\s foo"</code>, but
% %% <code>"% % foo"</code> -> <code>"\s % foo"</code>, since the second
% %% <code>%</code> is preceded by whitespace.

strip([$% | Cs]) -> [$\s | strip(Cs)];
strip(Cs) -> Cs.

%% =====================================================================
%% Forming an XHTML output tree.

make_layout(Name, HTags, Es, Ls, Opts) ->
    NL = #xmlText{value = "\n"},
    ETitle = #xmlElement{name = 'a',
			 attributes =
			 [#xmlAttribute{name = 'name',
					value = "exported"}],
			 content = 
			 [#xmlText{value = "Exported Functions"}]},
    Functions = [NL, NL,
		 #xmlElement{name = 'h2', content = [ETitle]}
		 | flatten([layout_fn(N, A, Ts, true)
			    || {{N, A}, Ts} <- Es])],
    LTitle = #xmlElement{name = 'a',
			 attributes =
			 [#xmlAttribute{name = 'name',
					value = "internal"}],
			 content = 
			 [#xmlText{value =
				   "Documented Internal Functions"}]},
    Locals = case flatten([layout_fn(N, A, Ts, false)
			   || {{N, A}, Ts} <- Ls]) of
		 [] -> [];
		 Ls1 ->
		     [NL, NL,
		      #xmlElement{name = 'h2', content = [LTitle]}
		      | Ls1]
	     end,
    TTitle = #xmlElement{name = 'a',
			 attributes =
			 [#xmlAttribute{name = 'name',
					value = "types"}],
			 content =
			 [#xmlText{value = "Data Types"}]},
    Types = case flatten(layout_types(HTags, Es, Ls)) of
		[] -> [];
		Ts ->
		    [NL, NL,
		     #xmlElement{name = 'h2', content = [TTitle]}
		     | Ts]
	    end,
    Title = #xmlText{value = io_lib:format("Module ~s",
					   [atom_to_list(Name)])},
    Col = proplists:get_value(index_columns, Opts, 1),
    Header = [NL, #xmlElement{name = 'h1', content = [Title]}, NL
	      | layout_header(HTags, Col, Es, Types, Ls)],
    Body = Header ++ Types ++ Functions ++ Locals,
    Head0 = [NL,
	     #xmlElement{name = 'title', content = [Title]},
	     NL],
    Head =
	case proplists:get_value(stylesheet, Opts, "stylesheet.css") of
	    none ->
		Head0;
	    CSS when list(CSS) ->
		Head0 ++
		    [#xmlElement{name = 'link',
				 attributes =
				 [#xmlAttribute{name = 'rel',
						value = "stylesheet"},
				  #xmlAttribute{name = 'type',
						value = "text/css"},
				  #xmlAttribute{name = 'href',
						value = CSS}],
				 content = []},
		     NL];
	    _ ->
		report_error("bad value for option `stylesheet'."),
		exit(error)
	end,
    BAttrs = [#xmlAttribute{name = 'bgcolor', value = "white"}],
    [#xmlElement{name = 'html',
		 content = [NL,
			    #xmlElement{name = 'head',
					content = Head},
			    NL,
			    #xmlElement{name = 'body',
					attributes = BAttrs,
					content = Body},
			    NL]}].

layout_header(Ts, Col, Es, Types, Ls) ->
    ETitle = "Exported Functions",
    Rows = if Ls == [] ->
		   index_rows(ETitle, Col, Es);
	      true ->
		   index_rows(ETitle, Col, Es) ++
		       index_rows("Internal Documented Functions",
				  Col, Ls)
	   end,
    NL = #xmlText{value = "\n"},
    [#xmlElement{name = 'ul',
		 content = [#xmlElement{name = 'li',
					content = [NL, I]}
			    || I <- quick_index(Types, Ls)]},
     NL, NL,
     #xmlElement{name = 'h2',
		 content = [#xmlText{value = "Description"}]},
     NL
     | layout_doc(Ts)] ++
	[NL, NL,
	 #xmlElement{name = 'h2',
		     content =
		     [#xmlElement{name = 'a',
				  attributes =
				  [#xmlAttribute{name = 'name',
						 value = "index"}],
				  content =
				  [#xmlText{value =
					    "Function Index"}]}]},
	 NL, NL,
	 #xmlElement{name = 'table',
		     attributes = [#xmlAttribute{name = 'width',
						 value = "100%"},
				   #xmlAttribute{name = 'border',
						 value = 1}],
		     content = Rows}].

quick_index(Types, Locals) ->
    Rs0 = [{"Function index", "index"},
	   {"Exported functions", "exported"}],
    Rs1 = case Types of
	     [] -> Rs0;
	     _ -> Rs0 ++ [{"Data Types", "types"}]
	 end,
    Rs = case Locals of
	     [] -> Rs1;
	     _ -> Rs1 ++ [{"Documented Internal Functions", "internal"}]
	 end,
    [#xmlElement{name = 'a',
		 attributes = [#xmlAttribute{name = 'href',
					     value = "#" ++ R}],
		 content = [#xmlText{value = T}]}
     || {T, R} <- Rs].

index_rows(Text, Col, Fs) ->
    Rows = (length(Fs) + (Col - 1)) div Col,
    Title = #xmlText{value = Text},
    Head = #xmlElement{name = 'th',
		       attributes = [#xmlAttribute{name = 'colspan',
						   value = Col * 2},
				     #xmlAttribute{name = 'align',
						   value = "left"}],
		       content = [Title]},
    NL = #xmlText{value = "\n"},
    [#xmlElement{name = 'tr', content = [Head]},
     NL
     | append([[#xmlElement{name = 'tr',
			    content = append(index_cols(Row))},
		NL]
	       || Row <- transpose(segment(Fs, Rows))])].

index_cols(Fs) ->
    [begin
	 F = format_function_name(N, A),
	 R = "#" ++ format_function_fragment(N, A),
	 E = #xmlElement{name = 'a',
			 attributes = [#xmlAttribute{name = 'href',
						     value = R}],
			 content = [#xmlText{value = F}]},
	 [#xmlElement{name = 'td', content = [E]},
	  #xmlElement{name = 'td', content = get_summary(Ts)}]
     end
     || {{N, A}, Ts} <- Fs].

layout_fn(N, A, Ts, Auto) ->
    case layout_spec(Ts, N, A, Auto) of
	none -> [];
	Spec ->
	    NL = #xmlText{value = "\n"},
	    D = [#xmlElement{name = 'code', content = Spec}],
	    Rs = case layout_refs(Ts) of
		     [] -> [];
		     Rs1 ->
			 Rh = #xmlText{value = "See also: "},
			 [NL,
			  #xmlElement{name = 'p',
				      content = [Rh | Rs1]}]
		 end,
	    Section = #xmlText{value = format_function_name(N, A)},
	    Link = format_function_fragment(N, A),
	    Ref = #xmlAttribute{name = 'name', value = Link},
	    [NL, NL,
	     #xmlElement{name = 'h3',
			 content = [#xmlElement{name = 'a',
						attributes = [Ref],
						content = [Section]}]},
	     NL, NL,
	     #xmlElement{name = 'p', content = D},
	     NL,
	     #xmlElement{name = 'p', content = layout_doc(Ts)}]
		++ Rs
    end.

layout_spec([{spec, _, Spec} | Ts], N, A, Auto) ->
    layout_spec(Spec, N);
layout_spec([_ | Ts], N, A, Auto) ->
    layout_spec(Ts, N, A, Auto);
layout_spec([], N, A, true) ->
    layout_spec({spec, {name, N},
		 {'fun', make_args(A), {type, term, []}}, []}, N);
layout_spec([], N, A, false) ->
    none.

layout_spec({spec, none, T, Ds}, N) ->
    layout_spec({spec, {name, N}, T, Ds}, N);
layout_spec(Spec, _) ->
    parse_xml(format_spec(Spec), 0, "").

make_args(N) -> make_args(1, N).

make_args(N, L) when N =< L ->
    [{par, none, {var, list_to_atom("Arg" ++ integer_to_list(N))}}
     | make_args(N + 1, L)];
make_args(N, L) ->
    [].

layout_doc([{doc, _, D} | Ts]) ->
    D;
layout_doc([{equiv, _, Expr} | Ts]) ->
    Txt = reverse(tl(reverse(erl_prettypr:format(Expr)))),
    E = #xmlElement{name = 'code',
		    content = [#xmlText{value = Txt}]},
    [#xmlText{value = "Equivalent to "},
     equiv_link(Expr, E),
     #xmlText{value = "."}];
layout_doc([_ | Ts]) ->
    layout_doc(Ts);
layout_doc([]) ->
    [].

equiv_link(Expr, E) ->
    case make_expr_ref(Expr) of
	{ok, _, R} ->
	    #xmlElement{name = 'a',
			attributes =
			[#xmlAttribute{name = 'href',
				       value = "#" ++ R}],
			content = [E]};
	error ->
	    E
    end.

%% TODO: generate references to other modules/files also

make_expr_ref(Expr) ->
    case catch {ok, erl_syntax_lib:analyze_application(Expr)} of
	{ok, {F, A}} when is_atom(F), is_integer(A) ->
	    {ok, format_function_name(F, A),
	     format_function_fragment(F, A)};
% 	{ok, {M, {F, A}}} when is_atom(M), is_atom(F), is_integer(A) ->
% 	    {ok, format_function_name(M, F, A),
% 	     format_module_url(M, format_function_fragment(F, A))};
	_ ->
	    error
    end.

layout_refs(Ts) ->
    Rs = [format_ref(R) || R <- sort([R || {see, _, R} <- Ts])],
    case Rs of
	"" ->
	    [];
	Rs1 ->
	    separate(Rs1, #xmlText{value = ", "},
		     [#xmlText{value = "."}])
    end.

format_ref({function, F, A}) ->
    N = #xmlText{value = format_function_name(F, A)},
    R = "#" ++ format_function_fragment(F, A),
    #xmlElement{name = 'a',
		attributes =
		[#xmlAttribute{name = 'href',
			       value = R}],
		content = [#xmlElement{name = 'code',
				       content = [N]}]};
format_ref({function, M, F, A}) ->
    N = #xmlText{value = format_function_name(M, F, A)},
    R = format_module_url(M, format_function_fragment(F, A)),
    #xmlElement{name = 'a',
		attributes =
		[#xmlAttribute{name = 'href',
			       value = R}],
		content = [#xmlElement{name = 'code',
				       content = [N]}]};
format_ref({module, M}) ->
    N = #xmlText{value = atom_to_list(M)},
    R = format_module_url(M),
    #xmlElement{name = 'a',
		attributes =
		[#xmlAttribute{name = 'href',
			       value = R}],
		content = [#xmlElement{name = 'code',
				       content = [N]}]}.

%% TODO: maybe separate section for types documented by local functions?

layout_types(HTags, Es, Ls) ->
    Ts = HTags ++ append([Ts || {_, Ts} <- Es]) ++
	append([Ts || {_, Ts} <- Ls]),
    NL = #xmlText{value = "\n"},
    [[NL, NL,
      #xmlElement{name = 'h3',
		  content = T},
      NL, NL,
      D]
     || {type, _, {T, D}} <- Ts].

%% Note that the parser will not produce two adjacent text segments;
%% thus, if a text segment ends with a period character, it marks the
%% end of the summary sentence only if it is also the last segment in
%% the list, in which case it makes no difference.

get_summary([{doc, _, D} | Ts]) ->
    get_summary_1(D);
get_summary([{equiv, _, Expr} | Ts]) ->
    case make_expr_ref(Expr) of
	{ok, F, R} ->
	    [#xmlElement{name = 'a',
			 attributes =
			 [#xmlAttribute{name = 'href',
					value = "#" ++ R}],
			 content = [#xmlText{value = "See "},
				    #xmlElement{name = 'code',
						content =
						[#xmlText{value = F}]},
				    #xmlText{value = "."}]}];
	error ->
	    [#xmlText{value = "Equivalent to "},
	     #xmlElement{name = 'code',
			 content =
			 [#xmlText{value = erl_prettypr:format(Expr)}]},
	     #xmlText{value = "."}]
    end;
get_summary([_ | Ts]) ->
    get_summary(Ts);
get_summary([]) ->
    [].

get_summary_1([E = #xmlText{value = Txt}]) ->
    {_, Txt1} = sentence(Txt),
    [E#xmlText{value = Txt1}];
get_summary_1([E = #xmlText{value = Txt} | Es]) ->
    case sentence(Txt) of
	{true, Txt1} ->
	    [E#xmlText{value = Txt1}];
	{false, _} ->
	    [E | get_summary_1(Es)]
    end;
get_summary_1([E | Es]) ->
    [E | get_summary_1(Es)];
get_summary_1([]) ->
    [#xmlText{value = "."}].

sentence(Cs) ->
    sentence(Cs, []).

sentence([$., $\s | Cs], As) -> {true, reverse([$. | As])};
sentence([$., $\t | Cs], As) -> {true, reverse([$. | As])};
sentence([$., $\n | Cs], As) -> {true, reverse([$. | As])};
sentence([$.], As) -> {false, reverse([$. | As])};   % see note above
sentence([C | Cs], As) -> sentence(Cs, [C | As]);
sentence([], As) -> {false, reverse([$. | strip_space(As)])}.


%% =====================================================================
%% Utility functions

split_at(C, Cs) ->
    split_at(C, Cs, []).

split_at(C, [C | Cs], As) ->
    {reverse(As), Cs};
split_at(C, [C1 | Cs], As) ->
    split_at(C, Cs, [C1 | As]);
split_at(_, [], As) ->
    {reverse(As), []}.

separate([S], T, F) ->
    [S | F];
separate([S | Ss], T, F) ->
    [S, T | separate(Ss, T, F)];
separate([], T, F) ->
    F.

strip_space([$\s | Cs]) -> strip_space(Cs);
strip_space([$\t | Cs]) -> strip_space(Cs);
strip_space([$\n | Cs]) -> strip_space(Cs);
strip_space(Cs) -> Cs.

segment(Es, N) ->
    segment(Es, [], [], 0, N).

segment([E | Es], As, Cs, N, M) when N < M ->
    segment(Es, [E | As], Cs, N + 1, M);
segment([_ | _] = Es, As, Cs, N, M) ->
    segment(Es, [], [reverse(As) | Cs], 0, M);
segment([], [], Cs, N, M) ->
    reverse(Cs);
segment([], As, Cs, N, M) ->
    reverse([reverse(As) | Cs]).

transpose([]) -> [];
transpose([[] | Xss]) -> transpose(Xss);
transpose([[X | Xs] | Xss]) ->
    [[X | [H || [H | T] <- Xss]]
     | transpose([Xs | [T || [H | T] <- Xss]])].

xml_text([$< | Cs]) ->
    "&lt;" ++ xml_text(Cs);
xml_text([$& | Cs]) ->
    "&amp;" ++ xml_text(Cs);
xml_text([C | Cs]) ->
    [C | xml_text(Cs)];
xml_text([]) ->
    [].

%% This is a "conservative" URI escaping, which escapes anything that
%% may not be in an NMTOKEN ([a-zA-Z0-9]|'.'|'-'|'_'), including ':'.

escape_uri([C | Cs]) when C >= $a, C =< $z ->
    [C | escape_uri(Cs)];
escape_uri([C | Cs]) when C >= $A, C =< $Z ->
    [C | escape_uri(Cs)];
escape_uri([C | Cs]) when C >= $0, C =< $9 ->
    [C | escape_uri(Cs)];
escape_uri([C = $. | Cs]) ->
    [C | escape_uri(Cs)];
escape_uri([C = $- | Cs]) ->
    [C | escape_uri(Cs)];
escape_uri([C = $_ | Cs]) ->
    [C | escape_uri(Cs)];
escape_uri([C | Cs]) ->
    "%" ++ hex_octet(C) ++ escape_uri(Cs);
escape_uri([]) ->
    [].

hex_octet(N) when N =< 9 ->
    [$0 + N];
hex_octet(N) when N > 15 ->
    hex_octet(N bsr 4) ++ hex_octet(N band 15);
hex_octet(N) ->
    [N - 10 + $a].

utf8([C | Cs]) when C > 16#7f ->
    [((C band 16#c0) bsr 6) + 16#c0, C band 16#3f ++ 16#80 | utf8(Cs)];
utf8([C | Cs]) ->
    [C | utf8(Cs)];
utf8([]) ->
    [].

filename([C | T]) when is_integer(C), C > 0, C =< 255 ->
    [C | filename(T)];
filename([H|T]) ->
    filename(H) ++ filename(T);
filename([]) ->
    [];
filename(N) when is_atom(N) ->
    atom_to_list(N);
filename(N) ->
    report_error("bad filename: `~P'.", [N, 25]),
    exit(error).

%% Initialise a module-info record with data about the module
%% represented by the syntax tree (or list of "forms"). Listed exports
%% are guaranteed to be in the set of function names.

get_module_info(Forms) ->
    L = case catch {ok, erl_syntax_lib:analyze_forms(Forms)} of
	    {ok, L1} ->
		L1;
	    syntax_error ->
		report_error("syntax error in input."),
		erlang:fault(badarg);
	    {'EXIT', R} ->
		exit(R);
	    R ->
		throw(R)
	end,
    Name = case keysearch(module, 1, L) of
	       {value, {module, N}} ->
		   N;
	       _ ->
		   report_error("in source code: module name "
				"missing or multiply defined."),
		   exit(error)
	   end,
    Functions = case keysearch(functions, 1, L) of
		    {value, {functions, Fs}} ->
			ordsets:from_list(Fs);
		    _ ->
			[]
		end,
    Exports = case keysearch(exports, 1, L) of
		  {value, {exports, Es}} ->
		      ordsets:from_list(Es);
		  _ ->
		      []
	      end,
    Attributes = case keysearch(attributes, 1, L) of
		     {value, {attributes, As}} ->
			 ordsets:from_list(As);
		     _ ->
			 []
		 end,
    Records = case keysearch(records, 1, L) of
		  {value, {records, Rs}} ->
		      Rs;
		  _ ->
		      []
	      end,
    #module{name = Name,
	    functions = Functions,
	    exports = ordsets:intersection(Exports, Functions),
	    attributes = Attributes,
	    records = Records}.

append_lines([L]) -> L;
append_lines([L | Ls]) -> L ++ [$\n | append_lines(Ls)];
append_lines([]) -> [].

%% =====================================================================
%% Formatting a specification or definition as XHTML-text

format_function_name(Name, Arity) ->
    io_lib:format("~w/~w", [Name,Arity]).

format_function_name(Module, Name, Arity) ->
    io_lib:format("~w:~w/~w", [Module,Name,Arity]).

format_function_fragment(Name, Arity) ->
    escape_uri(utf8(atom_to_list(Name) ++ "-"
		    ++ integer_to_list(Arity))).

format_type_name(Type) -> atom_to_list(Type).

format_type_fragment(Type) ->
    escape_uri("type-" ++ atom_to_list(Type)).

format_module_url(ModuleName) ->
    format_module_url(ModuleName, "").

format_module_url(ModuleName, "") ->
    escape_uri(utf8(atom_to_list(ModuleName))) ++ ".html";
format_module_url(ModuleName, Fragment) ->
    escape_uri(utf8(atom_to_list(ModuleName)))
	++ ".html#" ++ Fragment.

format_typedef({typedef, {type, N, As}, Ds}) ->
    "<a name=\"" ++ format_type_fragment(N) ++ "\">"
	++ format_type_name(N) ++ "(" ++ format_spec_seq(As)
	++ ")</a>" ++ format_spec_defs(Ds);
format_typedef({typedef, D = {def, _, _}, Ds}) ->
    format_spec(D) ++ format_spec_defs(Ds).

format_spec({spec, none, T, Ds}) ->
    format_spec(T) ++ format_spec_defs(Ds);
format_spec({spec, {name, F}, T, Ds}) ->
    atom_to_xml(F) ++ format_spec(T) ++ format_spec_defs(Ds);
format_spec({'fun', As, T}) ->
    "(" ++ format_spec_seq(As) ++ ") -> " ++ format_spec(T);
format_spec({par, none, T}) ->
    format_spec(T);
format_spec({par, {name, N}, T}) ->
    atom_to_xml(N) ++ "::" ++ format_spec(T);
format_spec({def, {type, N, As}, T}) ->
    "<a name=\"" ++ format_type_fragment(N) ++ "\">"
	++ format_type_name(N) ++ "(" ++ format_spec_seq(As)
	++ ")</a>" ++ " = " ++ format_spec(T);
format_spec({def, V, T}) ->
    format_spec(V) ++ " = " ++ format_spec(T);
format_spec({union, As}) ->
    format_spec_union(As);
format_spec({type, N, As}) ->
    Name = atom_to_xml(N),
    case is_predefined_type(N) of
	true ->
	   Name ++ "(" ++ format_spec_seq(As) ++ ")";
	false ->
	    "<a href=\"#" ++ format_type_fragment(N) ++ "\">"
		++ Name ++ "(" ++ format_spec_seq(As) ++ ")</a>"
    end;
format_spec({type, M, N, As}) ->
    "<a href=\"" ++ format_module_url(M, format_type_fragment(N))
	++ "\">" ++ atom_to_xml(M) ++ ":" ++ atom_to_xml(N)
	++ "(" ++ format_spec_seq(As) ++ ")</a>";
format_spec({tuple, As}) ->
    "{" ++ format_spec_seq(As) ++ "}";
format_spec({list, T}) ->
    "[" ++ format_spec(T) ++ "]";
format_spec({var, V}) ->
    atom_to_xml(V);
format_spec(nil) ->
    "[]";
format_spec({atom, V}) ->
    atom_to_xml(V);
format_spec({integer, V}) ->
    integer_to_list(V);
format_spec({float, V}) ->
    float_to_list(V).

atom_to_xml(A) ->
    xml_text(atom_to_list(A)).

format_spec_union(Ts) -> format_spec_seq(Ts, " | ").

format_spec_seq(Ts) -> format_spec_seq(Ts, ", ").

format_spec_seq([T], S) ->
    format_spec(T);
format_spec_seq([T | Ts], S) ->
    format_spec(T) ++ S ++ format_spec_seq(Ts, S);
format_spec_seq([], _) ->
    "".

format_spec_defs([]) ->
    "";
format_spec_defs(Ds) ->
    "<ul>" ++ format_spec_defs_1(Ds) ++ "</ul>".

format_spec_defs_1([D | Ds]) ->
    "<li>" ++ format_spec(D) ++ "</li>" ++ format_spec_defs_1(Ds);
format_spec_defs_1([]) ->
    "".

is_predefined_type(term) -> true;
is_predefined_type(atom) -> true;
is_predefined_type(integer) -> true;
is_predefined_type(float) -> true;
is_predefined_type(number) -> true;
is_predefined_type(char) -> true;
is_predefined_type(string) -> true;
is_predefined_type(binary) -> true;
is_predefined_type(pid) -> true;
is_predefined_type(port) -> true;
is_predefined_type(reference) -> true;
is_predefined_type(function) -> true;
is_predefined_type(bool) -> true;
is_predefined_type(_) -> false.


%% =====================================================================
%% Reporting

error_multiple_tag(Tag, {F, A}) ->
    report_error("at function ~w/~w: multiple @~w tag.", [F, A, Tag]).

error_mutex_tag(Tags, {F, A}) ->
    report_error("at function ~w/~w: tags @~w are mutually exclusive.",
		 [F, A, Tags]).

error_forbidden_tags(Tags, module) ->
    report_error("at module header: tags @~w not allowed here.",
		 [Tags]);
error_forbidden_tags(Tags, {F, A}) ->
    report_error("at function ~w/~w: tags @~w not allowed here.",
		 [F, A, Tags]).

error_read_file(Name) ->
    report_error("error reading file `~s'.", [filename(Name)]).

% verbose(S, Opts) ->
%     verbose(S, [], Opts).

verbose(S, Vs, Opts) ->
    case proplists:get_bool(verbose, Opts) of
	true ->
	    report(S, Vs);
	false ->
	    ok
    end.

report_error(S) ->
    report_error(S, []).

report_error(S, Vs) ->
    report(S, Vs).

% report_warning(S) ->
%     report_warning(S, []).

report_warning(S, Vs) ->
    report("warning: " ++ S, Vs).

% report(S) ->
%     report(S, []).

report(S, Vs) ->
    io:fwrite(concat([?MODULE, ": ", S, "\n"]), Vs).


%% =====================================================================
