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
%% @copyright 2001-2003 Richard Carlsson
%% @author Richard Carlsson <richardc@csd.uu.se>
%% @see edoc
%% @end
%% =====================================================================

%% @doc EDoc documentation extraction.

-module(edoc_extract).

-export([source/4, source/5, file/4, text/4]).

-import(edoc_report, [report/3]).

-include("edoc.hrl").

%% @spec source(Forms, Comments::[comment()], File::filename(),
%%              Env::edoc_env(), Options::option_list()) ->
%%           {ModuleName, edoc_module()}
%%
%%    Forms = syntaxTree() | [syntaxTree()]
%%    comment() = {Line, Column, Indentation, Text}
%%    Line = integer()
%%    Column = integer()
%%    Indentation = integer()
%%    Text = [string()]
%%    ModuleName = atom()
%%
%% @doc Like {@link source/4}, but first inserts the given comments in
%% the syntax trees. The syntax trees must contain valid position
%% information. (Cf. {@link edoc:read_comments/2}.)
%%
%% @see edoc:read_comments/2
%% @see edoc:read_source/2
%% @see source/4
%% @see //syntax_tools/erl_recomment

source(Forms, Comments, File, Env, Opts) when list(Forms) ->
    Forms1 = erl_syntax:form_list(Forms),
    source(Forms1, Comments, File, Env, Opts);
source(Forms, Comments, File, Env, Opts) ->
    Tree = erl_recomment:quick_recomment_forms(Forms, Comments),
    source(Tree, File, Env, Opts).

%% @spec source(Forms, File::filename(), Env::edoc_env(),
%%              Options::option_list()) ->
%%           {ModuleName, edoc_module()}
%%
%%	    Forms = syntaxTree() | [syntaxTree()]
%%	    ModuleName = atom()
%%          edoc_module() = edoc:edoc_module()
%%          edoc_env() = edoc_lib:edoc_env()
%%
%% @doc Extracts EDoc documentation from commented source code syntax
%% trees. The given `Forms' must be a single syntax tree of
%% type `form_list', or a list of syntax trees representing
%% "program forms" (cf. {@link edoc:read_source/2}.
%% `Env' is an environment created by {@link
%% edoc_lib:get_doc_env/4}. The `File' argument is used for
%% error reporting and output file name generation only.
%%
%% <p>See {@link edoc:get_doc/2} for descriptions of the `def',
%% `hidden', and `private' options.</p>
%%
%% @see source/5
%% @see erl_recomment

%% Note that the actual module name found in the source file will be
%% used for generating the documentation, creating relative links, etc.

%% INHERIT-OPTIONS: get_macro_defs/1
%% INHERIT-OPTIONS: edoc_data:module/4

source(Forms, File, Env, Opts) when list(Forms) ->
    source(erl_syntax:form_list(Forms), File, Env, Opts);
source(Tree, File0, Env, Opts) ->
    File = edoc_lib:filename(File0),
    Module = get_module_info(Tree, File),
    Forms = preprocess_forms(erl_syntax:form_list_elements(
			       erl_syntax:flatten_form_list(Tree))),
    {Header, Footer, Entries} = collect(Forms, Module),
    Defs = get_macro_defs(Opts),
    Name = Module#module.name,
    Package = list_to_atom(packages:strip_last(Name)),
    Env1 = Env#env{module = Name,
		   package = Package,
		   root = edoc_refs:relative_package_path('', Package)},
    Defs1 = dict:from_list(Defs ++ module_macros(Env1)),
    Entries1 = get_tags([Header, Footer | Entries], Defs1, Env1, File),
    Data = edoc_data:module(Module, Entries1, Env1, Opts),
    {Name, Data}.


%% NEW-OPTIONS: def
%% DEFER-OPTIONS: source/4

get_macro_defs(Opts) ->
    Defs = proplists:append_values(def, Opts),
    edoc_macros:check_defs(Defs),
    Defs.


%% @spec file(File::filename(), Context, Env::edoc_env(),
%%            Options::option_list()) -> {ok, Tags} | {error, Reason}
%%   Context = overview | package
%%   Tags = [term()]
%%   edoc_env() = edoc_lib:edoc_env()
%%   Reason = term()
%%
%% @doc Reads a text file and returns the list of tags in the file. Any
%% lines of text before the first tag are ignored. `Env' is an
%% environment created by {@link edoc_lib:get_doc_env/4}. Upon error,
%% `Reason' is an atom returned from the call to {@link
%% //kernel/file:read_file/1}.
%%
%% <p>See {@link text/4} for options.</p>

%% INHERIT-OPTIONS: text/4

file(File, Context, Env, Opts) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    {ok, text(binary_to_list(Bin), Context, Env, Opts, File)};
	{error, R} ->
	    {error, R}
    end.


%% @spec (Text::string(), Context, Env::edoc_env(),
%%        Options::option_list()) -> Tags
%%     Context = overview | package
%%     Tags = [term()]
%%     edoc_env() = edoc_lib:edoc_env()
%%
%% @doc Returns the list of tags in the text. Any lines of text before
%% the first tag are ignored. `Env' is an environment created by {@link
%% edoc_lib:get_doc_env/4}.
%%
%% <p>See {@link source/4} for a description of the `def' option.</p>

%% INHERIT-OPTIONS: get_macro_defs/1
%% DEFER-OPTIONS: source/4

text(Text, Context, Env, Opts) ->
    text(Text, Context, Env, Opts, "").

text(Text, Context, Env, Opts, Where) ->
    Defs = get_macro_defs(Opts),
    Defs1 = dict:from_list(Defs ++ file_macros(Context, Env)),
    Cs = edoc_lib:lines(Text),
    Ts0 = edoc_tags:scan_lines(Cs, 1),
    Tags = sets:from_list(edoc_tags:tag_names()),
    Ts1 = edoc_tags:filter_tags(Ts0, Tags, Where),
    Single = sets:from_list(edoc_tags:tags(single)),
    Allow = sets:from_list(edoc_tags:tags(Context)),
    case edoc_tags:check_tags(Ts1, Allow, Single, Where) of
	true ->
	    exit(error);
	false ->
	    Ts2 = edoc_macros:expand_tags(Ts1, Defs1, Env, Where),
	    How = dict:from_list(edoc_tags:tag_parsers()),
	    edoc_tags:parse_tags(Ts2, How, Where)
    end.


%% @spec (Forms::[syntaxTree()], File::filename()) -> moduleInfo()
%% @doc Initialises a module-info record with data about the module
%% represented by the list of forms. Exports are guaranteed to exist in
%% the set of defined names.

get_module_info(Forms, File) ->
    L = case catch {ok, erl_syntax_lib:analyze_forms(Forms)} of
	    {ok, L1} ->
		L1;
	    syntax_error ->
		report(File, "syntax error in input.", []),
		exit(error);
	    {'EXIT', R} ->
		exit(R);
	    R ->
		throw(R)
	end,
    Name = case lists:keysearch(module, 1, L) of
	       {value, {module, N}} ->
		   N;
	       _ ->
		   report(File, "module name missing.", []),
		   exit(error)
	   end,
    Functions = ordsets:from_list(get_list_keyval(functions, L)),
    Exports = ordsets:from_list(get_list_keyval(exports, L)),
    Attributes = ordsets:from_list(get_list_keyval(attributes, L)),
    Records = get_list_keyval(records, L),
    #module{name = Name,
	    functions = Functions,
	    exports = ordsets:intersection(Exports, Functions),
	    attributes = Attributes,
	    records = Records}.

get_list_keyval(Key, L) ->
    case lists:keysearch(Key, 1, L) of
	{value, {Key, As}} ->
	    ordsets:from_list(As);
	_ ->
	    []
    end.

%% @spec (Forms::[syntaxTree()]) -> [syntaxTree()]
%% @doc Preprocessing: copies any precomments on forms to standalone
%% comments, and removes "invisible" forms from the list.

preprocess_forms([F | Fs]) ->
    case erl_syntax:get_precomments(F) of
	[] ->
	    preprocess_forms_1(F, Fs);
	Cs ->
	    Cs ++ preprocess_forms_1(F, Fs)
    end;
preprocess_forms([]) ->
    [].

preprocess_forms_1(F, Fs) ->
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

%% This collects the data for the header and the functions of the
%% module. Note that the list of forms is assumed to have been
%% preprocessed first, so that all "invisible" forms are removed, and
%% the only interesting comments are those that are standalone comments
%% in the list.

collect(Fs, Mod) ->
    collect(Fs, [], [], undefined, Mod).

collect([F | Fs], Cs, As, Header, Mod) ->
    case erl_syntax_lib:analyze_form(F) of
	comment ->
	    collect(Fs, [F | Cs], As, Header, Mod);
	{function, Name} ->
	    L = erl_syntax:get_pos(F),
	    Export = ordsets:is_element(Name, Mod#module.exports),
	    Args = parameters(erl_syntax:function_clauses(F)),
	    collect(Fs, [], [#entry{name = Name, args = Args, line = L,
				    export = Export,
				    data = comment_text(Cs)} | As],
		    Header, Mod);
	{rule, Name} ->
	    L = erl_syntax:get_pos(F),
	    Export = ordsets:is_element(Name, Mod#module.exports),
	    Args = parameters(erl_syntax:rule_clauses(F)),
	    collect(Fs, [], [#entry{name = Name, args = Args, line = L,
				    export = Export,
				    data = comment_text(Cs)} | As],
		    Header, Mod);
	{attribute, {module, _}} when Header == undefined ->
	    L = erl_syntax:get_pos(F),
	    collect(Fs, [], As, #entry{name = module, line = L,
				       data = comment_text(Cs)},
		    Mod);
	_ ->
	    %% Drop current seen comments.
	    collect(Fs, [], As, Header, Mod)
    end;
collect([], Cs, As, Header, _Mod) ->
    Footer = #entry{name = footer, data = comment_text(Cs)},
    As1 = lists:reverse(As),
    if Header == undefined ->
	    {#entry{name = module, data = []}, Footer, As1};
       true ->
	    {Header, Footer, As1}
    end.

%% Returns a list of simplified comment information (position and text)
%% for a list of abstract comments. The order of elements is reversed.

comment_text(Cs) ->
    comment_text(Cs, []).

comment_text([C | Cs], Ss) ->
    L = erl_syntax:get_pos(C),
    comment_text(Cs, [#comment{line = L,
			       text = [remove_percent_chars(S)
				       || S <- erl_syntax:comment_text(C)]}
		      | Ss]);
comment_text([], Ss) ->
    Ss.

%% @spec (string()) -> string()
%%
%% @doc Replaces leading `%' characters by spaces. For example, `"%%%
%% foo" -> "\s\s\s foo"', but `"% % foo" -> "\s % foo"', since the
%% second `%' is preceded by whitespace.

remove_percent_chars([$% | Cs]) -> [$\s | remove_percent_chars(Cs)];
remove_percent_chars(Cs) -> Cs.

%% Extracting possible parameter names from Erlang clause patterns.  The
%% atom '_' is used when no name can be found. (Better names are made up
%% later, when we also may have typespecs available; see edoc_data.)

parameters(Clauses) ->
    select_names([find_names(Ps) || Ps <- patterns(Clauses)]).

patterns(Cs) ->
    edoc_lib:transpose([erl_syntax:clause_patterns(C) || C <- Cs]).

find_names(Ps) ->
    find_names(Ps, []).

%% TODO: also handle patterns like '"..."++Cs' as list patterns.
%% TODO: make variable name from record name in record patterns

find_names([P | Ps], Ns) ->
    case erl_syntax:type(P) of
	variable ->
	    find_names(Ps, [tidy_name(erl_syntax:variable_name(P)) | Ns]);
	match_expr ->
	    %% Right-hand side gets priority over left-hand side!
	    %% Note that the list is reversed afterwards.
	    P1 = erl_syntax:match_expr_pattern(P),
	    P2 = erl_syntax:match_expr_body(P),
	    find_names([P1, P2 | Ps], Ns);
	list ->
	    P1 = erl_syntax:list_tail(P),
	    find_names([P1 | Ps], Ns);
	_ ->
	    find_names(Ps, Ns)
    end;
find_names([], Ns) ->
    lists:reverse(Ns).

select_names(Ls) ->
    select_names(Ls, [], sets:new()).

select_names([Ns | Ls], As, S) ->
    A = select_name(Ns, S),
    select_names(Ls, [A | As], sets:add_element(A, S));
select_names([], As, _) ->
    lists:reverse(As).

select_name([A | Ns], S) -> 
    case sets:is_element(A, S) of
	true ->
	    select_name(Ns, S);
	false ->
	    A
    end;
select_name([], _S) ->
    '_'.

%% Strip leading underscore characters from parameter names. If the
%% result does not begin with an uppercase character, we add a single
%% leading underscore. If the result would be empty, the atom '_' is
%% returned.

tidy_name(A) ->
    case atom_to_list(A) of
	[$_ | Cs] ->
	    list_to_atom(tidy_name_1(Cs));
	_ ->
	    A
    end.

tidy_name_1([$_ | Cs]) -> tidy_name_1(Cs);
tidy_name_1([C | _]=Cs) when C >= $A, C =< $Z -> Cs;
tidy_name_1([C | _]=Cs) when C >= $\300, C =< $\336, C =/= $\327-> Cs;
tidy_name_1(Cs) -> [$_ | Cs].

%% Collects the tags belonging to each entry, checks them, expands
%% macros and parses the content.

-record(tags, {names,single,module,function,footer}).

get_tags(Es, Defs, Env, File) ->
    %% Cache this stuff for quick lookups.
    Tags = #tags{names = sets:from_list(edoc_tags:tag_names()),
		 single = sets:from_list(edoc_tags:tags(single)),
		 module = sets:from_list(edoc_tags:tags(module)),
		 footer = sets:from_list(edoc_tags:tags(footer)),
		 function = sets:from_list(edoc_tags:tags(function))},
    How = dict:from_list(edoc_tags:tag_parsers()),
    get_tags(Es, Tags, Defs, Env, How, File).

get_tags([#entry{name = Name, data = Cs} = E | Es], Tags, Defs, Env,
	 How, File) ->
    Where = {File, Name},
    Ts0 = scan_tags(Cs),
    Ts1 = check_tags(Ts0, Tags, Where),
    Ts2 = edoc_macros:expand_tags(Ts1, Defs, Env, Where),
    Ts = edoc_tags:parse_tags(Ts2, How, Where),
    [E#entry{data = Ts} | get_tags(Es, Tags, Defs, Env, How, File)];
get_tags([], _, _, _, _, _) ->
    [].

%% Scanning a list of separate comments for tags.

scan_tags([#comment{line = L, text = Ss} | Es]) ->
    edoc_tags:scan_lines(Ss, L) ++ scan_tags(Es);
scan_tags([]) ->
    [].

%% Check the set of found tags (depending on context).
%% Completely unknown tags are filtered out with a warning.

check_tags(Ts0, Tags, Where) ->
    Ts = edoc_tags:filter_tags(Ts0, Tags#tags.names, Where),
    case check_tags_1(Ts, Tags, Where) of
	false -> Ts;
	true -> exit(error)
    end.

check_tags_1(Ts, Tags, {_, module} = Where) ->
    Allow = Tags#tags.module,
    Single = Tags#tags.single,
    edoc_tags:check_tags(Ts, Allow, Single, Where);
check_tags_1(Ts, Tags, {_, footer} = Where) ->
    Allow = Tags#tags.footer,
    Single = Tags#tags.single,
    edoc_tags:check_tags(Ts, Allow, Single, Where);
check_tags_1(Ts, Tags, Where) ->
    Allow = Tags#tags.function,
    Single = Tags#tags.single,
    edoc_tags:check_tags(Ts, Allow, Single, Where).

%% Macros for modules

module_macros(Env) ->
    [{module, atom_to_list(Env#env.module)}]
	++ edoc_macros:std_macros(Env).

%% Macros for reading auxiliary edoc-files

file_macros(_Context, Env) ->
    edoc_macros:std_macros(Env).
