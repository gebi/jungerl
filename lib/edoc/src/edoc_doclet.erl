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
%% @copyright 2003 Richard Carlsson
%% @author Richard Carlsson <richardc@csd.uu.se>
%% @see edoc
%% @end
%% =====================================================================

%% @doc Standard doclet module for EDoc.

%% Note that this is written so that it is *not* depending on edoc.hrl!

%% TODO: copy "doc-files" subdirectories, recursively.

-module(edoc_doclet).

-export([run/2]).

-import(edoc_report, [report/2, warning/2]).

-include("../include/edoc_doclet.hrl").

-define(EDOC_APP, edoc).
-define(DEFAULT_FILE_SUFFIX, ".html").
-define(INDEX_FILE, "index.html").
-define(OVERVIEW_FILE, "overview.edoc").
-define(PACKAGE_SUMMARY, "package-summary.html").
-define(OVERVIEW_SUMMARY, "overview-summary.html").
-define(PACKAGES_FRAME, "packages-frame.html").
-define(MODULES_FRAME, "modules-frame.html").
-define(STYLESHEET, "stylesheet.css").
-define(NL, "\n").

-include("xmerl.hrl").

%% Sources is the list of inputs in the order they were found.  Packages
%% and Modules are sorted lists of atoms without duplicates. (They
%% usually include the data from the edoc-info file in the target
%% directory, if it exists.) Note that the "empty package" is never
%% included in Packages!

%% @doc Main doclet entry point. See the file <a
%% href="../include/edoc_doclet.hrl">`edoc_doclet.hrl'</a> for the data
%% structures used for passing parameters.
%%
%% <p>Also see {@link edoc:layout/2} for layout-related options, and
%% {@link edoc:get_doc/2} for options related to reading source
%% files.</p>
%%
%% <p>Options:
%% <dl>
%%  <dt>{@type {file_suffix, string()@}}
%%  </dt>
%%  <dd>Specifies the suffix used for output files. The default value is
%%      `".html"'.
%%  </dd>
%%  <dt>{@type {hidden, bool()@}}
%%  </dt>
%%  <dd>If the value is `true', documentation of hidden modules and
%%      functions will also be included. The default value is `false'.
%%  </dd>
%%  <dt>{@type {overview, edoc:filename()@}}
%%  </dt>
%%  <dd>Specifies the name of the overview-file. By default, this doclet
%%      looks for a file `"overview.edoc"' in the target directory.
%%  </dd>
%%  <dt>{@type {private, bool()@}}
%%  </dt>
%%  <dd>If the value is `true', documentation of private modules and
%%      functions will also be included. The default value is `false'.
%%  </dd>
%%  <dt>{@type {stylesheet, string()@}}
%%  </dt>
%%  <dd>Specifies the URI used for referencing the stylesheet. The
%%      default value is `"stylesheet.css"'. If an empty string is
%%      specified, no stylesheet reference will be generated.
%%  </dd>
%%  <dt>{@type {stylesheet_file, edoc:filename()@}}
%%  </dt>
%%  <dd>Specifies the name of the stylesheet file. By default, this
%%      doclet uses the file `"stylesheet.css"' in the `priv'
%%      subdirectory of the EDoc installation directory. The named file
%%      will be copied to the target directory.
%%  </dd>
%%  <dt>{@type {title, string()@}}
%%  </dt>
%%  <dd>Specifies the title of the overview-page.
%%  </dd>
%% </dl></p>

%% INHERIT-OPTIONS: title/2
%% INHERIT-OPTIONS: sources/5
%% INHERIT-OPTIONS: overview/4
%% INHERIT-OPTIONS: copy_stylesheet/2
%% INHERIT-OPTIONS: stylesheet/1

run(#doclet_gen{}=Cmd, Ctxt) ->
    gen(Cmd#doclet_gen.sources,
	Cmd#doclet_gen.app,
	Cmd#doclet_gen.packages,
	Cmd#doclet_gen.modules,
	Cmd#doclet_gen.filemap,
	Ctxt);
run(#doclet_toc{}=Cmd, Ctxt) ->
    toc(Cmd#doclet_toc.paths, Ctxt).

gen(Sources, App, Packages, Modules, FileMap, Ctxt) ->
    Dir = Ctxt#context.dir,
    Env = Ctxt#context.env,
    Options = Ctxt#context.opts,
    Title = title(App, Options),
    CSS = stylesheet(Options),
    Modules1 = sources(Sources, Dir, Modules, Env, Options),
    modules_frame(Dir, Modules1, Title, CSS),
    packages(Packages, Dir, FileMap, Env, Options),
    packages_frame(Dir, Packages, Title, CSS),
    overview(Dir, Title, Env, Options),
    index_file(Dir, length(Packages) > 1, Title),
    edoc_lib:write_info_file(App, Packages, Modules1, Dir),
    copy_stylesheet(Dir, Options),
    ok.


%% NEW-OPTIONS: title
%% DEFER-OPTIONS: run/2

title(App, Options) ->
    proplists:get_value(title, Options,
			if App == ?NO_APP ->
				"Overview";
			   true ->
				io_lib:fwrite("Application: ~s", [App])
			end).


%% Processing the individual source files.

%% NEW-OPTIONS: file_suffix, private, hidden
%% INHERIT-OPTIONS: edoc:layout/2
%% INHERIT-OPTIONS: edoc:get_doc/3
%% DEFER-OPTIONS: run/2

sources(Sources, Dir, Modules, Env, Options) ->
    Suffix = proplists:get_value(file_suffix, Options,
				 ?DEFAULT_FILE_SUFFIX),
    Private = proplists:get_bool(private, Options),
    Hidden = proplists:get_bool(hidden, Options),
    Ms = lists:foldl(fun (Src, Set) ->
			     source(Src, Dir, Suffix, Env, Set,
				    Private, Hidden, Options)
		     end,
		     sets:new(), Sources),
    [M || M <- Modules, sets:is_element(M, Ms)].


%% Generating documentation for a source file, adding its name to the
%% set if it was successful.

source({M, P, Name, Path}, Dir, Suffix, Env, Set, Private, Hidden,
       Options) ->
    File = filename:join(Path, Name),
    case catch {ok, edoc:get_doc(File, Env, Options)} of
	{ok, {Module, Doc}} ->
	    check_name(Module, M, P, File),
	    Text = edoc:layout(Doc, Options),
	    Name1 = packages:last(M) ++ Suffix,
	    edoc_lib:write_file(Text, Dir, Name1, P),
	    case ((not is_private(Doc)) orelse Private)
		andalso ((not is_hidden(Doc)) orelse Hidden) of
		true ->
		    sets:add_element(Module, Set);
		false ->
		    Set
	    end;
	R ->
	    report("skipping source file '~s': ~W.", [File, R, 15]),
	    Set
    end.

check_name(M, M0, P0, File) ->
    P = list_to_atom(packages:strip_last(M)),
    N = packages:last(M),
    N0 = packages:last(M0),
    if N =/= N0 ->
	    warning("file '~s' actually contains module '~s'.", [File, M]);
       true ->
	    ok
    end,
    if P =/= P0 ->
	    warning("file '~s' belongs to package '~s', not '~s'.",
		    [File, P, P0]);
       true ->
	    ok
    end.


%% Generating the summary files for packages.

%% INHERIT-OPTIONS: read_file/4
%% INHERIT-OPTIONS: edoc_lib:run_layout/2

packages(Packages, Dir, FileMap, Env, Options) ->
    lists:foreach(fun (P) ->
			  package(P, Dir, FileMap, Env, Options)
		  end,
		  Packages).

package(P, Dir, FileMap, Env, Opts) ->
    Tags = case FileMap(P) of
	       "" ->
		   [];
	       File ->
		   read_file(File, package, Env, Opts)
	   end,
    Data = edoc_data:package(P, Tags, Env, Opts),
    F = fun (M) ->
		M:package(Data, Opts)
	end,
    Text = edoc_lib:run_layout(F, Opts),
    edoc_lib:write_file(Text, Dir, ?PACKAGE_SUMMARY, P).


%% Creating an index file, with some frames optional.

index_file(Dir, Packages, Title) ->
    Frame1 = {frame, [{src,?PACKAGES_FRAME},
		      {name,"packagesFrame"},{title,""}],
	      []},
    Frame2 = {frame, [{src,?MODULES_FRAME},
		      {name,"modulesFrame"},{title,""}],
	      []},
    Frame3 = {frame, [{src,?OVERVIEW_SUMMARY},
		      {name,"overviewFrame"},{title,""}],
	      []},
    Frameset = {frameset, [{cols,"20%,80%"}],
		case Packages of
		    true ->
			[?NL,
			 {frameset, [{rows,"30%,70%"}],
			  [?NL, Frame1, ?NL, Frame2, ?NL]}
			];
		    false ->
 			[?NL, Frame2, ?NL]
		end
		++ [?NL, Frame3, ?NL,
		    {noframes,
		     [?NL,
		      {h2, ["This page uses frames"]},
		      ?NL,
		      {p, ["Your browser does not accept frames.",
			   ?NL, br,
			   "You should go to the ",
			   {a, [{href, ?OVERVIEW_SUMMARY}],
			    ["non-frame version"]},
			   " instead.", ?NL]},
		      ?NL]},
		    ?NL]},
    XML = xhtml_1(Title, [], Frameset),
    Text = xmerl:export_simple([XML], xmerl_html, []),
    edoc_lib:write_file(Text, Dir, ?INDEX_FILE).

packages_frame(Dir, Ps, Title, CSS) ->
    Body = [?NL,
	    {h2, ["Packages"]},
	    ?NL,
	    {table, [{width, "100%"}, {border, 0}],
	     [{tr, [{td, [], [{a, [{href, package_ref(P)},
				   {target,"overviewFrame"}],
			       [atom_to_list(P)]}]}]}
	      || P <- Ps]},
	    ?NL],
    XML = xhtml(Title, CSS, Body),
    Text = xmerl:export_simple([XML], xmerl_html, []),
    edoc_lib:write_file(Text, Dir, ?PACKAGES_FRAME).

modules_frame(Dir, Ms, Title, CSS) ->
    Body = [?NL,
	    {h2, ["Modules"]},
	    ?NL,
	    {table, [{width, "100%"}, {border, 0}],
	     [{tr, [{td, [],
		     [{a, [{href, module_ref(M)},
			   {target,"overviewFrame"}],
		       [atom_to_list(M)]}]}]}
	      || M <- Ms]},
	    ?NL],
    XML = xhtml(Title, CSS, Body),
    Text = xmerl:export_simple([XML], xmerl_html, []),
    edoc_lib:write_file(Text, Dir, ?MODULES_FRAME).

module_ref(M) ->
    edoc_refs:relative_package_path(M, '') ++ ?DEFAULT_FILE_SUFFIX.

package_ref(P) ->
    edoc_lib:join_uri(edoc_refs:relative_package_path(P, ''),
		      ?PACKAGE_SUMMARY).

xhtml(Title, CSS, Content) ->
    xhtml_1(Title, CSS, {body, [{bgcolor, "white"}], Content}).

xhtml_1(Title, CSS, Body) ->
    {html, [?NL,
	    {head, [?NL, {title, [Title]}, ?NL] ++ CSS},
	    ?NL,
	    Body,
	    ?NL]
    }.

%% NEW-OPTIONS: overview
%% INHERIT-OPTIONS: read_file/4
%% INHERIT-OPTIONS: edoc_lib:run_layout/2
%% INHERIT-OPTIONS: edoc_extract:file/4
%% DEFER-OPTIONS: run/2

overview(Dir, Title, Env, Opts) ->
    File = proplists:get_value(overview, Opts,
			       filename:join(Dir, ?OVERVIEW_FILE)),
    Tags = read_file(File, overview, Env, Opts),
    Data = edoc_data:overview(Title, Tags, Env, Opts),
    F = fun (M) ->
		M:overview(Data, Opts)
	end,
    Text = edoc_lib:run_layout(F, Opts),
    edoc_lib:write_file(Text, Dir, ?OVERVIEW_SUMMARY).


%% NEW-OPTIONS: stylesheet_file
%% DEFER-OPTIONS: run/2

copy_stylesheet(Dir, Options) ->
    case proplists:get_value(stylesheet, Options) of
	undefined ->
	    From = case proplists:get_value(stylesheet_file, Options) of
		       File when list(File) ->
			   File;
		       _ ->
			   case code:priv_dir(?EDOC_APP) of
			       PrivDir when list(PrivDir) ->
				   filename:join(PrivDir, ?STYLESHEET);
			       _ ->
				   report("cannot find default "
					  "stylesheet file.", []),
				   exit(error)
			   end
		   end,
	    edoc_lib:copy_file(From, filename:join(Dir, ?STYLESHEET));
	_ ->
	    ok
    end.

%% NEW-OPTIONS: stylesheet
%% DEFER-OPTIONS: run/2

stylesheet(Options) ->
    case proplists:get_value(stylesheet, Options) of
	"" ->
	    [];
	S ->
	    Ref = case S of
		      undefined ->
			  ?STYLESHEET;
		      "" ->
			  "";    % no stylesheet
		      S when list(S) ->
			  S;
		      _ ->
			  report("bad value for option 'stylesheet'.",
				 []),
			  exit(error)
		  end,
	    [{link, [{rel, "stylesheet"},
		     {type, "text/css"},
		     {href, Ref}], []},
	     ?NL]
    end.

is_private(E) ->
    case get_attrval(private, E) of
 	"yes" -> true;
 	_ -> false
    end.

is_hidden(E) ->
    case get_attrval(hidden, E) of
 	"yes" -> true;
 	_ -> false
    end.

get_attrval(Name, #xmlElement{attributes = As}) ->
    case get_attr(Name, As) of
	[#xmlAttribute{value = V}] ->
	    V;
	[] -> ""
    end.

get_attr(Name, [#xmlAttribute{name = Name} = A | As]) ->
    [A | get_attr(Name, As)];
get_attr(Name, [_ | As]) ->
    get_attr(Name, As);
get_attr(_, []) ->
    [].

%% Read external source file. Fails quietly, returning empty tag list.

%% INHERIT-OPTIONS: edoc_extract:file/4

read_file(File, Context, Env, Opts) ->
    case edoc_extract:file(File, Context, Env, Opts) of
	{ok, Tags} ->
	    Tags;
	{error, _} ->
	    []
    end.


%% FIXME: meta-level index generation

%% Creates a Table of Content from a list of Paths (ie paths to applications)
%% and an overview file.

-define(EDOC_DIR, "doc").
-define(INDEX_DIR, "doc/index").
-define(CURRENT_DIR, ".").

toc(Paths, Ctxt) ->
    Opts = Ctxt#context.opts,
    Dir = Ctxt#context.dir,
    Env = Ctxt#context.env,
    app_index_file(Paths, Dir, Env, Opts).

%% FIXME: it's unclear how much of this is working at all

%% NEW-OPTIONS: title
%% INHERIT-OPTIONS: overview/4

app_index_file(Paths, Dir, Env, Options) ->
    Title = proplists:get_value(title, Options,"Overview"),
%    Priv = proplists:get_bool(private, Options),
    CSS = stylesheet(Options),
    Apps1 = [{filename:dirname(A),filename:basename(A)} || A <- Paths],
    index_file(Dir, false, Title),
    application_frame(Dir, Apps1, Title, CSS),
    modules_frame(Dir, [], Title, CSS),
    overview(Dir, Title, Env, Options),
%    edoc_lib:write_info_file(Prod, [], Modules1, Dir),
    copy_stylesheet(Dir, Options).

application_frame(Dir, Apps, Title, CSS) ->
    Body = [?NL,
	    {h2, ["Applications"]},
	    ?NL,
	    {table, [{width, "100%"}, {border, 0}],
	     [{tr, [{td, [], [{a, [{href,app_ref(Path,App)},
				   {target,"_top"}],
			       [App]}]}]}
	      || {Path,App} <- Apps]},
	    ?NL],
    XML = xhtml(Title, CSS, Body),
    Text = xmerl:export_simple([XML], xmerl_html, []),
    edoc_lib:write_file(Text, Dir, ?MODULES_FRAME).

app_ref(Path,M) ->
    filename:join([Path,M,?EDOC_DIR,?INDEX_FILE]).
