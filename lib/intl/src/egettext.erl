%%% File    : egettext.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : msgstr extractor for erlang
%%% Created : 27 Dec 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(egettext).

-export([start/1, start/0]).

-compile(export_all).


%%
%% options accepted by egettext
%%
%% input file(s)
%%
%%  -f FILE | --files-from=FILE
%%  -D DIRECTORY | --directory=DIRECTORY
%%  - 
%%
%% output file
%%   -d NAME  | --default-domain=NAME
%%   -o FILE  | --output=FILE
%%   -p DIR   | --output-dir=DIR
%%
%% output details (* to be implemented)
%% *  -e, --no-escape
%% *  -E, --escape
%% *  --force-po
%% *  -i, --indent
%%   --no-location
%%   -n, --add-location
%% *  --strict
%% *  -w, --width=NUMBER
%% *  --no-wrap
%%   -s, --sort-output
%%   -F, --sort-by-file
%%   --omit-header
%% *  --copyright-holder
%% *  --foreign-user
%%   -m, --msgstr-prefix[=STRING]
%%   -M, --msgstr-suffix[=STRING]
%%
%%   -h, --help
%%   -V, --version
%%

-record(msgid,
	{
	  id,
	  id_plural,
	  location = []   %% [{File,Line}] | Line
	 }).


start() ->
    start([]).

start(Fs) ->
    Input  = input_files(Fs),
    Output = output_file(),
    Messages =
	lists:foldl(
	  fun (stdin, Msg0) ->
		  Res = fd(user, "*stdin*"),
		  erase(), %% erase dictionary
		  case Res of
		      {ok,Msg1} ->
			  Msg0 ++ Msg1;
		      Error ->
			  Msg0
		  end;
	      (File, Msg0) ->
		  case filename:extension(File) of
		      ".erl" ->
			  Res = file(File),
			  erase(), %% erase dictionary
			  case Res of
			      {ok,Msg1} ->
				  Msg0 ++ Msg1;
			      Error ->
				  Msg0
			  end;
		      _ ->
			  Msg0
		  end
	  end, [], Input),
    Messages1 = uniq(Messages),
    emit_file(Output, Messages1),
    halt(0).

%%
%% get all input files
%%
input_files(Fs) ->
    Acc0 = [],
    Acc1 = directory_files(Acc0),
    Acc2 = file_list_files(Acc1),
    case Fs of
	['-'] ->
	    [ stdin | Acc2];
	_ ->
	    lists:foldl(
	      fun(F, Acc3) ->
		      [atom_to_list(F)|Acc3]
	      end, Acc2, Fs)
    end.

%%
%% get output file name
%%
output_file() ->
    File0 = case init:get_argument('d') of
		{ok,[Nm|_]} -> Nm++".po";
		_ -> "messages.po"
	    end,
    File = case init:get_argument('o') of
	       {ok,[""]} -> stdout;
	       {ok,["-"]} -> stdout;
	       {ok,["/dev/stdout"]} -> stdout;
	       {ok,[F|_]} -> F;
	       _ -> File0
	   end,
    Dir = case init:get_argument('p') of
	      {ok,[D|_]} -> D;
	      _ -> "."
	  end,
    if File == stdout ->
	    stdout;
       true ->
	    filename:join(Dir, File)
    end.

%%
%% process -D option
%%
directory_files(Acc0) ->    
    case init:get_argument('D') of
	{ok,Ds} ->
	    lists:foldl(
	      fun(D, Acc1) ->
		      case file:list_dir(D) of
			  {ok,Files} ->
			      lists:foldl(
				fun(F, Acc2) ->
					[filename:join(D, F) | Acc2]
				end, Acc1, Files);
			  _ ->
			      Acc1
		      end
	      end,  Acc0, Ds);
	_ ->
	    Acc0
    end.

%%
%% process -F option
%%
file_list_files(Acc0) ->
    case init:get_argument('F') of
	{ok, Files} ->
	    lists:foldl(
	      fun(F, Acc) ->
		      case file:read_file(F) of
			  {ok, Bin} ->
			      Acc++
				  string:tokens(binary_to_list(Bin), 
						"\r\n\s\t");
			  Error ->
			      Acc
		      end
	      end, Acc0, Files);
	Error ->
	    Acc0
    end.

add_location() ->
    case init:get_argument('n') of
	{ok,_} -> true;
	_ ->
	    case init:get_argument('N') of
		{ok,_} -> false;
		_ -> true
	    end
    end.

omit_header() ->	    
    case init:get_argument('O') of
	{ok,_} -> true;
	_ -> false
    end.

%% check if messages are to be sorted
sort_output() ->
    case init:get_argument('s') of
	{ok,_} ->
	    msgid;
	_ ->
	    case init:get_argument('F') of
		{ok,_} ->
		    location;
		_ ->
		    no
	    end
    end.

msgstr_wrap() ->
    Prefix = case init:get_argument('m') of
		{ok,[P|_]} -> P;
		_ -> ""
	    end,
    Suffix = case init:get_argument('M') of
		{ok,[S|_]} -> S;
		_ -> ""
	    end,
    {Prefix,Suffix}.




file(File) ->
    case file:open(File, [read]) of
	{ok,Fd} ->
	    Res = fd(Fd, File),
	    file:close(Fd),
	    Res;
	Error ->
	    Error
    end.


fd(Fd, File) ->
    case epp_dodger:parse(Fd) of
	{ok, Forms} ->
	    case check_forms(Forms, File) of
		ok ->
		    Msg0 = lists:foldl(fun collect/2, [], Forms),
		    %% add file to message locations (and reverse list)
		    Msg1 = lists:foldl(
			     fun(M,Acc) ->
				     Line = M#msgid.location,
				     [M#msgid { location = [{File,Line}]}|Acc]
			     end,[], Msg0),
		    {ok, Msg1};
		error ->
		    {error, "parse error"}
	    end;
	Error ->
	    Error
    end.

%% Sort the messages and remove duplicates
%% But keep locations.
uniq(Msg0) ->
    case sort_output() of
	msgid ->
	    Msg1 = lists:keysort(#msgid.id, Msg0),
	    umerge(Msg1, []);
	location ->
	    Msg1 = merge_duplicates(Msg0),
	    lists:sort(fun cmp_location/2, Msg1);
	no ->
	    merge_duplicates(Msg0)
    end.

%% Heavy duty version keep order of input
merge_duplicates([M1 | Ms]) ->
    case lists:keysearch(M1#msgid.id, #msgid.id, Ms) of
	false ->
	    [M1 | merge_duplicates(Ms)];
	{value, M2} ->
	    merge_duplicates([merge(M1,M2) | 
			      lists:keydelete(M1#msgid.id, #msgid.id, Ms)])
    end;
merge_duplicates([]) ->
    [].


%% light version for sorted input
umerge([M1,M2 | Ms], Acc) ->
    if M1#msgid.id == M2#msgid.id ->
	    umerge([merge(M1,M2) | Ms], Acc);
       true ->
	    umerge([M2 | Ms], [M1|Acc])
    end;		    
umerge([M1], Acc) ->
    lists:reverse([M1|Acc]);
umerge([], Acc) ->
    lists:reverse(Acc).


%% sort on first location only !
cmp_location(M1, M2) ->
    [{F1,L1}|_] = M1#msgid.location,
    [{F2,L2}|_] = M2#msgid.location,
    if F1 == F2 ->
	    L1 < L2;
       true ->
	    F1 < F2
    end.
    

%% merge two message id when message is are equal
%% assumption (M1#msgid.id == M2#msgid.id)
merge(M1, M2) ->
    Location = M1#msgid.location ++ M2#msgid.location,
    if M1#msgid.id_plural == M2#msgid.id_plural ->
	    M1#msgid { location = Location };
       M1#msgid.id_plural == undefined ->
	    M2#msgid { location = Location };
       M2#msgid.id_plural == undefined ->
	    M1#msgid { location = Location };
       true ->
	    %% different forms of plural for same msgid!!!
	    [{F1,L1}|_] = M1#msgid.location,
	    [{F2,L2}|_] = M2#msgid.location,
	    io:format("egettext: Warning two plural forms\n"
		      "  ~s:~w: ~s\n"
		      "  ~s:~w: ~s\n"
		      " for msgid: ~s\n",
		      [F1,L1,M1#msgid.id_plural,
		       F2,L2,M2#msgid.id_plural,
		       M1#msgid.id]),
	    %% keep plural form of M1!
	    M1#msgid { location = Location }
    end.

    
    
    

emit_file(stdout, Messages) ->
    emit_fd(user, Messages);
emit_file(File, Messages) ->
    case file:open(File, [write]) of
	{ok,Fd} ->
	    Res = emit_fd(Fd, Messages),
	    file:close(Fd),
	    Res;
	Error ->
	    Error
    end.

emit_fd(Fd, Messages) ->
    case omit_header() of
	true -> ok;
	false -> emit_header(Fd)
    end,
    emit_msg(Fd, Messages).

emit_header(Fd) ->
    {{Year,Mon,Day},{H,M,S}} = calendar:now_to_universal_time(now()),
    {Prefix,Suffix} = msgstr_wrap(),
    io:put_chars(Fd, "# SOME DESCRIPTIVE TITLE.\n"),
    io:format(Fd, "# Copyright (C) YEAR THE PACKAGE'S COPYRIGHT HOLDER\n", []),
    io:format(Fd, "# This file is distributed under the same license as the PACKAGE package.\n"),
    io:format(Fd, "# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.\n",[]),
    io:put_chars(Fd, "#\n"),
    io:put_chars(Fd, "#, fuzzy\n"),
    io:put_chars(Fd, "msgid \"\"\n"),
    io:format(Fd, "msgstr \"~s~s\"\n", [Prefix,Suffix]),
    io:put_chars(Fd, "\"Project-Id-Version: PACKAGE VERSION\\n\"\n"),
    io:format(Fd, "\"POT-Creation-Date: ~4..0w-~2..0w-~2..0w "
	      "~2..0w:~2..0wZ\\n\"\n", [Year,Mon,Day,H,M]),
    io:format(Fd, "\"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n\"\n",[]),
    io:put_chars(Fd, "\"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n\"\n"
		 "\"Language-Team: LANGUAGE <LL@li.org>\\n\"\n"
		 "\"MIME-Version: 1.0\\n\"\n"
		 "\"Content-Type: text/plain; charset=CHARSET\\n\"\n"
		 "\"Content-Transfer-Encoding: 8bit\\n\"\n"
		 "\"Plural-Forms: nplurals=INTEGER;"
		 "plural=EXPRESSION;\\n\"\n").



emit_msg(Fd, Messages) ->
    AddLocation = add_location(),
    {Prefix,Suffix} = msgstr_wrap(),
    lists:foreach(
      fun(M) ->
	      io:format(Fd,"\n", []),
	      if AddLocation == true ->
		      io:put_chars(Fd,"#:"),
		      lists:foreach(
			fun({File,Line}) ->
				io:format(Fd, " ~s:~w", [File,Line])
			end, M#msgid.location),
		      io:nl(Fd);
		 true ->
		      ok
	      end,
	      %% Howto- make multi programming language support ??
	      %% i.e erl-format "There are ~w boys"
	      %%     c-format   "There are %d boys"
	      %% io:put_chars(Fd,"#, no-c-format\n"),
	      %%
	      io:format(Fd,"msgid ~p\n", [M#msgid.id]),
	      if M#msgid.id_plural =/= undefined ->
		      io:format(Fd,"msgid_plural ~p\n", [M#msgid.id_plural]),
		      io:format(Fd,"msgstr[0] \"~s~s\"\n", [Prefix,Suffix]),
		      io:format(Fd,"msgstr[1] \"~s~s\"\n", [Prefix,Suffix]);
		 true ->
		      io:format(Fd, "msgstr \"~s~s\"\n", [Prefix,Suffix])
	      end
      end, Messages).


check_forms(Fs, Name) ->
    lists:foldl(
      fun (F,Status) ->
	      case erl_syntax:type(F) of
		  error_marker ->
		      S = case erl_syntax:error_marker_info(F) of
			      {_, M, D} ->
				  M:format_error(D);
			      _ ->
				  "unknown error"
			  end,
		      report_error({Name, erl_syntax:get_pos(F),
				    " ~s"}, [S]),
		      error;
		  _ ->
		      Status
	      end
      end, ok, Fs).


report_error({F, L, D}, Vs) ->
    report({F, L, {error, D}}, Vs);
report_error(D, Vs) ->
    report({error, D}, Vs).

report(D, Vs) ->
    report(D, Vs, 1).
report(D, Vs, N) ->
    report(1, D, Vs, N).

report(Level, D, Vs, N) when integer(N), N < Level ->
    ok;
report(Level, D, Vs, N) when integer(N) ->
    io:put_chars(format(D, Vs)).


format({error, D}, Vs) ->
    ["error: ", format(D, Vs)];
format({warning, D}, Vs) ->
    ["warning: ", format(D, Vs)];
format({"", L, D}, Vs) when integer(L), L > 0 ->
    [io_lib:fwrite("~w: ", [L]), format(D, Vs)];
format({"", L, D}, Vs) ->
    format(D, Vs);
format({F, L, D}, Vs) when integer(L), L > 0 ->
    [io_lib:fwrite("~s:~w: ", [filename(F), L]), format(D, Vs)];
format({F, L, D}, Vs) ->
    [io_lib:fwrite("~s: ", [filename(F)]), format(D, Vs)];
format({F, L, D}, Vs) ->
    format(D, Vs);
format(S, Vs) when list(S) ->
    [io_lib:fwrite(S, Vs), $\n].


filename([C | T]) when integer(C), C > 0, C =< 255 ->
    [C | filename(T)];
filename([H|T]) ->
    filename(H) ++ filename(T);
filename([]) ->
    [];
filename(N) when atom(N) ->
    atom_to_list(N);
filename(N) ->
    report_error("bad filename: `~P'.", [N, 25]),
    exit(error).


%%
%% collect MsgId's of
%%
%% intl:gettext(MsgId)
%% intl:ngettext(MsgId, MsgIdPlural, N)
%% intl:dgettext(DomainName, MsgId)
%% intl:dngettext(DomainName, MsgId, MsgIdPlural, N)
%% intl:dcgettext(DomainName, MsgId, Category)
%% intl:dcngettext(DomainName, MsgId, MsgIdPlural, N, Category)
%% 
%% ?_(MsgId)
%% ?N_(MsgId)   -- used for logs etc
%%
%%
collect(Node, Acc) ->
    case erl_syntax:type(Node) of
	attribute ->
	    do_attribute(Node),
	    Acc;
	_ ->
	    erl_syntax_lib:fold(fun collect_node/2, Acc, Node)
    end.

do_attribute(Node) ->
    Name = erl_syntax:attribute_name(Node),
    case erl_syntax:atom_value(Name) of
	import ->
	    do_import(Node);
	_ ->
	    ok
    end.

do_import(Node) ->
    case erl_syntax:attribute_arguments(Node) of
	[Mod,List] ->
	    Args = erl_syntax:list_elements(List),
	    case erl_syntax:atom_value(Mod) of
		intl ->
		    lists:map(
		      fun(A) ->
			      NAri = erl_syntax:arity_qualifier_argument(A),
			      NFun = erl_syntax:arity_qualifier_body(A),
			      Ari = erl_syntax:integer_value(NAri),
			      Fun = erl_syntax:atom_value(NFun),
			      put({import,intl,Fun}, Ari)
		      end, Args);
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end.


collect_node(Node, Acc) ->
    case erl_syntax:type(Node) of
	macro ->
	    Name = erl_syntax:macro_name(Node),
	    Args = erl_syntax:macro_arguments(Node),
	    N = if list(Args) -> length(Args);
		   true -> -1
		end,
	    case Name  of
		{var,Ln,'N_'} when N == 1 ->
		    collect_msgid(lists:nth(1,Args), Acc);
		{var,Ln,'_'} when N == 1 ->
		    collect_msgid(lists:nth(1,Args), Acc);
		_ ->
		    Acc
	    end;
	application ->
	    Op = erl_syntax:application_operator(Node),
	    Args = erl_syntax:application_arguments(Node),
	    NArgs = length(Args),
	    case call(Op) of
		ignore -> Acc;
		{intl,Fun} ->
		    collect_call(Fun,NArgs,Args,Acc);
		Fun when atom(Fun) ->
		    %% check for imported intl functions
		    case get({import,intl,Fun}) of
			undefined -> Acc;
			Arity when Arity == NArgs ->
			    collect_call(Fun,NArgs,Args,Acc);
			_ ->
			    Acc
		    end;
		_ ->
		    Acc
	    end;
	_ ->
	    Acc
    end.

%% Collect message ids from intl calls
collect_call(gettext, 1, Args, Acc) ->
    collect_msgid(lists:nth(1, Args), Acc);
collect_call(gettext_noop, 1, Args, Acc) ->
    collect_msgid(lists:nth(1, Args), Acc);
collect_call(ngettext, 3, Args, Acc) ->
    collect_msgid_plural(lists:nth(1, Args),lists:nth(2, Args),Acc);
collect_call(dgettext, 2, Args, Acc) ->
    collect_msgid(lists:nth(2, Args), Acc);
collect_call(dngettext, 4, Args, Acc) ->
    collect_msgid_plural(lists:nth(2, Args),lists:nth(3, Args),Acc);    
collect_call(dcgettext, 3, Args, Acc) ->
    collect_msgid(lists:nth(2, Args), Acc);
collect_call(dcngettext, 5, Args, Acc) ->
    collect_msgid_plural(lists:nth(2, Args),lists:nth(3, Args),Acc);    
collect_call(Fun, N, Args, Acc) ->
    Acc.

call(Node) ->
    case erl_syntax:type(Node) of
	module_qualifier ->
	    case erl_syntax:module_qualifier_argument(Node) of
		{atom,Ln,Mod} ->
		    case erl_syntax:module_qualifier_body(Node) of
			{atom,_,Fun} -> {Mod,Fun};
			_ -> ignore
		    end;
		_ ->
		    ignore
	    end;
	atom ->
	    case Node of
		{_, Ln, Fun} ->
		    Fun;
		_ ->
		    ignore
	    end;
	_ ->
	    ignore
    end.
	    
collect_msgid(Node, Acc) ->
    case erl_syntax:type(Node) of
	string ->
	    MsgId = erl_syntax:string_value(Node),
	    Pos = erl_syntax:get_pos(Node),
	    [#msgid { id = MsgId, location = Pos} | Acc];
	_ ->
	    Acc
    end.

collect_msgid_plural(Node,NodePlural,Acc) ->
    case erl_syntax:type(Node) of
	string ->
	    MsgId = erl_syntax:string_value(Node),
	    Pos = erl_syntax:get_pos(Node),
	    case erl_syntax:type(NodePlural) of
		string ->
		    MsgIdPlural = erl_syntax:string_value(NodePlural),
		    [#msgid { id = MsgId, id_plural=MsgIdPlural, location=Pos}|
		     Acc];
		_ ->
		    [#msgid { id = MsgId,location=Pos} |Acc]
	    end;
	_ ->
	    Acc
    end.

    


	      
    


