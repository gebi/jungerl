-module(gettext_compile).
%%%----------------------------------------------------------------------
%%% Created:  27 Oct 2003 by tobbe@bluetail.com
%%% Function: Tools for multi-lingual capabilities,
%%%           similar to GNU gettext.
%%%
%%%   NB: MAKE SURE TO NOT CALL ANY OTHER MODULE IN THE SYSTEM.
%%%       THIS CODE RUNS IN A PRE-BUILD PHASE !!!
%%%
%%% $Id$
%%%----------------------------------------------------------------------
-export([parse_transform/2, epot2po/0]).

-include("gettext.hrl").

-ifdef(debug).
%%-define(debug(S,A), io:format( S, A)).
-define(debug(S,A), io:format(get(fd), S, A)).
-else.
%%-define(debug(S,A), io:format(get(fd), S, A)).
-define(debug(S,A), true).
-endif.

%%% --------------------------------------------------------------------
%%% From the Erlang po-Template file, create a GNU po-file.
%%% --------------------------------------------------------------------
epot2po() ->
    {Gettext_App_Name, GtxtDir, DefLang} = get_env(),
    open_epot_file(Gettext_App_Name, GtxtDir),
    Es = lists:keysort(1, get_epot_data()),
    close_epot_file(),
    open_po_file(Gettext_App_Name, GtxtDir, DefLang),
    write_header(),
    write_entries(Es),
    close_file(),
    init:stop().


write_entries(L) ->
    Fd = get(fd),
    F = fun({Id,Finfo}) ->
		Fi = fmt_fileinfo(Finfo),
		io:format(Fd, "~n#: ~s~n", [Fi]),
		file:write(Fd, "msgid \"\"\n"),
		write_pretty(Id),
		file:write(Fd, "msgstr \"\"\n"),
		write_pretty(Id)
	end,
    lists:foreach(F, L).

-define(ENDCOL, 72).
-define(PIVOT, 4).
-define(SEP, $\s).

write_pretty(Str) ->
    write_pretty(Str, get(fd)).

write_pretty([], _) ->
    true;
write_pretty(Str, Fd) when length(Str) =< ?ENDCOL ->
    write_string(Str, Fd);
write_pretty(Str, Fd) ->
    {Line, Rest} = get_line(Str),
    write_string(Line, Fd),
    write_pretty(Rest, Fd).

write_string(Str, Fd) ->
    file:write(Fd, "\""),
    file:write(Fd, Str),
    file:write(Fd, "\"\n").


%%% Split the string into substrings, 
%%% aligned around a specific column.
get_line(Str) ->
    get_line(Str, ?SEP, 1, ?ENDCOL, []).

%%% End of string reached.
get_line([], _Sep, _N, _End, Acc) ->
    {lists:reverse(Acc), []};
%%% Eat characters.
get_line([H|T], Sep, N, End, Acc) when N < End ->
    get_line(T, Sep, N+1, End, [H|Acc]);
%%% Ended with a Separator on the End boundary.
get_line([Sep|T], Sep, End, End, Acc) ->
    {lists:reverse([Sep|Acc]), T};
%%% At the end, try to find end of token within
%%% the given constraint, else backup one token.
get_line([H|T] = In, Sep, End, End, Acc) ->
    case find_end(T, Sep) of
	{true, Racc, Rest} ->
	    {lists:reverse(Racc ++ [H|Acc]), Rest};
	false ->
	    case reverse_tape(Acc, In) of
		{true, Bacc, Rest} ->
		    {lists:reverse(Bacc), Rest};
		{false,Str} ->
		    %%% Ugh...the word is longer than ENDCOL...
		    split_string(Str, ?ENDCOL)
	    end
    end.

find_end(Str, Sep) ->
    find_end(Str, Sep, 1, ?PIVOT, []).

find_end([Sep|T], Sep, N, Pivot, Acc) when N =< Pivot ->
    {true, [Sep|Acc], T};
find_end(_Str, _Sep, N, Pivot, _Acc) when N > Pivot ->
    false;
find_end([H|T], Sep, N, Pivot, Acc) ->
    find_end(T, Sep, N+1, Pivot, [H|Acc]);
find_end([], _Sep, _N, _Pivot, Acc) ->
    {true, Acc, []}.
    
reverse_tape(Acc, Str) ->
    reverse_tape(Acc, Str, ?SEP).

reverse_tape([Sep|_T] = In, Str, Sep) ->
    {true, In, Str};
reverse_tape([H|T], Str, Sep) ->
    reverse_tape(T, [H|Str], Sep);
reverse_tape([], Str, _Sep) ->
    {false, Str}.

split_string(Str, End) ->
    split_string(Str, End, 1, []).

split_string(Str, End, End, Acc) ->
    {lists:reverse(Acc), Str};
split_string([H|T], End, N, Acc) when N < End ->
    split_string(T, End, N+1, [H|Acc]);
split_string([], _End, _N, Acc) ->
    {lists:reverse(Acc), []}.
    


fmt_fileinfo(Finfo) ->
    F = fun({Fname,LineNo}, Acc) ->
		Fname ++ ":" ++ to_list(LineNo) ++ [$\s|Acc]
	end,
    lists:foldr(F,[],Finfo).
		

   
write_header() ->
    io:format(get(fd),
	      "# SOME DESCRIPTIVE TITLE.\n"
	      "# Copyright (C) YEAR THE PACKAGE'S COPYRIGHT HOLDER\n"
	      "# This file is distributed under the same license as the PACKAGE package.\n"
	      "# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.\n"
	      "#\n"
	      "# NB: Consider using poEdit <http://poedit.sourceforge.net>\n"
	      "#\n"
	      "#\n"
	      "#, fuzzy\n"
	      "msgid \"\"\n"
	      "msgstr \"\"\n"
	      "\"Project-Id-Version: PACKAGE VERSION\\n\"\n"
	      "\"POT-Creation-Date: 2003-10-21 16:45+0200\\n\"\n"
	      "\"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n\"\n"
	      "\"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n\"\n"
	      "\"Language-Team: LANGUAGE <LL@li.org>\\n\"\n"
	      "\"MIME-Version: 1.0\\n\"\n"
	      "\"Content-Type: text/plain; charset=iso-8859-1\\n\"\n"
	      "\"Content-Transfer-Encoding: 8bit\\n\"\n",
	      []).

 
%%% --------------------------------------------------------------------
%%% NB: We assume that the surrounding code does some preparations:
%%%
%%%   1. Setup the environment variables: 'gettext_dir' and 'gettext_tmp_name'
%%%   
%%%   2. The compiler is called with the 'gettext' flag.
%%%
%%%   3. The file $(gettext_dir)/lang/$(gettext_tmp_name)/epot.dets is 
%%%      removed before the first erlang/yaws file is processed.
%%%      (entrys are appended to the file)
%%% --------------------------------------------------------------------
parse_transform(Form,Opts) ->
    case lists:member(gettext, Opts) of
	true ->
	    {Gettext_App_Name, GtxtDir, _} = get_env(),
	    open_epot_file(Gettext_App_Name, GtxtDir),
	    ?debug( "--- Opts --- ~p~n",[Opts]),
	    ?debug("--- Env --- isd_type=~p , gettext_dir=~p~n", [Gettext_App_Name,GtxtDir]),
	    pt(Form, Opts),
	    close_file(),
	    Form;
	_ ->
	    Form
    end.

get_env() ->
    {os:getenv("gettext_tmp_name"),
     os:getenv("gettext_dir"),
     os:getenv("gettext_def_lang")}.


pt(Form, Opts) ->
    put(fname, ""),
    pt(Form, Opts, undefined).

pt([H|T],Opts,Func) when list(H) ->
    ?debug( "--- 1 --- ~p~n",[H]),
    F = fun (X) -> pt(X,Opts,Func) end,
    [lists:map(F,H)|pt(T,Opts,Func)];
%%%
pt({call,L1,{remote,L2,{atom,L3,gettext},{atom,L4,key2str}},
    [{string,L5,String}]}, _Opts, _Func) ->
    ?debug( "++++++ String=<~p>~n",[String]),
    dump(String, L5),
    {call,L1,
     {remote,L2,
      {atom,L3,gettext},
      {atom,L4,key2str}},
     [{string,L5,String}]};
%%%
pt([{call,_,{remote,_,{atom,_,gettext},{atom,_,key2str}},
    [{string,L5,String}]} = H | T], Opts, Func) ->
    ?debug( "++++++ String=<~p>~n",[String]),
    dump(String, L5),
    [H | pt(T, Opts, Func)];
%%%
pt([{attribute,_L,module,Mod} = H | T], Opts, Func) ->
    put(fname, to_list(Mod) ++ ".erl"),
    ?debug( "++++++ Filename 1 =<~p>~n",[get(fname)]),
    [H | pt(T, Opts, Func)];
%%%
pt([{attribute,_L,yawsfile,Fname} = H | T], Opts, Func) ->
    put(fname, to_list(Fname)),
    ?debug( "++++++ Filename 2 =<~p>~n",[get(fname)]),
    [H | pt(T, Opts, Func)];
%%%
pt([{block,N,B}|T], Opts, Func) ->
    ?debug( "--- 2 --- ~p~n",[block]),
    Block = {block,N,pt(B,Opts,Func)},
    [Block|pt(T, Opts, Func)];
%%%
pt([H|T], Opts, Func) when tuple(H) ->
    ?debug( "--- 3 --- ~p~n",[H]),
    [while(size(H), H, Opts, Func) | pt(T, Opts, Func)];
%%%
pt([H|T], Opts, Func) ->
    ?debug( "--- 4 --- ~p~n",[H]),
    [H | pt(T, Opts, Func)];
%%%
pt(T, Opts, Func) when tuple(T) ->
    ?debug( "--- 5 --- ~p~n",[T]),
    while(size(T), T, Opts, Func);
%%%
pt(X, _, _) ->
    ?debug( "--- 6 --- ~p~n",[X]),
    X.

while(_,{block,N,B},Opts,Func) ->
    {block,N,pt(B,Opts,Func)};
while(N,T,Opts,Func) when N>0 ->
    NT = setelement(N,T,pt(element(N,T),Opts,Func)),
    while(N-1,NT,Opts,Func);
while(0,T,_,_) ->
    T.


dump(Str,L) -> 
    Fname = get(fname),
    Finfo = get_file_info(Str),
    dets:insert(?EPOT_TABLE, {escape_chars(Str), [{Fname,L}|Finfo]}).

get_file_info(Key) ->
    case dets:lookup(?EPOT_TABLE, Key) of
	[]            -> [];
	[{_,Finfo}|_] -> Finfo
    end.

escape_chars(Str) ->
    F = fun($", Acc)  -> [$\\,$"|Acc];
           ($\\, Acc) -> [$\\,$\\|Acc];
           ($\n, Acc) -> [$\\,$n|Acc];
	   (C, Acc)   -> [C|Acc] 
	end,
    lists:foldr(F, [], Str).

open_epot_file(Gettext_App_Name, GtxtDir) ->
    Fname = mk_epot_fname(Gettext_App_Name, GtxtDir),
    filelib:ensure_dir(Fname),
    {ok, _} = dets:open_file(?EPOT_TABLE, [{file, Fname}]).

close_epot_file() ->
    dets:close(?EPOT_TABLE).

get_epot_data() ->
    dets:foldl(fun(E, Acc) -> [E|Acc] end, [], ?EPOT_TABLE).

mk_epot_fname(Gettext_App_Name, GtxtDir) ->
    GtxtDir ++ "/lang/" ++ Gettext_App_Name ++ "/epot.dets".

open_po_file(Gettext_App_Name, GtxtDir, DefLang) ->
    DefDir = filename:join([GtxtDir, "lang", Gettext_App_Name, DefLang]),
    Fname = filename:join([DefDir, "gettext.po"]),
    filelib:ensure_dir(Fname),
    {ok,Fd} = file:open(Fname, [write]),
    put(fd,Fd).

close_file() ->
    file:close(get(fd)).

to_list(A) when atom(A)    -> atom_to_list(A);
to_list(I) when integer(I) -> integer_to_list(I);
to_list(B) when binary(B)  -> binary_to_list(B);
to_list(L) when list(L)    -> L.




