-module(html_expand).

-export([dir/0, file/1]).

-import(lists, [foreach/2, reverse/1]).

%% try the command html

dir() ->
    foreach(fun file/1, find:out_of_date(".", ".mhtml", ".html")).
    
file(File) ->
    io:format("expanding:~s.mhtml~n", [File]),
    Toks =  html_tokenise:file2toks(File ++ ".mhtml"),
    {Mod, Toks1} = get_macro_name(Toks),
    Toks2 = expand(Toks1, Mod),
    %% io:format("Toks2:~p~n", [Toks2]),
    html_tokenise:toks2file(Toks2, File).

expand([{tagStart,[$$|Start]}|Toks], Mod) ->
    {Args, Toks1} = collect_args([$$|Start], Toks, Mod),
    Toks2 = apply_macro(Mod, Start, Args),
    expand(Toks2 ++ Toks1, Mod);
expand([H|T], Mod) ->
    [H|expand(T, Mod)];
expand([], _) ->
    [].

collect_args(Stop, Toks, Mod) ->
    collect_args(Stop, Toks, Mod, [], []).

collect_args(Stop, [{tagEnd, Stop}|Toks], Mod, C, L) ->
    {reverse([reverse(C)|L]), Toks};
collect_args(Stop, [{tagStart, [$$|Start]}|Toks], Mod, C, L) ->
    {Args, Toks1} = collect_args([$$|Start], Toks, Mod),
    Toks2 = apply_macro(Mod, Start, Args),
    collect_args(Stop, Toks2, Mod, C, L);
collect_args(Stop, [{tagStart, "<>"}|Toks], Mod, C, L) ->
    collect_args(Stop, Toks, Mod, [], [reverse(C)|L]);
collect_args(Stop, [H|T], Mod, C, L) ->
    collect_args(Stop, T, Mod, [H|C], L);
collect_args(Stop, [], Mod, C, L) ->
    {reverse([reverse(C)|L]), []}.

get_macro_name([{tagStart,"$macro"},{raw,Macro},{tagEnd,"$macro"}|Toks]) ->
    {list_to_atom(Macro), Toks}.

apply_macro(Mod, Name, Args) ->
    %% io:format("Here: ~p ~p :: ~p~n", [Mod, Name, Args]),
    Func = list_to_atom(Name),
    String = case catch apply(Mod, Func, Args) of 
		 {'EXIT', Why} ->
		     io:format("Bad macro call:~p~n", [{Mod,Func,Args}]),
		     "** error **";
		 Str ->
		     Str
	     end,
    html_tokenise:string2toks(String).

    



















