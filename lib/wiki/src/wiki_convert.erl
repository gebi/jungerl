-module(wiki_convert).

-compile(export_all).
-import(lists, [foreach/2]).

root() -> os:getenv("HOME") ++ "/erl/erl.now/wiki_store-1.0".

go() ->
    F = find:files(root(), "*.wib", false), 
    foreach(fun(I) -> cvt(I) end, F).

cvt(I) ->
    io:format("Cvt:~p~n",[I]),
    case file:read_file(I) of
	{ok, Bin} ->
	    case (catch cvt2(binary_to_term(Bin))) of
		  {'EXIT', Why} ->
			 io:format("conversion error~p ~p~n", [I, Why]);
		  NewBin ->
		    F = filename:rootname(I) ++ ".wob",
		    io:format("~p ~p sizes ~p => ~p ok~n",
			      [I,F,size(Bin),size(NewBin)]),
		    file:write_file(F, NewBin)
		  end;
	_ ->
	    io:format("read error~p~n", [I])
    end.


cvt2({wik000, Pwd, Email, [H|T]}) -> 
    {B,Time,Who} = H,
    Txt = binary_to_list(B),
    Patches = cvt3(Txt, T),
    New = {wik001, Pwd, Email, Time, Who, Txt, Patches},
    term_to_binary(New).

cvt3(New, [{Bold,Time,Who}|T]) ->
    TxtOld = binary_to_list(Bold),
    Patch = diff:diff(New, TxtOld),
    [{Patch,Time,Who}|cvt3(TxtOld, T)];
cvt3(_,[]) ->
    [].
