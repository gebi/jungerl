-module(html_analyse).

%IA Joe Armstrong
%ID 970314
%IK [html,analyse,img]
%IH Extracts names of all images from an HTML file
%IT <p><b>html_analyse(File) -> [File]</b> returns a list of all relative
% URL's or images contained in a given document.

-compile(export_all).

-import(lists, [member/2, foldl/3]).

file(File) ->
    Toks = html_tokenise:file2toks(File),
    analyse(Toks).

analyse(Toks) ->
    Hrefs = [H || {tagStart, "a", L} <- Toks, {"href", H} <- L],
    Images1 = [S || {tagStart, "img", L} <- Toks, {"src", S} <- L],
    Images2 = [S || {tagStart, "body", L} <- Toks, {"background", S} <- L],
    {remove_duplicates(Hrefs), remove_duplicates(Images1++Images2)}.

remove_duplicates(L) ->
    foldl(fun(X, Acc) ->
		 case member(X, Acc) of
		     true  -> Acc;
		     false -> [X|Acc]
		 end
	 end, [], L).





			 
