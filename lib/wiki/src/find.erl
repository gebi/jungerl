-module(find).

%IA Joe Armstrong
%ID 970203
%IK [find,out_of_date,make,erl,jam]
%IH Find all files. Find all out of date files
%IT A find utilitiy which finds all files relative to a given root
% directory. <p><b>find:files(".", "*.erl", false)</b> finds all 
% files in the current directory.
% Recursive scan of sub-directories is also allowed.
% An "out of date" utility which checks for "out of date files".
% <p>For example <b>find:out_of_date(".",".erl",".jam")</b> finds all out of date
% Erlang files in the current directory.

-export([files/3, out_of_date/3]).

-import(lists, [suffix/2, sublist/3, map/2, filter/2]).
-import(misc, [is_file/1, outofdate/2, writeable/1]).

%% find(Dir, ReExpr, Recursive) -> [File]
%%     Find regular files starting from Dir
%%     Which match ReExpr
%%     If Recursive is true do recursivly on all sub-directories
%%     Example find(".", "*.erl", false) will find all erlang files in the 
%%     Current directory
%%
%% out_of_date(Dir, SrcExt, ObjExt) find all "out of date files" in
%%     Dir.
%%     Example:
%%         out_of_date(".", ".erl", ".jam") 
%%             Finds all out of date files in the current directory

%%+type files(string(), string(), bool()) -> [string()].

files(Dir, Re, Flag) -> 
    Re1 = string:re_sh_to_awk(Re),
    find_files(Dir, Re1, Flag, []).

find_files(Dir, Re, Flag, L) -> 
    case file:list_dir(Dir) of
	{ok, Files} -> find_files(Files, Dir, Re, Flag, L);
	{error, _}  -> L
    end.

find_files([File|T], Dir, Re, Recursive, L) ->
    FullName = Dir ++  [$/|File],
    case file_type(FullName) of
	regular ->
	    case string:re_match(FullName, Re) of
		{match, _, _}  -> 
		    find_files(T, Dir, Re, Recursive, [FullName|L]);
		_ ->
		    find_files(T, Dir, Re, Recursive, L)
	    end;
	directory -> 
	    case Recursive of
		true ->
		    L1 = find_files(FullName, Re, Recursive, L),
		    find_files(T, Dir, Re, Recursive, L1);
		false ->
		    find_files(T, Dir, Re, Recursive, L)
	    end;
	error -> 
	    find_files(T, Dir, Re, Recursive, L)
    end;
find_files([], _, _, _, L) ->
    L.

file_type(File) ->
    case file:file_info(File) of
	{ok, Facts} ->
	    case element(2, Facts) of
		regular   -> regular;
		directory -> directory;
		_         -> error
	    end;
	_ ->
	    error
    end.


%%______________________________________________________________________
%% outofdate(Dir, InExtension, OutExtension)
%%   scans Dir for all files with the extension "InExtension"
%%   If a file with this extension is found then "OutExtension" is checked
%%
%%   returns a list of files in <Dir> where *.OutExtension is
%%   "out of date" with respect to *.InExtension
%%   in the sence of "make"

out_of_date(Dir, In, Out) ->
    case file:list_dir(Dir) of
	{ok, Files0} ->
	    Files1 = filter(fun(F) -> suffix(In, F) end, Files0),
	    Files2 = map(fun(F) -> sublist(F, 1, length(F) - length(In)) end, 
			 Files1),
	    filter(fun(F) -> update(F, In, Out) end, Files2);
	_ ->
	    []
    end.
	  
update(File, In, Out) ->
    InFile  = File ++ In,
    OutFile = File ++ Out,
    case is_file(OutFile) of
	true ->
	    case writeable(OutFile) of
		true ->
		    outofdate(InFile, OutFile);
		false ->
		    %% can't write so we can't update
		    false
	    end;
	false ->
	    %% doesn't exist
	    true
    end.
