%%% File    : edep.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : Dependecy generator for erlang files
%%% Created :  8 Dec 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(edep).

-compile(export_all).

-include_lib("kernel/include/file.hrl").

start(Fs) ->
    lists:foreach(
      fun(F) ->
	      File = atom_to_list(F),
	      case filename:extension(File) of
		  ".erl" ->
		      file(File);
		  _ ->
		      %% What about other files???
		      ignore
	      end
      end, Fs),
    halt(0).
      
%%
%% Arguments needed to edp
%%  -I used for locating -include
%% -pa -pz are used implicitly to locate include libs
%%

file(File) ->
    case parse(File) of
	{ok,Includes} ->
	    emit(File, resolve(Includes));
	Error ->
	    io:format("Error: ~p\n", [Error]),
	    Error
    end.

%%
%% Note! uses dictionary to store file -> include lists
%%
parse(File) ->
    case get({includes,File}) of
	undefined ->
	    case epp_dodger:parse_file(File) of
		{ok, Forms} ->
		    Incs = lists:foldl(fun collect/2, [], Forms),
		    put({includes,File}, Incs),
		    {ok, Incs};
		Error ->
		    Error
	    end;
	Incs -> {ok,Incs}
    end.
    
%% 
%% emit make dependency rules
%% FIXME: add support for other target than beam's
%%        add filename quoting support?
%%
emit(Source, Deps) ->
    Ext    = filename:extension(Source),
    Obj    = filename:basename(Source, Ext)++".beam",
    Dir    = case init:get_argument('o') of
		 {ok,[Path|_]} -> Path;
		 _ -> "."
	     end,
    Target = filename:join(Dir, Obj),
    Line = make_depline([Source | Deps], [Target,$:], length(Target)+1),
    io:format("~s\n", [Line]).


make_depline([D|Ds], Acc, Len) ->
    DLen = length(D),
    if Len+DLen+1 >= 80 ->
	    make_depline(Ds, [Acc,$\s,$\\,$\n,$\s,D], DLen+1);
       true ->
	    make_depline(Ds, [Acc,$\s,D], Len+DLen+1)
    end;
make_depline([], Acc, Len) ->
    lists:flatten(Acc).

    

resolve(Includes) ->
    resolve(Includes, []).

resolve([{include,File}|Is], Cs) ->
    case find_include(File) of
	{ok, Path} ->
	    case lists:member(Path, Cs) of
		true ->
		    resolve(Is, Cs);
		false ->
		    case parse(Path) of
			{ok, Is1} ->
			    resolve(Is++Is1, [Path | Cs]);
			{error, Err} ->
			    io:format("Error: ~p\n", [Err]),
			    resolve(Is, [Path | Cs])
		    end
	    end;
	Error ->
	    io:format("Warning: can not find include file ~p\n",[File]),
	    resolve(Is, Cs)
    end;
resolve([{include_lib,LFile}|Is], Cs) ->
    case find_include_lib(LFile) of
	{ok, Path} ->
	    case ignore_lib(Path) of
		true ->
		    resolve(Is, Cs);
		false ->
		    case lists:member(Path, Cs) of
			true -> 
			    resolve(Is, Cs);
			false ->
			    case parse(Path) of
				{ok, Is1} ->
				    resolve(Is++Is1, [Path | Cs]);
				{error, Err} ->
				    io:format("Error: ~p\n", [Err]),
				    resolve(Is, [Path | Cs])
			    end
		    end;
		Error ->
		    io:format("Warning: can not find include_lib file ~p\n",
			      [LFile]),
		    resolve(Is, Cs)
	    end
    end;
resolve([],Cs) ->
    Cs.

%%
%% Determine if system include_lib's are to be included or not
%%
ignore_lib(Path) ->
    case init:get_argument('MM') of
	{ok,_} ->
	    LibDir = code:lib_dir(),
	    lists:prefix(LibDir, Path);
	_ ->
	    false
    end.
	    

find_include(File) ->
    case filename:pathtype(File) of
	absolute -> 
	    {ok,File}; %% check file
	relative ->
	    Paths =
		case init:get_argument('I') of
		    {ok,PPaths} ->
			["." | lists:foldr(
				 fun([P|_],A) -> [P|A];
				    ([],A) -> A
				 end, [],PPaths)];
		    error ->
			["."]
		end,
	    find_file(Paths, File)
    end.

find_include_lib(LFile) ->
    [Name|LFile1] = filename:split(LFile),
    find_include_lib(filename:join(LFile1), Name, code:get_path()).

find_include_lib(LFile, Name, [Path|Ps]) ->
    case lists:reverse(filename:split(Path)) of
	["ebin", Component | RPath ] ->
	    case find_lib_name(Name, Component, Ps) of
		{value,Component1} ->
		    Lib = filename:join(lists:reverse([Component1 | RPath])),
		    Path1 = filename:join(Lib,LFile),
		    case file_exist(Path1) of
			true  -> {ok,Path1};
			false -> find_include_lib(LFile, Name, Ps)
		    end;
		false ->
		    find_include_lib(LFile, Name, Ps)
	    end;
	_ ->
	    find_include_lib(LFile, Name, Ps)
    end;
find_include_lib(LFile, Name, []) ->
    {error, not_found}.


find_lib_name(Name, Component, Ps) ->
    case string:tokens(Component,"-.") of
	[Name | Version] ->
	    %% fixme: check version and locate the highest version
	    {value, Component};
	_ ->
	    false
    end.

find_file([Path|Ps], File) ->
    Path1 = filename:join(Path, File),
    case file_exist(Path1) of
	true -> {ok,Path1};
	false -> find_file(Ps, File)
    end;
find_file([], File) ->
    {error, not_found}.



file_exist(Path) ->
    case file:read_file_info(Path) of
	{ok,Info} ->
	    Info#file_info.type == regular;
	{ok,_} ->
	    false;
	Error ->
	    false
    end.
    

%%
%% Collect {include_lib, Path}
%% and     {include,Path}
%%

collect(Node, Cs) ->
    case erl_syntax:type(Node) of
	attribute ->
	    collect_attribute(Node,Cs);
	_ ->
	    Cs
    end.

collect_attribute(Node, Cs) ->
    case erl_syntax:attribute_name(Node) of
	{atom,_,include_lib} ->
	    collect_include(include_lib, Node, Cs);
	{atom,_,include} ->
	    collect_include(include, Node, Cs);
	_ ->
	    Cs
    end.

collect_include(Tag, Node, Cs) ->
    case erl_syntax:attribute_arguments(Node) of
	[{string,_,File}] ->
	    [ {Tag,File} | Cs];
	_ ->
	    Cs
    end.






	





