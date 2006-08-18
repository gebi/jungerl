%%% File    : bic.erl
%%% Author  : Tony Rogvall <tony@iMac.local>
%%% Description : BEAM interpreted C (ode)
%%% Created : 27 Dec 2005 by Tony Rogvall <tony@iMac.local>

-module(bic).

-compile(export_all).

-import(lists, [map/2]).

-include("../include/bic.hrl").

cpp(File) ->
    bic_cpp:file(File).

file(File) ->
    file(File,[]).

file(File,Env) ->
    case bic_cpp:open(File) of
	{ok,Fd} ->
	    bic_scan:init(),  %% setup some dictionay stuff
	    bic_parse:init(), %% setup some dictionay stuff
	    Res = (catch bic_parse:parse_and_scan({bic_scan, scan, [Fd]})),
	    bic_cpp:close(Fd),
	    case Res of 
		{error,{{Fn,Ln},Mod,Message}} when integer(Ln) ->
		    io:format("~s:~w: ~s\n",
			      [Fn,Ln,Mod:format_error(Message)]),
		    {error,parse_error};
		{error,{Ln,Mod,Message}} when integer(Ln) ->
		    io:format("~s:~w: ~s\n",
			      [File,Ln,Mod:format_error(Message)]),
		    {error,parse_error};
		{ok,List} when list(List) ->
		    map(fun(D) ->
				io:format("~p\n", [D])
			end, List),
		    List;
		Other -> 
		    Other
	    end;
	Error ->
	    Error
    end.








