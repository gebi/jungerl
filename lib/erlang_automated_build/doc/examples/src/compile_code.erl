%%%-----------------------------------------------------------------------------
%%% File        : compile_code.erl
%%% Author      : Gordon Guthrie <gordonguthie@backawinner.gg>
%%% Description : 
%%%
%%% Created : 18 Jan 2004 by Gordon Guthrie
%%%           <gordonguthrie@backawinner.gg>
%%%
%%%-----------------------------------------------------------------------------
-module(compile_code).

-export([start/0]).

start() ->
    compile_modules().

compile_modules() ->   

%%%   _____ _             _     ______    _ _ _   
%%%  / ____| |           | |   |  ____|  | (_) |  
%%% | (___ | |_ __ _ _ __| |_  | |__   __| |_| |_ 
%%%  \___ \| __/ _` | '__| __| |  __| / _` | | __|
%%%  ____) | || (_| | |  | |_  | |___| (_| | | |_ 
%%% |_____/ \__\__,_|_|   \__| |______\__,_|_|\__|
%%%
%%% Edit the code below to include all the files you want to compile
%%%
%%% You may have C code - if you do, this script wont help you

    %% Set up some root directories

    App_rt_dir = "/path/to/your/code",

    io:fwrite("~nStarting the compilation~n~n", []),

    %% First set up the include file
    Inc_list = [{i, App_rt_dir ++ "/path/to/your/include"},
		{i, App_rt_dir ++ "/path/to/another/include"},
		{i, "/maybe/add/usr/local/lib/yaws/include"}],

    %% Now define the supervisor compile files
    Sup1        = App_rt_dir ++ "/lib/app1-1.0/src/sup1.erl",
    Sup1_out    = App_rt_dir ++ "/lib/app1-1.0/ebin/",
    Sup2        = App_rt_dir ++ "/lib/app2-1.0/src/sup2.erl",
    Sup2_out    = App_rt_dir ++ "/lib/app2-1.0/ebin/",

    Sup_comp_list = [{Sup1, Sup1_out},
		     {Sup,  Sup2_out}],

    %% Now define the main compile files
    App1       = App_rt_dir ++ "/lib/app1-1.0/src/app1.erl",
    App1_out   = App_rt_dir ++ "/lib/app1-1.0/ebin/",
    App2       = App_rt_dir ++ "/lib/app2-1.0/src/app2.erl",
    App2_out   = App_rt_dir ++ "/lib/app2-1.0/ebin/",

    Main_comp_list = [{App1, App1_out},
		      {App2, App2_out},
],

    %% Now define the tools files
    Tools1     = App_rt_dir ++"/lib/tools-1.0/src/tools.erl",
    Tools1_out = App_rt_dir ++"/lib/tools-1.0/ebin/",
    Tools2     = App_rt_dir ++"/lib/tools-1.0/src/tools2.erl",
    Tools2_out = App_rt_dir ++"/lib/tools-1.0/ebin/",

    Tools_comp_list = [{Tools1, Tools1_out},
		       {Tools2, Tools2_out}],

%%%  ______           _   ______    _ _ _   
%%% |  ____|         | | |  ____|  | (_) |  
%%% | |__   _ __   __| | | |__   __| |_| |_ 
%%% |  __| | '_ \ / _` | |  __| / _` | | __|
%%% | |____| | | | (_| | | |___| (_| | | |_ 
%%% |______|_| |_|\__,_| |______\__,_|_|\__|


    Compile_lists = lists:append([Sup_comp_list,Main_comp_list,Tools_comp_list]),
    compile_funcs(Compile_lists, Inc_list).

compile_funcs(List, Inc_list) ->
    Options = lists:append(Inc_list, [return_errors]),
    %%io:fwrite("List is ~p~n", [List]),
    %%io:fwrite("Inc_list is ~p~n", [Inc_list]),
    New_list = [{X, [debug_info, {outdir, Y} | Options]}
		|| {X, Y} <- List],
    comp_lists(New_list).

comp_lists(List) ->
    comp_lists(List, ok).		
comp_lists([{File, Options}|T], OldStatus) ->
    ensure_outdir(Options),
    NewStatus = compile:file(File, Options),
    case OldStatus of
	ok ->	
	    case NewStatus of
		{ok, FileName} ->
		    io:fwrite("OK: ~p~n", [File]),
		       _Purge_results1 = code:purge(FileName),
		       _Delete_results = code:delete(FileName),
		       _Purge_results2 = code:purge(FileName),
		       _Load_results = code:load_file(FileName),
		       comp_lists(T, ok);
		       Error ->
		    io:fwrite("   Compile failure:    ~p~n", [File]),
		    io:fwrite("   Error is       :    ~p~n~n", [Error]),
			    comp_lists(T, error)
		    end;
		Other ->
		    case NewStatus of
			{ok, FileName2} ->
			    io:fwrite("OK: ~p~n", [File]),
			    _Delete_results = code:delete(FileName2),
			    _Purge_results = code:purge(FileName2),
			    _Load_results = code:load_file(FileName2),
			    %% This is the key - pass an error message on once
			    %% you get a single failure to compile
			    comp_lists(T, Other);
			Error ->
			    io:fwrite("   Compile failure:    ~p~n", [File]),
			    io:fwrite("   Error is       :    ~p~n~n", [Error]),
			    comp_lists(T, Other)
		    end
	    end;
	comp_lists([], Status) ->
	    io:fwrite("   Termination Status: ~p~n", [Status]),
	    Status.

%% Ensures that the output directory exists before compiling
ensure_outdir([debug_info, {outdir, Dir} | _]) ->
    %%io:fwrite("Checking Directory ~p~n", [Dir]),
    filelib:ensure_dir(Dir).
