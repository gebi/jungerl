%%%-------------------------------------------------------------------
%%% File    : rec_info.erl
%%% Author  :  Vlad Dumitrescu
%%% Description : Extract information about record definitions
%%%               and convert them into keylists.
%%% Created : 24 Mar 2005 by  Vlad Dumitrescu
%%%-------------------------------------------------------------------
-module(rec_info).

-export([
	 to_list/1, 
	 to_list/2,
	 get_fields/1,
	 get_fields/2,
	 get_names/2
	]).

%%%
-export([test/0]).

-record(a, {b,c=ok,d=[1,{2,3}]}).

test() ->
    A = #a{},
    to_list(A#a{c="hej"}).
%%%
    
to_list(Rec) when is_tuple(Rec) ->
    to_list(Rec, ?MODULE).

to_list(Rec, Module) when is_tuple(Rec) ->
    [R|_L] = tuple_to_list(Rec),
    F = get_fields(R, Module), 
    [ {Fx, element(Nx, Rec)} || {Nx, {Fx, _}} <- lists:zip(lists:seq(2, length(F)+1), F)].

get_fields(N) ->
    get_fields(N, ?MODULE).

get_fields(N, Module) ->
    {value, {N, F}} = lists:keysearch(N, 1, get_records(Module, [])),
    F.

get_names(N, Module) ->
    [F || {F,_} <- get_fields(N, Module)].

%%%%%%%%%%%%%%%%%%%%%%

fix_record({Name, Fields}) ->
    {Name, fix_record(Fields, [])}.

fix_record([], Res)  ->
    lists:reverse(Res);
fix_record([{record_field,_,{atom,_,N}}| T], Res) ->
    fix_record(T, [{N, undefined}|Res]);
fix_record([{record_field,_,{atom,_,N},X}| T], Res) ->
    fix_record(T, [{N, erl_syntax:concrete(X)}|Res]).
    
get_records(FileOrModule, Opts) ->
    L = read_records(FileOrModule, Opts),
    [fix_record(R) || {attribute, _, record, R} <- L].

%%% taken from shell.erl

%%% Read record information from file(s)

read_records(FileOrModule, Opts0) ->
    Opts = lists:delete(report_warnings, Opts0),
    case find_file(FileOrModule) of
        {files,[File]} ->
            read_file_records(File, Opts);
        {files,Files} ->
            lists:flatmap(fun(File) ->
                                  case read_file_records(File, Opts) of
                                      RAs when is_list(RAs) -> RAs;
                                      _ -> []
                                  end
                          end, Files);
        Error ->
            Error
    end.

-include_lib("kernel/include/file.hrl").

find_file(Mod) when is_atom(Mod) ->
    case code:which(Mod) of
	File when is_list(File) ->
	    {files,[File]};
	preloaded ->
	    {_M,_Bin,File} = code:get_object_code(Mod),
            {files,[File]};
        _Else -> % non_existing, interpreted, cover_compiled
            {error,nofile}
    end;
find_file(File) ->
    case catch filelib:wildcard(File) of
        {'EXIT',_} ->
            {error,invalid_filename};
        Files ->
            {files,Files}
    end.

read_file_records(File, Opts) ->
    case filename:extension(File) of
        ".beam" ->
            case beam_lib:chunks(File, [abstract_code,"CInf"]) of
                {ok,{_Mod,[{abstract_code,{Version,Forms}},{"CInf",CB}]}} ->
                    case record_attrs(Forms) of
                        [] when Version =:= raw_abstract_v1 ->
                            [];
                        [] -> 
                            %% If the version is raw_X, then this test
                            %% is unnecessary.
                            try_source(File, CB);
                        Records -> 
                            Records
                    end;
                {ok,{_Mod,[{abstract_code,no_abstract_code},{"CInf",CB}]}} ->
                    try_source(File, CB);
                Error ->
                    %% Could be that the "Abst" chunk is missing (pre R6).
                    Error
            end;
        _ ->
            parse_file(File, Opts)
    end.

%% This is how the debugger searches for source files. See int.erl.
try_source(Beam, CB) ->
    Os = case lists:keysearch(options, 1, binary_to_term(CB)) of
             false -> [];
             {value,{_,Os0}} -> Os0
    end,
    Src0 = filename:rootname(Beam) ++ ".erl",
    case is_file(Src0) of
	true -> parse_file(Src0, Os);
	false ->
	    EbinDir = filename:dirname(Beam),
	    Src = filename:join([filename:dirname(EbinDir), "src",
				 filename:basename(Src0)]),
	    case is_file(Src) of
		true -> parse_file(Src, Os);
		false -> {error, nofile}
	    end
    end.

is_file(Name) ->
    case filelib:is_file(Name) of
	true ->
	    not filelib:is_dir(Name);
	false ->
	    false
    end.

parse_file(File, Opts) ->
    Cwd = ".",
    Dir = filename:dirname(File),
    IncludePath = [Cwd,Dir|inc_paths(Opts)],
    case epp:parse_file(File, IncludePath, pre_defs(Opts)) of
        {ok,Forms} ->
            record_attrs(Forms);
        Error ->
            Error
    end.

pre_defs([{d,M,V}|Opts]) ->
    [{M,V}|pre_defs(Opts)];
pre_defs([{d,M}|Opts]) ->
    [M|pre_defs(Opts)];
pre_defs([_|Opts]) ->
    pre_defs(Opts);
pre_defs([]) -> [].

inc_paths(Opts) ->
    [P || {i,P} <- Opts, list(P)].

record_attrs(Forms) ->
    [A || A = {attribute,_,record,_D} <- Forms].

