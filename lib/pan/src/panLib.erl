%%%----------------------------------------------------------------------
%%% File    : panLib.erl
%%% Author  : Mats Cronqvist <etxmacr@avc386>
%%% Purpose : 
%%% Created :  8 Mar 2001 by Mats Cronqvist <etxmacr@avc386>
%%%----------------------------------------------------------------------

-module(panLib).
-author('etxmacr@avc386').

-include_lib("kernel/include/file.hrl").
-include_lib("kernel/include/inet.hrl").
-include("pan.hrl").

-export([get_files/1]).
-export([del_dir/1]).

-export([fname/0, fname/1]).
-export([verify_dir/1]).
-export([get_dir/1]).
-export([delete_dir/1]).
-export([emu_vers/0]).
-export([timestamp/0,timestamp/1]).
-export([host/0]).
-export([get_ip/0]).
-export([ls/1]).
-export([log/5]).
-export([slogger/1]).
-export([llogger/1]).
-export([empty/1]).

-define(TSFORM, "~2.2.0w~2.2.0w~2.2.0w-~2.2.0w~2.2.0w~2.2.0w").
%-define(LOGGER, error_logger).
%-define(ERRORLOG, error_report).
%-define(INFOLOG, info_report).
-define(LOGGER, ?MODULE).
-define(ERRORLOG, llogger).
-define(INFOLOG, slogger).
%%%-define(DBGLOG, empty)
-define(DBGLOG, llogger).
%%%---------------------------------------------------------------------------
fname() -> fname(now()).
fname(Now) -> atom_to_list(node())++"-"++timestamp(Now).

get_dir(Now) ->
    verify_dir(filename:join([get_base(), pan, fname(Now)])).

get_base() ->
    case catch sysI:log_dir() of
	{'EXIT', _} -> os:getenv("HOME");
	LogDir -> LogDir
    end.

verify_dir(Dir) ->				       %sysI:verify_dir
    case file:read_file_info(Dir) of
        {ok, #file_info{type = directory, access = read_write}} -> %exists, OK
	    Dir;
        {ok, FI} ->				       %exists, nok
            exit({access, Dir});
        {error, Reason} ->			       %doesn't exist
            try_create(Dir)
    end.

try_create("/") -> exit({create, "/"});
try_create(".") -> exit({create, "/"});
try_create(Dir) ->
    case file:make_dir(Dir) of
        {error, Reason} ->
            try_create(filename:dirname(Dir)),
            try_create(Dir);
        ok ->
	    ?LOG(info, {'created log directory', Dir}),
	    file:write_file_info(Dir, #file_info{mode = 8#777}),
            Dir
    end.

delete_dir(Dir) -> 
    Cmd = "\\rm -rf "++Dir,
    ?LOG(dbg, {list_to_atom(Cmd), list_to_atom(os:cmd(Cmd))}).

del_dir(Dir) ->
    lists:foreach(fun(F) -> del_file(F) end, get_files(Dir)).
del_file({Type, F}) ->
    case file:read_link_info(F) of
	{ok, FI} -> 
	    NFI = FI#file_info{mode = FI#file_info.mode band 8#200},
	    case file:write_file_info(F, NFI) of
		ok ->
		    case del_file(Type, F) of
			ok -> ok;
			{error, R} -> 
			    file:write_file_info(F, FI),
			    ?LOG(error, {error_deleting, R, F})
		    end;
		{error, R} -> ?LOG(error, {error_chmodding, R, F})
	    end;
	{error, R} -> already_gone %% probably...
    end.
del_file(regular, F) -> file:delete(F);
del_file(symlink, F) -> file:delete(F);
del_file(dir, F) -> file:del_dir(F);
del_file(Typ, _) -> {error, {bad_type, Typ}}.

get_files(Top) -> get_files([Top], []).
get_files([], O) -> O;
get_files([File|Files], O) ->
    case chk_file(File) of
	directory -> get_files(Files, get_files(lsl(File), [{dir, File}|O]));
	T -> get_files(Files, [{T, File}|O])
    end.

chk_file(File) ->
    case file:read_link_info(File) of
	{ok, FI} -> FI#file_info.type;
	{error, R} -> ?LOG(error, {R, File})
    end.	    

lsl(Dir) ->
    case file:list_dir(Dir) of
	{ok, Fs} -> [filename:join(Dir, F) || F <- Fs];
	{error, R} -> ?LOG(error, {R}), []
    end.

emu_vers() ->
    case catch erlang:system_info(system_version) of
	{'EXIT',R} -> {ok, 0};
	VersStr -> 
	    case regexp:first_match(VersStr,"[0-9]\.[0-9]") of 
		nomatch -> {unknown, post_nine};
		{match,F,L} -> 
		    case string:substr(VersStr,F, L) of
			"5.0" -> {ok, 7};
			"5.1" -> {ok, 8};
			"5.2" -> {ok, 9};
			_ -> {unknown, post_nine}
		    end
	    end
    end.

timestamp() -> timestamp(now()).
timestamp(Now) ->
    {{A,B,C},{D,E,F}} = calendar:now_to_datetime(Now),
    lists:flatten(io_lib:fwrite(?TSFORM, [A-2000, B, C, D, E, F])).%%Y2.1K-bug!!

host() -> element(2,inet:gethostname()).

get_ip() ->
    {ok ,#hostent{h_addr_list = IPs}} = inet:gethostbyname(host()),
    hd(IPs).

ls(Dir) ->
    {ok, Nams} = file:list_dir(Dir),
    [Nam || Nam <- Nams, is_notempty(filename:join([Dir,Nam]))].
is_notempty(Nam) ->
    {ok, #file_info{size = Siz}} = file:read_file_info(Nam),
    Siz > 0.

log(dbg, M, L, F, I) -> ?LOGGER:?DBGLOG(report(M, L, F, I));
log(info, M, L, F, I) -> ?LOGGER:?INFOLOG(report(M, L, F, I));
log(error, M, L, F, I) -> ?LOGGER:?ERRORLOG(report(M, L, F, I)).
report(Mod, Line, Func, Info) -> 
%%%    {Info, {node, node()}, {module, Mod}, {line, Line}, Func}.
    {Info, node(), Mod, Line, element(2, Func)}.

empty(_) -> ok.
llogger({I, N, _, L, {M, F, A}}) ->
    io:fwrite("(~w) ~w:~w (~w/~w) - ~p~n", [N, M, L, F, A, I]).
slogger({I, N, _, _, _}) ->
    io:fwrite("(~w) - ~p~n", [N, I]).
