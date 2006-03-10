%%%------------------------------------------------------------------------
%%% File: $Id$
%%%------------------------------------------------------------------------
%%% @doc This module implements LAMA - Log and Alarm Management Applicaiton.
%%% This module provides interface functions for .<p/>
%%%
%%% @type expected_type() = ExpectedType
%%%          ExpectedType = atom | {value, ValidItems::list()} |
%%%             tuple   | boolean | string |
%%%             integer | {integer, Min::integer(), Max::integer()} |
%%%             float   | {float, Min::float(), Max::float()} |
%%%             list    | {list, ValidItems::list()} | {list_of, Type} |
%%%             Fun
%%%          Type = integer | float | atom | list | tuple |
%%%                 number | string | ValidItems
%%%          Fun = (Opt, Value) -> ok |
%%%                      throw({error, {bad_config, {Key, Val}, Reason}})
%%%          ValidItems = list()
%%% @type options() = [ Option::option() ]
%%% @type option()  = {Option::atom(), Value::term()}
%%% @type typed_options() = [ TypedOption::typed_option() ]
%%% @type typed_option()  = {Option::atom(), Value::term(), ExpType::expected_type()}
%%% @author   Serge Aleynikov <serge@hq.idt.net>
%%% @version $Rev: 540 $
%%%          $LastChangedDate: 2006-02-23 16:36:33 -0500 (Thu, 23 Feb 2006) $
%%% @end
%%%------------------------------------------------------------------------
%%% Created 20-Mar-2003
%%% $Url$
%%%------------------------------------------------------------------------
-module(lama).
-author('serge@corp.idt.net').
-id("$Id$").

-behaviour(application).

%% External exports
-export([start/0, stop/0]).

%% Usability functions
-export([set_alarm/1, clear_alarm/1, get_alarms/0,
         check_alarm/2, add_alarm_trap/3,
         verify_config/1, verify_config/2,
         get_app_env/3, get_app_env/4,
         get_app_opt/3, get_app_opt/4,
         get_app_options/3, get_app_options2/3,
         get_opt/2, get_opt/3, os_path/1, is_string/1,
         bytestring_to_hexstring/1, hexstring_to_bytestring/1,
         throw_error/3, create_release_file/2]).

%% Internal exports
-export([start/2, stop/1, init/1, get_app/0]).

-include_lib("sasl/src/systools.hrl").

%%------------------------------------------------------------------------
%% @spec start() -> ok | {error, Reason}
%% @doc Start LAMA application.  This is a shortcut for
%%      application:start/1.
%% @see application:start/1
%% @end
%%------------------------------------------------------------------------
start() ->
    application:start(lama).

%%------------------------------------------------------------------------
%% @spec stop() -> ok | {error, Reason}
%% @doc Stop LAMA application.  This is a shortcut for
%%      application:stop/1.
%% @see application:stop/1
%% @end
%%------------------------------------------------------------------------
stop() ->
    application:stop(lama).

%% Note: Options is taken from {mod, Options} option in lama.app
%% @private
start(_, Options) ->
    AlarmOpts  = get_app_options(lama, Options, lama_alarm_h:get_def_options()),
    SyslogOpts = get_app_options(lama, Options, lama_syslog_h:get_def_options()),
    supervisor:start_link({local, lama_sup},
                          ?MODULE, [AlarmOpts, SyslogOpts]).

%% @private
stop(_State) ->
    ok.

%%%-----------------------------------------------------------------
%%% supervisor functionality
%%%-----------------------------------------------------------------
%% @private
init([AlarmOptions, SyslogOpts]) ->
    SafeSupervisor =
        {lama_safe_sup,
            {supervisor, start_link,
                [{local, lama_sup_safe}, ?MODULE,
                 {safe, AlarmOptions, SyslogOpts}]},
            permanent, infinity, supervisor, [?MODULE]},
    %% Reboot node if lama_logger or lama_alarmer or lama_snmp_trapper crashes!
    {ok, {_SupFlags = {one_for_one, 0, 1}, [SafeSupervisor]}};

init({safe, AlarmOptions, SyslogOpts}) ->
    SyslogH =
        {lama_syslog_sup,
            {lama_guard, start_link,
                [lama_guard_syslog, lama_syslog_h, SyslogOpts]},
            permanent, 2000, worker, dynamic},
    AlarmH =
        {lama_alarm_sup,
            {lama_guard, start_link,
                [lama_guard_alarm, lama_alarm_h, AlarmOptions]},
            permanent, 2000, worker, dynamic},
    {ok, {_SupFlags = {one_for_one, 4, 3600}, [SyslogH, AlarmH]}}.

%%----------------------------------------------------------------------
%% Exported miscelaneous functions
%%----------------------------------------------------------------------

%%------------------------------------------------------------------------
%% @spec set_alarm(Alarm::alarm()) -> ok
%%          alarm() = lama_alarm_h:alarm()
%%
%% @doc Set alarm. This function
%%      is equivalent to {@link lama_alarm_h:set_alarm/1}.
%% @see lama_alarm_h:set_alarm/1
%% @see check_alarm/2
%% @end
%%------------------------------------------------------------------------
set_alarm(Alarm) ->
    lama_alarm_h:set_alarm(Alarm).

%%------------------------------------------------------------------------
%% @spec clear_alarm(AlarmId::alarm_id()) -> ok
%%          alarm_id() = lama_alarm_h:alarm_id()
%%
%% @doc Clear alarm that was previously set by set_alarm/1. This function
%%      is equivalent to {@link lama_alarm_h:clear_alarm/1}.
%% @see lama_alarm_h:clear_alarm/1
%% @see check_alarm/2
%% @end
%%------------------------------------------------------------------------
clear_alarm(AlarmId) ->
    lama_alarm_h:clear_alarm(AlarmId).

%%------------------------------------------------------------------------
%% @spec check_alarm(Fun::function(), Alarm::alarm()) -> ok
%%          Fun = () -> boolean()
%%          alarm() = lama_alarm_h:alarm()
%% @doc Based on the return value of Fun set or clear an Alarm.
%% @end
%%------------------------------------------------------------------------
check_alarm(F, {AlarmID,_} = Alarm) ->
    case F() of
    true  -> lama_alarm_h:set_alarm(Alarm);
    false -> lama_alarm_h:clear_alarm(AlarmID)
    end.

%%------------------------------------------------------------------------
%% @spec get_alarms() -> Alarms
%%          Alarms = [ Alarm::alarm() ]
%% @doc Get currently active alarms.
%% @end
%%------------------------------------------------------------------------
get_alarms() ->
    lama_alarm_h:get_alarms().

%%------------------------------------------------------------------------
%% @spec add_alarm_trap(AlarmID::atom(), Trap::atom(),
%%                      Varbinds::list()) -> ok
%% @doc Add an alarm to trap mapping to the internal #state.alarm_map list.
%% Same as {@link lama_alarm_h:add_alarm_trap/3}.
%% @see lama_alarm_h:add_alarm_trap/3
%% @end
%%------------------------------------------------------------------------
add_alarm_trap(AlarmID, Trap, Varbinds) when is_atom(AlarmID) ->
    lama_alarm_h:add_alarm_trap(AlarmID, Trap, Varbinds).

%%------------------------------------------------------------------------
%% @spec get_app_env(App::atom(), Key::atom(), Default::term()) ->
%%              Value::term()
%%
%% @doc Fetch a value of the application's environment variable ``Key''.
%%      Use ``Default'' if that value is not set.
%% @end
%%------------------------------------------------------------------------
get_app_env(App, Key, Default) ->
    case application:get_env(App, Key) of
    {ok, Val} -> Val;
    _         -> Default
    end.

%%------------------------------------------------------------------------
%% @spec get_app_opt(App::atom(), Key::atom(), Default::term()) ->
%%                Option::option()
%%
%% @doc Same as get_app_env/3, but returns an Option tuple {Key, Value}
%%      rather than just a Value.
%% @end
%%------------------------------------------------------------------------
get_app_opt(App, Key, Default) ->
    {Key, get_app_env(App, Key, Default)}.

%%------------------------------------------------------------------------
%% @spec get_app_env(App::atom(), Key::atom(), Default::term(),
%%                   ExpectedType::expected_type()) -> Value::term()
%% @doc Fetch a value of the application's environment variable ``Key''.
%%      Use ``Default'' if that value is not set.  Perform validation of
%%      value type given ExpectedType.
%% @see get_app_env/5
%% @end
%%------------------------------------------------------------------------
get_app_env(App, Key, Default, ExpectedType) ->
    Value = get_app_env(App, Key, Default),
    verify_type(Key, Value, ExpectedType).

%%------------------------------------------------------------------------
%% @spec get_app_opt(App::atom(), Key::atom(), Default::term(),
%%                   ExpectedType::expected_type()) -> Option::option()
%%
%% @doc Same as get_app_env/4 but returns a tuple {Key, Value} rather than
%%      a Value.
%% @see get_app_env/4
%% @see get_opt/2
%% @see get_opt/3
%% @end
%%------------------------------------------------------------------------
get_app_opt(App, Key, Default, ExpectedType) ->
    {Key, get_app_env(App, Key, Default, ExpectedType)}.

%%------------------------------------------------------------------------
%% @spec get_app_options(App::atom(), DefOptions::options(),
%%            TypedOptions::typed_options()) -> Options::options()
%%
%% @doc Look up TypedOptions in the environment. If not found use defaults
%%      in the DefOptions list, and if not found there, use the default
%%      value from a TypedOption's tuple: {_Key, DefaultValue, _Type}.
%% ```
%% Example:
%%    start_application(StartArgs) ->
%%        get_app_options(test, StartArgs, [{file, "bar", string}]),
%%        ... .
%%
%%    start_application([])              ->   [{file, "bar"}]
%%    start_application([{file, "foo"}]) ->   [{file, "foo"}]
%%
%%    If application's environment had {file, "zzz"}, than this tuple
%%    would've been returned instead.
%% '''
%% @see get_app_env/4
%% @end
%%------------------------------------------------------------------------
get_app_options(App, Defaults, TypedOptions) ->
    GetVal = fun(K, D, T) ->
                 V = get_opt(K, Defaults, D),
                 verify_type(K, V, T)
             end,
    [get_app_opt(App, Key, GetVal(Key, Def, ExpectedType)) ||
        {Key, Def, ExpectedType} <- TypedOptions].

%%------------------------------------------------------------------------
%% @spec get_app_options2(App::atom(), Overrides::options(),
%%            TypedOptions::typed_options()) -> Options::options()
%%
%% @doc Same as get_app_options/3, but will override environment options
%%      with values in the ``Overrides'' list, instead of using them as
%%      defaults.
%% @end
%%------------------------------------------------------------------------
get_app_options2(App, Overrides, TypedOptions) ->
    GetVal = fun({Opt, Val}, T) ->
                 V = get_opt(Opt, Overrides, Val),
                 {Opt, verify_type(Opt, V, T)}
             end,
    [GetVal(get_app_opt(App, Key, Def), ExpType) ||
        {Key, Def, ExpType} <- TypedOptions].

%%------------------------------------------------------------------------
%% @spec get_opt(Opt::atom(), Options) -> term() | throw({error, Reason})
%%       Options = [{Option::atom(), Value::term()}]
%%
%% @doc Fetch the Option's value from the ``Options'' list
%% @see get_app_env/4
%% @see get_app_env/5
%% @see get_opt/3
%% @end
%%------------------------------------------------------------------------
get_opt(Opt, Options) ->
    case lists:keysearch(Opt, 1, Options) of
    {value, {_, V}} ->
        V;
    false ->
        throw({error, {undefined_option, Opt}})
    end.

%%------------------------------------------------------------------------
%% @spec get_opt(Opt::atom(), Options, Default::term()) -> term()
%%       Options = [{Option::atom(), Value::term()}]
%%
%% @doc Same as get_opt/2, but instead of throwing an error, it returns
%%      a ``Default'' value if the ``Opt'' is not in the ``Options'' list.
%% @see get_app_env/4
%% @see get_app_env/5
%% @see get_opt/2
%% @end
%%------------------------------------------------------------------------
get_opt(Opt, Options, Default) ->
    case lists:keysearch(Opt, 1, Options) of
    {value, {_, V}} ->
        V;
    false ->
        Default
    end.

%%------------------------------------------------------------------------
%% @spec verify_config(App::atom(), TypedOptions) -> ok | throw({error, Reason})
%%           TypedOptions = typed_options()
%%
%% @doc Fetch selected options from the application's environment and
%%      perform type verification.
%% @see get_app_env/4
%% @end
%%------------------------------------------------------------------------
verify_config(App, Options) ->
    [get_app_opt(App, Opt, Def, Type) || {Opt, Def, Type} <- Options].

%%------------------------------------------------------------------------
%% @spec verify_config(TypedOptions::typed_options()) ->
%%           Options::options() | throw({error, Reason})
%%
%% @doc For each ``Option'' in the ``Options'' list
%%      perform type verification.
%% @end
%%------------------------------------------------------------------------
verify_config(Options) ->
    F = fun(Op,V,T) -> {Op, verify_type(Op,V,T)} end,
    [F(Opt, Value, Type) || {Opt, Value, Type} <- Options].

%% This function is used by the logger.hrl macros
%% @private
get_app() ->
    case application:get_application() of
    {ok, App} -> App;
    undefined -> undefined
    end.

%%------------------------------------------------------------------------
%% @spec os_path(OsPath::string()) -> Path::string()
%% @doc Perform replacement of environment variable values in the OsPath.
%% ```
%%    Example:
%%       lama:os_path("$ROOTDIR/myapp") -> "/usr/local/lib/erlang/myapp"
%% '''
%% @see os:getenv/1
%% @end
%%------------------------------------------------------------------------
os_path(OsPath) ->
    List = filename:split(OsPath),
    F = fun([$$ | Env] = Word) -> case os:getenv(Env) of
                                  false -> Word;
                                  Val   -> Val
                                  end;
           (Other) -> Other
        end,
    filename:join([F(W) || W <- List]).

%%------------------------------------------------------------------------
%% @spec is_string(S::term()) -> boolean()
%% @doc Returns true if S is a string.
%% @end
%%------------------------------------------------------------------------
is_string(S) ->
    lama_syslog_h:is_string(S).

%%------------------------------------------------------------------------
%% @spec bytestring_to_hexstring(ByteString::list()) -> string()
%% @doc Converts a byte string to a list of hexadecimal digits
%%      (one byte will be represented as two hex digits).
%% @end
%%------------------------------------------------------------------------
bytestring_to_hexstring(ByteString) ->
    HexString = bytestring_to_hexstring(ByteString, []),
    lists:reverse(HexString).

bytestring_to_hexstring([], Acc) ->
    Acc;
bytestring_to_hexstring([B |Rest], Acc) ->
    [C1, C2] = int_to_hex(B),
    bytestring_to_hexstring(Rest,[C2, C1| Acc]).

int_to_hex(B) when is_integer(B), B < 256, B >= 0 ->
    [code_character(B div 16), code_character(B rem 16)].

code_character(N) when N < 10 -> $0 + N;
code_character(N)             -> $a + (N - 10).

%%------------------------------------------------------------------------
%% @spec hexstring_to_bytestring(HexString::list()) -> list()
%% @doc Converts a hex string to a list of integers.
%% @end
%%------------------------------------------------------------------------
hexstring_to_bytestring(HexString) ->
    hexstring_to_bytestring(HexString, []).
hexstring_to_bytestring([], Acc) ->
    lists:reverse(Acc);
hexstring_to_bytestring([H1, H2 |Rest], Acc) ->
    I = hex_to_int(H1) bsl 4 + hex_to_int(H2),
    hexstring_to_bytestring(Rest, [I | Acc]).

hex_to_int(H) when H >= $a -> 10 + H - $a;
hex_to_int(H) when H >= $A -> 10 + H -$A;
hex_to_int(H)              -> H - $0.

%%%-----------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------

verify_type(_Tag, Val, atom)    when is_atom(Val) -> Val;
verify_type( Tag, Val, {Type, ValidItems}) when Type == value; Type == atom ->
    find_in_list(Tag, Val, ValidItems), Val;
verify_type(_Tag, Val, tuple)   when is_tuple(Val)  -> Val;
verify_type(_Tag, Val, integer) when is_integer(Val) -> Val;
verify_type( Tag, Val, {integer, Min, Max}) when is_integer(Val) ->
    (Val >= Min andalso Val =< Max) orelse throw_error(Tag,Val,{out_of_range,Min,Max}),
    Val;
verify_type(_Tag, Val, float)   when is_float(Val) -> Val;
verify_type( Tag, Val, {float, Min, Max})   when is_float(Val) ->
    (Val >= Min andalso Val =< Max) orelse throw_error(Tag,Val,{out_of_range,Min,Max}),
    Val;
verify_type(_Tag, Val, list)    when is_list(Val) -> Val;
verify_type(Tag,  Val, {list, ValidItems}) when is_list(Val) ->
    [find_in_list(Tag, I, ValidItems) || I <- Val], Val;
verify_type(_Tag, Val, boolean) when is_boolean(Val) -> Val;
verify_type(Tag, Val, {list_of, Type}) when is_list(Val)  ->
    F = fun(A) when Type =:= atom,    is_atom(A)    -> ok;
           (A) when Type =:= string                 -> is_string(A);
           (A) when Type =:= tuple,   is_tuple(A)   -> ok;
           (A) when Type =:= integer, is_integer(A) -> ok;
           (A) when Type =:= float,   is_float(A)   -> ok;
           (A) when Type =:= number,  is_number(A)  -> ok;
           (A) when is_list(Type)                   -> find_in_list(Tag, A, Type);
           (_) -> throw_error(Tag, Val, {list_of, Type, expected})
        end,
    [F(V) || V <- Val], Val;
verify_type( Tag, Val, Fun) when is_function(Fun, 2), not is_tuple(Fun) -> % In current Erlang release is_function({a, b},2) returns true
    Fun(Tag, Val), Val;
verify_type(Tag, Val, string) -> is_string(Tag, Val), Val;
verify_type(Tag, Val, _) ->
    throw_error(Tag, Val, unknown_param).

throw_error(Tag, Val, Reason) ->
    throw({error, {bad_config, {Tag, Val, Reason}}}).

find_in_list(Tag, Ele, List) ->
    case lists:member(Ele, List) of
    true  -> ok;
    false -> throw_error(Tag, Ele, unknown_param)
    end.

is_string(Tag, S) ->
    case is_string(S) of
    true  -> ok;
    false -> throw_error(Tag, S, not_a_string)
    end.

%%-------------------------------------------------------------------
%% @spec create_release_file(TemplateRelFile, OutRelFile) -> ok
%%          TemplateRelFile = filename()
%%          OutRelFile      = filename()
%% @doc Create a release file given a release template file.  The
%%      release template file should have the same structure as the
%%      release file.  This function will ensure that latest
%%      application versions are included in the release.  It will
%%      also ensure that the latest erts version is specified in 
%%      the release file.  Note that both arguments may contain the
%%      ".rel" extension, however the actual TemplateRelFile must 
%%      have a ".rel" extension.  The TemplateRelFile doesn't need
%%      to contain application version numbers - an empty string will
%%      do: <code>{kernel, ""}</code>.  This function will populate
%%      the version number with current version of the application.
%% ```
%% Example: 
%%   lama:create_release_file(".drp.template.rel", "./ebin/drp.rel").
%% '''
%% @end
%%-------------------------------------------------------------------
create_release_file(TemplateRelFile, OutRelFile) ->
    Rel = get_release(TemplateRelFile),
    OutFileName = filename:join(filename:dirname(OutRelFile), 
                                filename:basename(OutRelFile,".rel")++".rel"),
    case file:open(OutFileName, [write]) of
    {ok, FD} ->
        io:format(FD, "%%%~n"
                      "%%% This file is automatically generated from ~s.rel~n"
                      "%%%~n~n", [TemplateRelFile]),
        io:format(FD, "{release, {~p, ~p}, {erts, ~p},~n  [~n~s  ]~n}.~n", 
                  [Rel#release.name, Rel#release.vsn, 
                   Rel#release.erts_vsn, 
                   format_list(Rel#release.applications)]),
        file:close(FD);
    {error, Reason} ->
        throw({error, file:format_error(Reason)})
    end.
    
get_release(Filename) ->
    File = filename:basename(Filename, ".rel"),
    Dir  = [filename:dirname(Filename) | code:get_path()],
    {ok, Release} = systools_make:read_release(File, Dir),
    case systools_make:get_release(File, Dir) of
    {ok, Rel, _, _} ->
        Rel#release{erts_vsn = erlang:system_info(version)};
    {error,systools_make,List} ->
        NewList =
            lists:foldl(fun({error_reading,{Mod,{not_found,AppFile}}}, {Ok, Err}) ->
                            {Ok, [{not_found, {Mod, AppFile}} | Err]};
                        ({error_reading,{Mod,{no_valid_version, 
                                {{"should be",_}, {"found file", _, Vsn}}}}}, {Ok, Err}) ->
                            {[{Mod, Vsn} | Ok], Err}
                        end, {[],[]}, List),
        case NewList of 
        {ModVsn, []} ->
            substitute_versions(Release, ModVsn);
        {_, ErrMod} ->
            throw({error, ErrMod})
        end
    end.

substitute_versions(Release, [])     -> Release;
substitute_versions(Release, [{Mod, Vsn} | Tail]) ->
    Apps = Release#release.applications,
    case lists:keysearch(Mod, 1, Apps) of
    {value, {Mod, _Vsn, Type}} ->
        NewApps = lists:keyreplace(Mod, 1, Apps, {Mod, Vsn, Type});
    false ->
        NewApps = Apps
    end,
    substitute_versions(Release#release{applications = NewApps,
                                        erts_vsn     = erlang:system_info(version)}, Tail).

format_list(A) ->
    {LN, LV} =
        lists:foldl(fun({N,V,_}, {L1, L2}) ->
                        {max(L1, length(atom_to_list(N))), 
                         max(L2, length(V))}
                    end, {0,0}, A),
    format_list(A, [], {LN, LV}).
format_list([], [$\n, $, | Acc], _) ->
    lists:reverse([$\n | Acc]);
format_list([{App,Vsn,permanent} | Tail], Acc, {LN, _LA} = Len) ->
    Str = lists:flatten(io_lib:format("    {~-*w, ~s},~n", [LN, App, [$"]++Vsn++[$"]])),
    format_list(Tail, lists:reverse(Str) ++ Acc, Len);
format_list([{App,Vsn,Type} | Tail], Acc, {LN, LA} = Len) ->
    Str = lists:flatten(io_lib:format("    {~-*w, ~-*s, ~p},~n", [LN, App, LA+2, [$"]++Vsn++[$"], Type])),
    format_list(Tail, lists:reverse(Str) ++ Acc, Len).

max(X,Y) when X > Y -> X;
max(_,Y)            -> Y.
