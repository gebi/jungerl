%%%----------------------------------------------------------------------
%%% File    : logger.erl
%%% Author  : Magnus Fr|berg <magnus@bluetail.com>
%%% Purpose : Implements a wrapping ascii error_logger handler.  Uses
%%%           disk_log_h as the handler.  Almost like the sasl error logger,
%%%           but formats the logs to ascii instead of binary.
%%%
%%%           Use in supervisor as e.g. 
%%%             {logger, {logger, start_link, []},
%%%              permanent, 2000, worker, [logger]},
%%%
%%%    start_errlog() ->
%%%        Opts = [{name, logger},
%%%                {file, "./elog"},
%%%                {type, wrap},
%%%                {format, external},
%%%                {force_size, true},
%%%                {size, {1024*1024, 5}}], % 5 files
%%%        gen_event:add_sup_handler(
%%%          error_logger,
%%%          {disk_log_h, logger},
%%%          disk_log_h:init(fun logger:form_no_progress/1, Opts)).
%%%    
%%%    test() ->
%%%        error_logger:error_msg("testing ~p\n", [self()]).
%%%
%%%           Initiate/deactivate system logging.
%%%           Owns the error log.
%%% Created : 13 Apr 1999 by Magnus Fr|berg <magnus@bluetail.com>
%%% Modified: 26 May 1999 by Martin Bjorklund <mbj@bluetail.com>
%%% Modified: 04 Dec 2000 by Martin Bjorklund <mbj@bluetail.com>
%%% Modified: 13 Nov 2003 by Martin Bjorklund <mbj@bluetail.com>
%%%           Cleanup for jungerl.
%%% Modified: 15 Aug 2007 by Martin Bjorklund <mbj@tail-f.com>
%%%           Added example, minor cleanups.
%%%----------------------------------------------------------------------

-module(logger).
-vsn("$Revision$ ").
-author('magnus@bluetail.com').
-author('mbj@bluetail.com').

-behaviour(gen_server).

%% External exports
-export([start_link/0]).
-export([m2s/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% disk_log_h formatting functions
-export([form_all/1, form_no_progress/1]).

-define(LOG(Format,Args),
	error_logger:info_msg(Format, Args)).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
%% Starts a gen_event supervisor process for the disk_log_h handler, so that
%% the handler is restarted by the supervisor.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    set_system_error_logging(),
    Type = get_error_logger_mf_type(),
    Mf = get_error_logger_mf(),
    ok = add_error_logger_mf(Mf, Type),
    {Name, Vsn} = init:script_id(),
    ?LOG("Starting system [~s-~s]\n", [Name, Vsn]),
    start_tell_started(),
    {ok, []}.

handle_call(_Req, _, S) ->
    {reply, unknown_request, S}.

handle_cast(_, S) ->
    {noreply, S}.

handle_info({gen_event_EXIT, logger, Reason}, S) ->
    {stop, Reason, S};

handle_info(_, S) ->
    {noreply, S}.

terminate(_Reason, _S) ->
    delete_error_logger_mf(),
    ok.

set_system_error_logging() ->
    Hs = gen_event:which_handlers(error_logger),
    case {lists:member(error_logger_tty_h, Hs),
	  lists:member(error_logger_file_h, Hs)} of
	{false, false} ->
	    %% Delete the (possibly existing) unformating handler
	    error_logger:delete_report_handler(error_logger),
	    %% Add the simplest handler that directs reports not
	    %% belonging to this node to the correct node.
	    error_logger:simple_logger(),
	    ok;
	_ ->
	    ok
    end.

%% check when the entire system is up and running
start_tell_started() ->
    spawn(fun tell_started/0).

tell_started() ->
    case init:get_status() of
	{started, started} ->
	    ?LOG("System started.\n", []);
	_ ->
	    timer:sleep(100),
	    tell_started()
    end.


t2s({{Year,Month,Day},{Hour,Minute,Second}}) ->
    io_lib:format("~w-~s-~w::~2..0w:~2..0w:~2..0w",
		  [Day,m2s(Month),Year,Hour,Minute,Second]).

m2s(1) -> "Jan";
m2s(2) -> "Feb";
m2s(3) -> "Mar";
m2s(4) -> "Apr";
m2s(5) -> "May";
m2s(6) -> "Jun";
m2s(7) -> "Jul";
m2s(8) -> "Aug";
m2s(9) -> "Sep";
m2s(10) -> "Oct";
m2s(11) -> "Nov";
m2s(12) -> "Dec".

%% --------------------------------------------------------------
%% We have the disk_log_h instead of sasl as we dont want progress
%% reports to appear in the log.
%% --------------------------------------------------------------

add_error_logger_mf(undefined, _) -> ok;
add_error_logger_mf({File, MaxB, MaxF}, Type) ->
    case nolog() of
	false ->
	    Opts = [{name, logger},
		    {file, File},
		    {type, wrap},
		    {format, external},
		    {size, {MaxB, MaxF}}],
	    gen_event:add_sup_handler(error_logger,
				      {disk_log_h, logger},
				      disk_log_h:init(form_func(Type), Opts));
	true ->
	    ok
    end.

delete_error_logger_mf() ->
    gen_event:delete_handler(error_logger, {disk_log_h, logger}, stop).

form_func(all) -> {logger, form_all};
form_func(_)   -> {logger, form_no_progress}.

form_all({_Type, GL, _Msg}) when node(GL) /= node() ->
    false;
form_all(Event) ->
    Str = 
	case Event of
	    {error_report, _GL, {Pid, Type, Report}} ->
		[mk_hdr("ERROR REPORT", Type, Pid),
		 io_lib:format("~p\n", [nobin(Report)])];
	    %% tail-f specific debug messages
	    {info_report, _GL, {_, debug, {Pid, Now, Level, Tag, MsgStr}}} ->
		["*dbg* ", t2s(calendar:now_to_local_time(Now)), " ",
		 pid_to_list(Pid), " ",
		 integer_to_list(Level), "/", atom_to_list(Tag),
		 "\n  ", MsgStr];
	    {info_report, _GL, {Pid, Type, Report}} ->
		[mk_hdr("INFO REPORT", Type, Pid),
		 io_lib:format("~p\n", [nobin(Report)])];
	    {error, _GL, {Pid, Format, Args}} ->
		[mk_hdr("ERROR", undefined, Pid),
		 io_lib:format(Format, nobin(Args))];
	    {info_msg, _GL, {Pid, Format, Args}} ->
		[mk_hdr("INFO MSG", undefined, Pid),
		 io_lib:format(Format, nobin(Args))];
	    {info, _GL, {Pid, Term, _Empty}} ->
		[mk_hdr("INFO", undefined, Pid),
		 io_lib:format("~p\n", [nobin(Term)])];
	    {emulator, _GL, EStr = "Out of space in dist table"} ->
		error_logger:info_msg("Restarting due to dist table space~n",
				      []),
		restart(),
		[mk_hdr("EMULATOR", undefined, undefined),
		 EStr];
	    {emulator, _GL, EStr} ->
		[mk_hdr("EMULATOR", undefined, undefined),
		 nobin(EStr)];
	    _ ->
		[mk_hdr("UNKNOWN", undefined, undefined),
		 io_lib:format("~p\n", [Event])]
	end,
    list_to_binary([Str, "\n"]).

mk_hdr(HStr, Type, Who) ->
    ["== ", t2s(erlang:localtime()), " == ", HStr, " - ", 
     pstr(Type), " ", pstr(Who), "\n"].

pstr(undefined) -> "";
pstr(T) -> io_lib:format("~p", [T]).
   

nobin(B) when binary(B), size(B) > 1024 ->
    <<ShortBin:32/binary, _/binary>> = B,
    lists:flatten(io_lib:format("~p(~w)", [ShortBin, size(B)]));
nobin(L) when list(L) ->
    map2(fun(X) -> nobin(X) end, L);
nobin(T) when tuple(T) ->
    list_to_tuple(nobin(tuple_to_list(T)));
nobin(X) ->
    X.

%% handles non-proper lists
map2(F, [H | T]) ->
    [F(H) | map2(F, T)];
map2(_F, []) ->
    [];
map2(_F, T) ->
    T.

form_no_progress({_Type, GL, _Msg}) when node(GL) /= node() ->
    false;
form_no_progress({info_report, _, {_, progress, [{application,_},
						 {started_at, _}]}} = Event) ->
    form_all(Event);
form_no_progress({info_report, _, {_, progress, _}}) ->
    false;
form_no_progress(Event) ->
    form_all(Event).

nolog() ->
    case application:get_env(nolog) of
	{ok, true} ->
	    true;
	_ ->
	    false
    end.

get_error_logger_mf_type() ->
    case application:get_env(errlog_type) of
	{ok, error} -> error;
	{ok, all} -> all;
	{ok, Bad} -> exit({bad_config, {errlog_type, Bad}});
	_ -> all
    end.

get_error_logger_mf() ->
    case catch get_mf() of
	{'EXIT', Reason} ->
	    exit(Reason);
	Mf ->
	    Mf
    end.

get_mf() -> 
    File = get_mf_file(),
    MaxB = get_mf_maxb(),
    MaxF = get_mf_maxf(),
    {File, MaxB, MaxF}.

get_mf_file() ->
    case application:get_env(error_logger_mf_file) of
	{ok, false} -> throw(undefined);
	{ok, File} when list(File) -> File;
	undefined -> throw(undefined);
	{ok, Bad} -> exit({bad_config, {error_logger_mf_file, Bad}})
    end.

get_mf_maxb() ->
    case application:get_env(error_logger_mf_maxbytes) of
	{ok, MaxB} when integer(MaxB) -> MaxB;
	undefined -> throw(undefined);
	{ok, Bad} -> exit({bad_config, {error_logger_mf_maxbytes, Bad}})
    end.

get_mf_maxf() ->
    case application:get_env(error_logger_mf_maxfiles) of
	{ok, MaxF} when integer(MaxF), MaxF > 0, MaxF < 256 -> MaxF;
	undefined -> throw(undefined);
	{ok, Bad} -> exit({bad_config, {error_logger_mf_maxfiles, Bad}})
    end.

%% Restart the node. 
%% If -heart flag is set reboot otherwise restart.
restart() ->
    case init:get_argument(heart) of
	{ok, _} -> init:reboot();
	_       -> init:restart()
    end.
