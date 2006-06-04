%%%-------------------------------------------------------------------
%%% Created : 11 Sep 2005 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Desc.   : A simple wrap log.
%%%
%%% @author Torbjörn Törnkvist <tobbe@tornkvist.org>
%%% 
%%% @doc <b>wraplog</b> is a simple wraplog.
%%%      It attaches itself to the standard error logger so that
%%%      all error messages produced by the {@link error_logger}
%%%      module ends up in the wraplog.
%%%
%%%      <p>At startup, a file is specified to the {@link start/1}
%%%      function. This file will get a number prefix for each
%%%      wraplog. The number of wraplogs and the size of each file
%%%      can be specified with the {@link start/3} function. Per
%%%      default, 9 files will be used, each having a size of 500 KB.</p>
%%%
%%% @end
%%%
%%% $Id$
%%%-------------------------------------------------------------------
-module(wraplog).

%% External exports
-export([start/1, start/3, 
	 start_sup_handler/1, start_sup_handler/3]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, 
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(LOG, ?MODULE).
-define(LOG_SIZE, 500000). 
-define(NO_LOG_FILES, 9).


-record(s, {}).

%%% @doc Start the specified wraplog.
%%%      Example: <code>start("/tmp/mywraplog").</code>
start(LogFile) ->
    start(LogFile, ?LOG_SIZE, ?NO_LOG_FILES).

%%% @doc Start the specified wraplog with specified size and number of files.
%%%      Example: <code>start("/tmp/mywraplog", 256000, 5).</code>
%%%      This will create a file: <i>/tmp/mywraplog.1</i>
start(LogFile, LogSize, NoLogFiles) ->
    error_logger:add_report_handler(?MODULE, {LogFile, LogSize, NoLogFiles}).

%%% @doc Start the specified wraplog; supervised.
%%%
%%% Use this way of starting the wraplog if you are running
%%% a gen_server. You will then get events if the error_handler
%%% crashes. 
%%%
%%% Example:
%%% <pre>
%%% handle_info({gen_event_EXIT, wraplog, Reason}, S) ->
%%%     case get(restarted) of
%%%        10 ->
%%%            {stop, Reason, S};
%%%        N0 ->
%%%            N = if N0 == undefined -> 1;
%%%                   true            -> N0 + 1
%%%                end,
%%%            {LogFile, LogSize, NoLogFiles}  = get(wraplog_args),
%%%            wraplog:start_sup_handler(LogFile, LogSize, NoLogFiles),
%%%            error_logger:info_msg("Restarted disk_log error logger "
%%%                                  "(Reason=~p)~n", [Reason]),
%%%            put(restarted, N),
%%%            {noreply, S}
%%%     end; 
%%%</pre>
%%%
start_sup_handler(LogFile) ->
    start_sup_handler(LogFile, ?LOG_SIZE, ?NO_LOG_FILES).

start_sup_handler(LogFile, LogSize, NoLogFiles) ->
    gen_event:add_sup_handler(error_logger, ?MODULE,
			      {LogFile, LogSize, NoLogFiles}).

%%% @private
init({LogFile, LogSize, NoLogFiles}) ->
    open(LogFile, LogSize, NoLogFiles),
    {ok, #s{}}.

%%% @private
handle_event({Type, _, {_, Fstr, Args}}, State) when list(Fstr) ->
    disk_log:blog(?LOG, wtime(Type)),
    disk_log:blog(?LOG, fmt(Fstr,Args)),
    {ok,State};
handle_event({_Type, _, {_, _Fstr, _Args}}, State) ->
    {ok,State}.
    
%%% @private
handle_info(_Info, State) ->
    {ok, State}.

%%% @private
handle_call(_Info, State) ->
    {ok, ok, State}.

%%% @private
terminate(_Reason, _State) ->
    disk_log:close(?LOG),
    ok.

%%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


open(LogFile, LogSize, NoLogFiles) ->
    filelib:ensure_dir(LogFile),
    L = [{name, ?LOG},
	 {file, LogFile},
	 {linkto, none},
	 {repair, false},
	 {type, wrap},
	 {format, external},
	 {size, {LogSize, NoLogFiles}}],
    disk_log:open(L).


wtime(info_msg)       -> wtime0("INFO REPORT");
wtime(info_report)    -> wtime0("INFO REPORT");
wtime(error)          -> wtime0("ERROR REPORT");
wtime(A) when atom(A) -> wtime0(atom_to_list(A)++" REPORT");
wtime(_)              -> wtime0("REPORT").
    
wtime0(Type) ->
    wtime0({date(),time()}, Type).

wtime0({{Y,Mo,D},{H,Mi,S}}, Type) ->
    fmt("~n=~s==== ~p-~s-~p::~s:~s:~s ===~n",
	[Type,D,month(Mo),Y,t(H),t(Mi),t(S)]).

fmt(F,A) ->
    lists:flatten(io_lib:format(F,A)).

t(X) when integer(X) ->
    t1(integer_to_list(X));
t(_) ->
    "".

t1([X]) -> [$0,X];
t1(X)   -> X.

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".



