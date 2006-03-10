%%%------------------------------------------------------------------------
%%% File: $Id$
%%%------------------------------------------------------------------------
%%% @doc This module implements a syslog event handler for error_logger.
%%% @author   Serge Aleynikov <serge@hq.idt.net>
%%% @version  $Rev: 464 $
%%%           $LastChangedDate: 2006-02-02 20:59:48 -0500 (Thu, 02 Feb 2006) $
%%% ``
%%% Example message as seen in the syslog file:
%%% Mar 19 21:26:44 10.231.12.2 n@spider:test[0.45.0]: [ALERT] Test msg
%%% '''
%%% @end
%%%----------------------------------------------------------------------
%%% $Url$
%%% Created: 19-Mar-2003
%%%----------------------------------------------------------------------
-module(lama_syslog_h).
-author('serge@corp.idt.net').
-id("$Id$").

-behaviour(gen_event).

%% External exports
-export([start_link/1, stop/0, log/4]).

%% gen_server callbacks
-export([init/1, handle_call/2, handle_event/2, handle_info/2,
         code_change/3, terminate/2]).

%% Internal and Debug exports
-export([get_def_options/0, is_string/1, get_state/0, test/0]).

-import(http_util,  [to_upper/1]).

-record(state, {host,         % Destination host for syslog messages
                indent,       % Default tag to use for syslog messages
                facility,     % Default facility to use for syslog messages
                sock,         % Syslog socket
                alarm_pri,    % {AlarmSetPriority, AlarmClearedPriority}
                syslog_types, % List of event types that need to be sent to syslog
                              %  Default: [alert,error,warning]. Allowed values:
                              %  [error,alert,warning,notice,info,debug]
                ignore_types  % List of event types that need to be ignored
                              %  with no action. Default: []. Allowed values:
                              %  [error,alert,warning,notice,info,debug]
               }).

-include("logger.hrl").

-define(SYSLOG_PORT, 514).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(Options) ->
    % gen_event:add_handler/2 doesn't check for duplicates
    % use swap_sup_handler, to avoid duplication
    case gen_event:swap_sup_handler(error_logger,
           {?MODULE, swap}, {?MODULE, Options}) of
    ok -> ok;
    {error, Reason} ->
        throw({error, {?MODULE, start_link, Reason}})
    end.

%%------------------------------------------------------------------------
%% @spec stop() -> ok | {error, Reason}
%% @doc Remore alarm handler.
%% @end
%%------------------------------------------------------------------------
stop() ->
    gen_event:delete_handler(error_logger, ?MODULE, normal).

log(Priority, Format, Msg, Optional) when is_list(Msg), is_list(Optional) ->
    gen_event:notify(error_logger,
        {info_report, group_leader(),
            {self(), syslog, {Priority, Format, Msg, Optional}}}).

%%------------------------------------------------------------------------
%% @spec get_state() -> state()
%% @private
%%------------------------------------------------------------------------
get_state() ->
    gen_event:call(error_logger, ?MODULE, get_state).

%%------------------------------------------------------------------------
%% @spec get_def_options() -> TypedOptions::typed_options()
%%          typed_options() = lama:typed_options()
%%
%% @doc Gets default module's options.
%% @end
%%------------------------------------------------------------------------
get_def_options() ->
    AllowedTypes = [alert,error,warning,notice,info,debug],
    [{syslog_host,            "localhost", string},
     {syslog_indent,          lama,        atom},
     {syslog_facility,        user,        atom},
     {syslog_types, [alert,error,warning], {list, AllowedTypes}},
     {ignore_types, [],                    {list, AllowedTypes}},
     {alarm_set_priority,     error,       atom},
     {alarm_cleared_priority, warning,     atom}
    ].

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init({Options, _Ignore}) ->
    catch
        begin
            Host     = lama:get_opt(syslog_host,            Options),
            Facility = lama:get_opt(syslog_facility,        Options),
            Indent   = lama:get_opt(syslog_indent,          Options),
            ASP      = lama:get_opt(alarm_set_priority,     Options),
            ACP      = lama:get_opt(alarm_cleared_priority, Options),
            SLopt    = lama:get_opt(syslog_types,           Options),
            Ignore   = lama:get_opt(ignore_types,           Options),
            case catch {encode_facility(Facility),
                        encode_priority(ASP),
                        encode_priority(ACP)} of
            {N,M,K} when is_integer(N), is_integer(M), is_integer(K) ->
                ok;
            _ ->
                throw({stop, {invalid_facility, Facility}})
            end,

            IP = get_host(Host),

            case gen_udp:open(0) of
            {ok, S} ->
                {ok, #state{host         = IP,
                            indent       = Indent,
                            facility     = Facility,
                            alarm_pri    = {ASP, ACP},
                            sock         = S,
                            syslog_types = SLopt,
                            ignore_types = Ignore}};
            {error, Why} ->
                throw({stop, {gen_udp, open, inet:format_error(Why)}})
            end
        end;
%% This one is called when a event is being installed to an event manager
%% using gen_event:add_[sup_]handler/3 function
init(Options) ->
    init({Options, undefined}).


%%----------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(get_state, State) -> {ok, State, State};

handle_call(_Request, State) ->
    {ok, {error, bad_request}, State}.

%%----------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

%----------------------------------------------------------------------
% logger.hrl support
%----------------------------------------------------------------------
handle_event({error_report, GL, {Pid, {lama,Type}, {Distr,Report}}},
             State) ->
    % Send distributed messages to other nodes, and also if the
    % Type is in [warning,error] forward it other gen_event handlers or
    % print [info,debug] messages to screen
    distribute(Distr, {error_report, GL, {Pid,{lama,Type},Report}}, State),
    {ok, State};

%----------------------------------------------------------------------
% don't log events not belonging to this node
%----------------------------------------------------------------------
handle_event({_Type, GL, _Event}, State) when node(GL) /= node() ->
    {ok, State};

%----------------------------------------------------------------------
% syslog events
%----------------------------------------------------------------------
handle_event({info_report, _GL, {Pid, syslog, {Priority, Fmt, Msg, Opt}}},
             #state{host=H, indent=I, facility=F, sock=S} = State) ->
    Indent   = lama:get_opt(indent,   Opt, I),
    Facility = lama:get_opt(facility, Opt, F),
    Header   = type_to_header(Priority),
    do_log(S, H, {Indent, Pid, Facility, Priority, Header, Fmt, Msg}),
    {ok, State};

%----------------------------------------------------------------------
% lama_alarm_h support
%----------------------------------------------------------------------
handle_event({error_report, GL, {Pid, {alarm,Type}, {AlarmID, Detail}, TrapSent}},
             #state{host=H, indent=Indent, facility=Facility,
                    sock=S, alarm_pri={ASP,ACP}} = State) ->
    {Priority, Header} =
        case Type of
        set   -> {ASP, "ALARM SET"};
        clear -> {ACP, "ALARM CLEARED"}
        end,
    TrapDetail =
        case TrapSent of
        {ok, Trap, Varbinds} ->
            io_lib:format("Trap {~p, ~p} sent successfully", [Trap, Varbinds]);
        no_trap -> "";
        {error, Reason} ->
            io_lib:format("Failed to send trap: ~p", [Reason])
        end,
    {Fmt, Msg} =
        case is_string(Detail) of
        true when Detail /= [] -> {"~p - ~s~n ~s~n", [AlarmID, Detail, TrapDetail]};
        true                   -> {"~p~n ~s~n"     , [AlarmID,         TrapDetail]};
        false                  -> {"~p - ~p~n ~s~n", [AlarmID, Detail, TrapDetail]}
        end,
    gen_event:notify(error_logger, {error, GL, {lama, " "++Header++": "++Fmt, Msg}}),
    do_log(S, H, {Indent, Pid, Facility, Priority, Header, Fmt, Msg}),
    {ok, State};

%----------------------------------------------------------------------
% error_logger interface support
%----------------------------------------------------------------------
handle_event({_Type, _GL, {lama, _Rep, _Args}}, State) ->
    % These are special messages that this module generates upon
    % receiving gen_events from logger.hrl.  We want standard handlers
    % to pick them up for proper printing and file logging.  In order
    % to avoid duplicate syslog logging in this module, we just skip them.
    {ok, State};
handle_event({error, _GL, {Pid, Format, Args}}, State) ->
    do_log_error(error, "ERROR", Pid, Format, Args, State),
    {ok, State};
handle_event({emulator, GL, Chars}, State) when is_list(Chars) ->
    do_log_error(error, "ERROR-EMU", GL, "~s", [Chars], State),
    {ok, State};
handle_event({info, _GL, {Pid, Info, _}}, State) ->
    do_log_error(info,  "INFO", Pid, "~p", [Info], State),
    {ok, State};
handle_event({error_report, _GL, {Pid, std_error, Rep}}, State) ->
    Msg = format_report(Rep),
    do_log_error(error, "ERROR", Pid, "~s", [Msg], State),
    {ok, State};
handle_event({info_report, _GL, {Pid, std_info, Rep}}, State) ->
    Msg = format_report(Rep),
    do_log_error(info,  "INFO", Pid, "~s", [Msg], State),
    {ok, State};
handle_event({info_msg, _GL, {Pid, Format, Args}}, State) ->
    do_log_error(info,  "INFO", Pid, Format, Args, State),
    {ok, State};
handle_event({warning_report,_GL, {Pid, std_warning, Rep}}, State) ->
    Msg = format_report(Rep),
    do_log_error(warning, "WARNING", Pid, "~s", [Msg], State),
    {ok, State};
handle_event({warning_msg, _GL, {Pid, Format, Args}}, State) ->
    do_log_error(info,  "WARNING", Pid, Format, Args, State),
    {ok, State};
%----------------------------------------------------------------------
% sasl event support
%----------------------------------------------------------------------
handle_event({error_report, _GL, {Pid, supervisor_report, Rep}}, State) ->
    Msg = format_supervisor(Rep),
    do_log_error(error, "ERROR-SUPERV", Pid, "~s", [Msg], State),
    {ok, State};
handle_event({error_report, _GL, {Pid, crash_report, Rep}}, State) ->
    Msg = format_crash(Rep),
    do_log_error(error, "ERROR-CRASH", Pid, "~s", [Msg], State),
    {ok, State};
%% Don't send progress reports to syslog
handle_event({info_report,  _GL, {_Pid, progress, _Rep}}, State) ->
    {ok, State};

handle_event(_Msg, State) ->
    {ok, State}.

%%-----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%-----------------------------------------------------------------------
handle_info({emulator, GL, Chars}, State) when node(GL) == node(); GL =:= noproc ->
    do_log_error(error, "ERROR-EMU", GL, "~s", [Chars], State),
    {ok, State};

handle_info({udp_error, S, econnreset}, #state{sock=S} = State) ->
    {ok, State};  %% XXX Should we ignore these?
                  %% A write attempt failed - syslog messages got lost...

handle_info(_Info, State) ->
    {ok, State}.

%%-----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% @private
%%-----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------------
%% This function is called when
%% gen_event:swap_handler(EventMgr, {?MODULE, swap}, {NewModule, Args})
%% is called, or the handler is being terminated for some other reason.
%% @private
%%-----------------------------------------------------------------------
terminate(_Reason, #state{sock = S}) ->
    gen_udp:close(S),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% Distribute logger.hrl reports to lama_syslog_h handlers at other nodes
%% and also if this is a warning/error/alert error, then
%% dispatch this report to standard event handlers or otherwise
%% print it to screen.
distribute(Distr, {_, _GL, {_Pid, {_,Type},_Rep}} = Report, State) ->
    case Distr of
    true  ->
        % Make sure the other node doesn't drop this message
        DistRep = setelement(2, Report, lama),
        [gen_event:notify({error_logger, N}, DistRep) || N <- nodes()];
    false -> ok
    end,
    distribute2(Type, Report, State).
%% Send
distribute2(Type, {_, GL, {Pid, _, {App,Mod,Line,Fmt,Args}=Report}},
            #state{ignore_types=Ignore} = State) ->
    case {Type, lists:member(Type, Ignore)} of
    {Type, false} when Type=:=warning; Type=:=error; Type=:=alert ->
        {F, A} = format_logger_args(Report),
        % Make sure it's properly logged to display/file by other event handlers
        ErrLogType = ?IF(Type, warning, warning_msg, error),
        gen_event:notify(error_logger, {ErrLogType, GL, {lama, F, A}}),
        do_log_error(Type, type_to_header(Type), Pid, F, A, State);
    {Type, false} when Type=:=notice; Type=:=info; Type=:=debug ->
        % Get custom message header
        Header = get_header(Type, Pid, App, Mod, Line),
        % output message to display
        io:format(?FLAT(Header)++Fmt, Args),
        % if configured, send this message to syslog
        do_log_error(Type, type_to_header(Type), Pid, Fmt, Args, State);
    _ ->
        ok
    end;
distribute2(_,_,_) -> ok.

format_logger_args({undefined,Mod,Line,Fmt,Args}) ->
    Fmt1  = " (~w:~w)~n " ++ Fmt,
    Args1 = [Mod,Line] ++ Args,
    {Fmt1, Args1};
format_logger_args({App,Mod,Line,Fmt,Args}) ->
    Fmt1  = " (~w/~w:~w)~n " ++ Fmt,
    Args1 = [App,Mod,Line] ++ Args,
    {Fmt1, Args1}.

do_log_error(Priority, Header, Pid, Format, Args,
   #state{host=H,indent=I,facility=F,sock=S,syslog_types=ST} = _State) ->
   case lists:member(Priority, ST) of
   true ->
      do_log(S, H, {I, Pid, F, Priority, Header, Format, Args});
   false ->
      ok
   end.

do_log(S, Host, {Indent, Pid, Facility, Priority, Header, Format, Args}) ->
    Msg =
        case catch io_lib:format(Format, Args) of
        Str when list(Str) -> Str;
        _ -> io_lib:format("~p - ~p~n", [Format,Args])
        end,
    Hdr = io_lib:format("<~w>~s ~w[~s]: [~s] ",
                [encode_facility(Facility) bor encode_priority(Priority),
                 timestamp(log), Indent, format_pid(Pid),
                 Header]),
    Packet = one_line(?FLAT(Hdr ++ Msg)),
    %io:format("~s", [Packet]),
    gen_udp:send(S,Host,?SYSLOG_PORT,Packet).

type_to_header(Type) ->
    to_upper(atom_to_list(Type)).

format_pid(Pid) ->
    [L] = io_lib:format("~w", [Pid]),
    case Pid of
    Pid when is_pid(Pid) ->        % Pid is given in the form: <X.Y.Z>
        atom_to_list(node(Pid)) ++ format_pid2(L);
    Pid when is_atom(Pid) ->       % Pid is given in the form: 'registered_name'
        case whereis(Pid) of
        undefined ->
            atom_to_list(node()) ++ "," ++ L;
        PidNum ->
            atom_to_list(node(PidNum)) ++ "," ++ L
        end;
    _ ->                           % What else?
        io_lib:format("~w,~s", [node(), L])
    end.
format_pid2([$< | T]) ->
    [$. | Tail] = lists:dropwhile(fun(C) -> C /= $. end, T),
    [$, | format_pid2(Tail)];
format_pid2([C,  $>]) -> [C];
format_pid2([C  | T]) -> [C | format_pid2(T)].

format_supervisor(Rep) ->
    Sup  = lama:get_opt(supervisor,   Rep, ""),
    Cont = lama:get_opt(errorContext, Rep, ""),
    Why  = lama:get_opt(reason,       Rep, ""),
    List = lama:get_opt(offender,     Rep, ""),
    Pid  = lama:get_opt(pid,  List, undefined),
    Name = lama:get_opt(name, List, undefined),
    MFA  = lama:get_opt(mfa,  List, undefined),
    io_lib:format("Supervisor: ~p, Context: ~p, Reason: ~p, "
                  "Offender: [Pid=~p, Name=~p, MFA=~p]",
                  [Sup, Cont, Why, Pid, Name, MFA]).

format_crash([OwnRep, _LinksRep]) ->
    Pid  = lama:get_opt(pid,             OwnRep, undefined),
    Name = lama:get_opt(registered_name, OwnRep, undefined),
    Why  = lama:get_opt(error_info,      OwnRep, undefined),
    io_lib:format("Pid: ~p, Name: ~p, Reason: ~p", [Pid, Name, Why]);
format_crash(Rep) ->
    proc_lib:format(Rep).

get_host(Host) ->
    case inet:gethostbyname(Host) of
    {ok,{hostent,_,_,inet,4,[IP|_]}} ->
        IP;
    {error, Reason} ->
        throw({stop, {gethostbyname, Host, inet:format_error(Reason)}})
    end.

encode_priority(emergency) -> 0; % system is unusable
encode_priority(alert)     -> 1; % action must be taken immediately
encode_priority(critical)  -> 2; % critical conditions
encode_priority(error)     -> 3; % error conditions
encode_priority(warning)   -> 4; % warning conditions
encode_priority(notice)    -> 5; % normal but significant condition
encode_priority(info)      -> 6; % informational
encode_priority(debug)     -> 7. % debug-level messages

encode_facility(local7)    -> (23 bsl 3); % local use 7
encode_facility(local6)    -> (22 bsl 3); % local use 6
encode_facility(local5)    -> (21 bsl 3); % local use 5
encode_facility(local4)    -> (20 bsl 3); % local use 4
encode_facility(local3)    -> (19 bsl 3); % local use 3
encode_facility(local2)    -> (18 bsl 3); % local use 2
encode_facility(local1)    -> (17 bsl 3); % local use 1
encode_facility(local0)    -> (16 bsl 3); % local use 0
encode_facility(clock)     -> (15 bsl 3); % clock daemon
encode_facility(log_alert) -> (14 bsl 3); % log alert (note 1)
encode_facility(log_audit) -> (13 bsl 3); % log audit (note 1)
encode_facility(ntp)       -> (12 bsl 3); % ntp daemon
encode_facility(ftp)       -> (11 bsl 3); % ftp daemon
encode_facility(authpriv)  -> (10 bsl 3); % security/authorization messages (private)
encode_facility(cron)      -> ( 9 bsl 3); % clock daemon
encode_facility(uucp)      -> ( 8 bsl 3); % UUCP subsystem
encode_facility(news)      -> ( 7 bsl 3); % network news subsystem
encode_facility(lpr)       -> ( 6 bsl 3); % line printer subsystem
encode_facility(syslog)    -> ( 5 bsl 3); % messages generated internally by syslogd
encode_facility(auth)      -> ( 4 bsl 3); % security/authorization messages
encode_facility(daemon)    -> ( 3 bsl 3); % system daemons
encode_facility(mail)      -> ( 2 bsl 3); % mail system
encode_facility(user)      -> ( 1 bsl 3); % random user-level messages
encode_facility(kern)      -> ( 0 bsl 3). % kernel messages

get_header(Type, Pid, App, Mod, Line) ->
    Node = node(Pid),
    Nd = ?IF(node(), Node, [""], io_lib:format("(~w) ", [Node])),
    case Type of
    Type when Type =:= info; Type =:= debug ->
        case App of
        undefined ->
            io_lib:format("[~s~w:~W] ", [Nd, Mod, Line, ?IF(Line=<999, true, 3, 5)]);
        _ ->
            io_lib:format("[~s~w/~w:~W] ", [Nd, App, Mod, Line, ?IF(Line=<999, true, 3, 5)])
        end;
    Type when Type =:= notice ->
        io_lib:format("~n=~s REPORT==== ~s ~s===~n ",
              [type_to_header(Type),timestamp(header),Nd]);
    _ -> [""]
    end.

timestamp(Type) ->
    {{YYYY,MM,DD}, {Hour,Min,Sec}} = calendar:local_time(),
    case Type of
    log ->
        io_lib:format("~3.s ~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",
                      [month(MM),DD,Hour,Min,Sec]);
    header ->
        io_lib:format("~2.2.0w-~3.s-~4w::~2.2.0w:~2.2.0w:~2.2.0w",
                      [DD,month(MM),YYYY,Hour,Min,Sec])
    end.

format_report(Rep) when list(Rep) ->
    case string_p(Rep) of
    true -> io_lib:format("~s~n",[Rep]);
    _    -> format_rep(Rep)
    end;
format_report(Rep) ->
    io_lib:format("~p~n",[Rep]).

format_rep([{Tag,Data}|Rep]) ->
    io_lib:format("~s: ~1000p. ",[proper_case(Tag),Data]) ++ format_rep(Rep);
format_rep([Other|Rep]) ->
    io_lib:format("~p. ",[Other]) ++ format_rep(Rep);
format_rep(_) ->
    [].

proper_case(Tag) when is_atom(Tag) ->
    proper_case2(atom_to_list(Tag));
proper_case(Tag) ->
    io_lib:format("~p", [Tag]).
proper_case2([C|Rest]) when C >= $a, C =< $z ->
    [C-($a-$A) | Rest];
proper_case2(Other) ->
    Other.

string_p([]) ->
    false;
string_p(Term) ->
    string_p1(Term).

string_p1([H|T]) when integer(H), H >= $\s, H < 255 ->
    string_p1(T);
string_p1([$\n|T]) -> string_p1(T);
string_p1([$\r|T]) -> string_p1(T);
string_p1([$\t|T]) -> string_p1(T);
string_p1([$\v|T]) -> string_p1(T);
string_p1([$\b|T]) -> string_p1(T);
string_p1([$\f|T]) -> string_p1(T);
string_p1([$\e|T]) -> string_p1(T);
string_p1([H|T]) when list(H) ->
    case string_p1(H) of
    true -> string_p1(T);
    _    -> false
    end;
string_p1([]) -> true;
string_p1(_) ->  false.

%%----------------------------------------------------------------------
is_string([H|T]) when is_integer(H), 0 < H, H < 256 ->
    is_string(T);
is_string([]) -> true;
is_string(_)  -> false.


one_line([]) ->
  [$\n];
one_line([$ ,$ |Rest]) ->
  one_line([$ |Rest]);
one_line([$\n|Rest]) ->
  one_line([$ |Rest]);
one_line([$\\,$n|Rest]) ->    % Avoid cases when a "\\n" string slips in.
  one_line([$ |Rest]);
one_line([$\\,$"|Rest]) ->    % Avoid cases when a "\"" string slips in.
  one_line([$"|Rest]);
one_line([$\r|Rest]) ->
  one_line(Rest);
one_line([$\t|Rest]) ->
  one_line([$ |Rest]);
one_line([Char|Rest]) ->
  [Char|one_line(Rest)];
one_line(_) ->
  false.

%% month

month( 1) -> "Jan";
month( 2) -> "Feb";
month( 3) -> "Mar";
month( 4) -> "Apr";
month( 5) -> "May";
month( 6) -> "Jun";
month( 7) -> "Jul";
month( 8) -> "Aug";
month( 9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

test() ->
    application:start(lama),
    % test alarms
    lama_alarm_h:set_alarm  ({test, "Test1"}),
    lama_alarm_h:clear_alarm(test),
    lama_alarm_h:set_alarm  ({{disk_full, "/usr"}, "Test2"}),
    lama_alarm_h:clear_alarm({disk_full, "/usr"}),
    %
    error_logger:error_report(test30),
    error_logger:error_report(type30, test31),
    error_logger:error_msg("test32~n"),
    error_logger:error_msg("test33: ~w~n", [good]),
    %
    error_logger:info_report(test50),
    error_logger:info_report(type50, test51),
    error_logger:info_msg("test52~n"),
    error_logger:info_msg("test53: ~w~n", [good]),
    %
    error_logger:warning_report(test70),
    error_logger:warning_report(type70, test71),
    error_logger:warning_msg("test72~n"),
    error_logger:warning_msg("test73: ~w~n", [good]),
    %
    ?ALERT("Logger alert: ~p~n", [test100]),
    ?ERROR("Logger error: ~p~n", [test101]),
    ?WARNING("Logger warning: ~p~n", [test102]),
    ?WARNING("Logger warning: ~p~n", [test103]),
    ?INFO("Logger info: ~p~n", [test104]),
    ?DEBUG("Logger debug: ~p~n", [test105]),
    ?INFO("Logger info: ~p~n", [test106]),
    ?NOTICE("Logger info: ~p~n", [test107]),
    %
    ok.
