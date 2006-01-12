%%%------------------------------------------------------------------------
%%% File: $Id$
%%%------------------------------------------------------------------------
%%% @author Serge Aleynikov <serge@hq.idt.net>
%%% @doc This module implements tracing support for function calls.  It is
%%% similar to dbg:c/3, however it can return the trace as its result.
%%% @version $Rev: 359 $
%%% $LastChangedDate: 2006-01-11 10:28:26 -0500 (Wed, 11 Jan 2006) $
%%% @end
%%% $URL: http://devlinuxpro.mis.idt.net/svn/apps/DRiP/current/lib/lama/src/tracer.erl $
%%%------------------------------------------------------------------------
-module(tracer).
-author('serge@corp.idt.net').
-id("$Id$").

% Debugging facility
-export([t/3, t/4, tp/3, tp/4, tf/3, tf/4, print_trace/1]).

%% @spec tp(M, F, Args) -> Result::term()
%% @doc Trace a function call and print trace result to the console.
%% @end
%%
tp(M, F, Args) ->
    tp(M, F, Args, call).

%% @spec tp(M, F, Args, FlagList) -> Result::term()
%%          FlagList = [ Flag::atom() ]
%% @doc Trace a function call and print trace result to the console.
%%      See {@link //kernel/erlang:trace/3} for description of Flags.
%% @end
%%
tp(M, F, Args, Flags) ->
    {R,T} = t(M, F, Args, Flags),
    print_trace(T),
    R.

%% @spec tf(M, F, Args) -> TraceList::term()
%% @doc Trace a function call and return trace result.
%% @end
%%
tf(M, F, Args) ->
    tf(M, F, Args, call).

%% @spec tf(M, F, Args, Flags) -> TraceList::term()
%% @doc Trace a function call and return trace result.
%%      See {@link //kernel/erlang:trace/3} for description of Flags.
%% @end
%%
tf(M, F, Args, Flags) ->
    {_,T} = t(M, F, Args, Flags),
    format_trace(T, []).

%% @spec t(M::atom(), F::atom(), Args::list()) -> {Result::term(), Trace::list()}
%% @doc Call M:F function with Args arguments, and return a tuple
%% {Result, Trace} containing call trace.
%% @end
%%
t(M, F, Args) ->
    t(M, F, Args, call).

%% @spec t(M::atom(), F::atom(), Args::list(), Flags::list()) ->
%%            {Result::term(), Trace::list()}
%% @doc Call M:F function with Args arguments, and return a tuple
%% {Result, Trace} containing call trace.
%% @end
%%
t(M, F, Args, Flags) ->
    % Set up a tracer
    Fun = fun(Msg,A) ->
              T=[Msg|A], put(trace, T), T
          end,
    dbg:tracer(process, {Fun, []}),
    dbg:p(self(), Flags),
    dbg:tpl(M, [{'_',[],[{return_trace}]}]),

    % Call the function
    Res = apply(M, F, Args),

    % Retrieve trace
    {ok, Tracer} = dbg:get_tracer(),

    {dictionary, Dict} = process_info(Tracer, dictionary),
    dbg:stop_clear(),
    case lists:keysearch(trace, 1, Dict) of
    {value, {_, Val}} ->
        {Res, match_trace(Val)};
    false ->
        {Res, undefined}
    end.
%   Another way to do this is through sending a stop message to
%   the tracer.  However this causes an '** exit **' message
%   to be printed on terminal, and doesn't clear the tracing options
%    dbg:ctp(),  % clear
%    Tracer ! {self(), stop},
%    receive
%    {stop, Val} ->
%        {Res, match_trace(Val)}
%    after 3000 ->
%        {Res, undefined}
%    end.

match_trace(Trace) ->
    match_trace(lists:reverse(Trace), [], 0).
match_trace([], Acc, _Indent) ->
    lists:reverse(Acc);
match_trace([{trace,_,call,{M,F,A}}|Rest], Acc, Indent) ->
    match_trace(Rest, [{call, {M, F, A}, Indent+1} | Acc], Indent+1);
match_trace([{trace,_,return_from,{M,F,A},R}|Rest], Acc, Indent) ->
    Acc1 = merge_call({M,F,A}, R, Acc, []),
    match_trace(Rest, Acc1, Indent-1);
match_trace([{A,B}|Rest], Acc, Indent) ->
    match_trace(Rest, [{other, A, B} | Acc], Indent).

merge_call(MFA, R, [], Acc) ->
    lists:reverse([{return, {MFA, R}} | Acc]);
merge_call({M,F,A}, R, [{call, {M, F, Args}, Indent} | Rest], Acc)
  when length(Args) == A ->
    lists:reverse(Acc) ++ [{Indent, {M, F, Args}, R}] ++ Rest;
merge_call({M,F,A}, R, [H | Rest], Acc) ->
    merge_call({M,F,A}, R, Rest, [H|Acc]).

print_trace(Trace) ->
    Str = format_trace(Trace, []),
    io:format("~s", [Str]),
    ok.

format_trace([], Acc) ->
    Acc;
format_trace([{Indent, {M,F,Args}, Res}|Tail], Acc) ->
    A = case Args of
        [] -> "";
        _  -> [[ 91,T,93 ]] = io_lib:format("~300p", [Args]), T
        end,
    R = io_lib:format("~300p", [Res]),
    R1 = lists:flatten(A ++ R),
    Offset = Indent-1,
    S = if (length(R1)) > 80 ->
            io_lib:format("~-*c~w:~w(~s) ->~n~-*c    ~s~n", [Offset,$., M,F,A,Offset,$ ,R]);
        true ->
            io_lib:format("~-*c~w:~w(~s) -> ~s~n", [Offset,$., M,F,A,R])
        end,
    format_trace(Tail, Acc ++ lists:flatten(S));
format_trace([Head | Tail], Acc) ->   % FIXME: This should never happen!
    format_trace(Tail, [io_lib:format("XXX Wrong Format: ~p~n", [Head]), Acc]).

%strip_calls([], _M, _List, Acc) ->
%    Acc;
%strip_calls([{trace,_,call,{M,F,_}} = L | Rest], M, List, Acc) ->
%    case lists:member(F, List) of
%    true  -> strip_calls(Rest, M, List, Acc);
%    false -> strip_calls(Rest, M, List, [L|Acc])
%    end;
%strip_calls([{trace,_,return_from,{M,F,_},_} = L | Rest], M, List, Acc) ->
%    case lists:member(F, List) of
%    true  -> strip_calls(Rest, M, List, Acc);
%    false -> strip_calls(Rest, M, List, [L|Acc])
%    end;
%strip_calls([H |Rest], M, List, Acc) ->
%    strip_calls(Rest, M, List, [H | Acc]).


