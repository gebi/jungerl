%%%----------------------------------------------------------------------
%%% File    : edit_eval.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Erlang code evaluation
%%% Created : 21 Jan 2001 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(edit_eval).
-author('luke@bluetail.com').

-include_lib("ermacs/include/edit.hrl").

-compile({parse_transform, edit_transform}).

-import(edit_lib, [buffer/1]).

-compile(export_all).
%%-export([Function/Arity, ...]).

-define(history, erlang_interaction_history).

eval_buffer(State) ->
    B = buffer(State),
    Text = edit_buf:get_text(B),
    {ok, Scan, _} = erl_scan:string(Text), % safe ?
    case erl_parse:parse_exprs(Scan) of
	{ok, Parse} ->
	    case catch erl_eval:exprs(Parse, []) of
		{value, V, _} ->
		    edit_util:status_msg(State, "~p", [V]);
		{'EXIT', Reason} ->
		    edit_util:status_msg(State, "** exited: ~p **", [Reason])
	    end;
	{error, {_, erl_parse, Err}} ->
	    edit_util:status_msg(State, "~p", [Err])
    end.

eval_string(State, String) ->
    {ok, Scan, _} = erl_scan:string(String), % safe ?
    eval_tokens(State, Scan, []).

eval_tokens(Buf, Tokens, Bindings) ->
    case erl_parse:parse_exprs(Tokens) of
	{ok, Parse} ->
	    case erl_eval:exprs(Parse, Bindings, lf_handler(Buf)) of
		{value, V, NewBs} ->
		    {ok, V, NewBs};
		Error ->
		    {error, Error}
	    end;
	{error, {_, erl_parse, Err}} ->
	    {error, fmt("~s", [Err])}
    end.

lf_handler(Buf) ->
    {eval, {?MODULE, local_func}, [Buf]}.

fmt(Fmt, Args) -> lists:flatten(io_lib:format(Fmt, Args)).

-command({eval_expression, [{erl_expr, "Eval:"}]}).

eval_expression(State, Expr) ->
    Str = case eval_string(State, Expr) of
	      {ok, Val, NewBS} ->
		  fmt("~p", [Val]);
	      {error, Rsn} ->
		  fmt("~p", [Rsn])
	  end,
    edit_util:popup_message(State, '*Eval*', Str).

region(Buffer) ->
    {find_start(Buffer), edit_lib:end_of_line_pos(Buffer)}.

%% ----------------------------------------------------------------------
%% Interactive evaluation major mode
%% ----------------------------------------------------------------------

-define(keymap, erlang_interaction_map).
-define(output_mark, erlang_output).
-define(PROMPT, ">> ").

erlang_interaction_mode(State) ->
    case edit_keymap:keymap_exists(?keymap) of
	false ->
	    init_map();
	true ->
	    ok
    end,
    Mode = #mode{name="Erlang Interaction",
		 id=erlang_interaction,
		 keymaps=[?keymap]},
    Buf = buffer(State),
    edit_buf:set_mode(Buf, Mode),
    edit_buf:add_mark(Buf, ?output_mark, 1, forward),
    ok.

init_map() ->
    edit_keymap:new(?keymap),
    edit_keymap:bind_each(?keymap, bindings()).

bindings() ->
    [{"C-m", {?MODULE, interactive_eval, []}},
     {"C-a", {?MODULE, beginning_of_prompt, []}}]
	++ edit_history:bindings(?history, {?MODULE, region}).

interactive_eval(State) ->
    P = find_start(buffer(State)),
    interactive_eval1(State, P, P).

interactive_eval1(State, CodeStart, SearchStart) ->
    Buf = buffer(State),
    Max = edit_buf:point_max(Buf),
    Pred = fun(C) -> C == $\n end,
    CodeEnd = edit_lib:find_char_forward(Buf, Pred, SearchStart, Max),
    Region = edit_buf:get_region(Buf, CodeStart, CodeEnd),
    Bindings = edit_var:lookup(erlang_interaction_bindings, []),
    case erl_scan:tokens([], Region ++ "\n", 1) of
	{done, {ok, Tokens, _}, _} ->	% ok - enough
	    %% Move point to the end
	    edit_buf:move_mark(Buf, point, CodeEnd),
	    %% eval
	    edit_util:spawn_with([Buf],
				 fun() ->
					 eval_async(Buf, Tokens, Bindings)
				 end),
	    edit_history:add(?history, Region);
	{more, _} when CodeEnd == Max ->
	    edit_buf:insert(Buf, "\n", edit_buf:mark_pos(Buf, point));
	{more, _} ->
	    interactive_eval1(State, CodeStart, CodeEnd + 1)
    end.

eval_async(Buf, Tokens, Bindings) ->
    ensure_serv_started(Buf),
    %% update output marker
    edit_buf:insert(Buf, "\n", edit_buf:mark_pos(Buf, point)),
    InsertionPoint = edit_buf:mark_pos(Buf, point),
    edit_buf:move_mark(Buf, ?output_mark, InsertionPoint),
    %% eval
    Str = case serv_eval(Buf, Tokens, Bindings) of
	      {ok, Value, NewBs} ->
		  %% FIXME: bindings
		  edit_var:set(erlang_interaction_bindings, NewBs),
		  fmt("=> ~p\n", [Value]);
	      {error, {'EXIT', Rsn}} ->
		  fmt("** exited: ~p **\n", [Rsn]);
	      {error, Rsn} ->
		  fmt("** ~s **\n", [Rsn])
	  end,
    edit_buf:insert(Buf, Str, edit_buf:mark_pos(Buf, point)),
    PromptPos = edit_buf:mark_pos(Buf, point),
    edit_buf:insert(Buf, ?PROMPT, PromptPos),
    edit_buf:move_mark(Buf, ?output_mark, PromptPos),
    redraw(),
    ok.

redraw() ->
    edit:invoke_async(edit_lib, nop, []).

%% Go to the beginning of the line or directly after the prompt,
%% whichever is closer.
beginning_of_prompt(State) ->
    Buf = buffer(State),
    edit_buf:move_mark(Buf, point, beginning_of_prompt_pos(Buf)).

beginning_of_prompt_pos(Buf) ->
    Point = edit_buf:mark_pos(Buf, point),
    BOL = edit_lib:beginning_of_line_pos(Buf),
    case find_start(Buf) of
	Pos when Pos > BOL, Pos =< Point ->
	    Pos;
	_ ->
	    BOL
    end.

find_start(Buf) ->
    case edit_lib:find_string_backward(Buf, ?PROMPT) of
	not_found ->
	    1;
	X ->
	    edit_lib:min(X + length(?PROMPT), edit_buf:point_max(Buf))
    end.

%% local_func(Function, Args, Bindings, Shell) ->
%%	{value,Val,Bs}
%%  Evaluate local functions, including shell commands.

local_func(F, As0, Bs0, Buf) ->
    {As,Bs} = erl_eval:expr_list(As0, Bs0, {eval,{?MODULE,local_func},[Buf]}),
    case erlang:function_exported(user_default, F, length(As)) of
	true ->
	    {value,apply(user_default, F, As),Bs};
	false ->
	    {value,apply(shell_default, F, As),Bs}
    end;
local_func(F, As0, Bs0, Buf) ->
    {As,Bs} = erl_eval:expr_list(As0, Bs0, {eval,{?MODULE,local_func},[Buf]}),
    {value,apply(F, As),Bs}.
    

%% ----------------------------------------------------------------------
%% Evaluation server API

%% ok | already_started
ensure_serv_started(Buffer) ->
    case whereis(serv_name(Buffer)) of
	undefined ->
	    Pid = spawn(?MODULE, eval_serv_init, [Buffer]),
	    register(serv_name(Buffer), Pid),
	    ok;
	Pid ->
	    already_started
    end.

serv_eval(Buffer, Tokens, Bindings) ->
    Serv = whereis(serv_name(Buffer)),
    erlang:monitor(process, Serv),
    Serv ! {call, self(), {eval, Tokens, Bindings}},
    receive
	{reply, Result} ->
	    Result;
	{'DOWN', _, process, Serv, Rsn} ->
	    {error, {'EXIT', Rsn}}
    end.

%% serv_name(foo) -> 'foo:eval_serv'
serv_name(Buffer) -> list_to_atom(atom_to_list(Buffer) ++ ":eval_serv").

%% ----------------------------------------------------------------------
%% Eval server loop

eval_serv_init(Buffer) ->
    erlang:monitor(process, whereis(Buffer)),
    GL = gl_spawn_link(Buffer),
    group_leader(GL, self()),
    eval_serv_loop(Buffer, GL).

eval_serv_loop(Buffer, GL) ->
    receive
	{call, From, {eval, Tokens, Bindings}} ->
	    GL ! {got_shared_lock, self()},
	    Result = eval_tokens(Buffer, Tokens, Bindings),
	    GL ! {releasing_shared_lock, self()},
	    receive gl_ack -> ok end,
	    From ! {reply, Result},
	    eval_serv_loop(Buffer, GL);
	{'DOWN', _, _, _, _} ->
	    exit(buffer_died)
    end.

%% ----------------------------------------------------------------------
%% Group leader process for putting output into a buffer.
%%
%% The evaluation server sends us:
%%   {got_shared_lock, HolderPid}
%%   followed by:
%%   {releasing_shared_lock, HolderPid}
%% Between these messages, we informally share the lock on the buffer.

gl_spawn_link(Buffer) ->
    spawn_link(?MODULE, gl_serv_init, [Buffer]).

gl_serv_init(Buffer) ->
    gl_serv_without_lock(Buffer).

%% State: Nothing known about Buffer's lock
gl_serv_without_lock(Buffer) ->
    ?debug("STATE: no lock~n", []),
    receive
	Msg = {io_request, From, ReplyAs, Req} ->
	    gl_serv_work(Buffer, Msg);
	{got_shared_lock, Holder} ->
	    gl_serv_with_lock(Buffer)
    end.

%% State: Buffer is locked by eval_server, so we can use it.
gl_serv_with_lock(Buffer) ->
    receive
	Msg = {io_request, From, ReplyAs, Req} ->
	    do_gl_request(Buffer, Msg),
	    gl_serv_with_lock(Buffer);
	{releasing_shared_lock, Holder} ->
	    Holder ! gl_ack,
	    gl_serv_without_lock(Buffer)
    end.

%% Action: Acquire the lock on Buffer, perform a request, and release.
gl_serv_work(Buffer, Req) ->
    edit_buf:async_borrow(Buffer),
    receive
	{loan, Buffer} ->
	    do_gl_request(Buffer, Req),
	    edit_buf:async_return(Buffer),
	    gl_serv_without_lock(Buffer);
	got_shared_lock ->
	    do_gl_request(Buffer, Req),
	    gl_serv_with_lock(Buffer)
    end.

%% Perform an I/O request by writing the result into the buffer.
do_gl_request(Buffer, {io_request, From, ReplyAs, {put_chars, M, F, A}}) ->
    case catch apply(M, F, A) of
	{'EXIT', Reason} ->
	    exit(From, Reason);
	IOList ->
	    do_gl_request(Buffer,
			  {io_request, From, ReplyAs, {put_chars, IOList}})
    end;
do_gl_request(Buffer, {io_request, From, ReplyAs, {put_chars, IOList}}) ->
    From ! {io_reply, ReplyAs, ok},
    Bin = list_to_binary(IOList),
    Pos = edit_buf:mark_pos(Buffer, ?output_mark),
    edit_buf:insert(Buffer, Bin, Pos),
    redraw();
do_gl_request(Buffer, {io_request, From, ReplyAs, {get_until, _, _, _}}) ->
    From ! {io_reply, ReplyAs, eof}.


