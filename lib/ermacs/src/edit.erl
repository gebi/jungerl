%%%----------------------------------------------------------------------
%%% File    : edit.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Main editor process.
%%%
%%% Grown out of Tobbe's 'edit' program, and slowly rewritten.
%%%----------------------------------------------------------------------
-module(edit).
-author('luke@bluetail.com').
-export([start/0]).

-include_lib("ermacs/include/edit.hrl").

-compile(export_all).

%% Command-line entry function. Starts the editor.
start() ->
    init().

%% Another command-line entry function. Starts the editor and loads
%% some files.
start(Args) ->
    %% Easy/hacky way - asynchronously ask that all the files be
    %% loaded. When this process is initialised it'll see the
    %% messages.
    Filenames = lists:map(fun(X) -> atom_to_list(X) end,
			  Args),
    lists:foreach(fun(Filename) ->
			  self() ! {invoke, {edit_file, find_file, [Filename]}}
		  end,
		  Filenames),
    start().

%% Another command-line entry function. Starts the editor with some
%% modules loaded for debugging.
debug() ->
    lists:foreach(fun(Mod) -> i:ii(Mod) end, debug_modules()),
    i:im(),
    sleep(1000),
    proc_lib:start_link(?MODULE, start, []).

debug_modules() ->
    [edit_display, edit_lib, ?EDIT_TERMINAL, edit_keymap, edit_buf,
     edit_extended, edit_file, cord, edit_eval, edit_util, edit_text].

%% ----------------------------------------------------------------------
%% API program

invoke_async(M, F, A) ->
    edit ! {invoke, {M, F, A}}.

invoke_async(Fn) ->
    edit ! {invoke, Fn}.

invoke_extended_async(M, F, A) ->
    edit ! {invoke_extended, {M, F, A}}.

get_key() ->
    edit ! {want_key, self()},
    receive {key_input, Ch} -> Ch end.

%% ----------------------------------------------------------------------
%% Turn this process into a lean, mean, editing machine
init() ->
    register(?MODULE, self()),
    init_io_traps(),
    process_flag(trap_exit, true),
    %% Initialize editor
    edit_keymap:start_link_server(),
    edit_globalmap:init(),
    ?EDIT_TERMINAL:setup(),
    edit_input:start_link(self()),
    init_vars(),
    init_buffers(),
    init_mods(),
    State = init_windows(#state{}),
    State1 = load_dot_ermacs(State),
    State2 = redraw(State1),
    io:format("INIT: ~p~n", [edit_mod:require(em_stdlib)]),
    %%profile(State2),
    loop(State2).

init_io_traps() ->
    {ok, Leader} = file_gl:start_link("/tmp/edit.out"),
    group_leader(Leader, self()),
    error_logger:tty(false).

%% Setup initial buffers (scratch and minibuffer)
init_buffers() ->
    edit_buf:new('*scratch*'),
    edit_buf:new(minibuffer),
    MBMode = #mode{name="Minibuffer",
		   id=minibuffer,
		   keymaps=[minibuffer_map]},
    edit_buf:set_mode(minibuffer, MBMode).

%% Setup initial windows
init_windows(State) ->
    Width = ?EDIT_TERMINAL:width(),
    Height = ?EDIT_TERMINAL:height(),
    ScratchWin = edit_window:make_window('*scratch*',
					 0,
					 Width,
					 Height - 1),
    MiniWin1 = edit_window:make_window(minibuffer,
				       Height - 1,
				       Width,
				       1),
    MiniWin2 = MiniWin1#window{active=false,
			       minibuffer=true},
    State#state{curwin=ScratchWin,
		buffers=['*scratch*'],
		windows=[MiniWin2]}.

init_minibuffer(State) ->
    edit_buf:new(minibuffer),
    State.

init_vars() ->
    edit_var:start_link(),
    edit_var:set(killring, []).

init_mods() ->
    edit_mod:init(),
    %% special-case explicit initialisations for the core stuffs
    edit_file:mod_init(),
    ok.

load_dot_ermacs(State) ->
    Filename = filename:join(os:getenv("HOME"), ".ermacs"),
    case file:read_file_info(Filename) of
	{error, _} ->
	    edit_util:status_msg(State, "No ~~/.ermacs to read.");
	{ok, _} ->
	    case catch file:eval(Filename) of
		ok ->
		    State;
		{error, Rsn} ->
		    edit_util:status_msg(State, "~/.ermacs failed: ~p", [Rsn])
	    end
    end.
	
%% -------------
%% The Main Loop

loop(S) ->
    State = S#state{curwin=edit_display:draw_window(S#state.curwin)},
    NewState = dispatch(State),
    ?MODULE:loop(redraw(NewState)).

redraw(State) ->
    Wins = [edit_display:draw_window(W) || W <- State#state.windows],
    lists:foreach(fun(W) -> edit_display:draw_window(W) end,
		  State#state.windows),
    Cur = edit_display:draw_window(State#state.curwin),
    ?EDIT_TERMINAL:refresh(),
    State#state{curwin=Cur,
		windows=Wins}.

%% Dispatch a command, based on the next message we receive.
dispatch(State) ->
    Buf = (State#state.curwin)#window.buffer,
    Keymaps = (edit_buf:get_mode(Buf))#mode.keymaps ++ [global_map],
    receive
	{invoke, {M, F, A}} ->
	    dispatch_proc(State, fun() -> apply(M, F, [State | A]) end);
	{invoke, Fun} when function(Fun) ->
	    dispatch_proc(State, fun() -> Fun(State) end);
	{invoke_extended, {Mod, Func, Args}} ->
	    dispatch_extended(State, Mod, Func, Args);
	{key_input, Ch} ->
	    case find_cmd(State, Keymaps, Ch) of
		unbound ->
		    edit_util:status_msg(State, "Unbound key");
		{Mod, Func, Args} ->
		    dispatch_extended(State, Mod, Func, Args);
		Other ->
		    edit_util:status_msg(State,"Bad binding: ~p~n",[Other])
	    end;
	{'EXIT', _Someone, _SomeReason} ->
	    dispatch(State);
	Other ->
	    io:format("Unexpected message: ~p~n", [Other])
    end.

dispatch_extended(State, Mod, Func, Args) ->
    F = fun() -> edit_extended:extended_command(State, Mod, Func, Args) end,
    dispatch_proc(State, F).

%% ----------------------------------------------------------------------
%% Dispatch a command in a new process. The process gets aborted if
%% the user presses C-g.
dispatch_proc(State, CommandFun) ->
    Self = self(),
    F = fun() ->
		Result = CommandFun(),
		Self ! {result, self(), Result}
	end,
    Pid = spawn_link(F),
    dispatch_loop(State, Pid, false).

dispatch_loop(State, Pid, WantKey) ->
    receive
	{result, Pid, Result} ->
	    Result;
	{key_input, $\^G} ->
	    exit(Pid, user_abort),
	    edit_util:status_msg(State, "Abort");
	{key_input, Ch} when WantKey == true ->
	    Pid ! {key_input, Ch},
	    dispatch_loop(State, Pid, false);
	{want_key, Pid} when WantKey == false ->
	    dispatch_loop(State, Pid, true);
	{'EXIT', Pid, Reason} ->
	    io:format("DE: ~p~n", [Reason]),
	    edit_util:status_msg(State,"Dispatch error: ~p~n",[Reason])
    end.


%% ----------------------------------------------------------------------
%% Keymap lookup

find_cmd(State, Keymaps) ->
    Ch = get_char(),
    find_cmd(State, Keymaps, Ch).

find_cmd(State, [], Ch) ->
    unbound;
find_cmd(State, [Keymap|Keymaps], Ch) ->
    case edit_keymap:lookup(Keymap, Ch) of
	{ok, {keymap, NewMap}} ->
	    find_cmd(State, [NewMap]);
	{ok, Cmd} ->
	    Cmd;
	unbound ->
	    find_cmd(State, Keymaps, Ch)
    end.

get_char() ->
    receive {key_input, C} -> C end.

sleep(T) -> receive after T -> true end.


%% ----------------------------------------------------------------------
%% Profiling

profile(State) ->
    receive after 100 -> ok end,
    Procs = [edit|State#state.buffers],
    timer:start_link(),
    spawn_link(fun() ->
		       analyse_loop(Procs)
	       end).

analyse_loop(Procs) ->
    eprof:start(),
    profiling = eprof:profile(Procs),
    receive after 15000 ->
		    eprof:total_analyse()
	    end,
    analyse_loop(Procs).


