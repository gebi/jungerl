%%%----------------------------------------------------------------------
%%% File    : edit_extended.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : execute-extended-command (emacs lingo)
%%% Created : 14 Jan 2001 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(edit_extended).
-author('luke@bluetail.com').

-include_lib("ermacs/include/edit.hrl").

-compile(export_all).
-compile({parse_transform, edit_transform}).

extended_command(State, Mod, Func, Args) ->
    {Params, _Doc} = find_cmd_info(Mod, Func),
    NewState = execute(State, Mod, Func, Args, Params),
    NewState#state{lastcmd={Mod, Func, Args}}.

execute(State, Mod, Func, Args, []) ->
    case catch apply(Mod, Func, [State | Args]) of
	S when record(S, state) ->
	    S;
	{'EXIT', Rsn} ->
	    io:format("** Crash: ~p~n", [Rsn]),
	    edit_util:popup_message(State, '*Error*', "** Crash: ~p", [Rsn]);
	Other ->
	    State
    end;
execute(State, Mod, Func, Args, Params)
  when State#state.pending_cmd /= undefined ->
    edit_util:status_msg(State, "Minibuffer busy!");
execute(State, Mod, Func, Args, Params) ->
    Cont = make_execute_continuation(Mod, Func, Args, Params),
    {Type, Prompt} = hd(Params),
    edit_var:set(extended_arg_type, Type),
    edit_complete:completion_init(Type),
    default_init(State, Type),
    State1 = activate_continuation(State, Cont, Prompt ++ " "),
    State2 = edit_util:select_window(State1,
				     fun(W) ->
					     W#window.minibuffer
				     end),
    case State#state.pending_win of
	undefined ->
	    State2#state{pending_win=(State#state.curwin)#window.id};
	_ ->
	    State2
    end.

default_init(State, file) ->
    edit_buf:set_text(minibuffer, edit_util:pwd(State));
default_init(State, _) ->
    ok.

make_execute_continuation(Mod, Func, Args, Params) ->
    fun(State, NewArg) ->
	    State1 = if length(Params) == 1 ->
			     %% last argument, we're done!
			     deactivate_continuation(State);
			true ->
			     State
		     end,
	    execute(State1, Mod, Func, Args ++ [NewArg], tl(Params))
    end.

activate_continuation(State, C, Prompt) ->
    State1 = State#state{pending_cmd=C},
    Enabler = fun(Win) -> Win#window{active=true, prefix=Prompt} end,
    edit_util:update_minibuffer_window(State1, Enabler).

deactivate_continuation(State) ->
    edit_buf:set_text(minibuffer, ""),
    State1 = State#state{pending_cmd=undefined},
    Disabler = fun(Win) -> Win#window{active=false, prefix=""}
	       end,
    %% disable minibuffer
    State2 = edit_util:update_minibuffer_window(State1, Disabler),
    %% reselect the window that the command is being done in
    State3 =
	edit_util:select_window(State2,
				fun(W) ->
					W#window.id == State2#state.pending_win
				end),
    State4 = State3#state{pending_win=undefined}.


find_cmd_info(Mod, Func) ->
    case catch Mod:command_info() of
	{'EXIT', _} ->
	    {[], ""};
	L when list(L) ->
	    case lists:keysearch(Func, 1, L) of
		{value, {_, Params, Doc}} ->
		    {Params, Doc};
		_ ->
		    {[], ""}
	    end
    end.

%% Callbacks from key bindings

take_argument(State) when State#state.pending_cmd /= undefined ->
    Text = edit_buf:get_text(minibuffer),
    edit_history:add(history_var_name(), Text),
    Cont = State#state.pending_cmd,
    Cont(State, Text).

abort(State) ->
    io:format("Aborted!~n"),
    State1 = deactivate_continuation(State).

%% M-x


-command({execute_extended_command,
	  [{mod_func, "M-x"}],
	  "Like M-x in emacs."}).

execute_extended_command(State, MF) ->
    case string:tokens(MF, " :") of
	[ModStr, FunStr] ->
	    Mod = list_to_atom(ModStr),
	    Fun = list_to_atom(FunStr),
	    edit:invoke_extended_async(Mod, Fun, []),
	    State;
	_ ->
	    edit_util:status_msg(State, "Bad string; must be \"Mod:Fun\"")
    end.

%% History

history_move(State, Dir) ->
    edit_history:move(State, history_var_name(), history_region_fun(), Dir).

-command({history_search, [{history_regexp, "History regexp:"}]}).
%% FIXME: This function fails when it tries to activate the minibuffer to
%% search regexp, but the minibuffer is already active.
history_search(State, RE) ->
    edit_history:search(State, history_var_name(), history_region_fun(), RE).

history_var_name() ->
    TypeString = atom_to_list(edit_var:lookup(extended_arg_type)),
    list_to_atom("extended_history:" ++ TypeString).

history_region_fun() ->
    fun(Buf) -> {1, edit_buf:point_max(Buf)} end.

