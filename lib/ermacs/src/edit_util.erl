%%%----------------------------------------------------------------------
%%% File    : edit_util.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Utility functions
%%% Created : 15 Oct 2000 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(edit_util).
-author('luke@bluetail.com').

-include_lib("ermacs/include/edit.hrl").

-export([keyname/1, status_msg/2, status_msg/3, update_minibuffer_window/2,
	 select_window/2, update/3]).

-compile(export_all).

keyname(263) -> "C-h";
keyname(127) -> "C-h";
keyname(272) -> "C-h";
keyname(C) ->
    case ctrl_p(C) of
	true ->
	    "C-" ++ keyname(tolower(unctrl(C)));
	false ->
	    case meta_p(C) of
		true ->
		    "M-" ++ keyname(tolower(unmeta(C)));
		false ->
		    [C]
	    end
    end.

%% Eighth bit set means meta
meta_p(C) -> C band meta_bit() /= 0.
unmeta(C) -> C band (bnot meta_bit()).

meta_bit() -> 2#10000000.

ctrl_p(C) -> Base = unmeta(C), ((Base band 2#01000000) == 0) and (Base < 32).
unctrl(C) -> C + 64.

tolower(C) when C >= $A, C =< $Z ->
    C - $A + $a;
tolower(C) ->
    C.

%% Show a one-line message in the status line (where the minibuffer is)
status_msg(State, Fmt, Args) ->
    status_msg(State, io_lib:format(Fmt, Args)).
status_msg(State, String) ->
    Text = lists:flatten(String),
    AllWindows = [State#state.curwin | State#state.windows],
    NewWindows = update(fun(Win) -> Win#window{status_text=Text} end,
			fun(Win) -> Win#window.minibuffer end,
			AllWindows),
    State#state{curwin=hd(NewWindows),
		windows=tl(NewWindows)}.

kill_newlines([]) ->
    [];
kill_newlines([$\n|T]) ->
    [$  |
     kill_newlines(lists:dropwhile(fun(C) -> C == $  end, T))];
kill_newlines([H|T]) ->
    [H|kill_newlines(T)].

%% Apply Fun to minibuffer window and put the result in the state.
update_minibuffer_window(State, Fun) ->
    AllWindows = [State#state.curwin | State#state.windows],
    NewWindows = update(Fun,
			fun(Win) -> Win#window.minibuffer end,
			AllWindows),
    State#state{curwin=hd(NewWindows),
		windows=tl(NewWindows)}.

%% Map over all windows in State with Fun.
%% Fun is fun(Win) -> Win'
window_map(State, Fun) ->
    State#state{curwin=Fun(State#state.curwin),
		windows=lists:map(Fun, State#state.windows)}.

%% Return a list of all windows in State.
windows(State) ->
    [State#state.curwin | State#state.windows].

%% Use F to update the first item of L which satisfies P.
%% e.g.
update(F, P, []) ->
    [];
update(F, P, [H|T]) ->
    case P(H) of
	true ->
	    [F(H)|T];
	false ->
	    [H|update(F, P, T)]
    end.

%% Select the first window satisfying P, or crash if no match
select_window(State, P) ->
    case P(State#state.curwin) of
	true ->
	    State;
	false ->
	    select_window1(State, P, State#state.windows, [State#state.curwin])
    end.

select_window1(State, P, [], Acc) ->
    exit(nomatch);
select_window1(State, P, [H|T], Acc) ->
    case P(H) of
	true ->
	    State#state{curwin=H,
			windows=T ++ lists:reverse(Acc)};
	false ->
	    select_window1(State, P, T, [H|Acc])
    end.

set_buffer(State, Buffer) ->
    Win = edit_window:attach(State#state.curwin, Buffer),
    State#state{curwin = Win,
		buffers = [Buffer | (State#state.buffers -- [Buffer])]}.

%% Display the message `String'. If it's only one line, it's shown in
%% the status line. Otherwise, a buffer called `Buf' is popped up to
%% show it.
popup_message(State, Buf, String) ->
    popup_message(State, Buf, "~s", [String]).

popup_message(State, Buf, Format, Args) ->
    String = lists:flatten(io_lib:format(Format, Args)),
    case monoline(String) of
        {true, Line} ->
            edit_util:status_msg(State, Line);
        false ->
            edit_buf:new(Buf),		% might be already started, s'ok
            edit_buf:set_text(Buf, String),
            popup_buffer(State, Buf)
    end.

%% See if a string fits on one line.
%% Returns: {true, Line} | false
%% Where Line is the same as the input string, but with no newlines.
monoline(S) -> monoline(S, []).

monoline([$\n], Acc)   -> {true, lists:reverse(Acc)};
monoline([],    Acc)   -> {true, lists:reverse(Acc)};
monoline([$\n|T], Acc) -> false;
monoline([H|T], Acc)   -> monoline(T, [H|Acc]).

%% Make `Buffer' visible (but not selected) in a window - either by hijacking
%% an existing window or creating a new one.
popup_buffer(State, Buffer) ->
    case lists:any(fun(W) -> W#window.buffer == Buffer end, windows(State)) of
	true ->
	    %% already visible
	    State;
	false ->
	    popup_buffer1(State, Buffer)
    end.

popup_buffer1(State = #state{windows=[MB]}, Buffer)
  when MB#window.minibuffer == true ->
    %% Clause: One normal window, which is curwin. Split it.
    {A, B} = split_window(State#state.curwin),
    popup_buffer1(State#state{curwin=A,
			      windows=[B,MB]},
		  Buffer);
popup_buffer1(State = #state{windows=[Win]}, Buffer)
  when (State#state.curwin)#window.minibuffer == true ->
    %% Clause: One normal window, minibuffer is curwin. Split normal
    %% window.
    {A, B} = split_window(Win),
    popup_buffer1(State#state{windows=[A,B]}, Buffer);
popup_buffer1(State, Buffer) when State#state.windows > 1 ->
    %% OK! The first non-minibuffer is used.
    State#state{windows=popup_buffer_in(State#state.windows, Buffer)}.

popup_buffer_in([W|Ws], Buffer) when W#window.minibuffer == false ->
    [edit_window:attach(W, Buffer) | Ws];
popup_buffer_in([MB|Ws], Buffer) ->
    [MB | popup_buffer_in(Ws, Buffer)].

split_window(W) ->
    Height = W#window.height,
    HeightA = trunc(Height / 2),
    HeightB = Height - HeightA,
    io:format("Height = ~p~nHeightA = ~p~nHeightB = ~p~n",
	      [Height, HeightA, HeightB]),
    New = edit_window:make_window(W#window.buffer,
				  W#window.y + HeightA,
				  W#window.width,
				  HeightB),
    {W#window{height=HeightA}, New}.

%% spawn process with some borrowed buffers

spawn_with(Buffer, M, F, A) ->
    spawn_with(Buffer, {M, F, A}).

spawn_with(Buffers, What) ->
    Ref = make_ref(),
    Pid = spawn_link(?MODULE, spawn_with_init, [self(), Ref, Buffers, What]),
    receive {ready, Ref} -> ok end.

spawn_with_init(Pid, Ref, Buffers, What) ->
    lists:foreach(fun(Buf) -> edit_buf:borrow(Buf) end,
		  Buffers),
    Pid ! {ready, Ref},
    spawn_with_apply(What),
    %% we miss this redraw if the command crashes. oops.
    edit:invoke_later(?MODULE, redraw, []).

spawn_with_apply({M, F, A}) ->
    apply(M, F, A);
spawn_with_apply(Fun) when function(Fun) ->
    Fun().


redraw(State) ->
    %% Actually we needn't do anything, because the screen redraws
    %% after each command. (What a pointless abstraction :-)
    State.

%% Get the current working directory for the state or buffer.
pwd(State) when record(State, state) ->
    pwd((State#state.curwin)#window.buffer);
pwd(Buf) when atom(Buf) ->
    case edit_buf:get_filename(Buf) of
	undefined ->
	    case file:get_cwd() of
		{ok, Path} ->
		    case lists:last(Path) of
			$/ ->
			    Path;
			_ ->
			    Path ++ "/"
		    end;
		_ ->
		    undefined
	    end;
	Filename ->
	    filename:dirname(Filename)++"/"
    end.

