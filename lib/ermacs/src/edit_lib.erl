-module(edit_lib).
-author('tobbe@serc.rmit.edu.au').
%%----------------------------------------------------------------------
%% Created : 15 Jun 1998 by tobbe@serc.rmit.edu.au
%% Function: Core library routines for the Edit editor.
%%----------------------------------------------------------------------
-vc('$Id$ ').

-include_lib("ermacs/include/edit.hrl").

-compile(export_all).
-compile({parse_transform, edit_transform}).

self_insert_command(S,Ch) when record(S,state) ->
    insert_char(S,Ch).

delete_char_backward(S) ->
    B = buffer(S),
    case edit_buf:mark_pos(B, point) of
	1 ->
	    S;
	N ->
	    edit_buf:delete(B, N-1, N)
    end.

delete_char_forward(S) ->
    B = buffer(S),
    Max = edit_buf:point_max(B),
    case edit_buf:mark_pos(B, point) of
	N when N < Max ->
	    edit_buf:delete(B, N, N+1);
	_ ->
	    true
    end,
    S.

%% Note: delete_word, not kill_word. Should single words really go on
%% the kill ring, like they do in emacs?
delete_word_forward(S) ->
    Buf = buffer(S),
    Point = edit_buf:mark_pos(Buf, point),
    P1 = find_char_forward(Buf, alphanumeric_P(), Point, Point),
    P2 = find_char_forward(Buf, not_P(alphanumeric_P()), P1, P1),
    edit_buf:delete(Buf, Point, P2).

insert_char(S, Ch) ->
    Buf = buffer(S),
    edit_buf:insert(Buf, [Ch], edit_buf:mark_pos(Buf, point)).

%% "open_line" is the emacs name for what C-o does
open_line(S) ->
    Buf = buffer(S),
    OrigPoint = edit_buf:mark_pos(Buf, point),
    insert_char(S, $\n),
    edit_buf:move_mark(Buf, point, OrigPoint).

%% Abort - interpreted to abort any asynchronous command which is
%% borrowing the current buffer (or nothing if there isn't one).
abort(S) ->
    case edit_buf:revoke((S#state.curwin)#window.buffer) of
	false ->
	    S;
	true ->
	    edit_util:status_msg(S, "Buffer revoked")
    end.

%% -----------------------------------
%% Move cursor vertically down 1 line.

next_line(S) when record(S,state) ->
    Buf = buffer(S),
    NewWindow = update_goal(S, Buf),
    Goal = NewWindow#window.goal_column,
    Max = edit_buf:point_max(Buf),
    case end_of_line_pos(Buf) of
	X when X == Max ->
	    ok;
	X ->
	    F = fun(Ch) -> walk_goal(Ch, Goal, X+1) end,
	    NewP = edit_buf:walk_forward(Buf, F, X+1),
	    edit_buf:move_mark(Buf, point, NewP),
	    S#state{curwin=NewWindow}
    end.

previous_line(S) when record(S,state) ->
    Buf = buffer(S),
    NewWindow = update_goal(S, Buf),
    Goal = NewWindow#window.goal_column,
    case beginning_of_line_pos(Buf) of
	1 ->
	    move_to(Buf, 1);
	X ->
	    From = beginning_of_line_pos(Buf, X-1),
	    F = fun(Ch) -> walk_goal(Ch, Goal, From) end,
	    NewP = edit_buf:walk_forward(Buf,
					  F,
					  beginning_of_line_pos(Buf, From)),
	    edit_buf:move_mark(Buf, point, NewP),
	    S#state{curwin=NewWindow}
    end.

update_goal(State, Buf) ->
    Win = State#state.curwin,
    case goal_preserving(State#state.lastcmd) of
	true ->
	    Win;
	false ->
	    Region = edit_buf:get_region(Buf,
					 point(Buf),
					 beginning_of_line_pos(Buf)),
	    Win#window{goal_column=goal_value(Region)}
    end.

goal_value(Region) ->
    lists:foldl(fun($\t, Acc) -> Acc + 8;
		   (_,   Acc) -> Acc + 1
		end,
		0,
		Region).

goal_preserving({?MODULE, next_line, []})     -> true;
goal_preserving({?MODULE, previous_line, []}) -> true;
goal_preserving(_)                            -> false.

%% Go forward until we reach the goal column, or the end of file
walk_goal(C, Z, P) when Z =< 0 ->
    {result, P};
walk_goal(C, Remaining, P) when C == finish;
				C == $\n ->
    {result, P};
walk_goal($\t, Remaining, P) when Remaining < 8 ->
    {result, P};
walk_goal($\t, Remaining, P) ->
    {more, fun(C) -> walk_goal(C, Remaining-8, P+1) end};
walk_goal(_, Remaining, P) ->
    {more, fun(C) -> walk_goal(C, Remaining-1, P+1) end}.

point(Buf) ->
    edit_buf:mark_pos(Buf, point).

%% ----------------------------
%% Move point left 1 character.

backward_char(S) when record(S,state) ->
    Buf = buffer(S),
    Pos = edit_buf:mark_pos(Buf, point) - 1,
    case Pos < 1 of
	true ->
	    S;
	false ->
	    edit_buf:move_mark(Buf, point, Pos)
    end.

%% -----------------------------
%% Move point right 1 character.
%% 

forward_char(S) when record(S,state) ->
    Buf = buffer(S),
    Pos = edit_buf:mark_pos(Buf, point) + 1,
    Max = edit_buf:point_max(Buf),
    case Pos > Max of
	true ->
	    S;
	false ->
	    edit_buf:move_mark(Buf, point, Pos)
    end.

%% ----------------------------------------
%% Move point to beginning of current line.

beginning_of_line(S) ->
    Pos = beginning_of_line_pos(buffer(S)),
    edit_buf:move_mark(buffer(S), point, Pos),
    S.

%% ugly function
backward_word(S) ->
    Buf = buffer(S),
    Point = edit_buf:mark_pos(Buf, point),
    if Point == 1 ->
	    S;
       true ->
	    X = find_char_backward(Buf, alphanumeric_P(), Point-1, 1),
	    if X == 1 ->
		    move_to(Buf, 1),
		    S;
	       true ->
		    NewP = 1 + find_char_backward(Buf,
						  not_P(alphanumeric_P()),
						  X,
						  0),
		    move_to(Buf, NewP),
		    S
	    end
    end.

beginning_of_line_pos(Buf) ->
    Pos = edit_buf:mark_pos(Buf, point),
    beginning_of_line_pos(Buf, Pos).
beginning_of_line_pos(Buf, Pos) ->
    %% We start before the point, incase we're on a newline
    case find_char_backward(Buf, const_P($\n), Pos - 1) of
	not_found -> 1;			% point_min
	N when integer(N) -> N + 1	% we want to be just after the newline
    end.

move_to_char_backward(Buf, Pred) ->
    Pos = edit_buf:mark_pos(Buf, point),
    edit_buf:move_mark(Buf, point, find_char_backward(Buf, Pred, Pos, 1)).

%% Returns CharPos | not_found
find_char_backward(Buf, Pred) ->
    find_char_backward(Buf, Pred, edit_buf:mark_pos(Buf, point)).
find_char_backward(Buf, Pred, Pos) ->
    find_char_backward(Buf, Pred, Pos, not_found).
find_char_backward(Buf, Pred, Pos, Default) ->
    case edit_buf:walk_backward(Buf,
				 fun(C) -> find_char_backward1(C,Pred,Pos) end,
				 Pos) of
	not_found ->
	    Default;
	P when integer(P) ->
	    P
    end.

%% This clause is a kludge; see edit_buf.erl for details
find_char_backward1(not_there, Pred, Pos) ->
    {more, fun(New) -> find_char_backward1(New, Pred, Pos-1) end};
find_char_backward1(finish, Pred, Pos) ->
    {result, not_found};
find_char_backward1(C, Pred, Pos) ->
    %%edit_buf:log("Char: ~p~n", [C]),
    case Pred(C) of
	true ->
	    {result, Pos};
	false ->
	    {more, fun(New) -> find_char_backward1(New, Pred, Pos-1) end}
    end.

%% ----------------------------------
%% Move point to end of current line.

end_of_line(S) ->
    move_to_char_forward(buffer(S), fun(C) -> C == $\n end),
    S.

forward_word(S) ->
    move_to_char_forward(buffer(S), alphanumeric_P()),
    move_to_char_forward(buffer(S), not_P(alphanumeric_P())),
    S.

end_of_line_pos(Buf) ->
    Pos = edit_buf:mark_pos(Buf, point),
    end_of_line_pos(Buf, Pos).
end_of_line_pos(Buf, Pos) ->
    P = case find_char_forward(Buf, fun(C) -> C == $\n end, Pos) of
	    not_found ->
		edit_buf:point_max(Buf);
	    N when integer(N) ->
		N
	end.

move_to_char_forward(Buf, Pred) ->
    Pos = edit_buf:mark_pos(Buf, point),
    NewPos = find_char_forward(Buf, Pred, Pos, edit_buf:point_max(Buf)),
    edit_buf:move_mark(Buf, point, NewPos).

%% Returns CharPos | not_found
find_char_forward(Buf, Pred, Pos) ->
    find_char_forward(Buf, Pred, Pos, not_found).
%% Returns CharPos | Default (if not found)
find_char_forward(Buf, Pred, Pos, Default) ->
    case edit_buf:walk_forward(Buf,
			       fun(C) -> find_char_forward1(C, Pred, Pos) end,
			       Pos) of
	not_found ->
	    Default;
	P when integer(P) ->
	    P
    end.

find_char_forward1(finish, Pred, Pos) ->
    {result, not_found};
find_char_forward1(C, Pred, Pos) ->
    case Pred(C) of
	true ->
	    {result, Pos};
	false ->
	    {more, fun(New) -> find_char_forward1(New, Pred, Pos+1) end}
    end.

recenter(State) ->
    Buf = buffer(State),
    Win = State#state.curwin,
    Lines = trunc(Win#window.height/2),
    Point = edit_buf:mark_pos(Buf, point),
    NewDStart = case find_nth(Buf, backward, Point, $\n, Lines) of
		    not_found ->
			1;
		    Pos ->
			Pos + 1
		end,
    edit_buf:move_mark(Buf, Win#window.start_mark, NewDStart),
    ?EDIT_TERMINAL:invalidate(),
    State.

%% These scrolling functions are ugly

scroll_up(State) ->
    Win = State#state.curwin,
    Height = edit_window:text_lines(Win),
    Buf = buffer(State),
    DStart = edit_buf:mark_pos(Buf, Win#window.start_mark),
    NewDStart =
	case find_nth(Buf, backward, DStart, $\n, Height - 1) of
	    not_found ->
		1;
	    X when integer(X) ->
		X + 1
	end,
    DEnd = case find_nth(Buf, forward, NewDStart, $\n, Height) of
	       not_found ->
		   edit_buf:point_max(Buf);
	       N ->
		   N
	   end,
    Point = edit_buf:mark_pos(Buf, point),
    if Point > DEnd ->
	    move_to(Buf, beginning_of_line_pos(Buf, DEnd));
       true ->
	    ok
    end,
    edit_buf:move_mark(Buf, Win#window.start_mark, NewDStart),
    State.

scroll_down(State) ->
    Win = State#state.curwin,
    Buf = buffer(State),
    DStart = edit_buf:mark_pos(Buf, Win#window.start_mark),
    PMax = edit_buf:point_max(Buf),
    case find_nth(Buf, forward, DStart, $\n, edit_window:text_lines(Win)-2) of
	X when integer(X),
	       X < PMax ->
	    %% We want to be just after the newline - i.e. start of next line
	    Pos = X + 1,
	    Point = edit_buf:mark_pos(Buf, point),
	    if Point < Pos ->
		    %% Scrolled the point off the screen, move the
		    %% point to the old display start (up near the
		    %% top).
		    edit_buf:move_mark(Buf, point, Pos);
	       true ->
		    ok
	    end,
	    edit_buf:move_mark(Buf, Win#window.start_mark, Pos),
	    State;
	_ ->
	    State
    end.

scroll_down_wrap(Win) ->
    Buf = Win#window.buffer,
    DStart = edit_buf:mark_pos(Buf, Win#window.start_mark),
    PMax = edit_buf:point_max(Buf),
    Lines = edit_window:text_lines(Win),
    Pos = case find_nth(Buf, forward, DStart, $\n, Lines) of
	      X when integer(X),
		     X < PMax ->
		  %% We want to be just after the newline - i.e. start
		  %% of next line
		  X + 1;
	      _ ->
		  1
	  end,
    edit_buf:move_mark(Buf, Win#window.start_mark, Pos),
    edit_buf:move_mark(Buf, point, Pos),
    Win.

start_of_buffer(State) ->
    B = buffer(State),
    edit_buf:move_mark(B, mark, edit_buf:mark_pos(B, point)),
    edit_buf:move_mark(B, point, 1),
    State.

end_of_buffer(State) ->
    B = buffer(State),
    edit_buf:move_mark(B, mark, edit_buf:mark_pos(B, point)),
    edit_buf:move_mark(B, point, edit_buf:point_max(B)),
    State.

mark_whole_buffer(State) ->
    Buf = buffer(State),
    edit_buf:move_mark(Buf, point, 1),
    edit_buf:move_mark(Buf, mark, edit_buf:point_max(Buf)),
    State.

find_nth(Buf, Direction, Pos, Char, N) ->
    case edit_buf:walk(Buf, Direction, find_nth_fun(Char, N), Pos) of
	not_found ->
	    not_found;
	Offset when Direction == forward ->
	    Pos + Offset;
	Offset when Direction == backward ->
	    Pos - Offset
    end.

find_nth_fun(Char, N) -> find_nth_fun(Char, N, 0).
find_nth_fun(Char, N, Offset) -> find_nth_fun(Char, N, Offset, not_found).

find_nth_fun(Char, N, Offset, Default) ->
    fun(X) when X == Char ->
	    case N of
		1 ->
		    {result, Offset};
		_ ->
		    {more, find_nth_fun(Char, N-1, Offset+1, Default)}
	    end;
       (finish) ->
	    {result, Default};
       (_) ->
	    {more, find_nth_fun(Char, N, Offset+1, Default)}
    end.

set_mark(State) ->
    Buf = buffer(State),
    Point = edit_buf:mark_pos(Buf, point),
    edit_buf:move_mark(Buf, mark, Point),
    State.

exchange_point_and_mark(State) ->
    Buf = buffer(State),
    Point = edit_buf:mark_pos(Buf, point),
    Mark = edit_buf:mark_pos(Buf, mark),
    edit_buf:move_mark(Buf, point, Mark),
    edit_buf:move_mark(Buf, mark, Point),
    State.

kill_region(State, KillFlag) ->
    Buf = buffer(State),
    Point = edit_buf:mark_pos(Buf, point),
    Mark = edit_buf:mark_pos(Buf, mark),
    Text = edit_buf:get_region_cord(Buf, Point, Mark),
    State1 = case KillFlag of
		 true ->
		     edit_buf:delete(Buf, Point, Mark),
		     State;
		 false ->
		     edit_util:status_msg(State, "Copied")
	     end,
    KR = edit_var:lookup(killring),
    edit_var:set(killring, [Text|KR]).
%%State1#state{killring=[Text|State#state.killring]}.

kill_line(State) ->
    Buf = buffer(State),
    P = edit_buf:mark_pos(Buf, point),
    EOL = end_of_line_pos(Buf),
    End = if P == EOL ->
		  min(P + 1, edit_buf:point_max(Buf));
	     true ->
		  EOL
	  end,
    Text = edit_buf:get_region_cord(Buf, P, End),
    edit_buf:delete(Buf, P, End),
    %%OldKillring = State#state.killring,
    OldKillring = edit_var:lookup(killring),
    NewKillring = case State#state.lastcmd of
		      %% Multiple kill_line's accumulate at the front
		      %% of the kill ring
		      {?MODULE, kill_line, _} ->
			  [cord:join(hd(OldKillring), Text) |
			   tl(OldKillring)];
		      _ ->
			  [Text|OldKillring]
		  end,
    edit_var:set(killring, NewKillring).
    %%State#state{killring=NewKillring}.

yank(State) ->
    case edit_var:lookup(killring) of
	[] ->
	    State;
	[Text|_] ->
	    Buf = buffer(State),
	    Point = edit_buf:mark_pos(Buf, point),
	    edit_buf:insert(Buf, Text, Point)
    end.

undo(State) ->
    Continuing = case State#state.lastcmd of
		     {?MODULE, undo, []} ->
			 true;
		     _ ->
			 false
		 end,
    edit_buf:undo(buffer(State), Continuing).

move_to(Buf, Pos) ->
    edit_buf:move_mark(Buf, point, Pos).

next_window(State) when State#state.windows == [] ->
    State;
next_window(State) ->
    next_window1(State).

next_window1(State) ->
    [Next | Rest] = State#state.windows,
    NewState = State#state{curwin=Next,
			   windows = Rest ++ [State#state.curwin]},
    if (NewState#state.curwin)#window.active == false ->
	    next_window1(NewState);
       true ->
	    NewState
    end.

split_window_vertically(State) ->
    case edit_window:physical_lines(State#state.curwin) of
	N when N < 4 ->			% too small to split
	    State;
	N ->
	    split_window_vertically1(State)
    end.

split_window_vertically1(State) ->
    Orig = State#state.curwin,
    OrigHeight = edit_window:physical_lines(Orig),
    TopHeight = trunc(OrigHeight / 2),
    BottomHeight = OrigHeight - TopHeight,
    BottomY = Orig#window.y + TopHeight,
    BottomW = edit_window:make_window(buffer(State),
				      BottomY,
				      Orig#window.width,
				      BottomHeight),
    State#state{curwin=Orig#window{height=TopHeight},
		windows=[BottomW | State#state.windows]}.

delete_other_windows(State)
  when (State#state.curwin)#window.minibuffer == true ->
    edit_util:status_msg(State, "Minibuffer can't expand to full frame");
delete_other_windows(State) ->
    Others = lists:filter(fun(W) -> not W#window.minibuffer end,
			  State#state.windows),
    ExtraSpace = lists:foldl(fun(W, Sum) -> Sum + W#window.height end,
			     0,
			     Others),
    [Minibuffer] = State#state.windows -- Others,
    OldWin = State#state.curwin,
    NewWin = OldWin#window{y=0, height = OldWin#window.height + ExtraSpace},
    State#state{curwin=NewWin,
		windows=[Minibuffer]}.

delete_window(State) when (State#state.curwin)#window.minibuffer == true ->
    State;
delete_window(State) when length(State#state.windows) =< 1 ->
    State;
delete_window(State) ->
    Cur = State#state.curwin,
    Wins = State#state.windows,
    [Prev | OthersReversed] = lists:reverse(Wins),
    Others = lists:reverse(OthersReversed),
    io:format("Prev.y = ~p~nCur.y  = ~p~n", [Prev#window.y, Cur#window.y]),
    if Prev#window.y > Cur#window.y ->
	    %% Topmost window is being deleted. In this special case,
	    %% its size is allocated to the next (rather than
	    %% previous, i.e. bottom) window.
	    Kin = hd(Wins),
	    Kin1 = Kin#window{height=Kin#window.height + Cur#window.height,
			      y=Cur#window.y},
	    State#state{curwin=Kin1,
			windows=tl(Wins)};
       true ->
	    Kin = Prev,
	    Kin1 = Kin#window{height=Kin#window.height + Cur#window.height},
	    Others1 = Others ++ [Kin1],
	    case Others1 of
		[Next = #window{minibuffer=false} | Rest] ->
		    State#state{curwin=Next,
				windows=Rest};
		[MB = #window{minibuffer=true}, Next | Rest] ->
		    State#state{curwin=Next,
				windows=(Rest ++ [MB])}
	    end
    end.

-command({quit, [], "Exit the editor process"}).
quit(State) ->
    ?EDIT_TERMINAL:teardown(),
    halt().

-command({printf, [{string, "String:"}],
	  "Print a string to standard output (the file edit.out)"}).

printf(State, String) ->
    io:format("*** PRINTF: ~s~n", [String]),
    State.

-command({switch_to_buffer, [{string, "Switch to buffer:"}]}).

switch_to_buffer(State, "") ->
    VisibleBuffers = [W#window.buffer || W <- edit_util:windows(State)],
    case State#state.buffers -- VisibleBuffers of
	[] ->
	    State;
	[Next|_] ->
	    switch_to_buffer(State, atom_to_list(Next))
    end;
switch_to_buffer(State, Name) ->
    Buffer = list_to_atom(Name),
    case whereis(Buffer) of		% FIXME: how do I know its a buffer?
	undefined ->
	    edit_util:status_msg(State, "No such buffer: ~s", [Name]);
	Pid ->
	    Buffers = State#state.buffers,
	    NewBuffers = [Buffer | (Buffers -- [Buffer])],
	    Win = edit_window:attach(State#state.curwin, Buffer),
	    State#state{curwin=Win, buffers=NewBuffers}
    end.

-command({kill_buffer, [{string, "Kill buffer:"}]}).

kill_buffer(State, "") ->
    kill_buffer(State, atom_to_list((State#state.curwin)#window.buffer));
kill_buffer(State, Name) ->
    BufferName = list_to_atom(Name),
    case State#state.buffers of
	[OnlyBuffer] ->
	    State;
	Buffers ->
            case whereis(BufferName) of
                Pid when pid(Pid) ->
                    edit_buf:kill(BufferName),
		    NewBuffers = Buffers -- [BufferName],
		    F = fun(Win) ->
				case Win#window.buffer of
				    BufferName ->
					edit_window:attach(Win,
							   hd(NewBuffers));
				    _ ->
					Win
				end
			end,
		    State1 = edit_util:window_map(State, F),
		    io:format("Killing ~p~n", [BufferName]),
		    State1#state{buffers=NewBuffers};
		undefined ->
		    edit_util:status_msg(State, "No such buffer: ~s", [Name])
	    end
    end.

min(X,Y) when X<Y -> X;
min(_,Y)          -> Y.

max(X,Y) when X>Y -> X;
max(_,Y)          -> Y.

%% Get the buffer from state - blocks if it's being borrowed by someone else.
buffer(State) ->
    Buf = (State#state.curwin)#window.buffer,
    edit_buf:wait_available(Buf),
    Buf.

find(Pred, []) ->
    not_found;
find(Pred, [H|T]) ->
    case Pred(H) of
	true ->
	    H;
	false ->
	    find(Pred, T)
    end.

%% FIXME: Expects the *end* of the string to be before the point
find_string_backward(Buf, String) ->
    find_string_backward(Buf, String, edit_buf:mark_pos(Buf, point)).
find_string_backward(Buf, String, Point) ->
    find_string_backward(Buf, String, Point, not_found).
find_string_backward(Buf, String, Point, Default) ->
    find_string_backward1(Buf, String, Point, Default).

find_string_backward1(Buf, String, Point, Default) ->
    PointMax = edit_buf:point_max(Buf),
    P = fun(C) -> C == hd(String) end,
    case min(Point, PointMax - length(String)) of
	Start when Start =< 0 ->
	    Default;
	Start ->
	    case find_char_backward(Buf, P, Start) of
		not_found ->
		    Default;
		Pos ->
		    case edit_buf:get_region(Buf, Pos, Pos + length(String)) of
			String ->
			    Pos;
			_Other when Pos == 1 ->
			    Default;
			_Other ->
			    find_string_backward1(Buf, String, Pos-1, Default)
		    end
	    end
    end.

%% Predicate functions on characters.
%% Functions ending in _P return predicate fun's.
whitespace_P()     -> fun(C) -> lists:member(C, "\n\t ") end.
alphabet_P()       -> fun(C) when C >= $A, C =< $Z -> true;
			 (C) when C >= $a, C =< $z -> true;
			 (_)                       -> false
		      end.
numeric_P()        -> fun(C) -> (C >= $0) and (C =< $9) end.
not_whitespace_P() -> not_P(whitespace_P()).

either_P(A, B)     -> fun(C) -> A(C) or B(C) end.
const_P(Char)      -> fun(C) -> C == Char end.
not_P(Pred)        -> fun(C) -> not Pred(C) end.

alphanumeric_P()   -> either_P(alphabet_P(), numeric_P()).

fundamental_mode(State) ->
    Buf = buffer(State),
    edit_buf:set_mode(Buf, fundamental_mode_rec()).

fundamental_mode_rec() ->
    #mode{name="Fundamental",
	  id=fundamental,
	  keymaps=[]}.

-command({unix_command, [{shell_command, "Shell command:"}]}).

unix_command(State, Cmd) ->
    %% FIXME: This looks suspiciously system dependent.
    %% Infact running the command as a port would be much nicer
    Text = os:cmd("sh -c '(cd "++edit_util:pwd(State)++" ; "++Cmd++")'"),
    edit_util:popup_message(State, '*Shell Command Output*', Text).

lines(Text) ->
    length(lists:filter(fun(C) -> C == $\n end, Text)).

-command({search, [{search_string, "Search:"}]}).

search(State, Dir, "") ->
    Str = edit_var:lookup(prev_search_string, ""),
    regexp_search1(State, Dir, cord_regexp:escape(Str));
search(State, Dir, Str) ->
    edit_var:set(prev_search_string, Str),
    regexp_search1(State, Dir, cord_regexp:escape(Str)).

-command({regexp_search, [{search_regexp, "Regexp:"}]}).

regexp_search(State, Dir, "") ->
    %% Blank search string - reuse previous
    RE = edit_var:lookup(prev_search_regexp, ""),
    regexp_search1(State, Dir, RE);
regexp_search(State, Dir, RE) ->
    edit_var:set(prev_search_regexp, RE),
    regexp_search1(State, Dir, RE).

regexp_search1(State, Direction, RE) ->
    B = buffer(State),
    P = edit_buf:mark_pos(B, point),
    case edit_buf:regexp_search(B, RE, clip(B, P), Direction) of
        nomatch ->
            edit_util:status_msg(State, "Not found: " ++ RE);
        {match, Start, Len} ->
            NewPos = case Direction of
                         forward ->
                             Start + Len;
                         backward ->
                             Start - (Len - 1)
                     end,
            ?debug("Start = ~p Len = ~p~n", [Start, Len]),
            mark_to_point(B),
            edit_buf:move_mark(B, point, NewPos)
    end.

%% Returns: Point' < point_max
clip(Buf, Point) ->
    max(1, min(Point, edit_buf:point_max(Buf) - 1)).

mark_to_point(Buf) ->
    edit_buf:move_mark(Buf, mark, edit_buf:mark_pos(Buf, point)).

%% Null command. Invoked by async processes to get the editor to
%% redraw.
nop(State) ->
    State.

%% ----------------------------------------------------------------------
%% Debug info

buffer_cord_info(State) ->
    B = buffer(State),
    C = edit_buf:get_cord(B),
    Fmt = "Byte size: ~p ; Max depth: ~p ; Mean leaf size: ~p ; # nodes: ~p",
    edit_util:status_msg(State,
			 Fmt,
			 [cord:cord_size(C),
			  cord:max_depth(C),
			  cord:mean_leaf_size(C),
			  cord:nr_nodes(C)]).

