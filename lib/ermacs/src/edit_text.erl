%%%----------------------------------------------------------------------
%%% File    : edit_text.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Text data structure with markers and undo
%%% Created :  2 Oct 2001 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(edit_text).
-author('luke@bluetail.com').

-export([new/0, new/1, replace/4, add_mark/4, move_mark/3, mark_pos/2,
	 undo/2, cord/1, walk_backward/3, walk_forward/3]).

-record(text, {cord, marks=[], undo=[], running_undo=[]}).

-record(mark,  {name, pos, direction}).

%% ----------------------------------------------------------------------
%% new

new() ->
    new(<<>>).

new(BCS) when list(BCS) ->
    new(cord:new(BCS));
new(Cord) ->
    #text{cord=Cord}.

%% ----------------------------------------------------------------------
%% replace (CBS means Cord, Binary, or String)

replace(Text0 = #text{cord=Cord0,marks=Marks0,undo=Undo0}, CBS, Start, Len) ->
    Cord1 = cord:replace(Cord0, CBS, Start, Len),
    Marks1 = update_marks(Marks0, Start, Start+Len, cbs_length(CBS)),
    Undo1 = [{Cord0, Marks0} | Undo0],
    %% FIXME: maybe better to chain undo through states (i.e. just
    %% know about previous state, which knows about previous state,
    %% ...) - if that plays nicely with "running undo"
    Text0#text{cord=Cord1, marks=Marks1, undo=Undo1, running_undo=[]}.

cbs_length(L) when list(L)   -> length(L);
cbs_length(B) when binary(B) -> size(B);
cbs_length(C)                -> cord:cord_size(C).

update_marks(Marks, Start, End, Len) ->
    [update_mark(Mark,Start,End,Len) || Mark <- Marks].

%% Inside the replaced region (includes being on an inserted character)
update_mark(Mark, Start, End, Len)
  when Mark#mark.pos >= Start,
       Mark#mark.pos < End;
       Mark#mark.pos == Start ->
    case Mark#mark.direction of
	forward ->
	    Mark#mark{pos=Start+Len};
	backward ->
	    Mark#mark{pos=Start}
    end;
%% After the replaced region
update_mark(Mark, Start, End, Len) when Mark#mark.pos >= End ->
    %% We move forward by the length-delta
    Mark#mark{pos = Mark#mark.pos - (End - Start) + Len};
%% Before the replaced region
update_mark(Mark, _Start, _End, _Len) ->
    Mark.

%% ----------------------------------------------------------------------
%% add_mark

add_mark(Text = #text{marks=Marks0}, Name, Pos, Direction) ->
    Mark = #mark{name=Name, pos=Pos, direction=Direction},
    Text#text{marks=[Mark|Marks0]}.

%% ----------------------------------------------------------------------
%% move_mark

move_mark(Text0, Name, Pos) ->
    {Front, [M | Back]} =
	lists:splitwith(fun(X) -> X#mark.name /= Name end,
			Text0#text.marks),
    NewMarks = Front ++ [M#mark{pos=Pos} | Back],
    Text0#text{marks=NewMarks}.

%% ----------------------------------------------------------------------
%% mark_pos

mark_pos(#text{marks=Marks}, Name) ->
    {found, Mark} = find(fun(M) -> M#mark.name == Name end, Marks),
    Mark#mark.pos.

find(Pred, []) ->
    not_found;
find(Pred, [H|T]) ->
    case Pred(H) of
	true ->
	    {found, H};
	false ->
	    find(Pred, T)
    end.

%% ----------------------------------------------------------------------
%% walk_backward

walk_backward(Text = #text{cord=Cord}, Fun, Pos) ->
    Size = cord:cord_size(Cord),
    if Pos == Size + 1 ->
	    case Fun(not_there) of
		{more, FNext} ->
		    walk_backward(Text, FNext, Pos-1);
		{result, R} ->
		    R
	    end;
       true ->
	    cord:walk(Cord, Pos, backward, Fun)
    end.

%% ----------------------------------------------------------------------
%% walk_forward

walk_forward(#text{cord=Cord}, Fun, Pos) ->
    cord:walk(Cord, Pos, forward, Fun).

%% ----------------------------------------------------------------------
%% undo

undo(Text0 = #text{undo=Undo, running_undo=Running}, Continuing) ->
    Text1 = case Continuing of
		true ->
		    Text0;
		false ->
		    Text0#text{running_undo=Undo}
	    end,
    case Running of
	%% Nothing to undo - recycle redo list
	[] ->
	    Text1#text{running_undo=Undo};
	[{UndoCord, UndoMarks}|Undos] ->
	    Text1#text{cord=UndoCord,
		       marks=UndoMarks,
		       undo=[{Text0#text.cord, Text0#text.marks} | Undo],
		       running_undo=Undos}
    end.

%% ----------------------------------------------------------------------
%% cord

cord(#text{cord=Cord}) -> Cord.

