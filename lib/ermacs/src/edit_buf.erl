%%%----------------------------------------------------------------------
%%% File    : edit_buf.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Buffer process
%%% Created : 14 Sep 2000 by Luke Gorrie <luke@bluetail.com>
%%%----------------------------------------------------------------------

-module(edit_buf).
-author('luke@bluetail.com').

-compile(export_all).
%%-export([Function/Arity, ...]).

-record(state, {name,
		filename,		% optional
		text,			% text()
		mode,
		borrower=nobody		% for a lock pid()
	       }).

%% ----------------------------------------------------------------------
%% API
%% ----------------------------------------------------------------------

new(Name) ->
    case start_link(Name) of
	{ok, B} ->
	    add_mark(Name, point, 1, forward),
	    add_mark(Name, mark, 1, backward),
	    {ok, Name};
	Err ->
	    Err
    end.

start_link(Name) when atom(Name) ->
    case whereis(Name) of
	undefined ->
	    Pid = proc_lib:spawn_link(?MODULE, init, [Name]),
	    register(Name, Pid),
	    {ok, Pid};
	Pid ->
	    {error, {already_started, Pid}}
    end.

kill(Buf) ->
    call(Buf, kill).

set_filename(Buf, Filename) ->
    call(Buf, {set_filename, Filename}).

get_filename(Buf) ->
    call(Buf, get_filename).

set_mode(Buf, Mode) ->
    call(Buf, {set_mode, Mode}).

get_mode(Buf) ->
    call(Buf, get_mode).

get_text(Buf) ->
    call(Buf, get_text).

set_text(Buf, Text) ->
    call(Buf, {set_text, Text}).

get_cord(Buf) ->
    call(Buf, get_cord).

get_size(Buf) ->
    call(Buf, get_size).

replace(Buf, New, Start, End) when Start > End ->
    replace(Buf, New, End, Start);
replace(Buf, New, Start, End) ->
    call(Buf, {replace, New, Start, End}).

get_region(Buf, Start, End) ->
    cord:to_list(get_region_cord(Buf, Start, End)).

get_region_cord(Buf, Start, End) when Start > End ->
    get_region_cord(Buf, End, Start);
get_region_cord(Buf, Start, End) ->
    call(Buf, {get_region_cord, Start, End}).

undo(Buf, Continuing) ->
    call(Buf, {undo, Continuing}).

delete(Buf, Start, End) ->
    replace(Buf, "", Start, End).

insert(Buf, New, Position) ->
    replace(Buf, New, Position, Position).

add_mark(Buf, Name, Pos, Direction)  ->
    call(Buf, {add_mark, Name, Pos, Direction}).

move_mark(Buf, Name, Pos) ->
    call(Buf, {move_mark, Name, Pos}).

mark_pos(Buf, Name) ->
    call(Buf, {mark_pos, Name}).

point_max(Buf) ->
    call(Buf, point_max).

point_min(Buf) ->
    1.

walk_backward(Buf, Fun, Pos) ->
    call(Buf, {walk_backward, Fun, Pos}).

walk_forward(Buf, Fun, Pos) ->
    call(Buf, {walk_forward, Fun, Pos}).

walk(Buf, Direction, Fun, Pos) ->
    call(Buf, {walk, Direction, Fun, Pos}).

wait_available(Buf) ->
    call(Buf, wait_available).

borrow(Buf) ->
    call(Buf, {borrow, self()}).

return(Buf) ->
    call(Buf, {return, self()}).

revoke(Buf) ->
    call(Buf, revoke).

regexp_search(Buf, RE, Pos, Direction) ->
    call(Buf, {regexp_search, RE, Pos, Direction}).

%% Asynchronous locking

%% Returns: ref()
async_borrow(Buf) ->
    Ref = make_ref(),
    cast(Buf, {borrow, self()}).

async_return(Buf) ->
    cast(Buf, {return, self()}).

%% Internals

call(Buf, Msg) ->
    Buf ! {call, self(), Msg},
    receive {reply, R} -> R end.

cast(Buf, Msg) ->
    Buf ! {cast, Msg},
    ok.

init(Name) ->
    {'EXIT', Reason} = (loop(#state{name=Name,
					  mode=edit_lib:fundamental_mode_rec(),
					  text=edit_text:new()})),
    io:format("Buffer ~p crashed: ~p~n", [Name, Reason]),
    exit(Reason).

loop(State) ->
    receive
	%% get_text
	{call, From, get_text} ->
	    From ! {reply, cord:to_list(edit_text:cord(State#state.text))},
	    edit_buf:loop(State);
	%% set_text
	{call, From, {set_text, Text}} ->
	    Cord = edit_text:cord(State#state.text),
	    NewCmd = {replace, Text, 1, cord:cord_size(Cord) + 1},
	    self() ! {call, From, NewCmd},
	    edit_buf:loop(State);
	%% get_cord
	{call, From, get_cord} ->
	    From ! {reply, edit_text:cord(State#state.text)},
	    edit_buf:loop(State);
	%% get_size
	{call, From, get_size} ->
	    From ! {reply, cord:cord_size(edit_text:cord(State#state.text))},
	    edit_buf:loop(State);
	%% set_filename
	{call, From, {set_filename, Filename}} ->
	    NewState = State#state{filename=Filename},
	    From ! {reply, ok},
	    edit_buf:loop(NewState);
	%% get_filename
	{call, From, get_filename} ->
	    From ! {reply, State#state.filename},
	    edit_buf:loop(State);
	%% set_mode
	{call, From, {set_mode, Mode}} ->
	    From ! {reply, ok},
	    edit_buf:loop(State#state{mode=Mode});
	%% get_mode
	{call, From, get_mode} ->
	    From ! {reply, State#state.mode},
	    edit_buf:loop(State);
	%% replace
	{call, From, {replace, Text, Start, End}} ->
	    NewText = edit_text:replace(State#state.text,Text,Start,End-Start),
	    From ! {reply, ok},
	    edit_buf:loop(State#state{text=NewText});

% 	    NewText = cord:replace(State#state.text, Text, Start, End-Start),
% 	    Marks1 = update_marks(State#state.marks, Text, Start, End),
% 	    State2 = State#state{text=NewText, marks=Marks1},
% 	    Undo = [{State#state.text, State#state.marks}|State2#state.undo],
% 	    State3 = State2#state{undo=Undo},
% 	    From ! {reply, ok},
% 	    edit_buf:loop(State3);
	%% undo
	{call, From, {undo, Continuing}} ->
	    NewText = edit_text:undo(State#state.text, Continuing),
	    From ! {reply, ok},
	    edit_buf:loop(State#state{text=NewText});

% 	    NewState = do_undo(State, Continuing),
% 	    From ! {reply, ok},
% 	    edit_buf:loop(NewState);
	%% get_region
	{call, From, {get_region_cord, Start, End}} ->
	    Cord = edit_text:cord(State#state.text),
	    Text = cord:region(Cord, Start, End-Start),
	    From ! {reply, Text},
	    edit_buf:loop(State);
	%% add_mark
	{call, From, {add_mark, Name, Pos, Direction}} ->
	    NewText = edit_text:add_mark(State#state.text,Name,Pos,Direction),
	    From ! {reply, ok},
	    edit_buf:loop(State#state{text=NewText});
	%% move_mark
	{call, From, {move_mark, Name, Pos}} ->
	    NewText = edit_text:move_mark(State#state.text, Name, Pos),
	    From ! {reply, ok},
	    edit_buf:loop(State#state{text=NewText});
	%% mark_pos
	{call, From, {mark_pos, Name}} ->
	    From ! {reply, edit_text:mark_pos(State#state.text, Name)},
	    edit_buf:loop(State);
	%% point_max
	{call, From, point_max} ->
	    Cord = edit_text:cord(State#state.text),
	    From ! {reply, cord:cord_size(Cord) + 1},
	    edit_buf:loop(State);
	%% walk_backward
	{call, From, {walk_backward, Fun, Pos}} ->
	    From ! {reply, edit_text:walk_backward(State#state.text,Fun,Pos)},
	    edit_buf:loop(State);
	%% walk_forward
	{call, From, {walk_forward, Fun, Pos}} ->
	    From ! {reply, edit_text:walk_forward(State#state.text,Fun,Pos)},
	    edit_buf:loop(State);
	%% walk
	{call, From, {walk, Direction, Fun, Pos}} ->
	    Result = case Direction of
			 forward ->
			     edit_text:walk_forward(State#state.text,Fun,Pos);
			 backward ->
			     edit_text:walk_backward(State#state.text,Fun,Pos)
		     end,
	    From ! {reply, Result},
	    edit_buf:loop(State);
	%% wait_available
	{call, From, wait_available} when State#state.borrower == nobody ->
	    From ! {reply, true},
	    edit_buf:loop(State);
	%% borrow
	{call, From, {borrow, Borrower}} when State#state.borrower == nobody ->
	    From ! {reply, true},
            io:format("Borrowing buffer for ~p at ~s~n", [Borrower,
                                                          os:cmd("date")]),
	    %% monitor the borrower so we know when to release
	    erlang:monitor(process, Borrower),
	    edit_buf:loop(State#state{borrower=Borrower});
	%% return
	{call, From, {return, Who}} when Who == State#state.borrower ->
	    edit_buf:loop(State#state{borrower=nobody});
	{'DOWN', _, process, Who, _} when Who == State#state.borrower ->
	    edit_buf:loop(State#state{borrower=nobody});
	%% revoke
	{call, From, revoke} ->
	    case State#state.borrower of
		nobody ->
		    From ! {reply, false},
		    true;
		Who ->
		    From ! {reply, true},
		    exit(Who, buffer_revoked)
	    end,
	    edit_buf:loop(State#state{borrower=nobody});
        %% regexp_search
        {call, From, {regexp_search, RE, Pos, Direction}} ->
	    Cord = edit_text:cord(State#state.text),
            From ! {reply, cord_regexp:first_match(RE, Cord, Pos, Direction)},
            edit_buf:loop(State);
	%% kill
	{call, From, kill} ->
	    From ! {reply, ok};
	%% ---------------------------------------------------------------
	%% Casts

	%% {borrow, Who} --> {loan, Buffer}
	{cast, {borrow, Who}} when State#state.borrower == nobody ->
	    Who ! {loan, State#state.name},
	    erlang:monitor(process, Who),
	    loop(State#state{borrower=Who});
	%% {return, ByWho} --> void
	{cast, {return, Who}} when Who == State#state.borrower ->
	    loop(State#state{borrower=nobody})
    end.

%% Debug

log(FmtString, Args) ->
    log(io_lib:format(FmtString, Args)).

log(String) ->
    {ok, Fd} = file:open("edit_buf.log", [append, raw]),
    file:write(Fd, String),
    file:close(Fd).

