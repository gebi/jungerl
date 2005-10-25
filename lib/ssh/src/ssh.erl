%%% File    : ssh.erl
%%% Author  : Tony Rogvall <tony@bulldog>
%%% Description : ssh client
%%% Created : 20 Aug 2004 by Tony Rogvall <tony@bulldog>

-module(ssh).

-vsn("$Revision$ ").

-rcsid("$Id$\n").

-compile(export_all).
-export([server/5]).

-import(lists, [reverse/1, duplicate/2]).

-include("../include/ssh.hrl").
-include("../include/ssh_connect.hrl").


connect(CM) ->
    case ssh_cm:attach(CM) of
	{ok,CMPid} ->
	    session(CMPid);
	Error ->
	    Error
    end.

connect(Host, Opts) ->
    connect(Host, 22, Opts).

connect(Host,Port,Opts) ->
    case ssh_cm:start(undefined, Host, Port, Opts) of
	{ok, CM} ->
	    session(CM);
	Error ->
	    Error
    end.

session(CM) ->
    case ssh_cm:session_open(CM) of
	{ok,Channel}  ->
	    case ssh_cm:shell(CM, Channel) of
		ok ->
		    {group_leader,GIO} = 
			process_info(self(), group_leader),
		    IO = spawn(?MODULE, input_loop, [GIO,self()]),
		    shell_loop(CM, Channel, IO, false);
		Error  ->
		    ssh_cm:close(CM, Channel),
		    Error
	    end;
	Error ->
	    Error
    end.


input_loop(Fd, Pid) ->
    case io:get_line(Fd, '>') of
	eof ->
	    Pid ! {input, self(), eof},
	    input_loop(Fd, Pid);
	Line ->
	    Pid ! {input, self(), Line},
	    input_loop(Fd, Pid)
    end.
    

shell_loop(CM, Channel, IO, SentClose) ->
    receive
	{input, IO, eof} ->
	    ssh_cm:send_eof(CM, Channel),
	    ?MODULE:shell_loop(CM, Channel, IO, SentClose);
	    
	{input, IO, Line} ->
	    ssh_cm:send(CM, Channel, Line),
	    ?MODULE:shell_loop(CM, Channel, IO, SentClose);

	{ssh_cm, CM, {data, Channel, Type, Data}} ->
	    if Type == 0 ->
		    io:format("~s", [binary_to_list(Data)]);
	       Type == ?SSH_EXTENDED_DATA_STDERR ->
		    io:format("STDERR: ~s", [binary_to_list(Data)]);
	       true ->
		    ok
	    end,
	    ssh_cm:adjust_window(CM, Channel, size(Data)),
	    ?MODULE:shell_loop(CM, Channel, IO, SentClose);

	{ssh_cm, CM, {exit_signal,Channel,SIG, Err, _Lang}} ->
	    io:format("SIGNAL: ~s (~s)\n", [SIG, Err]),
	    send_close(SentClose, CM, Channel),
	    shell_loop(CM, Channel, IO, true);

	{ssh_cm, CM, {exit_status,Channel,_Status}} ->
	    send_close(SentClose, CM, Channel),
	    shell_loop(CM, Channel, IO, true);

	{ssh_cm, CM, {eof, Channel}} ->
	    send_close(SentClose, CM, Channel),
	    ?MODULE:shell_loop(CM, Channel, IO, true);

	{ssh_cm, CM, {closed, Channel}} ->
	    io:format("Closed: Channel=~p\n",[Channel]),
	    ssh_cm:detach(CM),
	    exit(IO, kill)
    end.

send_close(false, CM, Channel) ->
    ssh_cm:close(CM, Channel);
send_close(_, _, _) ->
    ok.
%%
%% SSH server part
%%
-define(EMPTY_LINE, {[],[]}).

-define(PUT_CHARS, 0).
-define(MOV_REL,   1).
-define(INS_CHARS, 2).
-define(DEL_CHARS, 3).
-define(BEEP,      4).

-define(DEL, 127).
-define(ESC, 27).
-define(NL,  $\n).
-define(CR,  $\r).
-define(BS,  $\b).
-define(SP,  $\s).
-define(TAB, $\t).
-define(LSQ, $\[).


server(SSH, CM, Channel,Pty,user) ->
    User = spawn(user, server, [self()]),
    server_loop(SSH, CM, Channel, Pty,?EMPTY_LINE, User);
server(SSH, CM, Channel,Pty,user_drv) ->
    User = spawn(user_drv, server, [self(),{shell,start,[init]}]),
    server_loop(SSH, CM, Channel,Pty, ?EMPTY_LINE, User).


server_loop(SSH, CM, Channel, Pty, Line, User) ->
    receive
	{User, {command,[?PUT_CHARS|Data]}} ->
	    Cs = if binary(Data) -> binary_to_list(Data);
		    true -> Data
		 end,
	    Line1 = input_chars(Cs, Line, CM, Channel, Pty),
	    ?MODULE:server_loop(SSH, CM, Channel, Pty, Line1, User);

	{User, {command,[?MOV_REL,NH,NL]}} ->
	    <<Rel:16/big-signed>> = <<NH,NL>>,
	    Line1 = move_relative(Rel, Line, CM, Channel, Pty),
	    ?MODULE:server_loop(SSH, CM, Channel, Pty, Line1, User);

	{User, {command,[?INS_CHARS|Cs]}} ->
	    Line1 = input_chars(Cs, Line, CM, Channel, Pty),	    
	    ?MODULE:server_loop(SSH, CM, Channel, Pty, Line1, User);

	{User, {command,[?DEL_CHARS,NH,NL]}} ->
	    <<N:16/big-signed>> = <<NH,NL>>,
	    Line1 = delete_chars(N, Line, CM, Channel, Pty),	    
	    ?MODULE:server_loop(SSH, CM, Channel, Pty, Line1, User);
	
	{User, {command,[?BEEP]}} ->
	    ssh_cm:send(CM, Channel, [7]),
	    ?MODULE:server_loop(SSH, CM, Channel, Pty, Line, User);
	    
	{User, {command,Data}} ->
	    ssh_cm:send(CM, Channel, Data),
	    ?MODULE:server_loop(SSH, CM, Channel, Pty, Line, User);

	{ssh_cm, CM, {exit_signal,Channel,_SIG, _Err, _Lang}} ->
	    send_close(false, CM, Channel),
	    exit;

	{ssh_cm, CM, {eof, Channel}} ->
	    User ! {self(),eof},
	    ok;

	{ssh_cm, CM, {closed, Channel}} ->
	    User ! {self(),eof},
	    ssh_cm:detach(CM),
	    closed;

	{ssh_cm, CM, {data, Channel, _Type, Data}} ->
	    User ! {self(),{data,binary_to_list(Data)}},
	    ssh_cm:adjust_window(CM, Channel, size(Data)),
	    ?MODULE:server_loop(SSH, CM, Channel, Pty, Line, User);

	{ssh_cm, CM, {pty_req,Channel,Pty1}} ->
	    winch_redraw(CM,Channel,Line,Pty,Pty1),
	    ?MODULE:server_loop(SSH, CM, Channel, Pty1, Line, User);

	{ssh_cm, CM, {window_change,Channel,
		      Width,Height,PixWidth,PixHeight}} ->
	    Pty1 = Pty#ssh_pty_params { width=Width,
					height=Height,
					pix_width=PixWidth,
					pix_height=PixHeight},
	    winch_redraw(CM,Channel,Line,Pty,Pty1),
	    ?MODULE:server_loop(SSH, CM, Channel, Pty1, Line, User);

	Other ->
	    io:format("ssh server got: ~p\n", [Other]),
	    ?MODULE:server_loop(SSH, CM, Channel, Pty, Line, User)
	    
    end.


output(CM, Channel, Chars, Line, OldLine, Pty) ->
    Term = Pty#ssh_pty_params.terminal,
    %% io:format("Ouput: Term=~p,Chars=~p,Line=~p,OldList=~p\n", [Term,Chars,Line,OldLine]),
    if Term == "vt100"; Term == "xterm" ->
	    ssh_cm:send(CM, Channel, update_vt100(Line,OldLine,Pty));
       true ->
	    ssh_cm:send(CM, Channel, Chars)
    end.

input_chars([], Line, _CM, _Channel, _Pty) ->
    Line;
input_chars([C|Cs], Line, CM, Channel,Pty) ->
    input_char(C, Cs, Line, CM, Channel,Pty).

input_char(C,Cs,_Line,CM,Channel,Pty) when C == ?CR; C == ?NL ->
    ssh_cm:send(CM, Channel, [?CR,?NL]),
    input_chars(Cs,?EMPTY_LINE,CM,Channel,Pty);
%% Fixme escape chars
input_char(C, Cs, Line, CM, Channel, Pty) ->
    {Output,Line1} = insert_char(C, Line),
    output(CM, Channel, Output, Line1, Line, Pty),
    input_chars(Cs,Line1,CM,Channel,Pty).

move_relative(0, Line, _CM, _Channel, _Pty) ->
    Line;
move_relative(I, Line, CM, Channel, Pty) when I < 0 ->
    {Output,Line1} = backward_char(Line),
    output(CM, Channel, Output, Line1, Line, Pty),
    move_relative(I+1, Line1, CM, Channel, Pty);
move_relative(I, Line, CM, Channel, Pty) when I > 0 ->
    {Output,Line1} = forward_char(Line),
    output(CM, Channel, Output, Line1, Line, Pty),
    move_relative(I-1, Line1, CM, Channel, Pty).

delete_chars(0, Line, _CM, _Channel, _Pty) ->
    Line;
delete_chars(I, Line, CM, Channel, Pty) when I < 0 ->
    {Output,Line1} = delete_backward_char(Line),
    output(CM, Channel, Output, Line1, Line, Pty),
    delete_chars(I+1,Line1,CM,Channel,Pty);
delete_chars(I, Line, CM, Channel, Pty) when I > 0 ->
    {Output,Line1} = delete_char(Line),
    output(CM, Channel, Output, Line1, Line, Pty),
    delete_chars(I-1,Line1,CM,Channel,Pty).

    

%% Update the screen to handle input changes using VT100 escapes.
update_vt100(Line, OldLine,PtyParams) ->
    {DrawPos, Cs} = part_to_draw(line_text(Line), line_text(OldLine), 0),
    if Cs /= [] ->
	    DrawnPos = DrawPos + length(Cs),
	    [goto_char(DrawPos,position(OldLine),PtyParams),
	     Cs,
	     wrap_at_eol(DrawnPos,PtyParams),
	     goto_char(position(Line),DrawPos+length(Cs),PtyParams)];
       true ->
	    goto_char(position(Line), position(OldLine),PtyParams)
    end.


%% part_to_draw(Line, OldLine, Acc) => {DrawPos, DrawChars}
%%
%% Return DrawChars to be drawn at DrawPos in order to update the
%% screen for new line contents. This is a "diff" to decide how the
%% screen needs to be updated.
part_to_draw([X|Ns], [X|Os], Pos) ->
    part_to_draw(Ns, Os, Pos+1);
part_to_draw(Ns, Os, Pos) ->
    case {length(Ns),length(Os)} of
	{N,O} when O =< N ->
	    {Pos,Ns};
	{_N,O} ->
	    {Pos, Ns++lists:duplicate(O, $\s)}
    end.

%% Goto Pos from CurPos. Both positions are integers giving a linear
%% position from the start of the line of input.
goto_char(Pos, CurPos, PtyParams) ->
    W = PtyParams#ssh_pty_params.width,
    {X0,Y0} = screen_position(CurPos, W),
    {X1,Y1} = screen_position(Pos, W),
    [if X0 > X1 -> move(left, X0-X1);
	X0 < X1 -> move(right, X1-X0);
	true    -> []
     end,
     if Y0 > Y1 -> move(up, Y0-Y1);
	Y0 < Y1 -> move(down, Y1-Y0);
	true    -> []
     end].

%% If we are at the end-of-line then make sure we scroll down a line.
wrap_at_eol(Pos,Pty) ->
    W = Pty#ssh_pty_params.width,
    case screen_position(Pos, W) of
	{0, _} ->
	    %% Insert/remove an extra character to force scrolling
	    [$\s, move(left,1)];
	_ ->
	    []
    end.

%% Handle a WINCH event: erase all drawn characters and then redraw.
%%
%% There seems to be a fundamental race condition: if the window
%% changes "asynchronously" while we are updating it then we end up in
%% an inconsistent state because we can't know how the terminal
%% actually handled our cursor motion (i.e. whether we "hit an edge"
%% during redraw).
%%
%% Can this be fixed? bash seems to have the same problem if you run
%% it with a little bit of latency and drag-resize the window rapidly.
%% -luke (22/Nov/2004)
winch_redraw(CM,Channel,Line, Pty0, Pty1) ->
    P = position(Line),
    Text = line_text(Line),
    N = length(Text),
    %% Before/after screen width
    W0 = Pty0#ssh_pty_params.width,
    W1 = Pty1#ssh_pty_params.width,
    %% Where we are now:
    X0 = min(P rem W0, W1-1),
    Y0 = P div W0,
    NumToClear = (N div W0) * W1 + (N rem W0) + 1,
    ssh_cm:send(CM,Channel,
		[%% Goto beginning, translating current position to new screen size
		 goto_char(0, Y0 * W1 + X0,Pty1),
		 %% Erase old input
		 lists:duplicate(NumToClear, $\s),
		 wrap_at_eol(NumToClear,Pty1),
		 goto_char(0, NumToClear,Pty1),
		 Text,
		 wrap_at_eol(length(Text),Pty1),
		 goto_char(position(Line), length(Text), Pty1)]).


min(X,Y) when X < Y -> X;
min(_X,Y)           -> Y.

move(_Direction, 0) -> [];
move(Direction, N) -> 
    Code = case Direction of
	       up    -> $A;
	       down  -> $B;
	       right -> $C;
	       left  -> $D
	   end,
    [?ESC,?LSQ,integer_to_list(N),Code].

position({Bs,_}) ->
    length(Bs).

screen_position(Point, Width) ->
    {Point rem Width, Point div Width}.


line_text({Bs,As}) -> reverse(Bs)++As.

%% Line editing
insert_char(C, {Bs,As}) ->
    Out = duplicate(length(As),?BS),
    {[[C|As],Out], {[C|Bs],As}}.

delete_char({Bs,[]}) -> 
    {[], {Bs,[]}};
delete_char({Bs,[_C|As]}) ->
    Out = duplicate(length(As)+1,?BS),
    {[As,?SP,Out], {Bs,As}}.

delete_backward_char({[_|Bs],[]}) ->
    {[?BS,?SP,?BS], {Bs,[]}};    
delete_backward_char({[_|Bs],As}) ->
    Out = duplicate(length(As)+1,?BS),
    {[[?BS|As],?SP,Out], {Bs,As}};
delete_backward_char({[],As}) ->
    {[], {[],As}}.

backward_char({[C|Bs],As}) ->
    {[?BS], {Bs,[C|As]}};
backward_char({[],As}) -> 
    {[], {[],As}}.

forward_char({Bs,[C|As]}) ->
    {[C], {[C|Bs],As}};
forward_char({Bs,[]}) ->
    {[], {Bs,[]}}.

insert_string(String, {Bs,As}) ->
    Out = duplicate(length(As),?BS),
    {[String,As,Out], {reverse(String)++Bs,As}}.

    
    
    
	    
	




	    
    
	    



	    
	    
	    

	    
	    


