%%% File    : gterm.erl
%%% Author  : Tony Rogvall <tony@bit.hemma.se>
%%% Description : Terminal example
%%% Created :  8 Mar 2001 by Tony Rogvall <tony@bit.hemma.se>

-module(gterm).

-include_lib("erlgtk/include/gtk.hrl").
-include_lib("erlgtk/include/gdk.hrl").
-include_lib("erlgtk/include/gdk_keysyms.hrl").

-import(lists, [reverse/1, foldl/3, map/2, foreach/2,
		sublist/2, duplicate/2]).

-compile(export_all).

%% emacs mode workaround.
-define(LB,   $[).  
-define(RB,   $]).
-define(ESC,  $\e).
-define(BEL,  $\^g).
-define(TAB,  $\t).
-define(NL,   $\n).
-define(CR,   $\r).
-define(BS,   $\b).
-define(FF,   $\f).
-define(VF,   $\v).

%% semi graphics
-ifdef(ascii_semi).
-define(HLINE_CHAR,      $-).
-define(VLINE_CHAR,      $|).
-define(ULCORN_CHAR,     $+).
-define(URCORN_CHAR,     $+).
-define(LLCORN_CHAR,     $+).
-define(LRCORN_CHAR,     $+).
-define(CKBRD_CHAR,      $#).
-define(RTEE_CHAR,       $+).
-define(LTEE_CHAR,       $+).
-define(UTEE_CHAR,       $+).
-define(DTEE_CHAR,       $+).
-define(PLUS_CHAR,       $+).
-define(DIAMOND_CHAR,    $+).
-define(DEGREE_CHAR,     $\\).
-define(PLMINUS_CHAR,    $#).
-define(BULLET_CHAR,     $o).
-define(LARROW_CHAR,     $<).
-define(RARROW_CHAR,     $>).
-define(DARROW_CHAR,     $v).
-define(UARROW_CHAR,     $^).
-define(BOARD_CHAR,      $#).
-define(BLOCK_CHAR,      $#).
-else.
-define(HLINE_CHAR,      0).
-define(VLINE_CHAR,      0).
-define(ULCORN_CHAR,     0).
-define(URCORN_CHAR,     0).
-define(LLCORN_CHAR,     0).
-define(LRCORN_CHAR,     0).
-define(CKBRD_CHAR,      0).
-define(RTEE_CHAR,       0).
-define(LTEE_CHAR,       0).
-define(UTEE_CHAR,       0).
-define(DTEE_CHAR,       0).
-define(PLUS_CHAR,       0).
-define(DIAMOND_CHAR,    0).
-define(DEGREE_CHAR,     0).
-define(PLMINUS_CHAR,    0).
-define(BULLET_CHAR,     0).
-define(LARROW_CHAR,     0).
-define(RARROW_CHAR,     0).
-define(DARROW_CHAR,     0).
-define(UARROW_CHAR,     0).
-define(BOARD_CHAR,      0).
-define(BLOCK_CHAR,      0).
-endif.


%% attributes: vt100 etc
%% 00 = none  (off)
%% 01 = bold  
%% 04 = underline
%% 05 = blinking
%% 07 = inverse
%% 08 = concealed ?
%%
%% color codes:
%%   0=black
%%   1=red
%%   2=green
%%   3=yellow
%%   4=blue
%%   5=magenta
%%   6=cyan
%%   7=white
%% 
%% 2x = ??? used by slang check it 
%% 3x = forground color
%% 4x = background color
%%

-define(ATTR_INVERSE,     16#000001).
-define(ATTR_HIGHLIGHT,   16#000002).
-define(ATTR_UNDERLINE,   16#000004).
-define(ATTR_BLINKING,    16#000008).
-define(ATTR_CONCEALED,   16#000020).  %% ?? 
-define(ATTR_FG_COLOR,    16#000040).  %% use forground color
-define(ATTR_BG_COLOR,    16#000080).  %% use background color
-define(ATTR_MASK,        16#0000FF).
-define(ATTR_OFF,         16#000000).

-define(ICRNL,            16#000100).  %% input: cr -> nl
-define(INLCR,            16#000200).  %% input: nl -> cr
-define(IGNCR,            16#000400).  %% input: cr -> 

-define(ONLRET,           16#001000).  %% output: nl -> cr nl
-define(AUTOWRAP,         16#002000).  %% auto next line 

-define(ECHOE,            16#010000).  %% echo erase
-define(ECHO,             16#020000).  %% local echo
-define(USCROLL,          16#040000).  %% update each scroll
-define(BEEP,             16#080000).  %% beep

-define(CURSOR_ON,        16#100000).  %% show cursor
-define(CURSOR_BLINKING,  16#200000).  %% blinking cursor
-define(CURSOR_UNDERLINE, 16#400000).  %% underline/block cursor

-define(FLAG_MAX_BIT,     16#400000).  %% highest numbered flag

-define(S_SPACE, [$\s | ?ATTR_OFF]).

-record(state,
	{
	  r         = 0,         %% row     (relative to r_top)
	  c         = 0,         %% column
	  columns   = 80,        %% number of columns (region)
	  rows      = 24,        %% number of rows (region)
	  r_top     = 0,         %% region top
	  r_bot     = 23,        %% region bottom
	  t_rows    = 24,        %% terminal rows
	  t_columns = 80,        %% terminal columns
	  window,                %% Gdkwindow
	  widget,                %% drawing area
	  font,                  %% current font
	  char_width,            %% char width  (including pad)
	  char_height,           %% char height (including pad)
	  style,
	  fg,                    %% foreground GC
	  bg,                    %% background GC

	  flags =
	  ?CURSOR_ON bor 
	  ?CURSOR_UNDERLINE bor 
	  ?USCROLL bor ?BEEP bor ?AUTOWRAP bor
	  %% ?CURSOR_BLINKING bor
	  %% ?ONLRET bor
	  %% ?ECHOE bor
	  %% ?ICRNL bor
	  %% ?ONLRET bor
	  0,

	  cfg,                   %% Color foreground GC
	  cbg,                   %% Color background GC
	  fg_color = 0,           %% foreground color
	  bg_color = 0,           %% background color
	  colormap,              %% tuple of colors, indexed by 1..8
	  char_set,              %% current char set

	  leds,                  %% pixmap leds {Pixmap1,....}
	  led_map,               %% {{Pixmap,Mask},{Pixmap,Mask}}
	  cstate,                %% storage for cursor store op
	  input = [],            %% input buffer
	  input_size = 10,       %% number of characters to buffer
	  connected = [],        %% process that need key input
	  wait  = [],            %% read queue
	  scrn,                  %% redraw structure
	  ifun   = fun vt100/2,  %% input mode function
	  timer,                 %% blink timer
	  istate = [],           %% input list
	  invalid_rect           %% update rect
	 }).

-record(cursor_state, 
	{
	  r,         %% row
	  r_top,     %% region top
	  r_bot,     %% region bot
	  c,         %% column
	  char_attr, %% char attributes
	  char_set,  %% character set
	  fg_color,  %% foreground color
	  bg_color   %% background color
	 }).


-record(scrn,
	{
	  r,       %% current row position
	  rows,    %% number of rows
	  columns, %% number of columns
	  buf,     %% tuple of {R1,R2,....,Rn} Ri #scrrow
	  row      %% current row #scrrow
	 }).

-record(srow,
	{
	  c,     %% current column
	  cr,    %% current character [<char-code>|<attributes>]
	  cb,    %% columns before cr (reversed) [chars] 
	  ca     %% columns after cr [chars]
	 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Functional API
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_data(T, Data)     -> call(T, {data,Data}).
send_output(T, Data)   -> call(T, {output,Data}).
set_flags(T, Flags)    -> call(T, {set_flags, encode_flags(Flags)}).
get_flags(T)           -> call(T, get_flags).
unset_flags(T, Flags)  -> call(T, {unset_flags, encode_flags(Flags)}).
toggle_flags(T, Flags) -> call(T, {toggle_flags, encode_flags(Flags)}).
goto(T, R, C)          -> call(T, {set_cursor, R, C}).
move(T, R, C)          -> call(T, {move_cursor, R, C}).
beep(T)                -> call(T, beep).
set_size_rc(T, R, C)   -> call(T, {set_size_rc, R, C}).
set_size_wh(T, W, H)   -> call(T, {set_size_wh, W, H}).
set_region(T, Top,Bot) -> call(T, {set_region, Top, Bot}).
set_font(T, N)         -> call(T, {set_font, N}).
erase_eol(T)           -> call(T, {erase_line, 0}).
erase_bol(T)           -> call(T, {erase_line, 1}).
erase_line(T)          -> call(T, {erase_line, 2}).
erase_eos(T)           -> call(T, {erase_screen, 0}).
erase_bos(T)           -> call(T, {erase_screen, 1}).
erase_screen(T)        -> call(T, {erase_screen, 2}).
scroll(T, How)         -> call(T, {scroll, How}).
redraw(T)              -> call(T, redraw).
connect(T, Pid)        -> call(T, {connect,Pid}).
stop(T)                -> call(T, stop).
get(T, What)           -> call(T, {get,What}).
save(T)                -> call(T, save).
restore(T)             -> call(T, restore).
cut(T)                 -> call(T, cut).
copy(T)                -> call(T, copy).
paste(T)               -> call(T, paste).

%% translate to internal flags
encode_flags(Flags) ->
    foldl(fun(F,Acc) -> e_flag(F) bor Acc end, 0, Flags).

decode_flags(Bits) ->
    decode_flags(Bits,?FLAG_MAX_BIT, []).

decode_flags(0, Bit, Flags) -> Flags;
decode_flags(Bits, Bit, Flags) ->
    Bit1  = Bit bsr 1,
    if Bits band Bit == Bit ->
	    Bits1 = Bits band (bnot Bit),
	    case d_flag(Bit) of
		undefined -> decode_flags(Bits1, Bit1, Flags);
		Flag -> decode_flags(Bits1, Bit1, [Flag|Flags])
	    end;
       true ->
	    decode_flags(Bits, Bit1, Flags)
    end.
		    

e_flag(inverse)          -> ?ATTR_INVERSE;
e_flag(highlight)        -> ?ATTR_HIGHLIGHT;
e_flag(underline)        -> ?ATTR_UNDERLINE;
e_flag(blinking)         -> ?ATTR_BLINKING;
e_flag(concealed)        -> ?ATTR_CONCEALED;
e_flag(fg_color)         -> ?ATTR_FG_COLOR;
e_flag(bg_color)         -> ?ATTR_BG_COLOR;
e_flag(all)              -> ?ATTR_MASK;
e_flag(icrnl)            -> ?ICRNL;
e_flag(inlcr)            -> ?ICRNL;
e_flag(igncr)            -> ?IGNCR;
e_flag(onlret)           -> ?ONLRET;
e_flag(autowrap)         -> ?AUTOWRAP;
e_flag(echoe)            -> ?ECHOE;
e_flag(echo)             -> ?ECHO;
e_flag(uscroll)          -> ?USCROLL;
e_flag(beep)             -> ?BEEP;
e_flag(cursor)           -> ?CURSOR_ON;
e_flag(cursor_blinking)  -> ?CURSOR_BLINKING;
e_flag(cursor_underline) -> ?CURSOR_UNDERLINE;
e_flag(Flag)             -> exit({einval, Flag}).

d_flag(?ATTR_INVERSE)     -> inverse;
d_flag(?ATTR_HIGHLIGHT)   -> highlight;
d_flag(?ATTR_UNDERLINE)   -> underline;
d_flag(?ATTR_BLINKING)    -> blinking;
d_flag(?ATTR_CONCEALED)   -> concealed;
d_flag(?ATTR_FG_COLOR)    -> fg_color;
d_flag(?ATTR_BG_COLOR)    -> bg_color;
d_flag(?ATTR_MASK)        -> all;
d_flag(?ICRNL)            -> icrnl;
d_flag(?ICRNL)            -> inlcr;
d_flag(?IGNCR)            -> igncr;
d_flag(?ONLRET)           -> onlret;
d_flag(?AUTOWRAP)         -> autowrap;
d_flag(?ECHOE)            -> echoe;
d_flag(?ECHO)             -> echo;
d_flag(?USCROLL)          -> uscroll;
d_flag(?BEEP)             -> beep;
d_flag(?CURSOR_ON)        -> cursor;
d_flag(?CURSOR_BLINKING)  -> cursor_blinking;
d_flag(?CURSOR_UNDERLINE) -> cursor_underline;
d_flag(_)                 -> undefined.


		  

call(Terminal, Call) ->
    Ref = make_ref(),
    Terminal ! {call,[self()|Ref],Call},
    receive
	{Ref,Reply} ->
	    Reply
    end.

cast(Terminal, Cast) ->
    Ref = make_ref(),
    Terminal ! {cast,[self()|Ref],Cast},
    ok.

reply([Pid|Ref], Reply) ->
    Pid ! {Ref, Reply}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% SCREEN BUFFER ROUTINES s_xyz
%%
%% screen buffer is a:
%%
%% tuple {R1,R2,R3,....,Rn} of record(srow)
%%
%% the current row is also available in row
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dup_row(Buf, I, J, Row) when I =< J ->
    dup_row(setelement(I+1, Buf, Row), I+1, J, Row);
dup_row(Buf, _, _, _) ->
    Buf.

move_rows(S0, Src, Dst, N) ->
    Buf = move_rows(S0#scrn.buf, Src, S0#scrn.buf, Dst, N),
    S0#scrn { buf = Buf }.

move_rows(SrcBuf, Src, DstBuf, Dst, 0) -> 
    DstBuf;
move_rows(SrcBuf, Src, DstBuf, Dst, N) ->
    move_rows(SrcBuf, Src+1,
	      setelement(Dst+1, DstBuf, element(Src+1,SrcBuf)), Dst+1,
	      N-1).

%% Create a new scrbuf with R rows and C columns
%% the position is in the (0,0) top left
%%
s_new(Rows, Columns) ->
    R = s_new_row(Columns),
    #scrn { columns = Columns,
	    rows    = Rows,
	    r = 0,
	    buf = list_to_tuple(duplicate(Rows,R)),
	    row = R
	   }.

s_new_row(Columns) ->
    #srow { c = 0,
	    cr = ?S_SPACE,
	    cb = [], 
	    ca = duplicate(Columns-1, ?S_SPACE) }.

s_erase_eol(S) ->
    #srow { ca = Ca } = R0 = S#scrn.row,
    R1 = R0#srow {  cr = ?S_SPACE, ca = duplicate(length(Ca), ?S_SPACE)},
    S#scrn { row = R1 }.

s_erase_bol(S) ->
    #srow { cb = Cb } = R0 = S#scrn.row,
    R1 = R0#srow {  cr = ?S_SPACE, cb = duplicate(length(Cb), ?S_SPACE)},
    S#scrn { row = R1 }.

s_erase_line(S) ->
    #srow { ca = Ca, cb = Cb } = R0 = S#scrn.row,    
    R1 = R0#srow {  cr = ?S_SPACE,
		    cb = duplicate(length(Cb), ?S_SPACE),
		    ca = duplicate(length(Ca), ?S_SPACE) },
    S#scrn { row = R1 }.
		    
s_erase_eos(S0) ->
    S1 = s_erase_eol(S0),
    R = s_new_row(S1#scrn.columns),
    Buf = dup_row(S1#scrn.buf, S1#scrn.r+1, S1#scrn.rows-1, R),
    S1#scrn { buf = Buf }.

s_erase_bos(S0) ->
    S1 = s_erase_bol(S0),
    R = s_new_row(S1#scrn.columns),
    Buf = dup_row(S1#scrn.buf, 0, S1#scrn.r-1, R),
    S1#scrn { buf = Buf }.

%% commit changes to current row into the buffer
s_commit(S0) ->
    Buf = setelement(S0#scrn.r+1, S0#scrn.buf, S0#scrn.row),
    S0#scrn { buf = Buf}.

%% update the current row from buffer
s_update(S0) ->
    Row = element(S0#scrn.r+1, S0#scrn.buf),
    S0#scrn { row = Row }.

s_set_row(S0, R, Row) ->
    Buf = S0#scrn.buf,
    S0#scrn { buf = setelement(R+1,Buf,Row) }.

%% preserve current pos
s_erase_screen(S0) ->
    s_erase_eos(s_erase_bos(S0)).

%% move left I columns
s_moveleft(Row, I, C, Cr, Cb, Ca) when I =< 0 ->
    Row#srow { c = C, cr = Cr, cb = Cb, ca = Ca };
s_moveleft(Row, I, C, Cr, [], Ca) ->
    Row#srow { c = C, cr = Cr, cb = [], ca = Ca };
s_moveleft(Row, I, C, Cr, [ColB|Cb], Ca) ->
    s_moveleft(Row,I-1,C-1,ColB,Cb,[Cr|Ca]).

%% move right I coumns
s_moveright(Row, I, C, Cr, Cb, Ca) when I =< 0 ->
    Row#srow { c = C, cr = Cr, cb = Cb, ca = Ca };
s_moveright(Row, I, C, Cr, Cb, []) ->
    Row#srow { c = C, cr = Cr, cb = Cb, ca = [] };
s_moveright(Row, I, C, Cr, Cb, [ColA|Ca]) ->
    s_moveright(Row,I-1,C+1,ColA,[Cr|Cb],Ca).

s_goto_col(S, C) ->
    Row = S#scrn.row,
    C0  = Row#srow.c,
    if C == C0 -> S;
       C <  C0 -> 
	    Row1 = s_moveleft(Row,C0-C,C0,Row#srow.cr,
			      Row#srow.cb, Row#srow.ca),
	    S#scrn { row = Row1 };
       true ->
	    Row1 = s_moveright(Row,C-C0,C0,Row#srow.cr,
			       Row#srow.cb, Row#srow.ca),
	    S#scrn { row = Row1 }
    end.

%% goto row R preserving column
s_goto_row(S0, R) ->
    Row = S0#scrn.row,
    S1 = s_commit(S0),
    S2 = s_update(S1#scrn { r = R }),
    s_goto_col(S2, Row#srow.c).

s_cursor_pos(S, R, C) ->
    s_goto_col(s_goto_row(S, R), C).

s_scroll(S0, Top, Bot, Dir) ->
    %% io:format("s_scroll: top=~p, bot=~p, dir=~p\n", [Top,Bot,Dir]),
    Row = S0#scrn.row,
    S1 = s_commit(S0),
    S2 = case Dir of
	     up ->
		 S11 = move_rows(S1, Top+1, Top, (Bot-Top)),
		 s_set_row(S11, Bot, s_new_row(S11#scrn.columns));
	     down ->
		 S11 = move_rows(S1, Top, Top+1, (Bot-Top)),
		 s_set_row(S11, Top, s_new_row(S11#scrn.columns))
	 end,
    S3 = s_update(S2),
    s_goto_col(S3, Row#srow.c).

%%
%% screen buffer managment
%% write character in current row current column
%% advance the column
%%
s_char(Scr, C, Attr) ->
    Row = Scr#scrn.row,
    #srow { ca = Ca, cb = Cb, c = Col  } = Row,
    Row1 = case Ca of
	       []  ->
		   Row#srow { cr = [C|Attr] };
	       [Cr|Ca2] ->
		   Row#srow { cr = Cr, 
				cb =[ [C|Attr] | Cb], 
				ca = Ca2,
				c = Col + 1}
	   end,
    Scr#scrn { row = Row1 }.


s_add_rows(Scr, N) ->
    if N > 0 ->
	    R = s_new_row(Scr#scrn.columns),
	    Buf = list_to_tuple(tuple_to_list(Scr#scrn.buf)++duplicate(N, R)),
	    Scr#scrn { buf = Buf };
       N < 0  ->
	    Buf = list_to_tuple(lists:nthtail(-N,tuple_to_list(Scr#scrn.buf))),
	    Scr#scrn { buf = Buf };
       true  ->
	    Scr
    end.

s_add_cols(Scr, N) ->
    if N > 0 ->
	    CA = duplicate(N, ?S_SPACE),
	    BL0 =  tuple_to_list(Scr#scrn.buf),
	    BL1 = map(
		   fun(C = #srow { ca = Ca }) ->
			   C#srow { ca = Ca ++ CA }
		   end, 
		   BL0),
	    Buf = list_to_tuple(BL1),
	    Scr#scrn { buf = Buf };
       N < 0 ->
	    BL0 =  tuple_to_list(Scr#scrn.buf),
	    %% remove N chars from end of each line, 
	    %% this operation will move the current pos to the
	    %% beginning of line
	    BL1 = map(
		    fun(C = #srow { cb = [], cr = Cr, ca = Ca }) ->
			    Ca1 = reverse(lists:nthtail(-N, reverse(Ca))),
			    C#srow { ca = Ca1 };
		       (C = #srow { cb = Cb, cr = Cr, ca = Ca  }) ->
			    [Cr1|Cb1] = reverse(Cb),
			    Ca1 = Cb1 ++ [Cr|Ca],
			    Ca2 = reverse(lists:nthtail(-N, reverse(Ca1))),
			    C#srow { cb = [], cr = Cr1, ca = Ca2, c = 0}
		    end, BL0),
	    Buf = list_to_tuple(BL1),
	    Scr#scrn { buf = Buf };
       true ->
	    Scr
    end.

s_set_size_rc(S0, Rows, Cols) ->
    Row = S0#scrn.row,
    Col = Row#srow.c,
    S1 = s_commit(S0),
    S2 = s_add_rows(S1, Rows - S0#scrn.rows),
    S3 = s_add_cols(S2, Cols - S0#scrn.columns),
    S4 = if S0#scrn.r >= Rows ->
		 s_update(S3#scrn { r = Rows -1 });
	    true ->
		 s_update(S3)
	 end,
    S5 = if Col >= Cols ->
		 s_goto_col(S4, Cols-1);
	    true ->
		 s_goto_col(S4, Col)
	 end,
    S5#scrn { rows = Rows, columns = Cols }.

	    
	    
s_string(Scr, [C|Cs], Attr) ->
    s_string(s_char(Scr, C, Attr), Cs, Attr);
s_string(Scr, [], Attr) ->
    Scr.


%% debug dump the screen
s_dump(Scr) ->
    Scr1 = s_commit(Scr),
    foldl(fun(R,I) -> 
		  io:format("~3w:", [I]),
		  s_dump_row(R),
		  I+1
	  end, 
	  1,
	  tuple_to_list(Scr1#scrn.buf)).

s_dump_row(Row) ->
    io:put_chars(
      [
       map(fun([C|_]) -> C end, reverse(Row#srow.cb)),
       hd(Row#srow.cr),
       map(fun([C|_]) -> C end, Row#srow.ca),
       $\n]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% PIXMAP DRAWING ROUTINES t_xyz
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

union_rect(A = #gdk_rectangle { x = XA, y = YA, width = WA, height = HA},
	   B = #gdk_rectangle { x = XB, y = YB, width = WB, height = HB}) ->
    XC = if XA < XB -> XA; true -> XB end,
    YC = if YA < YB -> YA; true -> YB end,
    XWA = XA+WA,
    XWB = XB+WB,
    YHA = YA+HA,
    YHB = YB+HB,
    #gdk_rectangle { x = XC, 
		y = YC,
		width  = (if XWA > XWB -> XWA; true -> XWB end) - XC,
		height = (if YHA > YHB -> YHA; true -> YHB end) - YC}.

t_invalidate(St, A) ->
    U = case St#state.invalid_rect of
	    undefined -> A;
	    B -> union_rect(A, B)
	end,
    St#state { invalid_rect = U }.

t_update(St) ->
    case St#state.invalid_rect of
	undefined -> 
	    St;
	R ->
	    gtk:widget_draw(St#state.widget, R),
	    St#state { invalid_rect = undefined }
    end.

%% create a new pixmap and clear it with the background GC
t_pixmap_new(St, W, H) ->
    Pixmap  = gdk:pixmap_new(St#state.window, W, H, -1),
    gdk:draw_rectangle(Pixmap, St#state.bg, true, 0, 0, W, H),
    Pixmap.

t_pixmap_delete() ->
    case get(pixmap) of
	undefined -> ok;
	Pixmap ->
	    gdk:pixmap_unref(Pixmap),
	    erase(pixmap),
	    ok
    end.

%% set terminal size in row and columns
t_set_size_rc(St, Rows, Cols) ->
    io:format("t_set_size_rc: rows=~p, cols=~p\n", [Rows, Cols]),
    OH = St#state.t_rows * St#state.char_height,
    OW = St#state.t_columns * St#state.char_width,
    io:format("t_set_size_rc: oh=~p, ow=~p\n", [OH, OW]),
    H = (Rows * St#state.char_height),
    W = (Cols * St#state.char_width),
    Pixmap = t_pixmap_new(St, W, H),
    Gc = St#state.fg,
    case get(pixmap) of
	undefined -> ok;
	Pixmap0 ->
	    HH = if OH < H -> OH; true -> H end,
	    WW = if OW < W -> OW; true -> W end,
	    gdk:draw_pixmap(Pixmap, Gc, Pixmap0,
			    0, 0, 0, 0, WW, HH),
	    gdk:pixmap_unref(Pixmap0)
    end,
    put(pixmap, Pixmap),
    St1 = t_invalidate(St,#gdk_rectangle { x=0, y=0, width=OW, height =OH}),
    St2 = t_invalidate(St1,#gdk_rectangle { x=0, y=0, width=W, height =H}),
    Scrn = s_set_size_rc(St#state.scrn, Rows, Cols),
    send_connected(St2#state.connected, {self(),{naws,Cols, Rows}}),
    St3 = St2#state { scrn = Scrn, 
		      columns = Cols, 
		      t_columns = Cols, 
		      t_rows = Rows },
    %% Only set the new number of rows if not in scrolling region!
    if St2#state.rows == St2#state.t_rows -> %% Yes previous state
	    St3#state { rows = Rows };
       true ->
	    St3
    end.


%% set terminal size in width and height
t_set_size_wh(St, W, H) ->
    %% io:format("t_set_wh: ~p, ~p, ~p\n", [W, H, St]),
    C = W div St#state.char_width,
    R = H div St#state.char_height,
    t_set_size_rc(St, R, C).

%% set terminal font (change width, height)
t_set_font(St, N) ->
    Font        = gdk:font_load(font(N)),
    Char_width  = gdk:char_width(Font, $0),
    Char_height = Font#gdk_font.ascent + Font#gdk_font.descent,
    W = Char_width * St#state.t_columns,
    H = Char_height * St#state.t_rows,
    gtk:drawing_area_size (?GTK_DRAWING_AREA (St#state.widget), W, H), 
    t_pixmap_delete(),
    Pixmap = t_pixmap_new(St, W, H),
    put(pixmap, Pixmap),
    Gc = St#state.fg,
    St1 = St#state {  font = Font,
		      char_width = Char_width,
		      char_height = Char_height },
    t_redraw_screen(St1),
    R = #gdk_rectangle { x = 0, y = 0, width = W, height = H },
    t_invalidate(St1, R).


    
t_scroll(St, Dir) ->
    %% move text one line up
    Pixmap = get(pixmap),
    Fg = St#state.fg,
    Bg = St#state.bg,
    CH = St#state.char_height,
    Height = ((St#state.rows-1) * CH),
    Width  = (St#state.columns * St#state.char_width),
    X0   = 0,
    Y0   = (St#state.r_top * CH),
    X1   = 0,
    Y1   = Y0 + CH,
    case Dir of
	up ->
	    gdk:draw_pixmap(Pixmap, Fg, Pixmap, 
			    X1, Y1, X0, Y0, Width, Height),
	    gdk:draw_rectangle(Pixmap, Bg, true, 
			       X0, Y0+Height, Width, CH);
	down ->
	    gdk:draw_pixmap(Pixmap, Fg, Pixmap, 
			    X0, Y0, X1, Y1, Width, Height),
	    gdk:draw_rectangle(Pixmap, Bg, true, 
			       X0, Y0, Width, CH);
	_ ->
	    not_yet
    end,
    St1 = St#state { scrn = s_scroll(St#state.scrn,
				     St#state.r_top,
				     St#state.r_bot, Dir) },
    R = #gdk_rectangle { x = X0, y = Y0, width = Width, height = Height+CH },
    St2 = t_invalidate(St1, R),
    if St2#state.flags band ?USCROLL == 0 ->
	    St2;
       true ->
	    t_update(St2)
    end.


%% must xor or blink ontop of pixmap!
t_cursor(St) ->
    Pixmap = get(pixmap),
    if Pixmap == undefined ->
	    St;
       St#state.c >= St#state.columns ->
	    St;
       true ->
	    {Y,H} = 
		if St#state.flags band ?CURSOR_UNDERLINE == 0 ->
			{ ((St#state.r+St#state.r_top)*St#state.char_height),
			  St#state.char_height };
		   true ->
			{ ((St#state.r+St#state.r_top)*St#state.char_height) +
			  St#state.char_height - 3,
			  3 }
		end,
	    X = (St#state.c * St#state.char_width),
	    W = St#state.char_width,
	    R = #gdk_rectangle { x = X, y = Y, width = W, height = H },
	    Fg = if (St#state.flags band ?ATTR_INVERSE) =/= 0 ->
			 St#state.fg;
		    true ->
			 St#state.bg
		 end,
	    gdk:gc_set_function(Fg, 'GDK_XOR'),
	    gdk:draw_rectangle(Pixmap, Fg, true, X, Y, W, H),
	    gdk:gc_set_function(Fg, 'GDK_COPY'),
	    gtk:widget_draw(St#state.widget, R),
	    St
    end.

    
t_erase_line(St, 0) -> %% erase to end of line
    St1 = t_erase_line(St, St#state.c, St#state.columns-1),
    St1#state { scrn = s_erase_eol(St#state.scrn) };
t_erase_line(St, 1) -> %% erase to beginning of line
    St1 = t_erase_line(St, 0, St#state.c),
    St1#state { scrn = s_erase_bol(St#state.scrn) };
t_erase_line(St, 2) -> %% erase entire line
    St1 = t_erase_line(St, 0, St#state.columns-1),
    St1#state { scrn = s_erase_line(St#state.scrn) }.

t_erase_line(St, From, To) when From =< To ->
    CH = St#state.char_height,
    Y  = ((St#state.r+St#state.r_top) * CH),
    X0 = (From * St#state.char_width),
    X1 = ((To+1) * St#state.char_width),
    W = X1 - X0,
    Pixmap = get(pixmap),
    R = #gdk_rectangle { x = X0, y = Y, width = W, height = CH },
    Bg = if (St#state.flags band ?ATTR_INVERSE) == 0 ->
		 St#state.bg;
	    true ->
		 St#state.fg
	 end,
    gdk:draw_rectangle(Pixmap, Bg, true, X0, Y, W, CH),
    t_invalidate(St, R);
t_erase_line(St, From, To) ->
    St.

t_erase_screen(St, 0) -> %% erase to end of sceen
    St1 = t_erase_line(St,0),
    St2 = t_erase_screen(St,
			 (St#state.r+St#state.r_top)+1, 
			 St#state.t_rows-1),
    St2#state { scrn = s_erase_eos(St#state.scrn) };
t_erase_screen(St, 1) -> %% erase to beginning of screen
    St1 = t_erase_line(St,1),
    St2 = t_erase_screen(St, 0, (St#state.r+St#state.r_top)-1),
    St2#state { scrn = s_erase_bos(St#state.scrn) };
t_erase_screen(St, 2) -> %% erase entire screen
    St1 = t_erase_screen(St, 0, St#state.t_rows-1),
    St1#state { scrn = s_erase_screen(St#state.scrn) }.

t_erase_screen(St, From, To) when From =< To ->
    Y0  = (From * St#state.char_height),
    Y1  = ((To+1) * St#state.char_height),
    X0  = 0,
    X1 = (St#state.columns * St#state.char_width),
    W = X1 - X0,
    H = Y1 - Y0,
    Pixmap = get(pixmap),
    R = #gdk_rectangle { x = X0, y = Y0, width = W, height = H },
    Bg = if (St#state.flags band ?ATTR_INVERSE) == 0 ->
		 St#state.bg;
	    true ->
		 St#state.fg
	 end,
    gdk:draw_rectangle(Pixmap, Bg, true, X0, Y0, W, H),
    t_invalidate(St, R);
t_erase_screen(St, From, To) ->
    St.

t_set_tab(St) ->
    St.

t_clr_tab(St, 0) ->     %% clear tab at current pos
    St;
t_clr_tab(St, 3) ->     %% clear all tabs
    St.
    
t_print(St, 0) ->  %% print page
    s_dump(St#state.scrn),                %% debug
    St;
t_print(St, 1) ->  %% print line
    s_dump_row((St#state.scrn)#scrn.row),  %% debug
    St.

t_cursor_col(St, C0) ->
    C = if C0 < 0 -> 0;
	   C0 >= St#state.columns -> St#state.columns - 1;
	   true -> C0
	end,
    St#state { c = C, scrn = s_goto_col(St#state.scrn, C) }.

t_cursor_row(St, R0) ->
    R = if R0 < 0 -> 0;
	   R0 >= St#state.rows -> St#state.rows - 1;
	   true -> R0
	end,
    St#state { r = R, scrn = s_goto_row(St#state.scrn, R+St#state.r_top) }.

%% set cursor position
t_cursor_pos(St, R0, C0) ->
    t_cursor_col(t_cursor_row(St,R0), C0).

t_cursor_offs(St, R, C) ->  %% relative cursor movement
    t_cursor_pos(St, St#state.r + R, St#state.c + C).

t_next_line(St) ->
    R0 = St#state.r + 1,
    if R0 >= St#state.rows ->
	    t_scroll(St, up);
       true ->
	    t_cursor_row(St, R0)
    end.

t_prev_line(St) ->
    R0 = St#state.r - 1,
    if R0 < 0 ->
	    t_scroll(St, down);
       true ->
	    St#state { r = R0 }
    end.
    
t_attr(St, As) ->
    {ATTR,FG,BG} = 
	foldl(
	  fun(0,{A,Fg,Bg}) -> {0,0,0};
	     (1,{A,Fg,Bg}) -> {A bor ?ATTR_HIGHLIGHT,Fg,Bg};
	     (4,{A,Fg,Bg}) -> {A bor ?ATTR_UNDERLINE,Fg,Bg};
	     (5,{A,Fg,Bg}) -> {A bor ?ATTR_BLINKING,Fg,Bg};
	     (7,{A,Fg,Bg}) -> {A bor ?ATTR_INVERSE,Fg,Bg};
	     (Fg,{A,_,Bg}) when Fg >= 30, Fg =< 37 ->
		  {A bor ?ATTR_FG_COLOR, (Fg-30)+1, Bg};
	     (Bg,{A,Fg,_}) when Bg >= 40, Fg =< 47 ->
		  {A bor ?ATTR_BG_COLOR, Fg, (Bg-40)+1};
	     (_,X) -> X
	  end, {0,St#state.fg_color,St#state.bg_color}, As),
    Fl = (St#state.flags band (bnot ?ATTR_MASK)) bor ATTR,
    %% io:format("attr = ~p, fg=~p, bg=~p\n", [ATTR,FG,BG]),
    St#state { flags = Fl, fg_color = FG, bg_color = BG }. 

t_set_mode(St, Ms) ->
    St.

t_reset_mode(St, Ms) ->
    St.

t_reset(St) ->
    St.

t_charset(St, G) ->
    St.

t_charset(St, G, Set) ->
    St.

t_cursor_save(St) ->
    St#state { 
      cstate = #cursor_state {
	r = St#state.r,
	c = St#state.c,
	char_attr = St#state.flags band ?ATTR_MASK,
	char_set = St#state.char_set,
	fg_color = St#state.fg_color,
	bg_color = St#state.bg_color,
	r_top    = St#state.r_top,
	r_bot    = St#state.r_bot
       }
     }.

t_cursor_restore(St) ->
    #cursor_state { r = R, r_top = Rtop, r_bot = Rbot, 
		    c = C, char_set = Set, char_attr = Attr,
		    fg_color = Fg, bg_color = Bg }
	= St#state.cstate,
    Flags = St#state.flags band (bnot ?ATTR_MASK) bor Attr,
    t_cursor_pos(St#state { r_top = Rtop, r_bot = Rbot, 
			    char_set = Set, 
			    fg_color = Fg,
			    bg_color = Bg,
			    flags = Flags },
		 R, C).


t_keypad_mode(St) ->
    St.

t_numeric_mode(St) ->
    St.

t_led(St, Leds) ->
    foreach(
      fun(0) ->
	      {{Pixmap,Mask},_} = St#state.led_map,
	      foreach(
		fun(P) ->
			gtk:pixmap_set(P, Pixmap,Mask)
		end, tuple_to_list(St#state.leds));
	 (N) when N > 0, N =< size(St#state.leds) ->
	      {_, {Pixmap,Mask}} = St#state.led_map,
	      P = element(N, St#state.leds),
	      gtk:pixmap_set(P, Pixmap,Mask);
	 (_) ->
	      ignore
      end, Leds),
    St.


t_region(St, R1, R2) ->
    %% io:format("t_region: ~p,~p\n", [R1,R2]),
    if R1 < R2, R1 >= 0, R1 < St#state.t_rows, R2 < St#state.t_rows ->
	    St1 = St#state { r_top = R1, r_bot = R2,
			     rows = (R2 - R1 + 1) },
	    t_cursor_pos(St1, 0, 0);
       true ->
	    St
    end.

t_test(St, Ts) ->
    St.

t_wide_line(St, Single, Where) ->
    St.

t_identify(St) ->
    St.

t_request(St0, Ps) ->
    foldl(
      fun(6,St) -> %% cursor position
	      t_send(St, "\e[" ++ integer_to_list(St#state.r+1) ++ ";" ++
		     integer_to_list(St0#state.c+1) ++ "R");
	 (5,St) -> %% status report (3n is not ok)
	      t_send(St, "\e[0n");
	 (_,St) ->
	      St
      end, St0, Ps).


t_device_attr(St,_) ->
    %% No options                  = 2#000
    %% STP (processor option)      = 2#001
    %% ACO (advanced video option) = 2#010
    %% GPO (graphic option)        = 2#100
    t_send(St, "\e[?1;0c").


t_beep(St) ->
    if St#state.flags band ?BEEP =/= 0 ->
	    %% add visible
	    gdk:beep(),
	    St;
       true ->
	    St
    end.

t_cut_clipboard(St) ->
    t_copy_clipboard(St),
    t_delete_selection(St),
    St.

t_copy_clipboard(St) ->
    St.

t_paste_clipboard(St) ->
    St.

t_delete_selection(St) ->
    St.


t_string(St, Text) ->
    if 
	Text == [] ->
	    St;
	St#state.c >= St#state.columns ->
	    t_string(t_cursor_col(t_next_line(St), 0), Text);
       true ->
	    %% max number of chars to write
	    N = St#state.columns - St#state.c, 
	    {Text1,Text2} = split_string(Text, N),
	    St1 = t_str(St, Text1),
	    t_string(St1, Text2)
    end.

t_str(St, Str) ->
    Len = length(Str),
    F = St#state.font,
    Y = ((St#state.r+St#state.r_top) * St#state.char_height),
    X = (St#state.c * St#state.char_width),
    W = Len * St#state.char_width,
    H = St#state.char_height,
    FColor = St#state.fg_color,
    BColor = St#state.bg_color,
    Pixmap = get(pixmap),
    Attr = St#state.flags band ?ATTR_MASK,

    FgGC = 
	if (Attr band ?ATTR_FG_COLOR) =/= 0, FColor > 0 ->
		gdk:gc_set_foreground(St#state.cfg, 
				     element(FColor, St#state.colormap)),
		St#state.cfg;
	   true ->
		St#state.fg
	end,

    BgGC = 
	if (Attr band ?ATTR_BG_COLOR) =/= 0, BColor > 0 ->
		gdk:gc_set_foreground(St#state.cbg,
				      element(BColor, St#state.colormap)),
		St#state.cbg;
	   true ->
		St#state.bg
	end,

    Fg = if (Attr band ?ATTR_INVERSE) == 0 -> FgGC; true -> BgGC end,
    Bg = if (Attr band ?ATTR_INVERSE) == 0 -> BgGC; true -> FgGC end,

    gdk:draw_rectangle(Pixmap, Bg, true, X, Y, W, H),
    Y0 = Y + F#gdk_font.ascent,
    gdk:draw_string(Pixmap, St#state.font, Fg, X, Y0, Str),
    if (Attr band ?ATTR_UNDERLINE) == 0 ->
	    ok;
       true ->
	    gdk:draw_line(Pixmap, Fg, X, Y0, X+St#state.char_width, Y0)
    end,

    Scrn = s_string(St#state.scrn, Str, Attr + ((FColor + 
						 (BColor bsl 4)) bsl 16)),
    
    St1 = t_invalidate(St, 
		       #gdk_rectangle { x = X, y = Y, width = W, height = H }),

    St1#state { c = St1#state.c + Len, scrn = Scrn }.


%% redraw all sccharacters
t_redraw_screen(St0) ->
    Scr = s_commit(St0#state.scrn),
    foldl(
      fun(R, St) -> t_redraw_line(R, St) end,
      St0#state { r = 0 },
      tuple_to_list(Scr#scrn.buf)),
    St0.


%% redraw a line (r,c are update)
t_redraw_line(Row, St0) ->
    Flags = St0#state.flags,
    St1 = St0#state { c = 0 },
    St2 = t_redraw_line(reverse(Row#srow.cb) ++ 
			[Row#srow.cr | Row#srow.ca],
			?ATTR_OFF,
			[],
			St1),
    St2#state { flags = Flags, r = St2#state.r + 1 }.

t_redraw_line([[C|Attr]|Cs], Attr, Acc, St) ->
    t_redraw_line(Cs, Attr, [C|Acc], St);
t_redraw_line([[C|NewAttr]|Cs], Attr, Acc, St) ->
    St1 = t_redraw_chars(reverse(Acc), Attr, St),
    t_redraw_line(Cs, NewAttr, [C], St1);
t_redraw_line([], Attr, Acc, St) ->
    t_redraw_chars(reverse(Acc), Attr, St).

t_redraw_chars([], Attr, St) ->
    St;
t_redraw_chars(Cs, Attr, St) ->
    N = length(Cs),
    F = St#state.font,
    Y = (St#state.r * St#state.char_height),
    X = (St#state.c * St#state.char_width),
    W = N*St#state.char_width,
    H = St#state.char_height,
    FColor = (Attr bsl 16) band 16#f,
    BColor = (Attr bsl 20) band 16#f,
    Pixmap = get(pixmap),
    R = #gdk_rectangle { x = X, y = Y, width = W, height = H },

    FgGC = 
	if (Attr band ?ATTR_FG_COLOR) =/= 0, FColor > 0 ->
		gdk:gc_set_foreground(St#state.cfg, 
				     element(FColor, St#state.colormap)),
		St#state.cfg;
	   true ->
		St#state.fg
	end,

    BgGC = 
	if (Attr band ?ATTR_BG_COLOR) =/= 0, BColor > 0 ->
		gdk:gc_set_forground(St#state.cbg,
				      element(BColor, St#state.colormap)),
		St#state.cbg;
	   true ->
		St#state.bg
	end,

    Fg = if (Attr band ?ATTR_INVERSE) == 0 -> FgGC; true -> BgGC end,
    Bg = if (Attr band ?ATTR_INVERSE) == 0 -> BgGC; true -> FgGC end,
    
    gdk:draw_rectangle(Pixmap, Bg, true, X, Y, W, H),
    Y0 = Y + F#gdk_font.ascent,
    gdk:draw_string(Pixmap, St#state.font, Fg, X, Y0, Cs),
    if (Attr band ?ATTR_UNDERLINE) == 0 ->
	    ok;
       true ->
	    gdk:draw_line(Pixmap, Fg, X, Y0, X+W, Y0)
    end,
    St#state { c = St#state.c + N }.


t_send(St, String) ->
    %% io:format("t_send ~p\n", [String]),
    send_connected(St#state.connected, {self(),{input,String}}),
    send_waiter(St#state.wait, St#state.input++String, St).

%%
split_string(String, Pos) ->
    if length(String) =< Pos -> {String, []};
       true -> split_string(String, [], Pos)
    end.

split_string(Tail, Head, 0) ->
    {reverse(Head), Tail};
split_string([C|Tail], Head, Pos) ->
    split_string(Tail, [C|Head], Pos-1);
split_string([], Head, Pos) ->
    {reverse(Head), []}.


%% split up in complete lines and 'r' for carraige return 
%% and 'n' for newline
split_text(Text, Flags) ->
    split_text(Text, [], [], Flags).

split_text([C|Cs], L, Acc, Flags) ->
    case C of
	$\r -> split_text(Cs, [], [r | add_text(L, Acc)], Flags);
	$\n -> split_text(Cs, [], [n | add_text(L, Acc)],Flags);
	$\b -> split_text(Cs, [], [b | add_text(L, Acc)],Flags);
	_   -> split_text(Cs, [C|L], Acc,Flags)
    end;
split_text([], L, Acc, Flags) -> reverse(add_text(L, Acc)).
    
add_text([], Acc) -> Acc;
add_text(Cs, Acc) -> [{chars,reverse(Cs)} | Acc].
%%  
%% send key codes to waiters, or buffer chars or store waiters
%%
send_waiter([Pid|Ps], [Key|Ks], St) ->
    Pid ! {self(),{key,Key}},
    send_waiter(Ps, Ks, St);
send_waiter(Ps, Ks, St) ->
    St#state {input = sublist(Ks,St#state.input_size), wait = Ps}.

send_connected([Pid|Ps], Term) ->
    Pid ! Term,
    send_connected(Ps, Term);
send_connected([], _) ->
    ok.
    

%% Font selection
font(Foundry, N) ->
    "-" ++ atom_to_list(Foundry) ++ 
	"-courier-medium-r-normal-*-*-" ++ integer_to_list(N*10) ++
	"-*-*-m-*-iso8859-1".

font(N) ->
    "-misc-fixed-medium-r-normal-*-*-" ++
	integer_to_list(N*10) ++ "-*-*-c-*-iso8859-1".

font() -> font(12).


led_on_xpm() ->
{
       "10 10 6 1\0",
       " 	c None\0",
       ".	c #2A9913\0",
       "+	c #39CC1A\0",
       "@	c #44F61F\0",
       "#	c #30AF16\0",
       "$	c #47FE21\0",
       "  .+@@+.  \0",
       " #$$$$$$# \0",
       ".$$$$$$$$.\0",
       "+$$$$$$$$+\0",
       "@$$$$$$$$@\0",
       "@$$$$$$$$@\0",
       "+$$$$$$$$+\0",
       ".$$$$$$$$.\0",
       " #$$$$$$# \0",
       "  .+@@+.  \0"}.

led_off_xpm() ->
{
       "10 10 6 1\0",
       " 	c None\0",
       ".	c #21720D\0",
       "+	c #20670C\0",
       "@	c #1A4E08\0",
       "#	c #22710D\0",
       "$	c #194908\0",
       "  .+@@+.  \0",
       " #$$$$$$# \0",
       ".$$$$$$$$.\0",
       "+$$$$$$$$+\0",
       "@$$$$$$$$@\0",
       "@$$$$$$$$@\0",
       "+$$$$$$$$+\0",
       ".$$$$$$$$.\0",
       " #$$$$$$# \0",
       "  .+@@+.  "}.


%% Map input characters (from keyboard and datain)

map_input([$\r | Cs], Flags) when (Flags band ?ICRNL) =/= 0 ->
    [$\n | map_input(Cs,Flags)];
map_input([$\r | Cs], Flags) when (Flags band ?IGNCR) =/= 0 ->
    map_input(Cs,Flags);
map_input([$\n | Cs], Flags) when (Flags band ?INLCR) =/= 0 ->
    [$\r | map_input(Cs,Flags)];
map_input([C | Cs], Flags) ->
    [C | map_input(Cs,Flags)];
map_input([], Flags) ->
    [].


%% Map output characters

map_output([$\n | Cs], Flags) when (Flags band ?ONLRET) =/= 0 ->
    [$\r,$\n | map_output(Cs,Flags)];
map_output([C | Cs], Flags) ->
    [C | map_output(Cs,Flags)];
map_output([], Flags) ->
    [].

%% character maps



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% VT100 emulation
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


vt100(Cs0, St) ->
    vt100_emu(St#state.istate ++ Cs0, St).

vt100_emu(Cs, St) ->
    vt100_emu(Cs, [], St).

vt100_emu([C|Cs], Acc, St) ->
    if C < 32 ->
	    vt100_ctl(C, Cs, vt100_flush(Acc, St));
       true -> 
	    vt100_emu(Cs, [C|Acc], St)
    end;
vt100_emu([], Acc, St) ->
    St1 = vt100_flush(Acc, St),
    St1#state { istate = [] }.

vt100_flush([], St) -> St;
vt100_flush(Cs, St) -> t_string(St, reverse(Cs)).



vt100_ctl(?ESC, Cs, St) -> vt100_esc(Cs, St);
vt100_ctl($\^N, Cs, St) -> vt100_emu(Cs, t_charset(St, g1));
vt100_ctl($\^O, Cs, St) -> vt100_emu(Cs, t_charset(St, g0));
vt100_ctl(?NL,  Cs, St) -> vt100_emu(Cs, t_next_line(St));
vt100_ctl(?FF,  Cs, St) -> vt100_emu(Cs, t_next_line(St));
vt100_ctl(?VF,  Cs, St) -> vt100_emu(Cs, t_next_line(St));
vt100_ctl(?CR,  Cs, St) -> vt100_emu(Cs, t_cursor_col(St, 0));
vt100_ctl(?TAB,  Cs, St) ->
    C0 = St#state.c, %% fix me use tab list 
    vt100_emu(Cs, t_cursor_col(St, C0 + (8 - (C0 rem 8))));
vt100_ctl(?BS, Cs, St) ->
    C0 = St#state.c,
    if C0 =< 0 ->
	    vt100_emu(Cs, St);
       St#state.flags band ?ECHOE == 0 ->
	    vt100_emu(Cs, t_cursor_col(St, C0-1));
       true ->
	    St1 = t_cursor_col(St, C0-1),
	    St2 = t_string(St, " "),
	    vt100_emu(Cs, t_cursor_col(St2, C0-1))
    end;
vt100_ctl(?BEL, Cs, St) -> 
    vt100_emu(Cs, t_beep(St));
vt100_ctl(C, Cs, St) ->
    io:format("vt100 control char ^~c not handled\n", [ (C - 1) + $A]),
    vt100_emu(Cs, St).


vt100_esc(Cs0, St) ->
    case Cs0 of
	"D"  ++ Cs  -> vt100_emu(Cs, t_next_line(St));
	"M"  ++ Cs  -> vt100_emu(Cs, t_prev_line(St));
	"7"  ++ Cs  -> vt100_emu(Cs, t_cursor_save(St));
	"8"  ++ Cs  -> vt100_emu(Cs, t_cursor_restore(St));
	"="  ++ Cs  -> vt100_emu(Cs, t_keypad_mode(St));
	">"  ++ Cs  -> vt100_emu(Cs, t_numeric_mode(St));
	"H"  ++ Cs  -> vt100_emu(Cs, t_set_tab(St));
	"c"  ++ Cs  -> vt100_emu(Cs, t_reset(St));
	"(A" ++ Cs  -> vt100_emu(Cs, t_charset(St,g0,uk));
	"(B" ++ Cs  -> vt100_emu(Cs, t_charset(St,g0,ascii));
	"(0" ++ Cs  -> vt100_emu(Cs, t_charset(St,g0,graph));
	"(1" ++ Cs  -> vt100_emu(Cs, t_charset(St,g0,alt1));
	"(2" ++ Cs  -> vt100_emu(Cs, t_charset(St,g0,alt2));
	")A" ++ Cs  -> vt100_emu(Cs, t_charset(St,g1,uk));
	")B" ++ Cs  -> vt100_emu(Cs, t_charset(St,g1,us));
	")0" ++ Cs  -> vt100_emu(Cs, t_charset(St,g1,graph));
	"(1" ++ Cs  -> vt100_emu(Cs, t_charset(St,g1,alt1));
	"(2" ++ Cs  -> vt100_emu(Cs, t_charset(st,g1,alt2));
	"#3" ++ Cs  -> vt100_emu(Cs, t_wide_line(St,on,top));
	"#4" ++ Cs  -> vt100_emu(Cs, t_wide_line(St,on,bottom));
	"#5" ++ Cs  -> vt100_emu(Cs, t_wide_line(St,off,off));
	"#6" ++ Cs  -> vt100_emu(Cs, t_wide_line(St,on,on));
	"#8" ++ Cs  -> vt100_emu(Cs, t_test(St, $E));
	[?LB | Cs]  -> vt100_csi(Cs, 0, [], [?LB,?ESC], St);
	[]          -> St#state { istate = "\e"};
	[C|Cs] ->
	    io:format("vt100,escap sequence ESC~c not handled\n", [C]), 
	    vt100_emu(Cs, St)
    end.


vt100_csi([C|Cs], P, Ps, Acc,St) when C >= $0, C =< $9 ->
    vt100_csi(Cs, P*10 + (C-$0), Ps, [C|Acc], St);
vt100_csi([$;|Cs], P, Ps,Acc, St) ->
    vt100_csi(Cs, 0, [P|Ps],[$;|Acc],St);
vt100_csi([C|Cs], P, Ps0, _,St) ->
    case {C, reverse([P|Ps0])} of
	{$A,[Pn]} ->
	    PN = if Pn == 0 -> 1; true -> Pn end,
	    vt100_emu(Cs,t_cursor_offs(St,-PN,0));
	{$B,[Pn]} ->
	    PN = if Pn == 0 -> 1; true -> Pn end,
	    vt100_emu(Cs,t_cursor_offs(St,PN,0));
	{$C,[Pn]} ->
	    PN = if Pn == 0 -> 1; true -> Pn end,
	    vt100_emu(Cs,t_cursor_offs(St,0,PN));
	{$D,[Pn]} ->
	    PN = if Pn == 0 -> 1; true -> Pn end,
	    vt100_emu(Cs,t_cursor_offs(St,0,-PN));
	{$q,Ps} ->
	    vt100_emu(Cs,t_led(St,Ps));
	{$m,Ps} ->
	    vt100_emu(Cs,t_attr(St,Ps));
	{$H,[0]} ->
	    vt100_emu(Cs,t_cursor_pos(St,0,0));
	{$H,[Pn,Pm]} ->
	    vt100_emu(Cs,t_cursor_pos(St,Pn-1,Pm-1));
	{$f,[0]}     ->
	    vt100_emu(Cs,t_cursor_pos(St,0,0));
	{$f,[Pn,Pm]} ->
	    vt100_emu(Cs,t_cursor_pos(St,Pn-1,Pm-1));
	{$r,[0]}     ->
	    vt100_emu(Cs,t_region(St,0,St#state.t_rows-1));
	{$r,[Pn,Pm]} ->
	    vt100_emu(Cs,t_region(St,Pn-1,Pm-1));
	{$K,[Pn]}    ->
	    vt100_emu(Cs,t_erase_line(St,Pn));
	{$J,[Pn]}    ->
	    vt100_emu(Cs,t_erase_screen(St,Pn));
	{$g,[Pn]}    ->
	    vt100_emu(Cs,t_clr_tab(St,Pn));
	{$i,[Pn]}    -> 
	    vt100_emu(Cs,t_print(St,Pn));
	{$y,[Pn]}    ->
	    vt100_emu(Cs,t_test(St,Pn));
	{$h,Ps}      ->
	    vt100_emu(Cs,t_set_mode(St,Ps));
	{$l,Ps}      ->
	    vt100_emu(Cs,t_reset_mode(St,Ps));
	{$n,Ps}      ->
	    vt100_emu(Cs,t_request(St,Ps));
	{Code,Ps} ->
	    io:format("vt100,csi sequence ~c ~p not handled\n", [Code,Ps]), 
	    vt100_emu(Cs,St)
    end;
vt100_csi([], P, Ps0, Acc, St) ->
    St#state { istate = reverse(Acc) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% VT52 emulation
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vt52(Cs0, St) ->
    vt52_emu(St#state.istate ++ Cs0, St).


vt52_emu([C|Cs], St) ->
    if C < 32 -> vt52_ctl(C, Cs, St);
       true -> vt52_emu(Cs, t_string(St, [C]))
    end;
vt52_emu([], St) ->
    St#state { istate = [] }.

vt52_ctl(?ESC, Cs, St) -> vt52_esc(Cs, St);
vt52_ctl($\^N, Cs, St) -> vt52_emu(Cs, t_charset(St, g1));
vt52_ctl($\^O, Cs, St) -> vt52_emu(Cs, t_charset(St, g0));
vt52_ctl($\n,  Cs, St) -> vt52_emu(Cs, t_next_line(St));
vt52_ctl($\f,  Cs, St) -> vt52_emu(Cs, t_next_line(St));
vt52_ctl($\v,  Cs, St) -> vt52_emu(Cs, t_next_line(St));
vt52_ctl($\r,  Cs, St) -> vt52_emu(Cs, t_cursor_col(St, 0));
vt52_ctl($\t,  Cs, St) ->
    C0 = St#state.c, %% fix me use tab list 
    vt52_emu(Cs, t_cursor_col(St, C0 + (8 - (C0 rem 8))));
vt52_ctl($\b, Cs, St) ->
    C0 = St#state.c,
    if C0 =< 0 ->
	    vt52_emu(Cs, St);
       St#state.flags band ?ECHOE == 0 ->
	    vt52_emu(Cs, t_cursor_col(St, C0-1));
       true ->
	    St1 = t_cursor_col(St, C0-1),
	    St2 = t_string(St1, " "),
	    vt52_emu(Cs, t_cursor_col(St2, C0-1))
    end;
vt52_ctl(C, Cs, St) ->
    io:format("vt52 control char ^~c not handled\n", [ (C - 1) + $A]),
    vt52_emu(Cs, St).

vt52_esc(Cs0, St) ->
    case Cs0 of
	"A"++Cs -> vt52_emu(Cs, t_cursor_offs(St,-1,0));
	"B"++Cs -> vt52_emu(Cs, t_cursor_offs(St,1,0));
	"C"++Cs -> vt52_emu(Cs, t_cursor_offs(St,0,1));
	"D"++Cs -> vt52_emu(Cs, t_cursor_offs(St,0,-1));
	"F"++Cs -> vt52_emu(Cs, t_charset(St,g0));
	"G"++Cs -> vt52_emu(Cs, t_charset(St,g1));
	"H"++Cs -> vt52_emu(Cs, t_cursor_pos(St,0,0));
	"I"++Cs -> vt52_emu(Cs, t_prev_line(St));
	"J"++Cs -> vt52_emu(Cs, t_erase_screen(St,0));
	"K"++Cs -> vt52_emu(Cs, t_erase_line(St,0));
	"Z"++Cs -> vt52_emu(Cs, t_identify(St));
	"="++Cs -> vt52_emu(Cs,St);  %% Enter Alternate keypad mode
	">"++Cs -> vt52_emu(Cs,St);  %% Exit Alternate keypad mode
	"<"++Cs -> vt100_emu(Cs,St#state { ifun = fun vt100/2 });
	"Y"++Cs ->
	    case Cs of
		[R,C | Cs1] ->
		    vt52_emu(Cs1, t_cursor_pos(St,R-8#37-1, C-8#37-1));
		_ ->
		    St#state { istate = "\eY"++Cs}
	    end;
	[] ->
	    St#state { istate = "\e" };
	[C | Cs] -> 
	    io:format("vt52,escape sequence ESC~c not handled\n", [C]), 
	    vt52_emu(Cs, St)	    
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Gtk callbacks and main routine
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

configure_event(Widget, Event, _) ->
    %% io:format("CONFIGURE ~p\n", [Event]),
    Allocation = gtk:widget_get_allocation(Widget),
    W = Allocation#gtk_allocation.width,
    H = Allocation#gtk_allocation.height,
    cast(self(), {set_size_wh,W,H}),
    true.


%% redraw from backing store
expose_event(Widget, Event, _) ->
    %% io:format("EXPOSE ~p\n", [Event]),
    Area = Event#gdk_event_expose.area,
    St = get('_state'),  %% FIXME
    case get(pixmap) of
	undefined -> 
	    true;
	Pixmap when St =/= undefined ->
	    gdk:draw_pixmap(St#state.window,
			    St#state.fg,
			    Pixmap,
			    Area#gdk_rectangle.x, Area#gdk_rectangle.y,
			    Area#gdk_rectangle.x, Area#gdk_rectangle.y,
			    Area#gdk_rectangle.width,
			    Area#gdk_rectangle.height),
	    true;
	_ ->
	    true
    end.

motion_notify_event(Widget, Event, _) ->
    %% io:format("MOTION: ~p\n", [Event]),
    true.

key_press_event(Widget, Event, _) ->
    if Event#gdk_event_key.length == 0 ->
	    %% io:format("KEYPRESS: ~p\n", [Event]),
	    case Event#gdk_event_key.keyval of
		?GDK_Up    -> cast(self(),{key, "\e[1A"});
		?GDK_Down  -> cast(self(),{key, "\e[1B"});
		?GDK_Right -> cast(self(),{key, "\e[1C"});
		?GDK_Left  -> cast(self(),{key, "\e[1D"});
		_ -> true
	    end;
       true ->
	    %% io:format("KEYS: ~p\n", [Event#gdk_event_key.string]),
	    cast(self(),{key, Event#gdk_event_key.string})
    end,
    gtk:signal_emit_stop_by_name(Widget, key_press_event),
    true.

    
handle_input(Message, St0) ->
    %% io:format("input = ~p\n", [Message]),
    %% Remove cursor
    St1 = if St0#state.flags band ?CURSOR_ON == 0 -> St0;
	     true -> t_cursor(St0)
	  end,
    St2 = handle_message(Message, St1),

    St3 = t_update(St2),
	
    %% Draw cursor
    St4 = if St3#state.flags band ?CURSOR_ON == 0 -> St3;
	     true -> t_cursor(St3)
	  end,
    {true, St4}.

 	
handle_message(Message, St) ->
    case Message of
	{call,From,Call} ->
	    {Reply,St1} = handle_call(Call, From, St),
	    reply(From, Reply),
	    St1;
	{cast,From,Cast} ->
	    St1 = handle_cast(Cast,From,St),
	    St1;
	Other ->
	    io:format("handle_message got ~p\n", [Other]),
	    St
    end.


handle_call({data,Data},From,St) ->
    St1 = apply(St#state.ifun,
		[map_output(map_input(Data,St#state.flags),
			    St#state.flags),St]),
    {ok, St1};

handle_call({output,Data},From,St) ->
    St1 = apply(St#state.ifun,
		[map_output(Data, St#state.flags),St]),
    {ok, St1};

handle_call({set_flags,Flags0},From,St) ->
    Flags = St#state.flags bor Flags0,
    St1 = handle_flags(St, Flags),
    {ok, St1};

handle_call(get_flags, From, St) ->
    { {ok, decode_flags(St#state.flags)}, St};

handle_call({unset_flags,Flags0},From,St) ->
    Flags = St#state.flags band (bnot Flags0),
    St1 = handle_flags(St, Flags),
    {ok, St1};

handle_call({toggle_flags,Flags0},From,St) ->
    Flags = St#state.flags bxor Flags0,
    St1 = handle_flags(St, Flags),
    {ok, St1};

handle_call({set_cusor, R, C},From,St) ->
    St1 = t_cursor_pos(St, R-1, C-1),
    {ok, St1};

handle_call({move_cursor, R, C},From,St) ->
    St1 = t_cursor_offs(St, R, C),
    {ok, St1};

handle_call({set_size_rc, R, C},From,St) ->
    St1 = t_set_size_rc(St,R,C),
    {ok, St1};

handle_call({set_size_wh, W, H},From,St) ->
    St1 = t_set_size_wh(St,W,H),
    {ok, St1};

handle_call({set_region, Top, Bot}, From, St) ->
    St1 = t_region(St, Top-1, Bot-1),
    {ok, St1};

handle_call({set_font, N},From,St) ->
    St1 = t_set_font(St, N),
    {ok, St1};

handle_call({erase_line, How},From,St) ->
    {ok, t_erase_line(St, How)};

handle_call({erase_screen, How},From,St) ->
    {ok, t_erase_screen(St, How)};

handle_call({scroll, How},From,St) ->
    {ok, t_scroll(St, How)};

handle_call(redraw,From,St) ->
    {ok, t_redraw_screen(St)};

handle_call(beep,From,St) ->
    {ok, t_beep(St)};

handle_call(dump,From,St) ->
    s_dump(St#state.scrn),
    {ok, St};


handle_call({get,What},From,St) ->
    case What of
	screen_cols ->
	    {St#state.t_columns, St};
	screen_rows ->
	    {St#state.t_rows, St};
	columns ->
	    {St#state.columns, St};
	rows ->
	    {St#state.rows, St};
	attributes ->
	    {decode_flags(St#state.flags band ?ATTR_MASK), St};
	fg_color ->
	    {St#state.fg_color, St};
	bg_color ->
	    {St#state.bg_color, St};
	cursor ->
	    { {St#state.r, St#state.c}, St};
	_ ->
	    {undefined, St}
    end;

handle_call(save, From, St) ->
    {ok, t_cursor_save(St)};

handle_call(restore, From, St) ->
    {ok, t_cursor_restore(St)};

    
handle_call({connect,Pid},From,St) ->
    Cons = [Pid|St#state.connected],
    {ok, St#state { connected = Cons }};

handle_call(stop,From,St) ->
    send_connected(St#state.connected, {self(),eof}),
    gtk:main_quit(),
    {ok, St};

handle_call(Call,From,St) ->
    io:format("gterm: got call ~p\n", [Call]),
    {error, St}.
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% handle_cast(Cast, State) -> State'
%%
%% Internal stuff is normally castes
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({key, Keys}, From, St) ->
    Keys1 = map_input(Keys, St#state.flags),
    St1 = if (St#state.flags band ?ECHO) == 0 ->
		  St;
	     true ->
		  apply(St#state.ifun,
			[map_output(Keys1,St#state.flags), St])
	  end,
    send_connected(St1#state.connected, {self(),{input,Keys1}}),
    send_waiter(St1#state.wait, St#state.input++Keys1, St1);

handle_cast(get_key, From,  St) ->
    [Pid|_] = From,
    send_waiter(St#state.wait ++ [Pid], St#state.input, St);

handle_cast({set_size_wh,W,H},From,St) ->
    t_set_size_wh(St,W,H);

handle_cast({set_flags,Flags0},From,St) ->
    Flags = St#state.flags bor Flags0,
    handle_flags(St, Flags);

handle_cast({unset_flags,Flags0},From,St) ->
    Flags = St#state.flags band (bnot Flags0),
    handle_flags(St, Flags);

handle_cast({toggle_flags,Flags0},From,St) ->
    Flags = St#state.flags bxor Flags0,
    handle_flags(St, Flags);

handle_cast({set_font,N},From,St) ->
    t_set_font(St, N);

handle_cast(stop, From, St) ->
    send_connected(St#state.connected, {self(),eof}),
    gtk:main_quit(),
    St;
    
handle_cast(Cast, From,  St) ->
    io:format("gterm: got cast ~p\n", [Cast]),
    St.


start_blinking(St) ->
    if St#state.timer =/= undefined ->
	    St;
       true ->
	    Timer = gtk:timeout_add(300, fun timeout/1, []),
	    St#state { timer = Timer }
    end.

stop_blinking(St) ->
    if St#state.timer == undefined ->
	    St;
       true ->
	    gtk:timeout_remove(St#state.timer),
	    St#state { timer = undefined }
    end.


handle_flags(St0, NewFlags) ->
    OldFlags = St0#state.flags,
    St1 = St0#state { flags = NewFlags },
    if OldFlags == NewFlags -> St0;
       OldFlags band ?CURSOR_BLINKING == 0,
       NewFlags band ?CURSOR_BLINKING =/= 0 ->
	    start_blinking(St1);
       OldFlags band ?CURSOR_BLINKING =/= 0,
       NewFlags band ?CURSOR_BLINKING == 0 ->
	    stop_blinking(St1);
       true ->
	    St1
    end.


timeout(Data) ->
    case get(blink) of
	undefined ->
	    cast(self(), {set_flags, ?CURSOR_ON}),
	    put(blink, on);
	off ->
	    cast(self(), {set_flags, ?CURSOR_ON}),
	    put(blink, on);
	on ->
	    cast(self(), {unset_flags, ?CURSOR_ON}),
	    put(blink, off)
    end,
    true.

quit () ->
    gtk:main_quit().

    

run() ->
    gtk:start(),
    Pid = spawn_link(?MODULE, main, [self()]),
    receive
	{Pid, ok} ->
	    {ok, Pid}
    end.



menu(Menu, [{Name,Callback,Arg,SubItems} | Items]) ->
    Item = gtk:menu_item_new_with_label(Name),
    gtk:menu_append(Menu, Item),
    gtk:signal_connect(Item, 'activate', Callback, Arg),
    gtk:widget_show(Item),
    if SubItems == [] ->
	    menu(Menu, Items);
       true ->
	    SubMenu = menu(gtk:menu_new(), SubItems),
	    gtk:menu_item_set_submenu(Item, SubMenu),
	    gtk:widget_show(SubMenu),
	    menu(Menu, Items)
    end;
menu(Menu, []) ->
    Menu.

menubar(SubItems) ->
    MBar    = gtk:menu_bar_new(),    
    gtk:menu_bar_set_shadow_type(MBar, 'GTK_SHADOW_OUT'),
    menu(MBar, SubItems).


m_toggle(_, Flags) ->
    cast(self(), {toggle_flags, Flags}).

m_font(_, N) ->
    cast(self(), {set_font, N}).

colormap(Colors) ->
    Cmap = gdk:colormap_get_system(),    
    list_to_tuple(colormap(Cmap, Colors)).

colormap(Cmap, [Color | Colors]) ->
    case gdk:color_alloc(Cmap, Color) of
	{true, Color1} ->
	    [Color1 | colormap(Cmap, Colors)];
	{false,_ } ->
	    io:format("couldn't allocate color ~p\n", [Color]),
	    [ Color | colormap(Cmap, Colors)]
    end;
colormap(Cmap, []) ->
    [].

    

main(Starter) ->
    Window = gtk:window_new ('GTK_WINDOW_TOPLEVEL'),
    gtk:window_set_policy(Window, true, true, true),
    gtk:widget_set_name (Window, "gterm"),
    gtk:window_set_title(Window, "Terminal"),

    Vbox = gtk:vbox_new (false, 0),    
    gtk:container_add (?GTK_CONTAINER (Window), Vbox),
    gtk:widget_show (Vbox),

    gtk:signal_connect (?GTK_OBJECT (Window), 'delete_event',
			fun(_,_,_) -> 
				cast(self(), stop)
			end, ?NULL),

    %% Add a menu bar
    MBar = 
	menubar(
	  [ 
	    { "File", fun(_,_) -> true end, [],
	      [{"Quit", fun(_, _) -> 
				cast(self(), stop)
			end,[], []}]},

	    { "Edit", fun(_,_) -> true end, [],
	      [{"_Cut", fun(_, _) -> false end,[], []},
	       {"_Copy", fun(_, _) -> false end,[], []},
	       {"_Paste", fun(_, _) -> false end,[], []},
	       {"_", fun(_, _) -> false end,[], []},
	       {"_Delete", fun(_, _) -> false end,[], []}
	      ]},
	      
	    { "Font", fun(_,_) -> true end, [],
	      [{"8",  fun m_font/2, 8, []},
	       {"10",  fun m_font/2, 10, []},
	       {"12",  fun m_font/2, 12, []},
	       {"14",  fun m_font/2, 14, []},
	       {"18",  fun m_font/2, 18, []},
	       {"24",  fun m_font/2, 24, []}
	      ] },

	    { "Options", fun(_,_) -> true end, [],
	      [
	       {"Cursor", fun m_toggle/2, ?CURSOR_ON, []},
	       {"Cursor/Blinking", fun m_toggle/2, ?CURSOR_BLINKING, []},
	       {"Cursor/Underline", fun m_toggle/2, ?CURSOR_UNDERLINE, []},
	       {"Local echo", fun m_toggle/2, ?ECHO, []},
	       {"NL => CR NL", fun m_toggle/2, ?ONLRET, []},
	       {"CR => NL", fun m_toggle/2, ?ICRNL, []}
	       ]}
	   ]),

    gtk:box_pack_start(?GTK_BOX(Vbox), MBar, false, false, 0),
    gtk:widget_show(MBar),

    %% Load a fixed font
    FontString  = font(12),
    Font        = gdk:font_load(FontString),
    %% io:format("font: ~s = ~p\n", [FontString, Font]),
    Char_width  = gdk:char_width(Font, $0),
    Char_height = Font#gdk_font.ascent + Font#gdk_font.descent,

    W = Char_width * 80,
    H = Char_height * 24,

    %% Create the drawing area 
    Drawing_area = gtk:drawing_area_new (),
    gtk:drawing_area_size (?GTK_DRAWING_AREA (Drawing_area), W, H),
    gtk:box_pack_start (?GTK_BOX(Vbox), Drawing_area, true, true, 0),
    ?GTK_WIDGET_SET_FLAGS(Drawing_area, ?GTK_CAN_FOCUS),
    gtk:widget_grab_focus(Drawing_area),
    gtk:widget_show (Drawing_area),

    gtk:widget_add_events (Drawing_area, ?GDK_KEY_PRESS_MASK),

    %% Signals used to handle backing pixmap 

    gtk:signal_connect (?GTK_OBJECT (Drawing_area), 'expose_event',
			fun expose_event/3, ?NULL),
    gtk:signal_connect (?GTK_OBJECT(Drawing_area), 'configure_event',
			fun configure_event/3, ?NULL),

    gtk:signal_connect (?GTK_OBJECT (Drawing_area), 'motion_notify_event',
			fun motion_notify_event/3, ?NULL),

    gtk:signal_connect (?GTK_OBJECT (Drawing_area), 'key_press_event',
			fun key_press_event/3, ?NULL),

    gtk:signal_connect (?GTK_OBJECT (Drawing_area), 'button_press_event',
			fun (Widget, _, _) ->
				gtk:widget_grab_focus(Widget)
			end, ?NULL),

    %% add a separator
    Sep = gtk:hseparator_new(),
    gtk:box_pack_start(?GTK_BOX(Vbox), Sep, false, true, 1),
    gtk:widget_show(Sep),

    %% show window here, otherwise we can not create leds
    gtk:widget_show (Window),

    GdkWindow = gtk:widget_get_window(Drawing_area),
    Style     = gtk:widget_get_style(Drawing_area),
    State = ?GTK_WIDGET_STATE(Drawing_area),
    SFg  = gtk:style_get_fg_gc(Style, State),
    SBg  = gtk:style_get_bg_gc(Style, State),

    Fg = gdk:gc_new(GdkWindow),
    Bg = gdk:gc_new(GdkWindow),    
    gdk:gc_copy(Fg, SFg),
    gdk:gc_copy(Bg, SBg),

    CFg = gdk:gc_new(GdkWindow),
    CBg = gdk:gc_new(GdkWindow),

    gdk:gc_copy(CFg, Fg),
    gdk:gc_copy(CBg, Bg),

    WinGdk   = gtk:widget_get_window(Window),
    WinStyle = gtk:widget_get_style(Window),
    WinState = ?GTK_WIDGET_STATE(Window),
    BgColor = gtk:style_get_bg(WinStyle,WinState),
    {Pixmap_on,Mask_on} = gdk:pixmap_create_from_xpm_d(WinGdk, BgColor,
						       led_on_xpm()),    
    {Pixmap_off,Mask_off} = gdk:pixmap_create_from_xpm_d(WinGdk, BgColor,
						       led_off_xpm()),    
    LED1 = gtk:pixmap_new( Pixmap_off, Mask_off), 
    gtk:widget_show(LED1),
    LED2 = gtk:pixmap_new( Pixmap_off, Mask_off), 
    gtk:widget_show(LED2),
    LED3 = gtk:pixmap_new( Pixmap_off, Mask_off), 
    gtk:widget_show(LED3),
    LED4 = gtk:pixmap_new( Pixmap_off, Mask_off), 
    gtk:widget_show(LED4),

    LEDbox = gtk:hbox_new(false,0),
    gtk:box_pack_end(?GTK_BOX(Vbox),  LEDbox, false, true, 2),
    gtk:box_pack_end (?GTK_BOX(LEDbox), LED4, false, false, 2),    
    gtk:box_pack_end (?GTK_BOX(LEDbox), LED3, false, false, 2),    
    gtk:box_pack_end (?GTK_BOX(LEDbox), LED2, false, false, 2),    
    gtk:box_pack_end (?GTK_BOX(LEDbox), LED1, false, false, 2),
    gtk:widget_show(LEDbox),

    %% Make color list
    Colors = lists:map(
	       fun(RGB) ->
		       R = if RGB band 1 == 1 -> 16#ffff; true -> 0 end,
		       G = if RGB band 2 == 2 -> 16#ffff; true -> 0 end,
		       B = if RGB band 4 == 4 -> 16#ffff; true -> 0 end,
		       #gdk_color{ red =R, green = G, blue = B }
	       end, lists:seq(0, 7)),
    %% Allocate the colors
    ColorMap = colormap(Colors),

    I1 = gtk:input_add(fun handle_input/2),

    St = #state { r         = 0,
		  c         = 0,
		  columns   = 80,
		  rows      = 24,
		  r_top     = 0,
		  r_bot     = 23,
		  t_rows    = 24,
		  t_columns = 80,
		  window = GdkWindow,
		  widget = Drawing_area,
		  font = Font,
		  char_width = Char_width,
		  char_height = Char_height,
		  style = Style,
		  fg = Fg,
		  bg = Bg,
		  cfg = CFg,
		  cbg = CBg,
		  leds = {LED1,LED2,LED3,LED4},
		  colormap = ColorMap,
		  led_map = {{Pixmap_off, Mask_off},{Pixmap_on, Mask_on}},
		  scrn = s_new(24,80)
		 },

    St1 = t_cursor_save(St), %% initial state 

    St2 = if St1#state.flags band ?CURSOR_BLINKING =/= 0 ->
		  start_blinking(St1);
	       true ->
		  St1
	  end,

    Starter ! {self(), ok},
    gtk:main (St2).

