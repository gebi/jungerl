%%% FILE    : slang.erl
%%% Author  : Claes Wikstrom <klacke@kaja.hemma.net>
%%% Purpose : interface to the cool multi platform tty slang lib
%%% Created : 22 Nov 2000 by Claes Wikstrom <klacke@kaja.hemma.net>
%%%----------------------------------------------------------------------

-module(slang).
-author('klacke@kaja.hemma.net').

-include("../include/slang.hrl").
-include("slang_int.hrl").

-compile(export_all).


stop_user() ->
    Flag=process_flag(trap_exit, true),
    {links, Lks0} = process_info(whereis(user),links),
    Lks = lists:delete(whereis(error_logger), Lks0),
    
    Hs = gen_event:which_handlers(error_logger),
    
    case lists:member(error_logger_tty_h, Hs) of
        true ->
	    error_logger:delete_report_handler(error_logger_tty_h),
	    

            supervisor:terminate_child(kernel_sup, user),
	    lists:foreach(fun(Pid) ->
				  exit(Pid, kill)
                          end, Lks),
	    {stopdata, true};
        false ->
	    {stopdata, false}
    end.


restart_user({stopdata, TTY_H}) ->
    case TTY_H of
	true ->
	    supervisor:restart_child(kernel_sup, user),
	    wait_user(whereis(user)),
	    error_logger:add_report_handler(error_logger_tty_h);
	false ->
	    supervisor:restart_child(kernel_sup, user)
    end.


%% restart chaild ain't synced
wait_user(undefined) ->
        timer:sleep(200),
        wait_user(whereis(user));
wait_user(_) ->
        ok.


init_tty(AbortChar, FlowControl, Opost) ->
    P = gp(),
    p_cmd(P, ?INIT_TTY, [{int, AbortChar}, 
			 {int, FlowControl}, 
			 {int, Opost}], int32).

set_abort_signal(null) ->
    P = gp(),
    p_cmd(P, ?SET_ABORT_SIGNAL, [{int, 0}], int32).

getkey() ->
    P = gp(),
     p_cmd(P, ?GETKEY, [], int32).

kp_getkey() ->
    P = gp(),
    p_cmd(P, ?KP_GETKEY, [], int32).


kp_init() ->
    P = gp(),
    p_cmd(P, ?KP_INIT, [], int32).

reset_tty() ->
    P = gp(),
    p_cmd(P, ?RESET_TTY, [], void).


ungetkey(Char) ->
    P = gp(),
    p_cmd(P, ?GETKEY, [{char, Char}], void).


%% read slang lib global variables
getvar(Var) when atom(Var) ->
    P = gp(),
    p_cmd(P, ?GETVAR, [{int, encode_var(Var)}], int).

%% set slang lib global variables
setvar(Var, IntegerValue) when atom(Var) ->
    P = gp(),
    p_cmd(P, ?SETVAR, [{int, encode_var(Var)}, {int, IntegerValue}], void).


isatty(Fd) ->
    P = gp(),
    p_cmd(P, ?ISATTY, [{int, Fd}], int).

eformat(Fmt, Args) ->
    P = gp(),
    p_cmd(P,?EFORMAT, [{string, lists:flatten(io_lib:format(Fmt,Args))}],void).


%% install Fun/0 as a Fun to be invoked
%% on signal Sig

signal(Sig, Fun) ->
    P = gp(),
    put({signal_handler, Sig}, Fun),
    p_cmd(P,?SIGNAL, [{int, Sig}], void).


%%% screen management


smg_fill_region (R, C, Nr, Nc, Ch) ->
    P = gp(),
    p_cmd(P,?SMG_FILL_REGION, [{int,R}, {int, C},
			       {int, Nr}, {int, Nc}, 
			       {char, Ch}], void).

smg_set_char_set (A) ->
    P = gp(),
    p_cmd(P, ?SMG_SET_CHAR_SET, [{int, A}], void).

smg_suspend_smg () ->
    P = gp(),
    p_cmd(P, ?SMG_SUSPEND_SMG, [], int).

smg_resume_smg () ->
    P = gp(),
    p_cmd(P, ?SMG_RESUME_SMG, [], int).

smg_erase_eol () ->
    P = gp(),
    p_cmd(P, ?SMG_ERASE_EOL, [], void).

smg_gotorc (R, C) ->
    P = gp(),
    p_cmd(P, ?SMG_GOTORC, [{int, R}, {int, C}], void).

smg_erase_eos () ->
    P = gp(),
    p_cmd(P, ?SMG_ERASE_EOS, [], void).

smg_reverse_video () ->
    P = gp(),
    p_cmd(P, ?SMG_REVERSE_VIDEO, [], void).

smg_set_color (C) ->
    P = gp(),
    p_cmd(P, ?SMG_SET_COLOR, [{int, C}], void).

smg_normal_video () ->
    P = gp(),
    p_cmd(P, ?SMG_NORMAL_VIDEO, [], void).

smg_printf (Format, Args) ->
    P = gp(),
    Str = io_lib:format(Format, Args),
    p_cmd(P, ?SMG_WRITE_STRING, [{string, lists:flatten(Str)}], void).

smg_vprintf () ->
    exit(nyi).

smg_write_string (Str) ->
    P = gp(),
    p_cmd(P, ?SMG_WRITE_STRING, [{string, Str}], void).

smg_write_nstring (S, N) ->
    L = lists:flatten(N),
    Len = length(L),
    if
	Len < N ->
	    smg_write_string(L ++ lists:duplicate(N - Len, 32));
	true ->
	    smg_write_string(L)
    end.

smg_write_char (Ch) ->
    P = gp(),
    p_cmd(P, ?SMG_WRITE_CHAR, [{char, Ch}], void).

smg_write_nchars (S, N) ->
    L = lists:sublist(lists:flatten(S), N),
    smg_write_string(L).

smg_write_wrapped_string (S, R, C, Nr, Nc, Fill) ->
    P = gp(),
    p_cmd(P, ?SMG_WRITE_WRAPPED_STRING, [{string, S},{int, R}, {int, C},
					{int, Nr}, {int, Nc}, {int, Fill}], 
	  void).

smg_cls () ->
    P = gp(),
    p_cmd(P, ?SMG_CLS, [], void).

smg_refresh () ->
    P = gp(),
    p_cmd(P, ?SMG_REFRESH, [], void).

smg_touch_lines (R, Nr) ->
    P = gp(),
    p_cmd(P, ?SMG_TOUCH_LINES, [{int, R}, {int, Nr}], void).

smg_touch_screen () ->
    P = gp(),
    p_cmd(P, ?SMG_TOUCH_SCREEN, [], void).

smg_init_smg () ->
    P = gp(),
    p_cmd(P, ?SMG_INIT_SMG, [], int).

smg_reinit_smg () ->
    P = gp(),
    p_cmd(P, ?SMG_REINIT_SMG, [], void).

smg_reset_smg () ->
    P = gp(),
    p_cmd(P, ?SMG_RESET_SMG, [], void).

smg_char_at () ->
    P = gp(),
    p_cmd(P, ?SMG_CHAR_AT, [], int).

%% 0 == NULL
smg_set_screen_start (R, C) ->
    P = gp(),
    p_cmd(P, ?SMG_SET_SCREEN_START, [{int, R}, {int, C}], int_int).

smg_draw_hline (Len) ->
    P = gp(),
    p_cmd(P, ?SMG_DRAW_HLINE, [{int, Len}], void).

smg_draw_vline (Len) ->
    P = gp(),
    p_cmd(P, ?SMG_DRAW_VLINE, [{int, Len}], void).

smg_draw_object (R, C, Obj) ->
    P = gp(),
    p_cmd(P, ?SMG_DRAW_OBJECT, [{int, R}, {int, C}, {int, Obj}], void).

smg_draw_box (R, C, Dr, Dc) ->
    P = gp(),
    p_cmd(P, ?SMG_DRAW_BOX, [], void).

smg_get_column () ->
    P = gp(),
    p_cmd(P, ?SMG_GET_COLUMN, [], int).

smg_get_row () ->
    P = gp(),
    p_cmd(P, ?SMG_GET_RO, [], int).

smg_forward (N) ->
    P = gp(),
    p_cmd(P, ?SMG_FORWARD, [{int, N}], void).

smg_write_color_chars (S, Len) ->
    P = gp(),
    p_cmd(P, ?SMG_WRITE_COLOR_CHARS, [{smg_char_type, S}, {int, Len}], void).

smg_read_raw (Len) ->
    P = gp(),
    p_cmd(P, ?SMG_READ_RAW, [{int, Len}], smg_char_type).

smg_write_raw (Str, Len) ->
    P = gp(),
    p_cmd(P, ?SMG_WRITE_RAW, [{smg_char_type, Str}, {int, Len}], int).

smg_set_color_in_region (Color, R, C, Dr, Dc) ->
    P = gp(),
    p_cmd(P, ?SMG_SET_COLOR_IN_REGION, [{int, Color}, {int, R}, {int, C},
				       {int, Dr}, {int, Dc}], void).





%%%%%%% auxilliary functions %%%%%%%%%%%%%%%%%


encode_var(baud_rate) ->         1;
encode_var(read_fd) ->           2;
encode_var(abort_char) ->        3;
encode_var(ignore_user_abort) -> 4;
encode_var(input_buffer_len) ->  5;
encode_var(keyboard_quit) ->     6;
encode_var(last_key_char) ->     7;
encode_var(rl_eof_char) ->       8;
encode_var(rline_quit) ->        9;
encode_var(screen_rows) ->       10;
encode_var(screen_cols) ->       11;
encode_var(tab_width) ->         12;
encode_var(newline_behaviour) -> 13;
encode_var(error) ->             14;
encode_var(version) ->           15;
encode_var(backspace_moves) ->   16;
encode_var(display_eight_bit) -> 17.



p_cmd(P, Op, ArgList, void) ->
    Cmd = [Op | mk_args(ArgList)],
    %?Debug("CMD ~p~n", [Cmd]),
    P ! {self(), {command, Cmd}};


p_cmd(P, Op, ArgList, Expect) ->
    Cmd = [Op | mk_args(ArgList)],
    P ! {self(), {command, Cmd}},
    case rec_loop(P, Expect, nosig) of
	{Reply, nosig} ->
	    Reply;
	{Reply, SignalFun} ->
	    SignalFun(),
	    Reply
    end.

rec_loop(P, Expect, Sig) ->
    receive 
	{P, {data, [1 |What]}} ->
	    {expect(What, Expect), Sig};
	{P, {data, [0 , X1, X2, X3, X4]}} ->
	    SigNo = ?i32(X1,X2, X3, X4),
	    case get({signal_handler, SigNo}) of
		undefined ->
		    rec_loop(P, Expect, Sig);
		Fun ->
		    rec_loop(P, Expect, Fun)
	    end;
	{'EXIT', P, Reason} ->
	    exit(Reason)
    after 200 ->
	    P ! {self(), {command, [255]}}, % tick 
	    rec_loop(P, Expect, Sig)
    end.


expect(List, smg_char_type) ->
    upack_smg_char_type(List);
expect([X1,X2, X3, X4], int32) ->
    ?i32(X1,X2, X3, X4);
expect([X1,X2, X3, X4], int) ->
    ?i32(X1,X2, X3, X4);
expect([X1,X2, X3, X4, Y1, Y2, Y3, Y4], int_int) ->
    {?i32(X1,X2, X3, X4), ?i32(Y1, Y2, Y3, Y4)};

expect(List, string) ->
    List.


upack_smg_char_type([X1, X2 |Tail]) ->
    [?u16(X1, X2) | upack_smg_char_type(Tail)];
upack_smg_char_type([]) ->
    [];
upack_smg_char_type([X]) ->
    [X].


mk_args([]) ->
    [];
mk_args([{int, Int} |Tail]) when integer(Int) ->
    [?int32(Int) | mk_args(Tail)];
mk_args([{char, Char} |Tail]) when integer(Char) ->
    [Char| mk_args(Tail)];
mk_args([{string, Str} |Tail]) when list(Str) ->
    [Str, 0 | mk_args(Tail)];
mk_args([{string, Str} |Tail]) when atom(Str) ->
    [atom_to_list(Str), 0 | mk_args(Tail)];
mk_args([{smg_char_type, Str} |Tail]) when list(Str) ->
    Len = 2 * length(Str),
    List = [?int32(Len) | lists:map(fun(I) -> ?int16(I) end, Str)] ,
    [List| mk_args(Tail)].
    

open_slang_driver() ->
    erl_ddll:start(),
    Path=case code:priv_dir(slang) of
	     {error, _} ->
		 {ok, Dir, _} =  regexp:sub(code:which(slang),
					    "ebin/slang.beam",[]),
		 Dir ++ "/priv";
	     Dir ->
		 Dir
	 end,
    case erl_ddll:load_driver(Path, "slang_drv") of
	ok ->
	    ok;
	{error,{already_started, _}} ->
	    ok;
	{error, What} ->
	    error_logger:format("Failed to open driver ~p~n", [What]),
	    exit(nodriver)
    end,
    P = open_port({spawn, slang_drv}, []),
    P.


gp() ->
    case get(slang_port) of
	undefined ->
	    Port = open_slang_driver(),
	    put(slang_port, Port),
	    Port;
	Port ->
	    Port
    end.


%% all the not so necessary tt_ functions
%% int
tt_flush_output() ->
    P = gp(),
    p_cmd(P, ?TT_FLUSH_OUTPUT, [], int).

%% void
tt_set_scroll_region(X,Y) ->
    P = gp(),
    p_cmd(P, ?TT_SET_SCROLL_REGION, [{int, X}, {int, Y}], void).

% void
tt_reset_scroll_region() ->
    P = gp(),
    p_cmd(P, ?TT_RESET_SCROLL_REGION,[],void).

tt_reverse_video(Int) ->
    P = gp(),
    p_cmd(P, ?TT_REVERSE_VIDEO,[{int, Int}],void).

tt_bold_video() ->
    P = gp(),
    p_cmd(P, ?TT_BOLD_VIDEO,[],void).

tt_begin_insert() ->
    P = gp(),
    p_cmd(P, ?TT_BEGIN_INSERT,[],void).

tt_end_insert() ->
    P = gp(),
    p_cmd(P, ?TT_END_INSERT,[],void).

tt_del_eol() ->
    P = gp(),
    p_cmd(P, ?TT_DEL_EOL,[],void).

tt_goto_rc() ->
    P = gp(),
    p_cmd(P, ?TT_GOTO_RC,[],void).

tt_delete_nlines(Int) ->
    P = gp(),
    p_cmd(P, ?TT_DELETE_NLINES,[{int, Int}],void).

tt_delete_char() ->
    P = gp(),
    p_cmd(P, ?TT_DELETE_CHAR,[],void).

tt_erase_line() ->
    P = gp(),
    p_cmd(P, ?TT_ERASE_LINE,[],void).

tt_normal_video() ->
    P = gp(),
    p_cmd(P, ?TT_NORMAL_VIDEO,[],void).

tt_cls() ->
    P = gp(),
    p_cmd(P, ?TT_CLS,[],void).

tt_beep() ->
    P = gp(),
    p_cmd(P, ?TT_BEEP,[],void).

tt_reverse_index(Int) ->
    P = gp(),
    p_cmd(P, ?TT_REVERSE_INDEX,[{int, Int}],void).

tt_smart_puts(S1, S2, X, Y) ->
    P = gp(),
    p_cmd(P, ?TT_SMART_PUTS,[{smg_char_type, S1}, {smg_char_type, S2},
			     {int, X}, {int, Y}],void).

tt_write_string(Str) ->
    P = gp(),
    p_cmd(P, ?TT_WRITE_STRING,[{string, Str}],void).

tt_putchar(Char) ->
    P = gp(),
    p_cmd(P, ?TT_PUTCHAR,[{int, Char}],void).

tt_init_video() ->
    P = gp(),
    p_cmd(P, ?TT_INIT_VIDEO,[],int).

tt_reset_video() ->
    P = gp(),
    p_cmd(P, ?TT_RESET_VIDEO,[],void).

tt_get_terminfo() ->
    P = gp(),
    p_cmd(P, ?TT_GET_TERMINFO,[],void).

tt_get_screen_size() ->
    P = gp(),
    p_cmd(P, ?TT_GET_SCREEN_SIZE,[],void).

tt_set_cursor_visibility(Int) ->
    P = gp(),
    p_cmd(P, ?TT_SET_CURSOR_VISIBILITY,[{int, Int}],int).

tt_set_mouse_mode(X, Y) ->
    P = gp(),
    p_cmd(P, ?TT_SET_MOUSE_MODE,[{int, X}, {int, Y}],int).

tt_initialize(Str) ->
    P = gp(),
    p_cmd(P, ?TT_INITIALIZE,[{string, Str}],int).


tt_enable_cursor_keys() ->
    P = gp(),
    p_cmd(P, ?TT_ENABLE_CURSOR_KEYS,[],void).

tt_set_term_vtxxx() ->
    exit(nyi),
    P = gp(),
    p_cmd(P, ?TT_SET_TERM_VTXXX,[],void).

tt_set_color_esc(Int, Str) ->
    P = gp(),
    p_cmd(P, ?TT_SET_COLOR_ESC,[{int, Int}, {string, Str}],void).

tt_wide_width() ->
    P = gp(),
    p_cmd(P, ?TT_WIDE_WIDTH,[],void).

tt_narrow_width() ->
    P = gp(),
    p_cmd(P, ?TT_NARROW_WIDTH,[],void).

tt_set_alt_char_set() ->
    P = gp(),
    p_cmd(P, ?TT_SET_ALT_CHAR_SET,[],void).

tt_write_to_status_line(Int, Str) ->
    P = gp(),
    p_cmd(P, ?TT_WRITE_TO_STATUS_LINE,[{int, Int}, {string, Str}],void).

tt_disable_status_line() ->
    P = gp(),
    p_cmd(P, ?TT_DISABLE_STATUS_LINE,[],void).

tt_tgetstr(Str) ->
    P = gp(),
    p_cmd(P, ?TT_TGETSTR,[{string, Str}],string).

tt_tgetnum(Str) ->
    P = gp(),
    p_cmd(P, ?TT_TGETNUM,[{string, Str}],int).

tt_tgetflag(Str) ->
    P = gp(),
    p_cmd(P, ?TT_TGETFLAG,[{string, Str}],int).

tt_tigetent(Str) ->
    P = gp(),
    p_cmd(P, ?TT_TIGETENT,[{string, Str}],string).


tt_tigetstr() ->
    exit(nyi),
    P = gp(),
    p_cmd(P, ?TT_TIGETSTR,[],void).

tt_tigetnum() ->
    exit(nyi),
    P = gp(),
    p_cmd(P, ?TT_TIGETNUM,[],void).

sltt_get_color_object(Int) ->
    P = gp(),
    p_cmd(P, ?SLTT_GET_COLOR_OBJECT,[{int, Int}], int).

tt_set_color_object(Int, CType) ->
    P = gp(),
    p_cmd(P, ?TT_SET_COLOR_OBJECT,[{int, Int}, {int, CType}],void).

tt_set_color(Obj, Name, Fg, Bg) ->
    P = gp(),
    p_cmd(P, ?TT_SET_COLOR,[{int, Obj}, {string, Name},
			    {string, Fg}, {string, Bg}],void).

tt_set_mono(Int, Str, Attr) ->
    P = gp(),
    p_cmd(P, ?TT_SET_MONO,[{int, Int}, {string, Str},
			   {int, Attr}],void).

tt_add_color_attribute(Int, Ctype) ->
    P = gp(),
    p_cmd(P, ?TT_ADD_COLOR_ATTRIBUTE,[{int, Int}, {int, Ctype}],void).

tt_set_color_fgbg(Int, CT1, CT2) ->
    P = gp(),
    p_cmd(P, ?TT_SET_COLOR_FGBG,[{int, Int}, {int, CT1}, {int, CT2}],void).



%% debug slang apps by tail -f 'ing /tmp/slang.debug
%% use ?DEBUG(F, A) or slang:debug/2,

debug(File, Line, Fmt, Args) ->
    DFD = case get(slang_debug_fd) of
	      undefined ->
		  case file:open("/tmp/slang.debug", [append]) of
		      {ok, Fd} ->
			  put(slang_debug_fd, Fd),
			  Fd;
		      Err ->
			  exit({nodebugfd, Err})
		  end;
	      Fd ->
		  Fd
	  end,
    
    Str = lists:flatten(
            io_lib:format("DEBUG ~s:~p, pid ~w: ~n",
                          [filename:basename(File), 
                           Line, self()])),
    
    case io:format(DFD, Str ++ Fmt ++ "~n", Args) of
        ok -> ok;
        _ -> io:format(DFD, "ERROR ~p:~p: Pid ~w: (bad format)~n~p,~p~n",
		       [File, Line, self(), Fmt, Args]),
	     
	     ok
    end.

