%%%----------------------------------------------------------------------
%%% File    : slang.hrl
%%% Author  : Claes Wikstrom <klacke@kaja.hemma.net>
%%% Purpose : 
%%% Created : 22 Nov 2000 by Claes Wikstrom <klacke@kaja.hemma.net>
%%%----------------------------------------------------------------------

-author('klacke@kaja.hemma.net').


%% Keypad constants
-define (SL_KEY_ERR,		16#FFFF).
-define (SL_KEY_UP,		16#101).
-define (SL_KEY_DOWN,		16#102).
-define (SL_KEY_LEFT,		16#103).
-define (SL_KEY_RIGHT,		16#104).
-define (SL_KEY_PPAGE,		16#105).
-define (SL_KEY_NPAGE,		16#106).
-define (SL_KEY_HOME,		16#107).
-define (SL_KEY_END,		16#108).
-define (SL_KEY_A1,		16#109).
-define (SL_KEY_A3,		16#10A).
-define (SL_KEY_B2,		16#10B).
-define (SL_KEY_C1,		16#10C).
-define (SL_KEY_C3,		16#10D).
-define (SL_KEY_REDO,		16#10E).
-define (SL_KEY_UNDO,		16#10F).
-define (SL_KEY_BACKSPACE,	16#110).
-define (SL_KEY_ENTER,		16#111).
-define (SL_KEY_IC,		16#112).
-define (SL_KEY_DELETE,		16#113).
-define (SL_KEY_F0,		16#200).
-define (SL_KEY_F(X),		(?SL_KEY_F0 + X)).



%% define some common signal numbers
-define(SIGINT,  1).
-define(SIGTSTP, 2).
-define(SIGQUIT, 3).
-define(SIGTTOU, 4).
-define(SIGTTIN, 5).
-define(SIGWINCH,6).



%% variable defines
-define(baud_rate,         1).
-define(read_fd,           2).
-define(abort_char,        3).
-define(ignore_user_abort, 4).
-define(input_buffer_len,  5).
-define(keyboard_quit,     6).
-define(last_key_char,     7).
-define(rl_eof_char,       8).
-define(rline_quit,        9).
-define(screen_rows,       10).
-define(screen_cols,       11).
-define(tab_width,         12).
-define(newline_behaviour, 13).
-define(error,             14).
-define(version,           15).
-define(backspace_moves,   16).
-define(display_eight_bit, 17).


-define('NEWLINE_IGNORED',	0). %% default
-define('NEWLINE_MOVES',        1). %%  moves to next line, column 0
-define('NEWLINE_SCROLLS',      2). %% moves but scrolls at bottom of screen
-define('NEWLINE_PRINTABLE',    3). %% prints as ^J





-ifdef (debug).
-define(Debug(F, A),
   slang:debug(?FILE,?LINE, F, A)).
-else.
-define(Debug(F, A),debug_disabled).
-endif.



