%%%----------------------------------------------------------------------
%%% File    : slang_int.hrl
%%% Author  : Claes Wikstrom <klacke@kaja.hemma.net>
%%% Purpose : 
%%% Created :  5 Dec 2000 by Claes Wikstrom <klacke@kaja.hemma.net>
%%%----------------------------------------------------------------------


-author('klacke@kaja.hemma.net').





%% ops list


-define(INIT_TTY,        1).
-define(SET_ABORT_SIGNAL,2).
-define(GETKEY,          3).
-define(RESET_TTY,       4).
-define(KP_GETKEY,       5).
-define(UNGETKEY,        6).
-define(SETVAR,          7).
-define(GETVAR,          8).
-define(KP_INIT,         9).

%% screen mgm

-define(SMG_FILL_REGION,       10).
-define(SMG_SET_CHAR_SET,      11).
-define(SMG_SUSPEND_SMG,       12).
-define(SMG_RESUME_SMG,        13).
-define(SMG_ERASE_EOL,         14).
-define(SMG_GOTORC,            15).
-define(SMG_ERASE_EOS,         16).
-define(SMG_REVERSE_VIDEO,     17).
-define(SMG_SET_COLOR,         18).
-define(SMG_NORMAL_VIDEO,      19).
-define(SMG_PRINTF,            20).
-define(SMG_VPRINTF,           21).
-define(SMG_WRITE_STRING,      22).
-define(SMG_WRITE_NSTRING,     23).
-define(SMG_WRITE_CHAR,        24).
-define(SMG_WRITE_NCHARS,      25).
-define(SMG_WRITE_WRAPPED_STRING, 26).
-define(SMG_CLS,               27).
-define(SMG_REFRESH,           28).
-define(SMG_TOUCH_LINES,       29).
-define(SMG_TOUCH_SCREEN,      30).
-define(SMG_INIT_SMG,          31).
-define(SMG_REINIT_SMG,        32).
-define(SMG_RESET_SMG,         33).
-define(SMG_CHAR_AT,           34).
-define(SMG_SET_SCREEN_START,  35).
-define(SMG_DRAW_HLINE,        36).
-define(SMG_DRAW_VLINE,        37).
-define(SMG_DRAW_OBJECT,       38).
-define(SMG_DRAW_BOX,          39).
-define(SMG_GET_COLUMN,        40).
-define(SMG_GET_RO,            41).
-define(SMG_FORWARD,           42).
-define(SMG_WRITE_COLOR_CHARS, 43).
-define(SMG_READ_RAW,          44).
-define(SMG_WRITE_RAW,         45).
-define(SMG_SET_COLOR_IN_REGION, 46).





%% a whole loong list of tt_ function ops

-define (TT_FLUSH_OUTPUT,        50).
-define (TT_SET_SCROLL_REGION,   51).
-define (TT_RESET_SCROLL_REGION, 52).
-define (TT_REVERSE_VIDEO,       53).
-define (TT_BOLD_VIDEO,          54).
-define (TT_BEGIN_INSERT,        55).
-define (TT_END_INSERT,          56).
-define (TT_DEL_EOL,             57).
-define (TT_GOTO_RC,             58).
-define (TT_DELETE_NLINES,       59).
-define (TT_DELETE_CHAR,         60).
-define (TT_ERASE_LINE,          61).
-define (TT_NORMAL_VIDEO,        62).
-define (TT_CLS,                 63).
-define (TT_BEEP,                64).
-define (TT_REVERSE_INDEX,       65).
-define (TT_SMART_PUTS,          66).
-define (TT_WRITE_STRING,        67).
-define (TT_PUTCHAR,             68).
-define (TT_INIT_VIDEO,          69).
-define (TT_RESET_VIDEO,         70).
-define (TT_GET_TERMINFO,        71).
-define (TT_GET_SCREEN_SIZE,     72).
-define (TT_SET_CURSOR_VISIBILITY, 73).
-define (TT_SET_MOUSE_MODE,      74).
-define (TT_INITIALIZE,          75).
-define (TT_ENABLE_CURSOR_KEYS,  76).
-define (TT_SET_TERM_VTXXX,      77).
-define (TT_SET_COLOR_ESC,       78).
-define (TT_WIDE_WIDTH,          79).
-define (TT_NARROW_WIDTH,        80).
-define (TT_SET_ALT_CHAR_SET,    81).
-define (TT_WRITE_TO_STATUS_LINE, 82).
-define (TT_DISABLE_STATUS_LINE,  83).
-define (TT_TGETSTR,             84).
-define (TT_TGETNUM,             85).
-define (TT_TGETFLAG,            86).
-define (TT_TIGETENT,            87).
-define (TT_TIGETSTR,            88).
-define (TT_TIGETNUM,            89).
-define (SLTT_GET_COLOR_OBJECT,  90).
-define (TT_SET_COLOR_OBJECT,    91).
-define (TT_SET_COLOR,           92).
-define (TT_SET_MONO,            93).
-define (TT_ADD_COLOR_ATTRIBUTE, 94).
-define (TT_SET_COLOR_FGBG,      95).


%% aux functions
-define(ISATTY,                  100).
-define(EFORMAT,                 101).
-define(SIGNAL,                  102).
-define(SIGNAL_CHECK,            103).


%% int macros

%%
%% Int to bytes
%%
-define(int8(X), [(X) band 16#ff]).

-define(int16(X), [((X) bsr 8) band 16#ff, (X) band 16#ff]).

-define(int24(X), [((X) bsr 16) band 16#ff,
                   ((X) bsr 8) band 16#ff, (X) band 16#ff]).

-define(int32(X), 
        [((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
         ((X) bsr 8) band 16#ff, (X) band 16#ff]).

%% Bytes to unsigned
-define(u64(X7,X6,X5,X4,X3,X2,X1,X0), 
        ( ((X7) bsl 56) bor ((X6) bsl 48) bor ((X5) bsl 40) bor
          ((X4) bsl 32) bor ((X3) bsl 24) bor ((X2) bsl 16) bor 
          ((X1) bsl 8) bor (X0)  )).

-define(u32(X3,X2,X1,X0), 
        (((X3) bsl 24) bor ((X2) bsl 16) bor ((X1) bsl 8) bor (X0))).

-define(u24(X2,X1,X0),
        (((X2) bsl 16) bor ((X1) bsl 8) bor (X0))).

-define(u16(X1,X0),
        (((X1) bsl 8) bor (X0))).
 
-define(u8(X0), (X0)).

%% Bytes to signed
-define(i32(X3,X2,X1,X0),
        (?u32(X3,X2,X1,X0) - 
         (if (X3) > 127 -> 16#100000000; true -> 0 end))).

-define(i24(X2,X1,X0),
        (?u24(X2,X1,X0) - 
         (if (X2) > 127 -> 16#1000000; true -> 0 end))).
        
-define(i16(X1,X0),
        (?u16(X1,X0) - 
         (if (X1) > 127 -> 16#10000; true -> 0 end))).

-define(i8(X0),
        (?u8(X0) -
         (if (X0) > 127 -> 16#100; true -> 0 end))).



