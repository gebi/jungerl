-ifndef(__XKEYSYM_HRL__).
-define(__XKEYSYM_HRL__, true).

-define(XK_VoidSymbol, 16#FFFFFF). %% void symbol 

-define(XK_BackSpace, 16#FF08). %% back space, back char 
-define(XK_Tab, 16#FF09).
-define(XK_Linefeed, 16#FF0A). %% Linefeed, LF 
-define(XK_Clear, 16#FF0B).
-define(XK_Return, 16#FF0D). %% Return, enter 
-define(XK_Pause, 16#FF13). %% Pause, hold 
-define(XK_Scroll_Lock, 16#FF14).
-define(XK_Sys_Req, 16#FF15).
-define(XK_Escape, 16#FF1B).
-define(XK_Delete, 16#FFFF). %% Delete, rubout 

%% International & multi-key character composition 

-define(XK_Multi_key, 16#FF20).  %% Multi-key character compose 
-define(XK_Codeinput, 16#FF37).
-define(XK_SingleCandidate, 16#FF3C).
-define(XK_MultipleCandidate, 16#FF3D).
-define(XK_PreviousCandidate, 16#FF3E).

%% Japanese keyboard support 

-define(XK_Kanji, 16#FF21). %% Kanji, Kanji convert 
-define(XK_Muhenkan, 16#FF22).  %% Cancel Conversion 
-define(XK_Henkan_Mode, 16#FF23).  %% Start/Stop Conversion 
-define(XK_Henkan, 16#FF23).  %% Alias for Henkan_Mode 
-define(XK_Romaji, 16#FF24).  %% to Romaji 
-define(XK_Hiragana, 16#FF25).  %% to Hiragana 
-define(XK_Katakana, 16#FF26).  %% to Katakana 
-define(XK_Hiragana_Katakana, 16#FF27).  %% Hiragana/Katakana toggle 
-define(XK_Zenkaku, 16#FF28).  %% to Zenkaku 
-define(XK_Hankaku, 16#FF29).  %% to Hankaku 
-define(XK_Zenkaku_Hankaku, 16#FF2A).  %% Zenkaku/Hankaku toggle 
-define(XK_Touroku, 16#FF2B).  %% Add to Dictionary 
-define(XK_Massyo, 16#FF2C).  %% Delete from Dictionary 
-define(XK_Kana_Lock, 16#FF2D).  %% Kana Lock 
-define(XK_Kana_Shift, 16#FF2E).  %% Kana Shift 
-define(XK_Eisu_Shift, 16#FF2F).  %% Alphanumeric Shift 
-define(XK_Eisu_toggle, 16#FF30).  %% Alphanumeric toggle 
-define(XK_Kanji_Bangou, 16#FF37).  %% Codeinput 
-define(XK_Zen_Koho, 16#FF3D). %% Multiple/All Candidate(s) 
-define(XK_Mae_Koho, 16#FF3E). %% Previous Candidate 

%% 16#FF31 thru 16#FF3F are under XK_KOREAN 

%% Cursor control & motion 

-define(XK_Home, 16#FF50).
-define(XK_Left, 16#FF51). %% Move left, left arrow 
-define(XK_Up, 16#FF52). %% Move up, up arrow 
-define(XK_Right, 16#FF53). %% Move right, right arrow 
-define(XK_Down, 16#FF54). %% Move down, down arrow 
-define(XK_Prior, 16#FF55). %% Prior, previous 
-define(XK_Page_Up, 16#FF55).
-define(XK_Next, 16#FF56). %% Next 
-define(XK_Page_Down, 16#FF56).
-define(XK_End, 16#FF57). %% EOL 
-define(XK_Begin, 16#FF58). %% BOL 


%% Misc Functions 

-define(XK_Select, 16#FF60). %% Select, mark 
-define(XK_Print, 16#FF61).
-define(XK_Execute, 16#FF62). %% Execute, run, do 
-define(XK_Insert, 16#FF63). %% Insert, insert here 
-define(XK_Undo, 16#FF65). %% Undo, oops 
-define(XK_Redo, 16#FF66). %% redo, again 
-define(XK_Menu, 16#FF67).
-define(XK_Find, 16#FF68). %% Find, search 
-define(XK_Cancel, 16#FF69). %% Cancel, stop, abort, exit 
-define(XK_Help, 16#FF6A). %% Help 
-define(XK_Break, 16#FF6B).
-define(XK_Mode_switch, 16#FF7E). %% Character set switch 
-define(XK_script_switch, 16#FF7E).  %% Alias for mode_switch 
-define(XK_Num_Lock, 16#FF7F).

%% Keypad Functions, keypad numbers cleverly chosen to map to ascii 

-define(XK_KP_Space, 16#FF80). %% space 
-define(XK_KP_Tab, 16#FF89).
-define(XK_KP_Enter, 16#FF8D). %% enter 
-define(XK_KP_F1, 16#FF91). %% PF1, KP_A, ... 
-define(XK_KP_F2, 16#FF92).
-define(XK_KP_F3, 16#FF93).
-define(XK_KP_F4, 16#FF94).
-define(XK_KP_Home, 16#FF95).
-define(XK_KP_Left, 16#FF96).
-define(XK_KP_Up, 16#FF97).
-define(XK_KP_Right, 16#FF98).
-define(XK_KP_Down, 16#FF99).
-define(XK_KP_Prior, 16#FF9A).
-define(XK_KP_Page_Up, 16#FF9A).
-define(XK_KP_Next, 16#FF9B).
-define(XK_KP_Page_Down, 16#FF9B).
-define(XK_KP_End, 16#FF9C).
-define(XK_KP_Begin, 16#FF9D).
-define(XK_KP_Insert, 16#FF9E).
-define(XK_KP_Delete, 16#FF9F).
-define(XK_KP_Equal, 16#FFBD). %% equals 
-define(XK_KP_Multiply, 16#FFAA).
-define(XK_KP_Add, 16#FFAB).
-define(XK_KP_Separator, 16#FFAC). %% separator, often comma 
-define(XK_KP_Subtract, 16#FFAD).
-define(XK_KP_Decimal, 16#FFAE).
-define(XK_KP_Divide, 16#FFAF).

-define(XK_KP_0, 16#FFB0).
-define(XK_KP_1, 16#FFB1).
-define(XK_KP_2, 16#FFB2).
-define(XK_KP_3, 16#FFB3).
-define(XK_KP_4, 16#FFB4).
-define(XK_KP_5, 16#FFB5).
-define(XK_KP_6, 16#FFB6).
-define(XK_KP_7, 16#FFB7).
-define(XK_KP_8, 16#FFB8).
-define(XK_KP_9, 16#FFB9).

%%
%% Auxilliary Functions; note the duplicate definitions for left and right
%% function keys;  Sun keyboards and a few other manufactures have such
%% function key groups on the left and/or right sides of the keyboard.
%% We've not found a keyboard with more than 35 function keys total.
 

-define(XK_F1, 16#FFBE).
-define(XK_F2, 16#FFBF).
-define(XK_F3, 16#FFC0).
-define(XK_F4, 16#FFC1).
-define(XK_F5, 16#FFC2).
-define(XK_F6, 16#FFC3).
-define(XK_F7, 16#FFC4).
-define(XK_F8, 16#FFC5).
-define(XK_F9, 16#FFC6).
-define(XK_F10, 16#FFC7).
-define(XK_F11, 16#FFC8).
-define(XK_L1, 16#FFC8).
-define(XK_F12, 16#FFC9).
-define(XK_L2, 16#FFC9).
-define(XK_F13, 16#FFCA).
-define(XK_L3, 16#FFCA).
-define(XK_F14, 16#FFCB).
-define(XK_L4, 16#FFCB).
-define(XK_F15, 16#FFCC).
-define(XK_L5, 16#FFCC).
-define(XK_F16, 16#FFCD).
-define(XK_L6, 16#FFCD).
-define(XK_F17, 16#FFCE).
-define(XK_L7, 16#FFCE).
-define(XK_F18, 16#FFCF).
-define(XK_L8, 16#FFCF).
-define(XK_F19, 16#FFD0).
-define(XK_L9, 16#FFD0).
-define(XK_F20, 16#FFD1).
-define(XK_L10, 16#FFD1).
-define(XK_F21, 16#FFD2).
-define(XK_R1, 16#FFD2).
-define(XK_F22, 16#FFD3).
-define(XK_R2, 16#FFD3).
-define(XK_F23, 16#FFD4).
-define(XK_R3, 16#FFD4).
-define(XK_F24, 16#FFD5).
-define(XK_R4, 16#FFD5).
-define(XK_F25, 16#FFD6).
-define(XK_R5, 16#FFD6).
-define(XK_F26, 16#FFD7).
-define(XK_R6, 16#FFD7).
-define(XK_F27, 16#FFD8).
-define(XK_R7, 16#FFD8).
-define(XK_F28, 16#FFD9).
-define(XK_R8, 16#FFD9).
-define(XK_F29, 16#FFDA).
-define(XK_R9, 16#FFDA).
-define(XK_F30, 16#FFDB).
-define(XK_R10, 16#FFDB).
-define(XK_F31, 16#FFDC).
-define(XK_R11, 16#FFDC).
-define(XK_F32, 16#FFDD).
-define(XK_R12, 16#FFDD).
-define(XK_F33, 16#FFDE).
-define(XK_R13, 16#FFDE).
-define(XK_F34, 16#FFDF).
-define(XK_R14, 16#FFDF).
-define(XK_F35, 16#FFE0).
-define(XK_R15, 16#FFE0).

%% Modifiers 

-define(XK_Shift_L, 16#FFE1). %% Left shift 
-define(XK_Shift_R, 16#FFE2). %% Right shift 
-define(XK_Control_L, 16#FFE3). %% Left control 
-define(XK_Control_R, 16#FFE4). %% Right control 
-define(XK_Caps_Lock, 16#FFE5). %% Caps lock 
-define(XK_Shift_Lock, 16#FFE6). %% Shift lock 

-define(XK_Meta_L, 16#FFE7). %% Left meta 
-define(XK_Meta_R, 16#FFE8). %% Right meta 
-define(XK_Alt_L, 16#FFE9). %% Left alt 
-define(XK_Alt_R, 16#FFEA). %% Right alt 
-define(XK_Super_L, 16#FFEB). %% Left super 
-define(XK_Super_R, 16#FFEC). %% Right super 
-define(XK_Hyper_L, 16#FFED). %% Left hyper 
-define(XK_Hyper_R, 16#FFEE). %% Right hyper 

%%
%% ISO 9995 Function and Modifier Keys
%% Byte 3 = 16#FE

-define(XK_ISO_Lock, 16#FE01).
-define(XK_ISO_Level2_Latch, 16#FE02).
-define(XK_ISO_Level3_Shift, 16#FE03).
-define(XK_ISO_Level3_Latch, 16#FE04).
-define(XK_ISO_Level3_Lock, 16#FE05).
-define(XK_ISO_Group_Shift, 16#FF7E). %% Alias for mode_switch 
-define(XK_ISO_Group_Latch, 16#FE06).
-define(XK_ISO_Group_Lock, 16#FE07).
-define(XK_ISO_Next_Group, 16#FE08).
-define(XK_ISO_Next_Group_Lock, 16#FE09).
-define(XK_ISO_Prev_Group, 16#FE0A).
-define(XK_ISO_Prev_Group_Lock, 16#FE0B).
-define(XK_ISO_First_Group, 16#FE0C).
-define(XK_ISO_First_Group_Lock, 16#FE0D).
-define(XK_ISO_Last_Group, 16#FE0E).
-define(XK_ISO_Last_Group_Lock, 16#FE0F).

-define(XK_ISO_Left_Tab, 16#FE20).
-define(XK_ISO_Move_Line_Up, 16#FE21).
-define(XK_ISO_Move_Line_Down, 16#FE22).
-define(XK_ISO_Partial_Line_Up, 16#FE23).
-define(XK_ISO_Partial_Line_Down, 16#FE24).
-define(XK_ISO_Partial_Space_Left, 16#FE25).
-define(XK_ISO_Partial_Space_Right, 16#FE26).
-define(XK_ISO_Set_Margin_Left, 16#FE27).
-define(XK_ISO_Set_Margin_Right, 16#FE28).
-define(XK_ISO_Release_Margin_Left, 16#FE29).
-define(XK_ISO_Release_Margin_Right, 16#FE2A).
-define(XK_ISO_Release_Both_Margins, 16#FE2B).
-define(XK_ISO_Fast_Cursor_Left, 16#FE2C).
-define(XK_ISO_Fast_Cursor_Right, 16#FE2D).
-define(XK_ISO_Fast_Cursor_Up, 16#FE2E).
-define(XK_ISO_Fast_Cursor_Down, 16#FE2F).
-define(XK_ISO_Continuous_Underline, 16#FE30).
-define(XK_ISO_Discontinuous_Underline, 16#FE31).
-define(XK_ISO_Emphasize, 16#FE32).
-define(XK_ISO_Center_Object, 16#FE33).
-define(XK_ISO_Enter, 16#FE34).

-define(XK_dead_grave, 16#FE50).
-define(XK_dead_acute, 16#FE51).
-define(XK_dead_circumflex, 16#FE52).
-define(XK_dead_tilde, 16#FE53).
-define(XK_dead_macron, 16#FE54).
-define(XK_dead_breve, 16#FE55).
-define(XK_dead_abovedot, 16#FE56).
-define(XK_dead_diaeresis, 16#FE57).
-define(XK_dead_abovering, 16#FE58).
-define(XK_dead_doubleacute, 16#FE59).
-define(XK_dead_caron, 16#FE5A).
-define(XK_dead_cedilla, 16#FE5B).
-define(XK_dead_ogonek, 16#FE5C).
-define(XK_dead_iota, 16#FE5D).
-define(XK_dead_voiced_sound, 16#FE5E).
-define(XK_dead_semivoiced_sound, 16#FE5F).
-define(XK_dead_belowdot, 16#FE60).
-define(XK_dead_hook, 16#FE61).
-define(XK_dead_horn, 16#FE62).

-define(XK_First_Virtual_Screen, 16#FED0).
-define(XK_Prev_Virtual_Screen, 16#FED1).
-define(XK_Next_Virtual_Screen, 16#FED2).
-define(XK_Last_Virtual_Screen, 16#FED4).
-define(XK_Terminate_Server, 16#FED5).

-define(XK_AccessX_Enable, 16#FE70).
-define(XK_AccessX_Feedback_Enable, 16#FE71).
-define(XK_RepeatKeys_Enable, 16#FE72).
-define(XK_SlowKeys_Enable, 16#FE73).
-define(XK_BounceKeys_Enable, 16#FE74).
-define(XK_StickyKeys_Enable, 16#FE75).
-define(XK_MouseKeys_Enable, 16#FE76).
-define(XK_MouseKeys_Accel_Enable, 16#FE77).
-define(XK_Overlay1_Enable, 16#FE78).
-define(XK_Overlay2_Enable, 16#FE79).
-define(XK_AudibleBell_Enable, 16#FE7A).

-define(XK_Pointer_Left, 16#FEE0).
-define(XK_Pointer_Right, 16#FEE1).
-define(XK_Pointer_Up, 16#FEE2).
-define(XK_Pointer_Down, 16#FEE3).
-define(XK_Pointer_UpLeft, 16#FEE4).
-define(XK_Pointer_UpRight, 16#FEE5).
-define(XK_Pointer_DownLeft, 16#FEE6).
-define(XK_Pointer_DownRight, 16#FEE7).
-define(XK_Pointer_Button_Dflt, 16#FEE8).
-define(XK_Pointer_Button1, 16#FEE9).
-define(XK_Pointer_Button2, 16#FEEA).
-define(XK_Pointer_Button3, 16#FEEB).
-define(XK_Pointer_Button4, 16#FEEC).
-define(XK_Pointer_Button5, 16#FEED).
-define(XK_Pointer_DblClick_Dflt, 16#FEEE).
-define(XK_Pointer_DblClick1, 16#FEEF).
-define(XK_Pointer_DblClick2, 16#FEF0).
-define(XK_Pointer_DblClick3, 16#FEF1).
-define(XK_Pointer_DblClick4, 16#FEF2).
-define(XK_Pointer_DblClick5, 16#FEF3).
-define(XK_Pointer_Drag_Dflt, 16#FEF4).
-define(XK_Pointer_Drag1, 16#FEF5).
-define(XK_Pointer_Drag2, 16#FEF6).
-define(XK_Pointer_Drag3, 16#FEF7).
-define(XK_Pointer_Drag4, 16#FEF8).
-define(XK_Pointer_Drag5, 16#FEFD).

-define(XK_Pointer_EnableKeys, 16#FEF9).
-define(XK_Pointer_Accelerate, 16#FEFA).
-define(XK_Pointer_DfltBtnNext, 16#FEFB).
-define(XK_Pointer_DfltBtnPrev, 16#FEFC).

%%
%% 3270 Terminal Keys
%% Byte 3 = 16#FD
 

-define(XK_3270_Duplicate, 16#FD01).
-define(XK_3270_FieldMark, 16#FD02).
-define(XK_3270_Right2, 16#FD03).
-define(XK_3270_Left2, 16#FD04).
-define(XK_3270_BackTab, 16#FD05).
-define(XK_3270_EraseEOF, 16#FD06).
-define(XK_3270_EraseInput, 16#FD07).
-define(XK_3270_Reset, 16#FD08).
-define(XK_3270_Quit, 16#FD09).
-define(XK_3270_PA1, 16#FD0A).
-define(XK_3270_PA2, 16#FD0B).
-define(XK_3270_PA3, 16#FD0C).
-define(XK_3270_Test, 16#FD0D).
-define(XK_3270_Attn, 16#FD0E).
-define(XK_3270_CursorBlink, 16#FD0F).
-define(XK_3270_AltCursor, 16#FD10).
-define(XK_3270_KeyClick, 16#FD11).
-define(XK_3270_Jump, 16#FD12).
-define(XK_3270_Ident, 16#FD13).
-define(XK_3270_Rule, 16#FD14).
-define(XK_3270_Copy, 16#FD15).
-define(XK_3270_Play, 16#FD16).
-define(XK_3270_Setup, 16#FD17).
-define(XK_3270_Record, 16#FD18).
-define(XK_3270_ChangeScreen, 16#FD19).
-define(XK_3270_DeleteWord, 16#FD1A).
-define(XK_3270_ExSelect, 16#FD1B).
-define(XK_3270_CursorSelect, 16#FD1C).
-define(XK_3270_PrintScreen, 16#FD1D).
-define(XK_3270_Enter, 16#FD1E).

%%
%%  Latin 1
%%  Byte 3 = 0
 
-define(XK_space, 16#020).
-define(XK_exclam, 16#021).
-define(XK_quotedbl, 16#022).
-define(XK_numbersign, 16#023).
-define(XK_dollar, 16#024).
-define(XK_percent, 16#025).
-define(XK_ampersand, 16#026).
-define(XK_apostrophe, 16#027).
-define(XK_quoteright, 16#027). %% deprecated 
-define(XK_parenleft, 16#028).
-define(XK_parenright, 16#029).
-define(XK_asterisk, 16#02a).
-define(XK_plus, 16#02b).
-define(XK_comma, 16#02c).
-define(XK_minus, 16#02d).
-define(XK_period, 16#02e).
-define(XK_slash, 16#02f).
-define(XK_0, 16#030).
-define(XK_1, 16#031).
-define(XK_2, 16#032).
-define(XK_3, 16#033).
-define(XK_4, 16#034).
-define(XK_5, 16#035).
-define(XK_6, 16#036).
-define(XK_7, 16#037).
-define(XK_8, 16#038).
-define(XK_9, 16#039).
-define(XK_colon, 16#03a).
-define(XK_semicolon, 16#03b).
-define(XK_less, 16#03c).
-define(XK_equal, 16#03d).
-define(XK_greater, 16#03e).
-define(XK_question, 16#03f).
-define(XK_at, 16#040).
-define(XK_A, 16#041).
-define(XK_B, 16#042).
-define(XK_C, 16#043).
-define(XK_D, 16#044).
-define(XK_E, 16#045).
-define(XK_F, 16#046).
-define(XK_G, 16#047).
-define(XK_H, 16#048).
-define(XK_I, 16#049).
-define(XK_J, 16#04a).
-define(XK_K, 16#04b).
-define(XK_L, 16#04c).
-define(XK_M, 16#04d).
-define(XK_N, 16#04e).
-define(XK_O, 16#04f).
-define(XK_P, 16#050).
-define(XK_Q, 16#051).
-define(XK_R, 16#052).
-define(XK_S, 16#053).
-define(XK_T, 16#054).
-define(XK_U, 16#055).
-define(XK_V, 16#056).
-define(XK_W, 16#057).
-define(XK_X, 16#058).
-define(XK_Y, 16#059).
-define(XK_Z, 16#05a).
-define(XK_bracketleft, 16#05b).
-define(XK_backslash, 16#05c).
-define(XK_bracketright, 16#05d).
-define(XK_asciicircum, 16#05e).
-define(XK_underscore, 16#05f).
-define(XK_grave, 16#060).
-define(XK_quoteleft, 16#060). %% deprecated 
-define(XK_a, 16#061).
-define(XK_b, 16#062).
-define(XK_c, 16#063).
-define(XK_d, 16#064).
-define(XK_e, 16#065).
-define(XK_f, 16#066).
-define(XK_g, 16#067).
-define(XK_h, 16#068).
-define(XK_i, 16#069).
-define(XK_j, 16#06a).
-define(XK_k, 16#06b).
-define(XK_l, 16#06c).
-define(XK_m, 16#06d).
-define(XK_n, 16#06e).
-define(XK_o, 16#06f).
-define(XK_p, 16#070).
-define(XK_q, 16#071).
-define(XK_r, 16#072).
-define(XK_s, 16#073).
-define(XK_t, 16#074).
-define(XK_u, 16#075).
-define(XK_v, 16#076).
-define(XK_w, 16#077).
-define(XK_x, 16#078).
-define(XK_y, 16#079).
-define(XK_z, 16#07a).
-define(XK_braceleft, 16#07b).
-define(XK_bar, 16#07c).
-define(XK_braceright, 16#07d).
-define(XK_asciitilde, 16#07e).

-define(XK_nobreakspace, 16#0a0).
-define(XK_exclamdown, 16#0a1).
-define(XK_cent, 16#0a2).
-define(XK_sterling, 16#0a3).
-define(XK_currency, 16#0a4).
-define(XK_yen, 16#0a5).
-define(XK_brokenbar, 16#0a6).
-define(XK_section, 16#0a7).
-define(XK_diaeresis, 16#0a8).
-define(XK_copyright, 16#0a9).
-define(XK_ordfeminine, 16#0aa).
-define(XK_guillemotleft, 16#0ab). %% left angle quotation mark 
-define(XK_notsign, 16#0ac).
-define(XK_hyphen, 16#0ad).
-define(XK_registered, 16#0ae).
-define(XK_macron, 16#0af).
-define(XK_degree, 16#0b0).
-define(XK_plusminus, 16#0b1).
-define(XK_twosuperior, 16#0b2).
-define(XK_threesuperior, 16#0b3).
-define(XK_acute, 16#0b4).
-define(XK_mu, 16#0b5).
-define(XK_paragraph, 16#0b6).
-define(XK_periodcentered, 16#0b7).
-define(XK_cedilla, 16#0b8).
-define(XK_onesuperior, 16#0b9).
-define(XK_masculine, 16#0ba).
-define(XK_guillemotright, 16#0bb). %% right angle quotation mark 
-define(XK_onequarter, 16#0bc).
-define(XK_onehalf, 16#0bd).
-define(XK_threequarters, 16#0be).
-define(XK_questiondown, 16#0bf).
-define(XK_Agrave, 16#0c0).
-define(XK_Aacute, 16#0c1).
-define(XK_Acircumflex, 16#0c2).
-define(XK_Atilde, 16#0c3).
-define(XK_Adiaeresis, 16#0c4).
-define(XK_Aring, 16#0c5).
-define(XK_AE, 16#0c6).
-define(XK_Ccedilla, 16#0c7).
-define(XK_Egrave, 16#0c8).
-define(XK_Eacute, 16#0c9).
-define(XK_Ecircumflex, 16#0ca).
-define(XK_Ediaeresis, 16#0cb).
-define(XK_Igrave, 16#0cc).
-define(XK_Iacute, 16#0cd).
-define(XK_Icircumflex, 16#0ce).
-define(XK_Idiaeresis, 16#0cf).
-define(XK_ETH, 16#0d0).
-define(XK_Eth, 16#0d0). %% deprecated 
-define(XK_Ntilde, 16#0d1).
-define(XK_Ograve, 16#0d2).
-define(XK_Oacute, 16#0d3).
-define(XK_Ocircumflex, 16#0d4).
-define(XK_Otilde, 16#0d5).
-define(XK_Odiaeresis, 16#0d6).
-define(XK_multiply, 16#0d7).
-define(XK_Ooblique, 16#0d8).
-define(XK_Oslash,   ?XK_Ooblique).
-define(XK_Ugrave, 16#0d9).
-define(XK_Uacute, 16#0da).
-define(XK_Ucircumflex, 16#0db).
-define(XK_Udiaeresis, 16#0dc).
-define(XK_Yacute, 16#0dd).
-define(XK_THORN, 16#0de).
-define(XK_Thorn, 16#0de). %% deprecated 
-define(XK_ssharp, 16#0df).
-define(XK_agrave, 16#0e0).
-define(XK_aacute, 16#0e1).
-define(XK_acircumflex, 16#0e2).
-define(XK_atilde, 16#0e3).
-define(XK_adiaeresis, 16#0e4).
-define(XK_aring, 16#0e5).
-define(XK_ae, 16#0e6).
-define(XK_ccedilla, 16#0e7).
-define(XK_egrave, 16#0e8).
-define(XK_eacute, 16#0e9).
-define(XK_ecircumflex, 16#0ea).
-define(XK_ediaeresis, 16#0eb).
-define(XK_igrave, 16#0ec).
-define(XK_iacute, 16#0ed).
-define(XK_icircumflex, 16#0ee).
-define(XK_idiaeresis, 16#0ef).
-define(XK_eth, 16#0f0).
-define(XK_ntilde, 16#0f1).
-define(XK_ograve, 16#0f2).
-define(XK_oacute, 16#0f3).
-define(XK_ocircumflex, 16#0f4).
-define(XK_otilde, 16#0f5).
-define(XK_odiaeresis, 16#0f6).
-define(XK_division, 16#0f7).
-define(XK_oslash, 16#0f8).
-define(XK_ooblique, ?XK_oslash).
-define(XK_ugrave, 16#0f9).
-define(XK_uacute, 16#0fa).
-define(XK_ucircumflex, 16#0fb).
-define(XK_udiaeresis, 16#0fc).
-define(XK_yacute, 16#0fd).
-define(XK_thorn, 16#0fe).
-define(XK_ydiaeresis, 16#0ff).

%%
%%   Latin 2
%%   Byte 3 = 1

-define(XK_Aogonek, 16#1a1).
-define(XK_breve, 16#1a2).
-define(XK_Lstroke, 16#1a3).
-define(XK_Lcaron, 16#1a5).
-define(XK_Sacute, 16#1a6).
-define(XK_Scaron, 16#1a9).
-define(XK_Scedilla, 16#1aa).
-define(XK_Tcaron, 16#1ab).
-define(XK_Zacute, 16#1ac).
-define(XK_Zcaron, 16#1ae).
-define(XK_Zabovedot, 16#1af).
-define(XK_aogonek, 16#1b1).
-define(XK_ogonek, 16#1b2).
-define(XK_lstroke, 16#1b3).
-define(XK_lcaron, 16#1b5).
-define(XK_sacute, 16#1b6).
-define(XK_caron, 16#1b7).
-define(XK_scaron, 16#1b9).
-define(XK_scedilla, 16#1ba).
-define(XK_tcaron, 16#1bb).
-define(XK_zacute, 16#1bc).
-define(XK_doubleacute, 16#1bd).
-define(XK_zcaron, 16#1be).
-define(XK_zabovedot, 16#1bf).
-define(XK_Racute, 16#1c0).
-define(XK_Abreve, 16#1c3).
-define(XK_Lacute, 16#1c5).
-define(XK_Cacute, 16#1c6).
-define(XK_Ccaron, 16#1c8).
-define(XK_Eogonek, 16#1ca).
-define(XK_Ecaron, 16#1cc).
-define(XK_Dcaron, 16#1cf).
-define(XK_Dstroke, 16#1d0).
-define(XK_Nacute, 16#1d1).
-define(XK_Ncaron, 16#1d2).
-define(XK_Odoubleacute, 16#1d5).
-define(XK_Rcaron, 16#1d8).
-define(XK_Uring, 16#1d9).
-define(XK_Udoubleacute, 16#1db).
-define(XK_Tcedilla, 16#1de).
-define(XK_racute, 16#1e0).
-define(XK_abreve, 16#1e3).
-define(XK_lacute, 16#1e5).
-define(XK_cacute, 16#1e6).
-define(XK_ccaron, 16#1e8).
-define(XK_eogonek, 16#1ea).
-define(XK_ecaron, 16#1ec).
-define(XK_dcaron, 16#1ef).
-define(XK_dstroke, 16#1f0).
-define(XK_nacute, 16#1f1).
-define(XK_ncaron, 16#1f2).
-define(XK_odoubleacute, 16#1f5).
-define(XK_udoubleacute, 16#1fb).
-define(XK_rcaron, 16#1f8).
-define(XK_uring, 16#1f9).
-define(XK_tcedilla, 16#1fe).
-define(XK_abovedot, 16#1ff).

%%
%%   Latin 3
%%   Byte 3 = 2

-define(XK_Hstroke, 16#2a1).
-define(XK_Hcircumflex, 16#2a6).
-define(XK_Iabovedot, 16#2a9).
-define(XK_Gbreve, 16#2ab).
-define(XK_Jcircumflex, 16#2ac).
-define(XK_hstroke, 16#2b1).
-define(XK_hcircumflex, 16#2b6).
-define(XK_idotless, 16#2b9).
-define(XK_gbreve, 16#2bb).
-define(XK_jcircumflex, 16#2bc).
-define(XK_Cabovedot, 16#2c5).
-define(XK_Ccircumflex, 16#2c6).
-define(XK_Gabovedot, 16#2d5).
-define(XK_Gcircumflex, 16#2d8).
-define(XK_Ubreve, 16#2dd).
-define(XK_Scircumflex, 16#2de).
-define(XK_cabovedot, 16#2e5).
-define(XK_ccircumflex, 16#2e6).
-define(XK_gabovedot, 16#2f5).
-define(XK_gcircumflex, 16#2f8).
-define(XK_ubreve, 16#2fd).
-define(XK_scircumflex, 16#2fe).


%%
%%   Latin 4
%%   Byte 3 = 3
 

-define(XK_kra, 16#3a2).
-define(XK_kappa, 16#3a2). %% deprecated 
-define(XK_Rcedilla, 16#3a3).
-define(XK_Itilde, 16#3a5).
-define(XK_Lcedilla, 16#3a6).
-define(XK_Emacron, 16#3aa).
-define(XK_Gcedilla, 16#3ab).
-define(XK_Tslash, 16#3ac).
-define(XK_rcedilla, 16#3b3).
-define(XK_itilde, 16#3b5).
-define(XK_lcedilla, 16#3b6).
-define(XK_emacron, 16#3ba).
-define(XK_gcedilla, 16#3bb).
-define(XK_tslash, 16#3bc).
-define(XK_ENG, 16#3bd).
-define(XK_eng, 16#3bf).
-define(XK_Amacron, 16#3c0).
-define(XK_Iogonek, 16#3c7).
-define(XK_Eabovedot, 16#3cc).
-define(XK_Imacron, 16#3cf).
-define(XK_Ncedilla, 16#3d1).
-define(XK_Omacron, 16#3d2).
-define(XK_Kcedilla, 16#3d3).
-define(XK_Uogonek, 16#3d9).
-define(XK_Utilde, 16#3dd).
-define(XK_Umacron, 16#3de).
-define(XK_amacron, 16#3e0).
-define(XK_iogonek, 16#3e7).
-define(XK_eabovedot, 16#3ec).
-define(XK_imacron, 16#3ef).
-define(XK_ncedilla, 16#3f1).
-define(XK_omacron, 16#3f2).
-define(XK_kcedilla, 16#3f3).
-define(XK_uogonek, 16#3f9).
-define(XK_utilde, 16#3fd).
-define(XK_umacron, 16#3fe).

%%
%% Latin-8
%% Byte 3 = 18
 
-define(XK_Babovedot, 16#12a1).
-define(XK_babovedot, 16#12a2).
-define(XK_Dabovedot, 16#12a6).
-define(XK_Wgrave, 16#12a8).
-define(XK_Wacute, 16#12aa).
-define(XK_dabovedot, 16#12ab).
-define(XK_Ygrave, 16#12ac).
-define(XK_Fabovedot, 16#12b0).
-define(XK_fabovedot, 16#12b1).
-define(XK_Mabovedot, 16#12b4).
-define(XK_mabovedot, 16#12b5).
-define(XK_Pabovedot, 16#12b7).
-define(XK_wgrave, 16#12b8).
-define(XK_pabovedot, 16#12b9).
-define(XK_wacute, 16#12ba).
-define(XK_Sabovedot, 16#12bb).
-define(XK_ygrave, 16#12bc).
-define(XK_Wdiaeresis, 16#12bd).
-define(XK_wdiaeresis, 16#12be).
-define(XK_sabovedot, 16#12bf).
-define(XK_Wcircumflex, 16#12d0).
-define(XK_Tabovedot, 16#12d7).
-define(XK_Ycircumflex, 16#12de).
-define(XK_wcircumflex, 16#12f0).
-define(XK_tabovedot, 16#12f7).
-define(XK_ycircumflex, 16#12fe).

%%
%% Latin-9 (a.k.a. Latin-0)
%% Byte 3 = 19

-define(XK_OE, 16#13bc).
-define(XK_oe, 16#13bd).
-define(XK_Ydiaeresis, 16#13be).

%%
%% Katakana
%% Byte 3 = 4
 

-define(XK_overline, 16#47e).
-define(XK_kana_fullstop, 16#4a1).
-define(XK_kana_openingbracket, 16#4a2).
-define(XK_kana_closingbracket, 16#4a3).
-define(XK_kana_comma, 16#4a4).
-define(XK_kana_conjunctive, 16#4a5).
-define(XK_kana_middledot, 16#4a5).  %% deprecated 
-define(XK_kana_WO, 16#4a6).
-define(XK_kana_a, 16#4a7).
-define(XK_kana_i, 16#4a8).
-define(XK_kana_u, 16#4a9).
-define(XK_kana_e, 16#4aa).
-define(XK_kana_o, 16#4ab).
-define(XK_kana_ya, 16#4ac).
-define(XK_kana_yu, 16#4ad).
-define(XK_kana_yo, 16#4ae).
-define(XK_kana_tsu, 16#4af).
-define(XK_kana_tu, 16#4af).  %% deprecated 
-define(XK_prolongedsound, 16#4b0).
-define(XK_kana_A, 16#4b1).
-define(XK_kana_I, 16#4b2).
-define(XK_kana_U, 16#4b3).
-define(XK_kana_E, 16#4b4).
-define(XK_kana_O, 16#4b5).
-define(XK_kana_KA, 16#4b6).
-define(XK_kana_KI, 16#4b7).
-define(XK_kana_KU, 16#4b8).
-define(XK_kana_KE, 16#4b9).
-define(XK_kana_KO, 16#4ba).
-define(XK_kana_SA, 16#4bb).
-define(XK_kana_SHI, 16#4bc).
-define(XK_kana_SU, 16#4bd).
-define(XK_kana_SE, 16#4be).
-define(XK_kana_SO, 16#4bf).
-define(XK_kana_TA, 16#4c0).
-define(XK_kana_CHI, 16#4c1).
-define(XK_kana_TI, 16#4c1).  %% deprecated 
-define(XK_kana_TSU, 16#4c2).
-define(XK_kana_TU, 16#4c2).  %% deprecated 
-define(XK_kana_TE, 16#4c3).
-define(XK_kana_TO, 16#4c4).
-define(XK_kana_NA, 16#4c5).
-define(XK_kana_NI, 16#4c6).
-define(XK_kana_NU, 16#4c7).
-define(XK_kana_NE, 16#4c8).
-define(XK_kana_NO, 16#4c9).
-define(XK_kana_HA, 16#4ca).
-define(XK_kana_HI, 16#4cb).
-define(XK_kana_FU, 16#4cc).
-define(XK_kana_HU, 16#4cc).  %% deprecated 
-define(XK_kana_HE, 16#4cd).
-define(XK_kana_HO, 16#4ce).
-define(XK_kana_MA, 16#4cf).
-define(XK_kana_MI, 16#4d0).
-define(XK_kana_MU, 16#4d1).
-define(XK_kana_ME, 16#4d2).
-define(XK_kana_MO, 16#4d3).
-define(XK_kana_YA, 16#4d4).
-define(XK_kana_YU, 16#4d5).
-define(XK_kana_YO, 16#4d6).
-define(XK_kana_RA, 16#4d7).
-define(XK_kana_RI, 16#4d8).
-define(XK_kana_RU, 16#4d9).
-define(XK_kana_RE, 16#4da).
-define(XK_kana_RO, 16#4db).
-define(XK_kana_WA, 16#4dc).
-define(XK_kana_N, 16#4dd).
-define(XK_voicedsound, 16#4de).
-define(XK_semivoicedsound, 16#4df).
-define(XK_kana_switch, 16#FF7E).  %% Alias for mode_switch 

%%
%%  Arabic
%%  Byte 3 = 5
 

-define(XK_Farsi_0, 16#590).
-define(XK_Farsi_1, 16#591).
-define(XK_Farsi_2, 16#592).
-define(XK_Farsi_3, 16#593).
-define(XK_Farsi_4, 16#594).
-define(XK_Farsi_5, 16#595).
-define(XK_Farsi_6, 16#596).
-define(XK_Farsi_7, 16#597).
-define(XK_Farsi_8, 16#598).
-define(XK_Farsi_9, 16#599).
-define(XK_Arabic_percent, 16#5a5).
-define(XK_Arabic_superscript_alef, 16#5a6).
-define(XK_Arabic_tteh, 16#5a7).
-define(XK_Arabic_peh, 16#5a8).
-define(XK_Arabic_tcheh, 16#5a9).
-define(XK_Arabic_ddal, 16#5aa).
-define(XK_Arabic_rreh, 16#5ab).
-define(XK_Arabic_comma, 16#5ac).
-define(XK_Arabic_fullstop, 16#5ae).
-define(XK_Arabic_0, 16#5b0).
-define(XK_Arabic_1, 16#5b1).
-define(XK_Arabic_2, 16#5b2).
-define(XK_Arabic_3, 16#5b3).
-define(XK_Arabic_4, 16#5b4).
-define(XK_Arabic_5, 16#5b5).
-define(XK_Arabic_6, 16#5b6).
-define(XK_Arabic_7, 16#5b7).
-define(XK_Arabic_8, 16#5b8).
-define(XK_Arabic_9, 16#5b9).
-define(XK_Arabic_semicolon, 16#5bb).
-define(XK_Arabic_question_mark, 16#5bf).
-define(XK_Arabic_hamza, 16#5c1).
-define(XK_Arabic_maddaonalef, 16#5c2).
-define(XK_Arabic_hamzaonalef, 16#5c3).
-define(XK_Arabic_hamzaonwaw, 16#5c4).
-define(XK_Arabic_hamzaunderalef, 16#5c5).
-define(XK_Arabic_hamzaonyeh, 16#5c6).
-define(XK_Arabic_alef, 16#5c7).
-define(XK_Arabic_beh, 16#5c8).
-define(XK_Arabic_tehmarbuta, 16#5c9).
-define(XK_Arabic_teh, 16#5ca).
-define(XK_Arabic_theh, 16#5cb).
-define(XK_Arabic_jeem, 16#5cc).
-define(XK_Arabic_hah, 16#5cd).
-define(XK_Arabic_khah, 16#5ce).
-define(XK_Arabic_dal, 16#5cf).
-define(XK_Arabic_thal, 16#5d0).
-define(XK_Arabic_ra, 16#5d1).
-define(XK_Arabic_zain, 16#5d2).
-define(XK_Arabic_seen, 16#5d3).
-define(XK_Arabic_sheen, 16#5d4).
-define(XK_Arabic_sad, 16#5d5).
-define(XK_Arabic_dad, 16#5d6).
-define(XK_Arabic_tah, 16#5d7).
-define(XK_Arabic_zah, 16#5d8).
-define(XK_Arabic_ain, 16#5d9).
-define(XK_Arabic_ghain, 16#5da).
-define(XK_Arabic_tatweel, 16#5e0).
-define(XK_Arabic_feh, 16#5e1).
-define(XK_Arabic_qaf, 16#5e2).
-define(XK_Arabic_kaf, 16#5e3).
-define(XK_Arabic_lam, 16#5e4).
-define(XK_Arabic_meem, 16#5e5).
-define(XK_Arabic_noon, 16#5e6).
-define(XK_Arabic_ha, 16#5e7).
-define(XK_Arabic_heh, 16#5e7).  %% deprecated 
-define(XK_Arabic_waw, 16#5e8).
-define(XK_Arabic_alefmaksura, 16#5e9).
-define(XK_Arabic_yeh, 16#5ea).
-define(XK_Arabic_fathatan, 16#5eb).
-define(XK_Arabic_dammatan, 16#5ec).
-define(XK_Arabic_kasratan, 16#5ed).
-define(XK_Arabic_fatha, 16#5ee).
-define(XK_Arabic_damma, 16#5ef).
-define(XK_Arabic_kasra, 16#5f0).
-define(XK_Arabic_shadda, 16#5f1).
-define(XK_Arabic_sukun, 16#5f2).
-define(XK_Arabic_madda_above, 16#5f3).
-define(XK_Arabic_hamza_above, 16#5f4).
-define(XK_Arabic_hamza_below, 16#5f5).
-define(XK_Arabic_jeh, 16#5f6).
-define(XK_Arabic_veh, 16#5f7).
-define(XK_Arabic_keheh, 16#5f8).
-define(XK_Arabic_gaf, 16#5f9).
-define(XK_Arabic_noon_ghunna, 16#5fa).
-define(XK_Arabic_heh_doachashmee, 16#5fb).
-define(XK_Farsi_yeh, 16#5fc).
-define(XK_Arabic_farsi_yeh, ?XK_Farsi_yeh).
-define(XK_Arabic_yeh_baree, 16#5fd).
-define(XK_Arabic_heh_goal, 16#5fe).
-define(XK_Arabic_switch, 16#FF7E).  %% Alias for mode_switch 

%%
%% Cyrillic
%% Byte 3 = 6
 

-define(XK_Cyrillic_GHE_bar, 16#680).
-define(XK_Cyrillic_ghe_bar, 16#690).
-define(XK_Cyrillic_ZHE_descender, 16#681).
-define(XK_Cyrillic_zhe_descender, 16#691).
-define(XK_Cyrillic_KA_descender, 16#682).
-define(XK_Cyrillic_ka_descender, 16#692).
-define(XK_Cyrillic_KA_vertstroke, 16#683).
-define(XK_Cyrillic_ka_vertstroke, 16#693).
-define(XK_Cyrillic_EN_descender, 16#684).
-define(XK_Cyrillic_en_descender, 16#694).
-define(XK_Cyrillic_U_straight, 16#685).
-define(XK_Cyrillic_u_straight, 16#695).
-define(XK_Cyrillic_U_straight_bar, 16#686).
-define(XK_Cyrillic_u_straight_bar, 16#696).
-define(XK_Cyrillic_HA_descender, 16#687).
-define(XK_Cyrillic_ha_descender, 16#697).
-define(XK_Cyrillic_CHE_descender, 16#688).
-define(XK_Cyrillic_che_descender, 16#698).
-define(XK_Cyrillic_CHE_vertstroke, 16#689).
-define(XK_Cyrillic_che_vertstroke, 16#699).
-define(XK_Cyrillic_SHHA, 16#68a).
-define(XK_Cyrillic_shha, 16#69a).

-define(XK_Cyrillic_SCHWA, 16#68c).
-define(XK_Cyrillic_schwa, 16#69c).
-define(XK_Cyrillic_I_macron, 16#68d).
-define(XK_Cyrillic_i_macron, 16#69d).
-define(XK_Cyrillic_O_bar, 16#68e).
-define(XK_Cyrillic_o_bar, 16#69e).
-define(XK_Cyrillic_U_macron, 16#68f).
-define(XK_Cyrillic_u_macron, 16#69f).

-define(XK_Serbian_dje, 16#6a1).
-define(XK_Macedonia_gje, 16#6a2).
-define(XK_Cyrillic_io, 16#6a3).
-define(XK_Ukrainian_ie, 16#6a4).
-define(XK_Ukranian_je, 16#6a4).  %% deprecated 
-define(XK_Macedonia_dse, 16#6a5).
-define(XK_Ukrainian_i, 16#6a6).
-define(XK_Ukranian_i, 16#6a6).  %% deprecated 
-define(XK_Ukrainian_yi, 16#6a7).
-define(XK_Ukranian_yi, 16#6a7).  %% deprecated 
-define(XK_Cyrillic_je, 16#6a8).
-define(XK_Serbian_je, 16#6a8).  %% deprecated 
-define(XK_Cyrillic_lje, 16#6a9).
-define(XK_Serbian_lje, 16#6a9).  %% deprecated 
-define(XK_Cyrillic_nje, 16#6aa).
-define(XK_Serbian_nje, 16#6aa).  %% deprecated 
-define(XK_Serbian_tshe, 16#6ab).
-define(XK_Macedonia_kje, 16#6ac).
-define(XK_Ukrainian_ghe_with_upturn, 16#6ad).
-define(XK_Byelorussian_shortu, 16#6ae).
-define(XK_Cyrillic_dzhe, 16#6af).
-define(XK_Serbian_dze, 16#6af).  %% deprecated 
-define(XK_numerosign, 16#6b0).
-define(XK_Serbian_DJE, 16#6b1).
-define(XK_Macedonia_GJE, 16#6b2).
-define(XK_Cyrillic_IO, 16#6b3).
-define(XK_Ukrainian_IE, 16#6b4).
-define(XK_Ukranian_JE, 16#6b4).  %% deprecated 
-define(XK_Macedonia_DSE, 16#6b5).
-define(XK_Ukrainian_I, 16#6b6).
-define(XK_Ukranian_I, 16#6b6).  %% deprecated 
-define(XK_Ukrainian_YI, 16#6b7).
-define(XK_Ukranian_YI, 16#6b7).  %% deprecated 
-define(XK_Cyrillic_JE, 16#6b8).
-define(XK_Serbian_JE, 16#6b8).  %% deprecated 
-define(XK_Cyrillic_LJE, 16#6b9).
-define(XK_Serbian_LJE, 16#6b9).  %% deprecated 
-define(XK_Cyrillic_NJE, 16#6ba).
-define(XK_Serbian_NJE, 16#6ba).  %% deprecated 
-define(XK_Serbian_TSHE, 16#6bb).
-define(XK_Macedonia_KJE, 16#6bc).
-define(XK_Ukrainian_GHE_WITH_UPTURN, 16#6bd).
-define(XK_Byelorussian_SHORTU, 16#6be).
-define(XK_Cyrillic_DZHE, 16#6bf).
-define(XK_Serbian_DZE, 16#6bf).  %% deprecated 
-define(XK_Cyrillic_yu, 16#6c0).
-define(XK_Cyrillic_a, 16#6c1).
-define(XK_Cyrillic_be, 16#6c2).
-define(XK_Cyrillic_tse, 16#6c3).
-define(XK_Cyrillic_de, 16#6c4).
-define(XK_Cyrillic_ie, 16#6c5).
-define(XK_Cyrillic_ef, 16#6c6).
-define(XK_Cyrillic_ghe, 16#6c7).
-define(XK_Cyrillic_ha, 16#6c8).
-define(XK_Cyrillic_i, 16#6c9).
-define(XK_Cyrillic_shorti, 16#6ca).
-define(XK_Cyrillic_ka, 16#6cb).
-define(XK_Cyrillic_el, 16#6cc).
-define(XK_Cyrillic_em, 16#6cd).
-define(XK_Cyrillic_en, 16#6ce).
-define(XK_Cyrillic_o, 16#6cf).
-define(XK_Cyrillic_pe, 16#6d0).
-define(XK_Cyrillic_ya, 16#6d1).
-define(XK_Cyrillic_er, 16#6d2).
-define(XK_Cyrillic_es, 16#6d3).
-define(XK_Cyrillic_te, 16#6d4).
-define(XK_Cyrillic_u, 16#6d5).
-define(XK_Cyrillic_zhe, 16#6d6).
-define(XK_Cyrillic_ve, 16#6d7).
-define(XK_Cyrillic_softsign, 16#6d8).
-define(XK_Cyrillic_yeru, 16#6d9).
-define(XK_Cyrillic_ze, 16#6da).
-define(XK_Cyrillic_sha, 16#6db).
-define(XK_Cyrillic_e, 16#6dc).
-define(XK_Cyrillic_shcha, 16#6dd).
-define(XK_Cyrillic_che, 16#6de).
-define(XK_Cyrillic_hardsign, 16#6df).
-define(XK_Cyrillic_YU, 16#6e0).
-define(XK_Cyrillic_A, 16#6e1).
-define(XK_Cyrillic_BE, 16#6e2).
-define(XK_Cyrillic_TSE, 16#6e3).
-define(XK_Cyrillic_DE, 16#6e4).
-define(XK_Cyrillic_IE, 16#6e5).
-define(XK_Cyrillic_EF, 16#6e6).
-define(XK_Cyrillic_GHE, 16#6e7).
-define(XK_Cyrillic_HA, 16#6e8).
-define(XK_Cyrillic_I, 16#6e9).
-define(XK_Cyrillic_SHORTI, 16#6ea).
-define(XK_Cyrillic_KA, 16#6eb).
-define(XK_Cyrillic_EL, 16#6ec).
-define(XK_Cyrillic_EM, 16#6ed).
-define(XK_Cyrillic_EN, 16#6ee).
-define(XK_Cyrillic_O, 16#6ef).
-define(XK_Cyrillic_PE, 16#6f0).
-define(XK_Cyrillic_YA, 16#6f1).
-define(XK_Cyrillic_ER, 16#6f2).
-define(XK_Cyrillic_ES, 16#6f3).
-define(XK_Cyrillic_TE, 16#6f4).
-define(XK_Cyrillic_U, 16#6f5).
-define(XK_Cyrillic_ZHE, 16#6f6).
-define(XK_Cyrillic_VE, 16#6f7).
-define(XK_Cyrillic_SOFTSIGN, 16#6f8).
-define(XK_Cyrillic_YERU, 16#6f9).
-define(XK_Cyrillic_ZE, 16#6fa).
-define(XK_Cyrillic_SHA, 16#6fb).
-define(XK_Cyrillic_E, 16#6fc).
-define(XK_Cyrillic_SHCHA, 16#6fd).
-define(XK_Cyrillic_CHE, 16#6fe).
-define(XK_Cyrillic_HARDSIGN, 16#6ff).

%%
%% Greek
%% Byte 3 = 7

-define(XK_Greek_ALPHAaccent, 16#7a1).
-define(XK_Greek_EPSILONaccent, 16#7a2).
-define(XK_Greek_ETAaccent, 16#7a3).
-define(XK_Greek_IOTAaccent, 16#7a4).
-define(XK_Greek_IOTAdieresis, 16#7a5).
-define(XK_Greek_IOTAdiaeresis, ?XK_Greek_IOTAdieresis). %% old typo 
-define(XK_Greek_OMICRONaccent, 16#7a7).
-define(XK_Greek_UPSILONaccent, 16#7a8).
-define(XK_Greek_UPSILONdieresis, 16#7a9).
-define(XK_Greek_OMEGAaccent, 16#7ab).
-define(XK_Greek_accentdieresis, 16#7ae).
-define(XK_Greek_horizbar, 16#7af).
-define(XK_Greek_alphaaccent, 16#7b1).
-define(XK_Greek_epsilonaccent, 16#7b2).
-define(XK_Greek_etaaccent, 16#7b3).
-define(XK_Greek_iotaaccent, 16#7b4).
-define(XK_Greek_iotadieresis, 16#7b5).
-define(XK_Greek_iotaaccentdieresis, 16#7b6).
-define(XK_Greek_omicronaccent, 16#7b7).
-define(XK_Greek_upsilonaccent, 16#7b8).
-define(XK_Greek_upsilondieresis, 16#7b9).
-define(XK_Greek_upsilonaccentdieresis, 16#7ba).
-define(XK_Greek_omegaaccent, 16#7bb).
-define(XK_Greek_ALPHA, 16#7c1).
-define(XK_Greek_BETA, 16#7c2).
-define(XK_Greek_GAMMA, 16#7c3).
-define(XK_Greek_DELTA, 16#7c4).
-define(XK_Greek_EPSILON, 16#7c5).
-define(XK_Greek_ZETA, 16#7c6).
-define(XK_Greek_ETA, 16#7c7).
-define(XK_Greek_THETA, 16#7c8).
-define(XK_Greek_IOTA, 16#7c9).
-define(XK_Greek_KAPPA, 16#7ca).
-define(XK_Greek_LAMDA, 16#7cb).
-define(XK_Greek_LAMBDA, 16#7cb).
-define(XK_Greek_MU, 16#7cc).
-define(XK_Greek_NU, 16#7cd).
-define(XK_Greek_XI, 16#7ce).
-define(XK_Greek_OMICRON, 16#7cf).
-define(XK_Greek_PI, 16#7d0).
-define(XK_Greek_RHO, 16#7d1).
-define(XK_Greek_SIGMA, 16#7d2).
-define(XK_Greek_TAU, 16#7d4).
-define(XK_Greek_UPSILON, 16#7d5).
-define(XK_Greek_PHI, 16#7d6).
-define(XK_Greek_CHI, 16#7d7).
-define(XK_Greek_PSI, 16#7d8).
-define(XK_Greek_OMEGA, 16#7d9).
-define(XK_Greek_alpha, 16#7e1).
-define(XK_Greek_beta, 16#7e2).
-define(XK_Greek_gamma, 16#7e3).
-define(XK_Greek_delta, 16#7e4).
-define(XK_Greek_epsilon, 16#7e5).
-define(XK_Greek_zeta, 16#7e6).
-define(XK_Greek_eta, 16#7e7).
-define(XK_Greek_theta, 16#7e8).
-define(XK_Greek_iota, 16#7e9).
-define(XK_Greek_kappa, 16#7ea).
-define(XK_Greek_lamda, 16#7eb).
-define(XK_Greek_lambda, 16#7eb).
-define(XK_Greek_mu, 16#7ec).
-define(XK_Greek_nu, 16#7ed).
-define(XK_Greek_xi, 16#7ee).
-define(XK_Greek_omicron, 16#7ef).
-define(XK_Greek_pi, 16#7f0).
-define(XK_Greek_rho, 16#7f1).
-define(XK_Greek_sigma, 16#7f2).
-define(XK_Greek_finalsmallsigma, 16#7f3).
-define(XK_Greek_tau, 16#7f4).
-define(XK_Greek_upsilon, 16#7f5).
-define(XK_Greek_phi, 16#7f6).
-define(XK_Greek_chi, 16#7f7).
-define(XK_Greek_psi, 16#7f8).
-define(XK_Greek_omega, 16#7f9).
-define(XK_Greek_switch, 16#FF7E).  %% Alias for mode_switch 

%%
%% Technical
%% Byte 3 = 8

-define(XK_leftradical, 16#8a1).
-define(XK_topleftradical, 16#8a2).
-define(XK_horizconnector, 16#8a3).
-define(XK_topintegral, 16#8a4).
-define(XK_botintegral, 16#8a5).
-define(XK_vertconnector, 16#8a6).
-define(XK_topleftsqbracket, 16#8a7).
-define(XK_botleftsqbracket, 16#8a8).
-define(XK_toprightsqbracket, 16#8a9).
-define(XK_botrightsqbracket, 16#8aa).
-define(XK_topleftparens, 16#8ab).
-define(XK_botleftparens, 16#8ac).
-define(XK_toprightparens, 16#8ad).
-define(XK_botrightparens, 16#8ae).
-define(XK_leftmiddlecurlybrace, 16#8af).
-define(XK_rightmiddlecurlybrace, 16#8b0).
-define(XK_topleftsummation, 16#8b1).
-define(XK_botleftsummation, 16#8b2).
-define(XK_topvertsummationconnector, 16#8b3).
-define(XK_botvertsummationconnector, 16#8b4).
-define(XK_toprightsummation, 16#8b5).
-define(XK_botrightsummation, 16#8b6).
-define(XK_rightmiddlesummation, 16#8b7).
-define(XK_lessthanequal, 16#8bc).
-define(XK_notequal, 16#8bd).
-define(XK_greaterthanequal, 16#8be).
-define(XK_integral, 16#8bf).
-define(XK_therefore, 16#8c0).
-define(XK_variation, 16#8c1).
-define(XK_infinity, 16#8c2).
-define(XK_nabla, 16#8c5).
-define(XK_approximate, 16#8c8).
-define(XK_similarequal, 16#8c9).
-define(XK_ifonlyif, 16#8cd).
-define(XK_implies, 16#8ce).
-define(XK_identical, 16#8cf).
-define(XK_radical, 16#8d6).
-define(XK_includedin, 16#8da).
-define(XK_includes, 16#8db).
-define(XK_intersection, 16#8dc).
-define(XK_union, 16#8dd).
-define(XK_logicaland, 16#8de).
-define(XK_logicalor, 16#8df).
-define(XK_partialderivative, 16#8ef).
-define(XK_function, 16#8f6).
-define(XK_leftarrow, 16#8fb).
-define(XK_uparrow, 16#8fc).
-define(XK_rightarrow, 16#8fd).
-define(XK_downarrow, 16#8fe).

%%
%%  Special
%%  Byte 3 = 9
 

-define(XK_blank, 16#9df).
-define(XK_soliddiamond, 16#9e0).
-define(XK_checkerboard, 16#9e1).
-define(XK_ht, 16#9e2).
-define(XK_ff, 16#9e3).
-define(XK_cr, 16#9e4).
-define(XK_lf, 16#9e5).
-define(XK_nl, 16#9e8).
-define(XK_vt, 16#9e9).
-define(XK_lowrightcorner, 16#9ea).
-define(XK_uprightcorner, 16#9eb).
-define(XK_upleftcorner, 16#9ec).
-define(XK_lowleftcorner, 16#9ed).
-define(XK_crossinglines, 16#9ee).
-define(XK_horizlinescan1, 16#9ef).
-define(XK_horizlinescan3, 16#9f0).
-define(XK_horizlinescan5, 16#9f1).
-define(XK_horizlinescan7, 16#9f2).
-define(XK_horizlinescan9, 16#9f3).
-define(XK_leftt, 16#9f4).
-define(XK_rightt, 16#9f5).
-define(XK_bott, 16#9f6).
-define(XK_topt, 16#9f7).
-define(XK_vertbar, 16#9f8).

%%
%%  Publishing
%%  Byte 3 = a
 

-define(XK_emspace, 16#aa1).
-define(XK_enspace, 16#aa2).
-define(XK_em3space, 16#aa3).
-define(XK_em4space, 16#aa4).
-define(XK_digitspace, 16#aa5).
-define(XK_punctspace, 16#aa6).
-define(XK_thinspace, 16#aa7).
-define(XK_hairspace, 16#aa8).
-define(XK_emdash, 16#aa9).
-define(XK_endash, 16#aaa).
-define(XK_signifblank, 16#aac).
-define(XK_ellipsis, 16#aae).
-define(XK_doubbaselinedot, 16#aaf).
-define(XK_onethird, 16#ab0).
-define(XK_twothirds, 16#ab1).
-define(XK_onefifth, 16#ab2).
-define(XK_twofifths, 16#ab3).
-define(XK_threefifths, 16#ab4).
-define(XK_fourfifths, 16#ab5).
-define(XK_onesixth, 16#ab6).
-define(XK_fivesixths, 16#ab7).
-define(XK_careof, 16#ab8).
-define(XK_figdash, 16#abb).
-define(XK_leftanglebracket, 16#abc).
-define(XK_decimalpoint, 16#abd).
-define(XK_rightanglebracket, 16#abe).
-define(XK_marker, 16#abf).
-define(XK_oneeighth, 16#ac3).
-define(XK_threeeighths, 16#ac4).
-define(XK_fiveeighths, 16#ac5).
-define(XK_seveneighths, 16#ac6).
-define(XK_trademark, 16#ac9).
-define(XK_signaturemark, 16#aca).
-define(XK_trademarkincircle, 16#acb).
-define(XK_leftopentriangle, 16#acc).
-define(XK_rightopentriangle, 16#acd).
-define(XK_emopencircle, 16#ace).
-define(XK_emopenrectangle, 16#acf).
-define(XK_leftsinglequotemark, 16#ad0).
-define(XK_rightsinglequotemark, 16#ad1).
-define(XK_leftdoublequotemark, 16#ad2).
-define(XK_rightdoublequotemark, 16#ad3).
-define(XK_prescription, 16#ad4).
-define(XK_minutes, 16#ad6).
-define(XK_seconds, 16#ad7).
-define(XK_latincross, 16#ad9).
-define(XK_hexagram, 16#ada).
-define(XK_filledrectbullet, 16#adb).
-define(XK_filledlefttribullet, 16#adc).
-define(XK_filledrighttribullet, 16#add).
-define(XK_emfilledcircle, 16#ade).
-define(XK_emfilledrect, 16#adf).
-define(XK_enopencircbullet, 16#ae0).
-define(XK_enopensquarebullet, 16#ae1).
-define(XK_openrectbullet, 16#ae2).
-define(XK_opentribulletup, 16#ae3).
-define(XK_opentribulletdown, 16#ae4).
-define(XK_openstar, 16#ae5).
-define(XK_enfilledcircbullet, 16#ae6).
-define(XK_enfilledsqbullet, 16#ae7).
-define(XK_filledtribulletup, 16#ae8).
-define(XK_filledtribulletdown, 16#ae9).
-define(XK_leftpointer, 16#aea).
-define(XK_rightpointer, 16#aeb).
-define(XK_club, 16#aec).
-define(XK_diamond, 16#aed).
-define(XK_heart, 16#aee).
-define(XK_maltesecross, 16#af0).
-define(XK_dagger, 16#af1).
-define(XK_doubledagger, 16#af2).
-define(XK_checkmark, 16#af3).
-define(XK_ballotcross, 16#af4).
-define(XK_musicalsharp, 16#af5).
-define(XK_musicalflat, 16#af6).
-define(XK_malesymbol, 16#af7).
-define(XK_femalesymbol, 16#af8).
-define(XK_telephone, 16#af9).
-define(XK_telephonerecorder, 16#afa).
-define(XK_phonographcopyright, 16#afb).
-define(XK_caret, 16#afc).
-define(XK_singlelowquotemark, 16#afd).
-define(XK_doublelowquotemark, 16#afe).
-define(XK_cursor, 16#aff).


%%
 %%  APL
 %%  Byte 3 = b
 

-define(XK_leftcaret, 16#ba3).
-define(XK_rightcaret, 16#ba6).
-define(XK_downcaret, 16#ba8).
-define(XK_upcaret, 16#ba9).
-define(XK_overbar, 16#bc0).
-define(XK_downtack, 16#bc2).
-define(XK_upshoe, 16#bc3).
-define(XK_downstile, 16#bc4).
-define(XK_underbar, 16#bc6).
-define(XK_jot, 16#bca).
-define(XK_quad, 16#bcc).
-define(XK_uptack, 16#bce).
-define(XK_circle, 16#bcf).
-define(XK_upstile, 16#bd3).
-define(XK_downshoe, 16#bd6).
-define(XK_rightshoe, 16#bd8).
-define(XK_leftshoe, 16#bda).
-define(XK_lefttack, 16#bdc).
-define(XK_righttack, 16#bfc).

%%
%% Hebrew
%% Byte 3 = c
 

-define(XK_hebrew_doublelowline, 16#cdf).
-define(XK_hebrew_aleph, 16#ce0).
-define(XK_hebrew_bet, 16#ce1).
-define(XK_hebrew_beth, 16#ce1).  %% deprecated 
-define(XK_hebrew_gimel, 16#ce2).
-define(XK_hebrew_gimmel, 16#ce2).  %% deprecated 
-define(XK_hebrew_dalet, 16#ce3).
-define(XK_hebrew_daleth, 16#ce3).  %% deprecated 
-define(XK_hebrew_he, 16#ce4).
-define(XK_hebrew_waw, 16#ce5).
-define(XK_hebrew_zain, 16#ce6).
-define(XK_hebrew_zayin, 16#ce6).  %% deprecated 
-define(XK_hebrew_chet, 16#ce7).
-define(XK_hebrew_het, 16#ce7).  %% deprecated 
-define(XK_hebrew_tet, 16#ce8).
-define(XK_hebrew_teth, 16#ce8).  %% deprecated 
-define(XK_hebrew_yod, 16#ce9).
-define(XK_hebrew_finalkaph, 16#cea).
-define(XK_hebrew_kaph, 16#ceb).
-define(XK_hebrew_lamed, 16#cec).
-define(XK_hebrew_finalmem, 16#ced).
-define(XK_hebrew_mem, 16#cee).
-define(XK_hebrew_finalnun, 16#cef).
-define(XK_hebrew_nun, 16#cf0).
-define(XK_hebrew_samech, 16#cf1).
-define(XK_hebrew_samekh, 16#cf1).  %% deprecated 
-define(XK_hebrew_ayin, 16#cf2).
-define(XK_hebrew_finalpe, 16#cf3).
-define(XK_hebrew_pe, 16#cf4).
-define(XK_hebrew_finalzade, 16#cf5).
-define(XK_hebrew_finalzadi, 16#cf5).  %% deprecated 
-define(XK_hebrew_zade, 16#cf6).
-define(XK_hebrew_zadi, 16#cf6).  %% deprecated 
-define(XK_hebrew_qoph, 16#cf7).
-define(XK_hebrew_kuf, 16#cf7).  %% deprecated 
-define(XK_hebrew_resh, 16#cf8).
-define(XK_hebrew_shin, 16#cf9).
-define(XK_hebrew_taw, 16#cfa).
-define(XK_hebrew_taf, 16#cfa).  %% deprecated 
-define(XK_Hebrew_switch, 16#FF7E).  %% Alias for mode_switch 

%%
%% Thai
%% Byte 3 = d
 

-define(XK_Thai_kokai, 16#da1).
-define(XK_Thai_khokhai, 16#da2).
-define(XK_Thai_khokhuat, 16#da3).
-define(XK_Thai_khokhwai, 16#da4).
-define(XK_Thai_khokhon, 16#da5).
-define(XK_Thai_khorakhang, 16#da6).  
-define(XK_Thai_ngongu, 16#da7).  
-define(XK_Thai_chochan, 16#da8).  
-define(XK_Thai_choching, 16#da9).   
-define(XK_Thai_chochang, 16#daa).  
-define(XK_Thai_soso, 16#dab).
-define(XK_Thai_chochoe, 16#dac).
-define(XK_Thai_yoying, 16#dad).
-define(XK_Thai_dochada, 16#dae).
-define(XK_Thai_topatak, 16#daf).
-define(XK_Thai_thothan, 16#db0).
-define(XK_Thai_thonangmontho, 16#db1).
-define(XK_Thai_thophuthao, 16#db2).
-define(XK_Thai_nonen, 16#db3).
-define(XK_Thai_dodek, 16#db4).
-define(XK_Thai_totao, 16#db5).
-define(XK_Thai_thothung, 16#db6).
-define(XK_Thai_thothahan, 16#db7).
-define(XK_Thai_thothong, 16#db8).
-define(XK_Thai_nonu, 16#db9).
-define(XK_Thai_bobaimai, 16#dba).
-define(XK_Thai_popla, 16#dbb).
-define(XK_Thai_phophung, 16#dbc).
-define(XK_Thai_fofa, 16#dbd).
-define(XK_Thai_phophan, 16#dbe).
-define(XK_Thai_fofan, 16#dbf).
-define(XK_Thai_phosamphao, 16#dc0).
-define(XK_Thai_moma, 16#dc1).
-define(XK_Thai_yoyak, 16#dc2).
-define(XK_Thai_rorua, 16#dc3).
-define(XK_Thai_ru, 16#dc4).
-define(XK_Thai_loling, 16#dc5).
-define(XK_Thai_lu, 16#dc6).
-define(XK_Thai_wowaen, 16#dc7).
-define(XK_Thai_sosala, 16#dc8).
-define(XK_Thai_sorusi, 16#dc9).
-define(XK_Thai_sosua, 16#dca).
-define(XK_Thai_hohip, 16#dcb).
-define(XK_Thai_lochula, 16#dcc).
-define(XK_Thai_oang, 16#dcd).
-define(XK_Thai_honokhuk, 16#dce).
-define(XK_Thai_paiyannoi, 16#dcf).
-define(XK_Thai_saraa, 16#dd0).
-define(XK_Thai_maihanakat, 16#dd1).
-define(XK_Thai_saraaa, 16#dd2).
-define(XK_Thai_saraam, 16#dd3).
-define(XK_Thai_sarai, 16#dd4).   
-define(XK_Thai_saraii, 16#dd5).   
-define(XK_Thai_saraue, 16#dd6).    
-define(XK_Thai_sarauee, 16#dd7).    
-define(XK_Thai_sarau, 16#dd8).    
-define(XK_Thai_sarauu, 16#dd9).   
-define(XK_Thai_phinthu, 16#dda).
-define(XK_Thai_maihanakat_maitho, 16#dde).
-define(XK_Thai_baht, 16#ddf).
-define(XK_Thai_sarae, 16#de0).    
-define(XK_Thai_saraae, 16#de1).
-define(XK_Thai_sarao, 16#de2).
-define(XK_Thai_saraaimaimuan, 16#de3).   
-define(XK_Thai_saraaimaimalai, 16#de4).  
-define(XK_Thai_lakkhangyao, 16#de5).
-define(XK_Thai_maiyamok, 16#de6).
-define(XK_Thai_maitaikhu, 16#de7).
-define(XK_Thai_maiek, 16#de8).   
-define(XK_Thai_maitho, 16#de9).
-define(XK_Thai_maitri, 16#dea).
-define(XK_Thai_maichattawa, 16#deb).
-define(XK_Thai_thanthakhat, 16#dec).
-define(XK_Thai_nikhahit, 16#ded).
-define(XK_Thai_leksun, 16#df0). 
-define(XK_Thai_leknung, 16#df1).  
-define(XK_Thai_leksong, 16#df2). 
-define(XK_Thai_leksam, 16#df3).
-define(XK_Thai_leksi, 16#df4).  
-define(XK_Thai_lekha, 16#df5).  
-define(XK_Thai_lekhok, 16#df6).  
-define(XK_Thai_lekchet, 16#df7).  
-define(XK_Thai_lekpaet, 16#df8).  
-define(XK_Thai_lekkao, 16#df9). 


%%
%%   Korean
%%   Byte 3 = e
 

-define(XK_Hangul, 16#ff31).    %% Hangul start/stop(toggle) 
-define(XK_Hangul_Start, 16#ff32).    %% Hangul start 
-define(XK_Hangul_End, 16#ff33).    %% Hangul end, English start 
-define(XK_Hangul_Hanja, 16#ff34).    %% Start Hangul->Hanja Conversion 
-define(XK_Hangul_Jamo, 16#ff35).    %% Hangul Jamo mode 
-define(XK_Hangul_Romaja, 16#ff36).    %% Hangul Romaja mode 
-define(XK_Hangul_Codeinput, 16#ff37).    %% Hangul code input mode 
-define(XK_Hangul_Jeonja, 16#ff38).    %% Jeonja mode 
-define(XK_Hangul_Banja, 16#ff39).    %% Banja mode 
-define(XK_Hangul_PreHanja, 16#ff3a).    %% Pre Hanja conversion 
-define(XK_Hangul_PostHanja, 16#ff3b).    %% Post Hanja conversion 
-define(XK_Hangul_SingleCandidate, 16#ff3c).    %% Single candidate 
-define(XK_Hangul_MultipleCandidate, 16#ff3d).    %% Multiple candidate 
-define(XK_Hangul_PreviousCandidate, 16#ff3e).    %% Previous candidate 
-define(XK_Hangul_Special, 16#ff3f).    %% Special symbols 
-define(XK_Hangul_switch, 16#FF7E).    %% Alias for mode_switch 

%% Hangul Consonant Characters 
-define(XK_Hangul_Kiyeog, 16#ea1).
-define(XK_Hangul_SsangKiyeog, 16#ea2).
-define(XK_Hangul_KiyeogSios, 16#ea3).
-define(XK_Hangul_Nieun, 16#ea4).
-define(XK_Hangul_NieunJieuj, 16#ea5).
-define(XK_Hangul_NieunHieuh, 16#ea6).
-define(XK_Hangul_Dikeud, 16#ea7).
-define(XK_Hangul_SsangDikeud, 16#ea8).
-define(XK_Hangul_Rieul, 16#ea9).
-define(XK_Hangul_RieulKiyeog, 16#eaa).
-define(XK_Hangul_RieulMieum, 16#eab).
-define(XK_Hangul_RieulPieub, 16#eac).
-define(XK_Hangul_RieulSios, 16#ead).
-define(XK_Hangul_RieulTieut, 16#eae).
-define(XK_Hangul_RieulPhieuf, 16#eaf).
-define(XK_Hangul_RieulHieuh, 16#eb0).
-define(XK_Hangul_Mieum, 16#eb1).
-define(XK_Hangul_Pieub, 16#eb2).
-define(XK_Hangul_SsangPieub, 16#eb3).
-define(XK_Hangul_PieubSios, 16#eb4).
-define(XK_Hangul_Sios, 16#eb5).
-define(XK_Hangul_SsangSios, 16#eb6).
-define(XK_Hangul_Ieung, 16#eb7).
-define(XK_Hangul_Jieuj, 16#eb8).
-define(XK_Hangul_SsangJieuj, 16#eb9).
-define(XK_Hangul_Cieuc, 16#eba).
-define(XK_Hangul_Khieuq, 16#ebb).
-define(XK_Hangul_Tieut, 16#ebc).
-define(XK_Hangul_Phieuf, 16#ebd).
-define(XK_Hangul_Hieuh, 16#ebe).

%% Hangul Vowel Characters 
-define(XK_Hangul_A, 16#ebf).
-define(XK_Hangul_AE, 16#ec0).
-define(XK_Hangul_YA, 16#ec1).
-define(XK_Hangul_YAE, 16#ec2).
-define(XK_Hangul_EO, 16#ec3).
-define(XK_Hangul_E, 16#ec4).
-define(XK_Hangul_YEO, 16#ec5).
-define(XK_Hangul_YE, 16#ec6).
-define(XK_Hangul_O, 16#ec7).
-define(XK_Hangul_WA, 16#ec8).
-define(XK_Hangul_WAE, 16#ec9).
-define(XK_Hangul_OE, 16#eca).
-define(XK_Hangul_YO, 16#ecb).
-define(XK_Hangul_U, 16#ecc).
-define(XK_Hangul_WEO, 16#ecd).
-define(XK_Hangul_WE, 16#ece).
-define(XK_Hangul_WI, 16#ecf).
-define(XK_Hangul_YU, 16#ed0).
-define(XK_Hangul_EU, 16#ed1).
-define(XK_Hangul_YI, 16#ed2).
-define(XK_Hangul_I, 16#ed3).

%% Hangul syllable-final (JongSeong) Characters 
-define(XK_Hangul_J_Kiyeog, 16#ed4).
-define(XK_Hangul_J_SsangKiyeog, 16#ed5).
-define(XK_Hangul_J_KiyeogSios, 16#ed6).
-define(XK_Hangul_J_Nieun, 16#ed7).
-define(XK_Hangul_J_NieunJieuj, 16#ed8).
-define(XK_Hangul_J_NieunHieuh, 16#ed9).
-define(XK_Hangul_J_Dikeud, 16#eda).
-define(XK_Hangul_J_Rieul, 16#edb).
-define(XK_Hangul_J_RieulKiyeog, 16#edc).
-define(XK_Hangul_J_RieulMieum, 16#edd).
-define(XK_Hangul_J_RieulPieub, 16#ede).
-define(XK_Hangul_J_RieulSios, 16#edf).
-define(XK_Hangul_J_RieulTieut, 16#ee0).
-define(XK_Hangul_J_RieulPhieuf, 16#ee1).
-define(XK_Hangul_J_RieulHieuh, 16#ee2).
-define(XK_Hangul_J_Mieum, 16#ee3).
-define(XK_Hangul_J_Pieub, 16#ee4).
-define(XK_Hangul_J_PieubSios, 16#ee5).
-define(XK_Hangul_J_Sios, 16#ee6).
-define(XK_Hangul_J_SsangSios, 16#ee7).
-define(XK_Hangul_J_Ieung, 16#ee8).
-define(XK_Hangul_J_Jieuj, 16#ee9).
-define(XK_Hangul_J_Cieuc, 16#eea).
-define(XK_Hangul_J_Khieuq, 16#eeb).
-define(XK_Hangul_J_Tieut, 16#eec).
-define(XK_Hangul_J_Phieuf, 16#eed).
-define(XK_Hangul_J_Hieuh, 16#eee).

%% Ancient Hangul Consonant Characters 
-define(XK_Hangul_RieulYeorinHieuh, 16#eef).
-define(XK_Hangul_SunkyeongeumMieum, 16#ef0).
-define(XK_Hangul_SunkyeongeumPieub, 16#ef1).
-define(XK_Hangul_PanSios, 16#ef2).
-define(XK_Hangul_KkogjiDalrinIeung, 16#ef3).
-define(XK_Hangul_SunkyeongeumPhieuf, 16#ef4).
-define(XK_Hangul_YeorinHieuh, 16#ef5).

%% Ancient Hangul Vowel Characters 
-define(XK_Hangul_AraeA, 16#ef6).
-define(XK_Hangul_AraeAE, 16#ef7).

%% Ancient Hangul syllable-final (JongSeong) Characters 
-define(XK_Hangul_J_PanSios, 16#ef8).
-define(XK_Hangul_J_KkogjiDalrinIeung, 16#ef9).
-define(XK_Hangul_J_YeorinHieuh, 16#efa).

%% Korean currency symbol 
-define(XK_Korean_Won, 16#eff).

%%
%%   Armenian
%%   Byte 3 = 16#14
 

-define(XK_Armenian_eternity, 16#14a1).
-define(XK_Armenian_ligature_ew, 16#14a2).
-define(XK_Armenian_full_stop, 16#14a3).
-define(XK_Armenian_verjaket, 16#14a3).
-define(XK_Armenian_parenright, 16#14a4).
-define(XK_Armenian_parenleft, 16#14a5).
-define(XK_Armenian_guillemotright, 16#14a6).
-define(XK_Armenian_guillemotleft, 16#14a7).
-define(XK_Armenian_em_dash, 16#14a8).
-define(XK_Armenian_dot, 16#14a9).
-define(XK_Armenian_mijaket, 16#14a9).
-define(XK_Armenian_separation_mark, 16#14aa).
-define(XK_Armenian_but, 16#14aa).
-define(XK_Armenian_comma, 16#14ab).
-define(XK_Armenian_en_dash, 16#14ac).
-define(XK_Armenian_hyphen, 16#14ad).
-define(XK_Armenian_yentamna, 16#14ad).
-define(XK_Armenian_ellipsis, 16#14ae).
-define(XK_Armenian_exclam, 16#14af).
-define(XK_Armenian_amanak, 16#14af).
-define(XK_Armenian_accent, 16#14b0).
-define(XK_Armenian_shesht, 16#14b0).
-define(XK_Armenian_question, 16#14b1).
-define(XK_Armenian_paruyk, 16#14b1).
-define(XK_Armenian_AYB, 16#14b2).
-define(XK_Armenian_ayb, 16#14b3).
-define(XK_Armenian_BEN, 16#14b4).
-define(XK_Armenian_ben, 16#14b5).
-define(XK_Armenian_GIM, 16#14b6).
-define(XK_Armenian_gim, 16#14b7).
-define(XK_Armenian_DA, 16#14b8).
-define(XK_Armenian_da, 16#14b9).
-define(XK_Armenian_YECH, 16#14ba).
-define(XK_Armenian_yech, 16#14bb).
-define(XK_Armenian_ZA, 16#14bc).
-define(XK_Armenian_za, 16#14bd).
-define(XK_Armenian_E, 16#14be).
-define(XK_Armenian_e, 16#14bf).
-define(XK_Armenian_AT, 16#14c0).
-define(XK_Armenian_at, 16#14c1).
-define(XK_Armenian_TO, 16#14c2).
-define(XK_Armenian_to, 16#14c3).
-define(XK_Armenian_ZHE, 16#14c4).
-define(XK_Armenian_zhe, 16#14c5).
-define(XK_Armenian_INI, 16#14c6).
-define(XK_Armenian_ini, 16#14c7).
-define(XK_Armenian_LYUN, 16#14c8).
-define(XK_Armenian_lyun, 16#14c9).
-define(XK_Armenian_KHE, 16#14ca).
-define(XK_Armenian_khe, 16#14cb).
-define(XK_Armenian_TSA, 16#14cc).
-define(XK_Armenian_tsa, 16#14cd).
-define(XK_Armenian_KEN, 16#14ce).
-define(XK_Armenian_ken, 16#14cf).
-define(XK_Armenian_HO, 16#14d0).
-define(XK_Armenian_ho, 16#14d1).
-define(XK_Armenian_DZA, 16#14d2).
-define(XK_Armenian_dza, 16#14d3).
-define(XK_Armenian_GHAT, 16#14d4).
-define(XK_Armenian_ghat, 16#14d5).
-define(XK_Armenian_TCHE, 16#14d6).
-define(XK_Armenian_tche, 16#14d7).
-define(XK_Armenian_MEN, 16#14d8).
-define(XK_Armenian_men, 16#14d9).
-define(XK_Armenian_HI, 16#14da).
-define(XK_Armenian_hi, 16#14db).
-define(XK_Armenian_NU, 16#14dc).
-define(XK_Armenian_nu, 16#14dd).
-define(XK_Armenian_SHA, 16#14de).
-define(XK_Armenian_sha, 16#14df).
-define(XK_Armenian_VO, 16#14e0).
-define(XK_Armenian_vo, 16#14e1).
-define(XK_Armenian_CHA, 16#14e2).
-define(XK_Armenian_cha, 16#14e3).
-define(XK_Armenian_PE, 16#14e4).
-define(XK_Armenian_pe, 16#14e5).
-define(XK_Armenian_JE, 16#14e6).
-define(XK_Armenian_je, 16#14e7).
-define(XK_Armenian_RA, 16#14e8).
-define(XK_Armenian_ra, 16#14e9).
-define(XK_Armenian_SE, 16#14ea).
-define(XK_Armenian_se, 16#14eb).
-define(XK_Armenian_VEV, 16#14ec).
-define(XK_Armenian_vev, 16#14ed).
-define(XK_Armenian_TYUN, 16#14ee).
-define(XK_Armenian_tyun, 16#14ef).
-define(XK_Armenian_RE, 16#14f0).
-define(XK_Armenian_re, 16#14f1).
-define(XK_Armenian_TSO, 16#14f2).
-define(XK_Armenian_tso, 16#14f3).
-define(XK_Armenian_VYUN, 16#14f4).
-define(XK_Armenian_vyun, 16#14f5).
-define(XK_Armenian_PYUR, 16#14f6).
-define(XK_Armenian_pyur, 16#14f7).
-define(XK_Armenian_KE, 16#14f8).
-define(XK_Armenian_ke, 16#14f9).
-define(XK_Armenian_O, 16#14fa).
-define(XK_Armenian_o, 16#14fb).
-define(XK_Armenian_FE, 16#14fc).
-define(XK_Armenian_fe, 16#14fd).
-define(XK_Armenian_apostrophe, 16#14fe).
-define(XK_Armenian_section_sign, 16#14ff).

%%
%%   Georgian
%%   Byte 3 = 16#15
 

-define(XK_Georgian_an, 16#15d0).
-define(XK_Georgian_ban, 16#15d1).
-define(XK_Georgian_gan, 16#15d2).
-define(XK_Georgian_don, 16#15d3).
-define(XK_Georgian_en, 16#15d4).
-define(XK_Georgian_vin, 16#15d5).
-define(XK_Georgian_zen, 16#15d6).
-define(XK_Georgian_tan, 16#15d7).
-define(XK_Georgian_in, 16#15d8).
-define(XK_Georgian_kan, 16#15d9).
-define(XK_Georgian_las, 16#15da).
-define(XK_Georgian_man, 16#15db).
-define(XK_Georgian_nar, 16#15dc).
-define(XK_Georgian_on, 16#15dd).
-define(XK_Georgian_par, 16#15de).
-define(XK_Georgian_zhar, 16#15df).
-define(XK_Georgian_rae, 16#15e0).
-define(XK_Georgian_san, 16#15e1).
-define(XK_Georgian_tar, 16#15e2).
-define(XK_Georgian_un, 16#15e3).
-define(XK_Georgian_phar, 16#15e4).
-define(XK_Georgian_khar, 16#15e5).
-define(XK_Georgian_ghan, 16#15e6).
-define(XK_Georgian_qar, 16#15e7).
-define(XK_Georgian_shin, 16#15e8).
-define(XK_Georgian_chin, 16#15e9).
-define(XK_Georgian_can, 16#15ea).
-define(XK_Georgian_jil, 16#15eb).
-define(XK_Georgian_cil, 16#15ec).
-define(XK_Georgian_char, 16#15ed).
-define(XK_Georgian_xan, 16#15ee).
-define(XK_Georgian_jhan, 16#15ef).
-define(XK_Georgian_hae, 16#15f0).
-define(XK_Georgian_he, 16#15f1).
-define(XK_Georgian_hie, 16#15f2).
-define(XK_Georgian_we, 16#15f3).
-define(XK_Georgian_har, 16#15f4).
-define(XK_Georgian_hoe, 16#15f5).
-define(XK_Georgian_fi, 16#15f6).

%%
%% Azeri (and other Turkic or Caucasian languages of ex-USSR)
%% Byte 3 = 16#16
 

%% latin 
-define(XK_Ccedillaabovedot, 16#16a2).
-define(XK_Xabovedot, 16#16a3).
-define(XK_Qabovedot, 16#16a5).
-define(XK_Ibreve, 16#16a6).
-define(XK_IE, 16#16a7).
-define(XK_UO, 16#16a8).
-define(XK_Zstroke, 16#16a9).
-define(XK_Gcaron, 16#16aa).
-define(XK_Obarred, 16#16af).
-define(XK_ccedillaabovedot, 16#16b2).
-define(XK_xabovedot, 16#16b3).
-define(XK_Ocaron, 16#16b4).
-define(XK_qabovedot, 16#16b5).
-define(XK_ibreve, 16#16b6).
-define(XK_ie, 16#16b7).
-define(XK_uo, 16#16b8).
-define(XK_zstroke, 16#16b9).
-define(XK_gcaron, 16#16ba).
-define(XK_ocaron, 16#16bd).
-define(XK_obarred, 16#16bf).
-define(XK_SCHWA, 16#16c6).
-define(XK_schwa, 16#16f6).
%% those are not really Caucasus, but I put them here for now 
%% For Inupiak 
-define(XK_Lbelowdot, 16#16d1).
-define(XK_Lstrokebelowdot, 16#16d2).
-define(XK_lbelowdot, 16#16e1).
-define(XK_lstrokebelowdot, 16#16e2).
%% For Guarani 
-define(XK_Gtilde, 16#16d3).
-define(XK_gtilde, 16#16e3).

%%
%%   Vietnamese
%%   Byte 3 = 16#1e
 
 
-define(XK_Abelowdot, 16#1ea0).
-define(XK_abelowdot, 16#1ea1).
-define(XK_Ahook, 16#1ea2).
-define(XK_ahook, 16#1ea3).
-define(XK_Acircumflexacute, 16#1ea4).
-define(XK_acircumflexacute, 16#1ea5).
-define(XK_Acircumflexgrave, 16#1ea6).
-define(XK_acircumflexgrave, 16#1ea7).
-define(XK_Acircumflexhook, 16#1ea8).
-define(XK_acircumflexhook, 16#1ea9).
-define(XK_Acircumflextilde, 16#1eaa).
-define(XK_acircumflextilde, 16#1eab).
-define(XK_Acircumflexbelowdot, 16#1eac).
-define(XK_acircumflexbelowdot, 16#1ead).
-define(XK_Abreveacute, 16#1eae).
-define(XK_abreveacute, 16#1eaf).
-define(XK_Abrevegrave, 16#1eb0).
-define(XK_abrevegrave, 16#1eb1).
-define(XK_Abrevehook, 16#1eb2).
-define(XK_abrevehook, 16#1eb3).
-define(XK_Abrevetilde, 16#1eb4).
-define(XK_abrevetilde, 16#1eb5).
-define(XK_Abrevebelowdot, 16#1eb6).
-define(XK_abrevebelowdot, 16#1eb7).
-define(XK_Ebelowdot, 16#1eb8).
-define(XK_ebelowdot, 16#1eb9).
-define(XK_Ehook, 16#1eba).
-define(XK_ehook, 16#1ebb).
-define(XK_Etilde, 16#1ebc).
-define(XK_etilde, 16#1ebd).
-define(XK_Ecircumflexacute, 16#1ebe).
-define(XK_ecircumflexacute, 16#1ebf).
-define(XK_Ecircumflexgrave, 16#1ec0).
-define(XK_ecircumflexgrave, 16#1ec1).
-define(XK_Ecircumflexhook, 16#1ec2).
-define(XK_ecircumflexhook, 16#1ec3).
-define(XK_Ecircumflextilde, 16#1ec4).
-define(XK_ecircumflextilde, 16#1ec5).
-define(XK_Ecircumflexbelowdot, 16#1ec6).
-define(XK_ecircumflexbelowdot, 16#1ec7).
-define(XK_Ihook, 16#1ec8).
-define(XK_ihook, 16#1ec9).
-define(XK_Ibelowdot, 16#1eca).
-define(XK_ibelowdot, 16#1ecb).
-define(XK_Obelowdot, 16#1ecc).
-define(XK_obelowdot, 16#1ecd).
-define(XK_Ohook, 16#1ece).
-define(XK_ohook, 16#1ecf).
-define(XK_Ocircumflexacute, 16#1ed0).
-define(XK_ocircumflexacute, 16#1ed1).
-define(XK_Ocircumflexgrave, 16#1ed2).
-define(XK_ocircumflexgrave, 16#1ed3).
-define(XK_Ocircumflexhook, 16#1ed4).
-define(XK_ocircumflexhook, 16#1ed5).
-define(XK_Ocircumflextilde, 16#1ed6).
-define(XK_ocircumflextilde, 16#1ed7).
-define(XK_Ocircumflexbelowdot, 16#1ed8).
-define(XK_ocircumflexbelowdot, 16#1ed9).
-define(XK_Ohornacute, 16#1eda).
-define(XK_ohornacute, 16#1edb).
-define(XK_Ohorngrave, 16#1edc).
-define(XK_ohorngrave, 16#1edd).
-define(XK_Ohornhook, 16#1ede).
-define(XK_ohornhook, 16#1edf).
-define(XK_Ohorntilde, 16#1ee0).
-define(XK_ohorntilde, 16#1ee1).
-define(XK_Ohornbelowdot, 16#1ee2).
-define(XK_ohornbelowdot, 16#1ee3).
-define(XK_Ubelowdot, 16#1ee4).
-define(XK_ubelowdot, 16#1ee5).
-define(XK_Uhook, 16#1ee6).
-define(XK_uhook, 16#1ee7).
-define(XK_Uhornacute, 16#1ee8).
-define(XK_uhornacute, 16#1ee9).
-define(XK_Uhorngrave, 16#1eea).
-define(XK_uhorngrave, 16#1eeb).
-define(XK_Uhornhook, 16#1eec).
-define(XK_uhornhook, 16#1eed).
-define(XK_Uhorntilde, 16#1eee).
-define(XK_uhorntilde, 16#1eef).
-define(XK_Uhornbelowdot, 16#1ef0).
-define(XK_uhornbelowdot, 16#1ef1).
-define(XK_Ybelowdot, 16#1ef4).
-define(XK_ybelowdot, 16#1ef5).
-define(XK_Yhook, 16#1ef6).
-define(XK_yhook, 16#1ef7).
-define(XK_Ytilde, 16#1ef8).
-define(XK_ytilde, 16#1ef9).
-define(XK_Ohorn, 16#1efa). %% U+01a0 
-define(XK_ohorn, 16#1efb). %% U+01a1 
-define(XK_Uhorn, 16#1efc). %% U+01af 
-define(XK_uhorn, 16#1efd). %% U+01b0 

-define(XK_combining_tilde, 16#1e9f). %% U+0303 
-define(XK_combining_grave, 16#1ef2). %% U+0300 
-define(XK_combining_acute, 16#1ef3). %% U+0301 
-define(XK_combining_hook, 16#1efe). %% U+0309 
-define(XK_combining_belowdot, 16#1eff). %% U+0323 

-define(XK_EcuSign, 16#20a0).
-define(XK_ColonSign, 16#20a1).
-define(XK_CruzeiroSign, 16#20a2).
-define(XK_FFrancSign, 16#20a3).
-define(XK_LiraSign, 16#20a4).
-define(XK_MillSign, 16#20a5).
-define(XK_NairaSign, 16#20a6).
-define(XK_PesetaSign, 16#20a7).
-define(XK_RupeeSign, 16#20a8).
-define(XK_WonSign, 16#20a9).
-define(XK_NewSheqelSign, 16#20aa).
-define(XK_DongSign, 16#20ab).
-define(XK_EuroSign, 16#20ac).

-endif.
