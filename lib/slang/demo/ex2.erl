%%%----------------------------------------------------------------------
%%% File    : ex2.erl
%%% Author  : Claes Wikstrom <klacke@kaja.hemma.net>
%%% Purpose : 
%%% Created : 30 Nov 2000 by Claes Wikstrom <klacke@kaja.hemma.net>
%%%----------------------------------------------------------------------

-module(ex2).
-author('klacke@kaja.hemma.net').

-compile(export_all).

start() ->
    slang:tt_get_terminfo(),
    slang:kp_init(),
    slang:init_tty(7, 0, 1),
    slang:smg_init_smg (),
    
    draw_stuff2(),

    Key = slang:kp_getkey(),
    slang:smg_printf("Just got key ~p~n", [Key]),
    slang:smg_refresh (),

    slang:kp_getkey(),

    slang:smg_reset_smg (),
    slang:reset_tty(),
    halt().


draw_stuff2() ->
    slang:smg_normal_video (),
    slang:smg_gotorc (5, 0),
    slang:smg_write_string ("Hello "),
    slang:smg_erase_eol (),
    slang:smg_refresh ().


draw_stuff(Pos) ->
    case slang:getkey() of
	$q ->
	    ok;
	Key ->
	    slang:smg_gotorc(4+Pos,6),
	    slang:smg_printf("Hello there ~n",[]),
	    draw_stuff(Pos+1)
    end.




