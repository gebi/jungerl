%%% File    : test.erl
%%% Author  :  <tony@bit.hemma.se>
%%% Description : small eintl test
%%% Created :  9 Sep 2003 by  <tony@bit.hemma.se>

-module(etest).

-compile(export_all).

-include("../include/eintl.hrl").

-import(eintl, [gettext/1, dcgettext/3]).

start() ->
    start("sv_SE").

start(Locale) ->
    LocaleDir = filename:join([code:lib_dir(intl), "test", "locale"]),
    eintl:start(),
    eintl:setlocale(messages, Locale),
    eintl:bindtextdomain("test", LocaleDir),
    eintl:textdomain("test"),
    eintl:bind_textdomain_codeset("test", "ISO-8859-1"),
    ok.
    

t1() ->
    io:format(?_("Hello world\n"), []),
    io:format(?_("Bend over\n"), []),
    io:format(?N_("This is translated later\n"), []).


t2() ->
    io:put_chars(eintl:gettext("You got mail\n")),
    io:put_chars(eintl:ngettext("You got one mail\n", "You got mail\n",
			       15)),
    io:put_chars(eintl:dgettext("test", "You got virus\n")),
    io:put_chars(eintl:dngettext("test", "You got one virus\n", 
				"You got plenty of viruses\n",
				20)),
    io:format(eintl:dcgettext("test", "You got a ~w coin\n",messages),
	      [40]),
    io:format(eintl:dcngettext("test", "You got a ~w coin\n", 
			      "You got tons of ~w\n", 30, messages),
	      [1000]).

t3() ->
    io:put_chars(gettext("You got mail\n")),
    io:format(dcgettext("test", "You got a ~w coin\n",messages),
	      [40]).


    

    
    
    


    





