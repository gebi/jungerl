%%% File    : test.erl
%%% Author  :  <tony@bit.hemma.se>
%%% Description : small intl test
%%% Created :  9 Sep 2003 by  <tony@bit.hemma.se>

-module(test).

-compile(export_all).

-include("../include/intl.hrl").

-import(intl, [gettext/1, dcgettext/3]).

start() ->
    start("sv_SE").

start(Locale) ->
    LocaleDir = filename:join([code:lib_dir(intl), "test", "locale"]),
    intl:start(),
    intl:setlocale(messages, Locale),
    intl:bindtextdomain("test", LocaleDir),
    intl:textdomain("test"),
    intl:bind_textdomain_codeset("test", "ISO-8859-1"),
    ok.
    

t1() ->
    io:format(?_("Hello world\n"), []),
    io:format(?_("Bend over\n"), []),
    io:format(?N_("This is translated later\n"), []).


t2() ->
    io:put_chars(intl:gettext("You got mail\n")),
    io:put_chars(intl:ngettext("You got one mail\n", "You got mail\n",
			       15)),
    io:put_chars(intl:dgettext("test", "You got virus\n")),
    io:put_chars(intl:dngettext("test", "You got one virus\n", 
				"You got plenty of viruses\n",
				20)),
    io:format(intl:dcgettext("test", "You got a ~w coin\n",messages),
	      [40]),
    io:format(intl:dcngettext("test", "You got a ~w coin\n", 
			      "You got tons of ~w\n", 30, messages),
	      [1000]).

t3() ->
    io:put_chars(gettext("You got mail\n")),
    io:format(dcgettext("test", "You got a ~w coin\n",messages),
	      [40]).


    

    
    
    


    





