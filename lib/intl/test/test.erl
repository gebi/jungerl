%%% File    : test.erl
%%% Author  :  <tony@bit.hemma.se>
%%% Description : small intl test
%%% Created :  9 Sep 2003 by  <tony@bit.hemma.se>

-module(test).

-compile(export_all).

-include("../include/intl.hrl").

start() ->
    TestDir = filename:join([code:lib_dir(intl), "test"]),
    intl:start(),
    intl:setlocale(messages, "sv_SE"),
    intl:bindtextdomain("test", TestDir),
    intl:textdomain("test"),
    intl:bind_textdomain_codeset("test", "ISO-8859-1"),
    ok.
    

t1() ->
    io:format(?_("Hello world"), []),
    io:format(?_("Bend over"), []).

    





