-module(test_gregexp).
-export([run/0]).

run() ->
    {match, ["name","domain.com"]} =
	groups(" name@domain.com ", " *\\([^ ]+\\)@\\([^ ]+\\)"),
    {match, []} = groups("abcd", ".*bc"),
    {match, ["bcd"]} = groups("abcdefgh", "a\\(.*\\)efgh"),
    {match, ["cd","h"]} = groups("abcdefh", ".*\\(c.\\).*\\(.\\)"),

    RE_URL="\\(.+\\)://\\(.+\\)\\(/.+\\)(\\?\\(.*\\)(&\\(.*\\))*)?",
    {match, ["http","localhost:80","/script"]} =
	groups("http://localhost:80/script", RE_URL),
    {match, ["http","localhost","/script","arg"]} =
	groups("http://localhost/script?arg", RE_URL),
    {match, ["http","localhost","/script","arg","arg2"]} =
	groups("http://localhost/script?arg&arg2", RE_URL),
    {match, ["http","localhost:81","/script","arg","arg2","arg3"]} =
	groups("http://localhost:81/script?arg&arg2&arg3", RE_URL),
    ok.

groups(S, RE) ->
    Res = gregexp:groups(S, RE),
    io:format("~p: ~p~n", [S, Res]),
    Res.
