
-module(unixdom_test).
-define(DRV, unixdom_drv).

%%%
%%% Change this macro to suit your local platform
%%%
-define(MY_OS, "FreeBSD").
%-define(MY_OS, "Linux").
%-define(MY_OS, "Solaris").

-export([regression/0, tcp_listen/2, receive_fd/1]).
-export([repeat/4]).				%XXX

repeat(0, _, _, _) ->
    ok;
repeat(N, M, F, A) ->
    apply(M, F, A),
    repeat(N - 1, M, F, A).

regression() ->
    SockPath = "/tmp/sock",
    io:format("\n\nThere is no regression test yet.\n"),
    io:format("On Erlang node A, run file:delete(\"~s\").\n", [SockPath]),
    io:format("Then run ~s:tcp_listen(5555, \"~s\").\n", [?MODULE, SockPath]),
    io:format("\n"),
    io:format("On Erlang node B (on the same machine as A!), run\n"),
    io:format("~s:receive_fd(\"~s\").\n", [?MODULE, SockPath]),
    io:format("\n"),
    io:format("Then run 'telnet node-A-hostname 5555' and\n"),
    io:format("then type some stuff!\n\n"),

    io:format("All regression tests PASSED.\n"),
    ok.

tcp_listen(TcpPort, SockPath) ->
    {ok, Port} = unixdom_drv:start(),
    {ok, MasterUSock} = unixdom_drv:open(Port, "/tmp/sock", 1),
    {ok, Val} = unixdom_drv:getfd(Port, MasterUSock),
    {ok, MasterSock} = gen_tcp:fdopen(Val, []),
    prim_inet:setopts(MasterSock, [{packet, raw}]),
    prim_inet:listen(MasterSock),
    {ok, Usock} = gen_tcp:accept(MasterSock),
    {ok, Usockfd} = prim_inet:getfd(Usock),
    %
    {ok, Tcpmsock} = gen_tcp:listen(TcpPort, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
    {ok, Tcpsock} = gen_tcp:accept(Tcpmsock),
    {ok, Tcpsockfd} = prim_inet:getfd(Tcpsock),
    %
    unixdom_drv:sendfd(Port, Usockfd, Tcpsockfd),
    ?DRV:shutdown(Port).

receive_fd(SockPath) ->
    {ok, Port} = unixdom_drv:start(),
    {ok, ClntSock} = unixdom_drv:open(Port, "/tmp/sock", 0),
    {ok, ClntSockFd} = unixdom_drv:getfd(Port, ClntSock),
    {ok, Tcpsockfd} = unixdom_drv:receivefd(Port, ClntSockFd),
    io:format("TCP socket fd ~w received, looping now\n", [Tcpsockfd]),
    ?DRV:shutdown(Port),
    {ok, Tcpsock} = gen_tcp:fdopen(Tcpsockfd, []),
    gen_tcp:send(Tcpsock, "Hello, there!  Please type some stuff:\r\n"),
    readloop(Tcpsock).

readloop(Tcpsock) ->
    case gen_tcp:recv(Tcpsock, 1) of
    	{ok, B} ->
	    io:format("~s", [B]),
	    readloop(Tcpsock);
	{error, _} ->
	    ok
    end.

