
-module(posix_test).
-define(DRV, posix_drv).

-include("posix_drv.hrl").
-include_lib("kernel/include/file.hrl").

-export([regression/0]).

regression() ->
    {ok, Port} = ?DRV:start(),
    %% {ok, 1} = ?DRV:debug(Port, 1),
    ok = r1(Port),
    io:format("All regression tests PASSED.\n"),
    ok.

r1(P) ->
    %% We don't know what our real & effective uid & gid is, so we'll
    %% just make certain that they're integers.
    case ?DRV:getuid(P) of
	{ok, U1} when integer(U1) -> ok
    end,
    case ?DRV:geteuid(P) of
	{ok, U2} when integer(U2) -> ok
    end,
    case ?DRV:getgid(P) of
	{ok, G1} when integer(G1) -> ok
    end,
    case ?DRV:getegid(P) of
	{ok, G2} when integer(G2) -> ok
    end,

    %% We'll pick on some things that should be found in any UNIX file system hierarchy.
    case ?DRV:lstat(P, "/") of
	{ok, STAT1} when record(STAT1, stat) -> ok
    end,
    case ?DRV:lstat(P, "/does-not-exist-here") of
	{error, N} when integer(N) -> ok	% Dunno what ENOENT's value is
    end,

    %% Choose accounts that we know/hope do or don't exist
    case ?DRV:getpwnam(P, "root") of
	{ok, PASSWD1} when record(PASSWD1, passwd) -> ok
    end,
    case ?DRV:getpwnam(P, "does-not-exist") of
	{error, 0} -> ok
    end,
    %% UID 0 should match "root"'s passwd entry
    case ?DRV:getpwuid(P, 0) of
	{ok, PASSWD1} when record(PASSWD1, passwd) -> ok
    end,
    case ?DRV:getpwuid(P, 987654321) of
	{error, 0} -> ok
    end,

    %% Choose groups that we know/hope do or don't exist
    case ?DRV:getgrnam(P, "wheel") of
	{ok, GROUP1} when record(GROUP1, group) -> ok
    end,
    case ?DRV:getgrnam(P, "does-not-exist") of
	{error, 0} -> ok
    end,
    %% GID 0 is not necessarily "wheel", so don't match against GROUP1
    case ?DRV:getgrgid(P, 0) of
	{ok, GROUP2} when record(GROUP2, group) -> ok
    end,
    case ?DRV:getgrgid(P, 987654321) of
	{error, 0} -> ok
    end,

    case ?DRV:getgroups(P) of
	{ok, {N1, L1}} when integer(N1), list(L1) -> ok
    end,

    case ?DRV:getlogin(P) of
	{ok, LOGIN1} when binary(LOGIN1) -> ok
    end,

    case ?DRV:getpgrp(P) of
	{ok, PGRP1} when integer(PGRP1) -> ok
    end,
    case ?DRV:getsid(P) of
	{ok, SID1} when integer(SID1) -> ok
    end,
    case ?DRV:getppid(P) of
	{ok, PGRP1} when integer(PGRP1) -> ok
    end,

    MyPid = list_to_integer(os:getpid()),
    {ok, 0} = ?DRV:kill(P, MyPid, 0),
    {error, _} = ?DRV:kill(P, MyPid, 987654321),
    {error, _} = ?DRV:kill(P, MyPid * 987654321, 15),

    TmpFile1 = "/tmp/VerYSiLLYnAMe",
    file:delete(TmpFile1),
    {ok, 0} = ?DRV:mkfifo(P, TmpFile1, 8#640),
    {ok, FileInfo1} = file:read_file_info(TmpFile1),
    8#640 = FileInfo1#file_info.mode band 8#777,
    ok = file:delete(TmpFile1),

    TmpFile2 = "/tmp/VerYSiLLYnAMe2",
    file:delete(TmpFile2),
    case {?DRV:mknod(P, TmpFile2, 8#640, 16#fe), ?DRV:geteuid(P)} of
	{{ok, 0}, 0} ->
	    io:format("\nWARNING: mknod call succeeded, why are your running this test as superuser??\n\n"),
	    timer:sleep(3000),
	    {ok, FileInfo2} = file:read_file_info(TmpFile2),
	    8#640 = FileInfo2#file_info.mode band 8#777,
	    ok = file:delete(TmpFile2);
	{{error, _}, X} when X =/= 0 ->
	    ok
    end,

    {ok, OldUMask} = ?DRV:umask(P, 8#222),
    {ok, 8#222} = ?DRV:umask(P, OldUMask),

    ok.

