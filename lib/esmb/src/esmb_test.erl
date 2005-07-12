%%%-------------------------------------------------------------------
%%% File    : esmb_test.erl
%%% Created : 14 Mar 2005 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%%-------------------------------------------------------------------
-module(esmb_test).
-export([start/1, start/2]).

-include("esmb_lib.hrl").

-define(lmsg(X,Y), get(pid) ! {X,Y}).

-define(log(T,X,Y), ?lmsg("TC(~w) ~p(~p): " X,
		 	  [T, ?MODULE, ?LINE | Y])).

-define(TC(N,Str), "TC("++i2l(N)++") "++Str).

-define(SZ_10K, 10000).

%%%
%%% Example: esmb_test:start("smb://tobbe@pungmes/tobbe")
%%%
start(Uri) ->
    Passwd = get_passwd(),
    start(Uri, Passwd).

start(Uri, Passwd) ->
    Self = self(),
    spawn_link(fun() -> go(Uri, Passwd, Self) end),
    loop().

loop() ->
    receive
	{Fmt,Args} -> 
	    io:format(Fmt, Args),
	    loop();
	stop ->
	    finished
    end.
   

go(Uri, Passwd, Pid) ->
    put(pid, Pid),
    md4:start(),
    iconv:start(),
    U0 = esmb:parse_uri(Uri),
    U = U0#user{pw = Passwd},
    Summary = do_all_testcases(U),
    ?lmsg("~n~s~n", [Summary]),
    Pid ! stop.

do_all_testcases(U) ->
    lists:map(fun(F) -> summary(F(info), catch F(U)) end, all_testcases()).

summary(Info, {error, Emsg}) when list(Emsg) ->
    lists:flatten(io_lib:format("~s: <Error> ~s~n", [Info, Emsg]));
summary(Info, Res) ->
    lists:flatten(io_lib:format("~s: ~p~n", [Info, Res])).

all_testcases() ->
    Start = 1,
    Stop  = 9,
    [tc(N) || N <- lists:seq(Start,Stop,1)].

tc(N) ->
    fun(U) -> tc(N,U) end.

%%%
%%% NB: When you add a test case, don't forget to also
%%%     change the 'Stop' variable in all_testcases/0 !!
%%%
tc(1, info) ->
    "TC(1) tree_connect operation";
tc(1 = N, U) ->
    case esmb:connect(U#user.host) of
	{ok,S,Neg} ->
	    case esmb:user_logon(S, Neg, U) of
		?IS_ERROR(Pdu0) ->
		    Emsg = ?EMSG(Pdu0),
		    ?log(N, "Login error: ~p~n", [Emsg]),
		    esmb:close(S),
		    {error, Emsg};
		?IS_OK(Pdu0) ->
		    Path = mk_path(U, Pdu0),
		    case esmb:tree_connect(S, Pdu0#smbpdu.neg, Pdu0, Path) of
			?IS_ERROR(Pdu1) ->
			    Emsg1 = ?EMSG(Pdu1),
			    ?log(N, "Login error: ~p~n", [Emsg1]),
			    esmb:close(S),
			    {error, Emsg1};
			?IS_OK(Pdu1) ->
			    esmb:close(S),
			    ok
		    end
	    end;
	Else ->
	    ?log(N, "Failed to connect: ~p~n", [Else]),
	    {error, Else}
    end;
%%%
%%% ---
%%%
tc(2, info) ->
    "TC(2) delete dir: esmb_test";
tc(2 = N, U) ->
    {S, Pdu} = setup(U),
    case esmb:rmdir(S, Pdu, to_ucs2(Pdu, "\\esmb_test")) of
	?IS_ERROR(Pdu1) ->
	    Emsg = ?EMSG(Pdu1),
	    ?log(N, "rmdir error (eclass=~p, ecode=~p): ~p~n", 
		 [Pdu1#smbpdu.eclass, Pdu1#smbpdu.ecode, Emsg]),
	    esmb:close(S),
	    {error, Emsg};
	?IS_OK(Pdu1) ->
	    esmb:close(S),
	    ok
    end;
%%%
%%% ---
%%%
tc(3, info) ->
    "TC(3) delete non-existing dir: esmb_test";
tc(3 = N, U) ->
    {S, Pdu} = setup(U),
    case esmb:rmdir(S, Pdu, to_ucs2(Pdu, "\\esmb_test")) of
	?IS_ERROR(Pdu1) ->
	    Emsg = ?EMSG(Pdu1),
	    ?log(N, "rmdir error (eclass=~p, ecode=~p): ~p~n", 
		 [Pdu1#smbpdu.eclass, Pdu1#smbpdu.ecode, Emsg]),
	    esmb:close(S),
	    {error, Emsg};
	?IS_OK(Pdu1) ->
	    esmb:close(S),
	    ok
    end;
%%%
%%% ---
%%%
tc(4, info) ->
    "TC(4) create dir: esmb_test";
tc(4 = N, U) ->
    {S, Pdu} = setup(U),
    case esmb:mkdir(S, Pdu, to_ucs2(Pdu, "\\esmb_test")) of
	?IS_ERROR(Pdu1) ->
	    Emsg = ?EMSG(Pdu1),
	    ?log(N, "mkdir error: ~p~n", [Emsg]),
	    esmb:close(S),
	    {error, Emsg};
	Pdu1 ->
	    esmb:close_file(S, Pdu1, Pdu1#smbpdu.fid),
	    esmb:close(S),
	    ok
    end;
%%%
%%% ---
%%%
tc(5, info) ->
    "TC(5) create existing dir: esmb_test";
tc(5 = N, U) ->
    {S, Pdu} = setup(U),
    case esmb:mkdir(S, Pdu, to_ucs2(Pdu, "\\esmb_test")) of
	?IS_ERROR(Pdu1) ->
	    Emsg = ?EMSG(Pdu1),
	    ?log(N, "mkdir error: ~p~n", [Emsg]),
	    esmb:close(S),
	    {error, Emsg};
	Pdu1 ->
	    esmb:close_file(S, Pdu1, Pdu1#smbpdu.fid),
	    esmb:close(S),
	    ok
    end;
%%%
%%% ---
%%%
tc(6, info) ->
    "TC(6) upload: 10k.file";
tc(6 = N, U) ->
    {S, Pdu} = setup(U),
    Fname = to_ucs2(Pdu, "\\esmb_test\\10k.file"),
    case esmb:open_file_rw(S, Pdu, Fname) of
	?IS_ERROR(Pdu1) ->
	    Emsg = ?EMSG(Pdu1),
	    ?log(N, "open_file_rw error(eclass=~p, ecode=~p): ~p~n", 
		 [Pdu1#smbpdu.eclass, Pdu1#smbpdu.ecode, Emsg]),
	    esmb:close(S),
	    {error, Emsg};
	Pdu1 ->
	    Bin = mk_bin(?SZ_10K),
	    Finfo = #file_info{name = Fname, data = [Bin]},
	    case esmb:write_file(S, Pdu1, Finfo) of
		?IS_OK(Pdu2) ->
		    ?log(N, "write_file wrote: ~p bytes~n", [?WRITTEN(Pdu2)]),
		    esmb:close_file(S, Pdu1, Pdu1#smbpdu.fid),
		    esmb:close(S),
		    ok;
		Else ->
		    ?log(N, "write_file error: ~p~n", [Else]),
		    esmb:close(S),
		    {error, Else}
	    end
    end;
%%%
%%% ---
%%%
tc(7, info) ->
    "TC(7) delete non-empty dir: esmb_test";
tc(7 = N, U) ->
    tc(2, U);
%%%
%%% ---
%%%
tc(8, info) ->
    "TC(8) delete: 10k.file";
tc(8 = N, U) ->
    {S, Pdu} = setup(U),
    Fname = to_ucs2(Pdu, "\\esmb_test\\10k.file"),
    case esmb:delete_file(S, Pdu, Fname) of
	?IS_ERROR(Pdu1) ->
	    Emsg = ?EMSG(Pdu1),
	    ?log(N, "open_file_rw error(eclass=~p, ecode=~p): ~p~n", 
		 [Pdu1#smbpdu.eclass, Pdu1#smbpdu.ecode, Emsg]),
	    esmb:close(S),
	    {error, Emsg};
	_Pdu1 ->
	    esmb:close(S),
	    ok
    end;
%%%
%%% ---
%%%
tc(9 = N, info) ->
    ?TC(N,"tree_connect operation to misspelled share");
tc(9 = N, U0) ->
    U = U0#user{share = U0#user.share ++ "JUNK"},
    case esmb:connect(U#user.host) of
	{ok,S,Neg} ->
	    case esmb:user_logon(S, Neg, U) of
		?IS_ERROR(Pdu0) ->
		    Emsg = ?EMSG(Pdu0),
		    ?log(N, "Login error: ~p~n", [Emsg]),
		    esmb:close(S),
		    {error, Emsg};
		?IS_OK(Pdu0) ->
		    Path = mk_path(U, Pdu0),
		    case esmb:tree_connect(S, Pdu0#smbpdu.neg, Pdu0, Path) of
			?IS_ERROR(Pdu1) ->
			    Emsg1 = ?EMSG(Pdu1),
			    ?log(N, "Login error: ~p~n", [Emsg1]),
			    esmb:close(S),
			    {error, Emsg1};
			?IS_OK(Pdu1) ->
			    esmb:close(S),
			    ok
		    end
	    end;
	Else ->
	    ?log(N, "Failed to connect: ~p~n", [Else]),
	    {error, Else}
    end.


mk_bin(Size) when Size > 0 ->
    l2b(mk_list(1, Size)).

mk_list(Stop, Stop)                    -> [0];
mk_list(Start, Stop) when Start < Stop -> [0|mk_list(Start + 1, Stop)].


setup(U) ->
    case esmb:connect(U#user.host) of
	{ok,S,Neg} ->
	    case catch esmb:user_logon(S, Neg, U) of
		{'EXIT', Reason} ->
		    ?elog("Login failed, reason: ~p~n", [Reason]),
		    {error, Reason};
		?IS_ERROR(Pdu0) ->
		    Emsg = ?EMSG(Pdu0),
		    ?elog("Login error: ~p~n", [Emsg]),
		    esmb:close(S),
		    {error, Emsg};
		Pdu0 ->
		    Path = mk_path(U, Pdu0),
		    case catch esmb:tree_connect(S, Pdu0#smbpdu.neg, Pdu0, Path) of
			{'EXIT', Reason} ->
			    ?elog("Tree-Connect failed, reason: ~p~n", [Reason]),
			    {error, Reason};
			?IS_ERROR(Pdu1) ->
			    Emsg1 = ?EMSG(Pdu1),
			    ?elog("Tree-Connect error: ~p~n", [Emsg1]),
			    esmb:close(S),
			    {error, Emsg1};
			Pdu1 ->
			    {S, Pdu1}
		    end
	    end;
	Else ->
	    ?elog("Failed to connect: ~p~n", [Else]),
	    {error, Else}
    end.

mk_path(U, Pdu) ->
    Path = "\\\\"++U#user.host++"\\"++U#user.share,
    to_ucs2(Pdu, Path).

to_ucs2(Pdu, Str) ->
    U = #user{},
    to_ucs2(Pdu#smbpdu.neg, Str, U#user.charset).
    
to_ucs2(Neg, Str, Cset) when ?USE_UNICODE(Neg) -> cset_to_ucs2(Str, Cset);
to_ucs2(_, Str, _)                             -> Str.

cset_to_ucs2(Str, Cset) ->
    case iconv:open(?CSET_UCS2LE, esmb:ucase(Cset)) of
	{ok, Cd} ->
	    Rstr = case iconv:conv(Cd, l2b(Str)) of
		       {ok, Ustr}      -> Ustr;
		       {error, Reason} ->
			   Str
		   end,
	    iconv:close(Cd),
	    Rstr;
	{error, Reason} ->
	    Str
    end.

l2b(L) when list(L)   -> list_to_binary(L);
l2b(B) when binary(B) -> B.

i2l(I) when integer(I) -> integer_to_list(I);
i2l(L) when list(L)    -> L.

get_passwd() ->
    rm_last_char(io:get_line('Password: ')).

rm_last_char([H])   -> [];
rm_last_char([H|T]) -> [H|rm_last_char(T)].

sleep(T) -> receive after T -> true end.
