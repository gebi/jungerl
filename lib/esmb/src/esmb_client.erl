-module(esmb_client).
%%% --------------------------------------------------------------------
%%% File    : esmb_client.erl
%%% Created : 30 Jan 2004 by Torbjorn Tornkvist <tobbe@bluetail.com>
%%% Purpose : Somewhat similar to Sambas 'smbclient' program
%%%
%%% $Id$
%%% --------------------------------------------------------------------
-export([start/2, start/3]).
-export([swap/3]).

-import(esmb, [ucase/1, lcase/1]).

-include("esmb_lib.hrl").


%%%
%%% Example: start("//korp/tobbe", "tobbe").
%%%
start(Path, User) ->
    start(Path, User, "WORKGROUP").

start(Path, User, Wgroup) ->
    case host_share(Path) of
	{Host, Share} ->
	    spawn(fun() -> init(Host, Share, User, Wgroup) end);
	Else ->
	    Else
    end.

init(Host, Share, User, Wgroup) ->
    Called = ucase(Host),
    Caller = esmb:caller(),
    {ok,S,Neg} = esmb:connect(Caller, Called),
    Pw = get_passwd(),
    U = #user{pw = Pw, name = User, primary_domain = Wgroup},
    Pdu0 = esmb:user_logon(S, Neg, U),
    esmb:exit_if_error(Pdu0, "Login failed"),
    Path = to_ucs2(Neg, mk_path(Called, User)),
    Pdu1 = esmb:tree_connect(S, Neg, Pdu0, Path),
    esmb:exit_if_error(Pdu1, "Tree connect failed"),
    shell(S, Neg, {Pdu1, "\\\\"}).

mk_path(Called, User) ->
    "\\\\" ++ Called ++ "\\" ++ ucase(User).

to_ucs2(Neg, Str) when ?USE_UNICODE(Neg) ->
    iconv:start(),
    {ok, Cd}   = iconv:open(?CSET_UCS2, ?CSET_ISO_8859_1),
    {ok, Ustr} = iconv:conv(Cd, l2b(Str)),
    iconv:close(Cd),
    Ustr;
to_ucs2(_, Str) ->
    Str.

to_iso8859_1(Neg, Str) when ?USE_UNICODE(Neg) ->
    iconv:start(),
    {ok, Cd}   = iconv:open(?CSET_ISO_8859_1, ?CSET_UCS2),
    Astr = case iconv:conv(Cd, l2b(Str)) of
	       {ok, Res}        -> Res;
	       {error, _Reason} -> string:copies("?", length(b2l(Str)) div 2)
	   end,
    iconv:close(Cd),
    Astr;
to_iso8859_1(_, Str) ->
    Str.


shell(S, Neg, {_Pdu, Cwd} = State) ->
    shell(S, Neg, cmd(read_line(Cwd), S, Neg, State)).

cmd("cat" ++ Cs, S, Neg, State)   -> cat(S, Neg, State, Cs);
cmd("cd" ++ Cs, S, Neg, State)    -> cd(S, Neg, State, Cs);
cmd("get" ++ Cs, S, Neg, State)   -> fetch(S, Neg, State, Cs);
cmd("help", S, Neg, State)        -> help(S, Neg, State);
cmd("ls", S, Neg, State)          -> ls(S, Neg, State);
cmd("mkdir" ++ Cs, S, Neg, State) -> mkdir(S, Neg, State, Cs);
cmd("put" ++ Cs, S, Neg, State)   -> store(S, Neg, State, Cs);
cmd("quit", S, Neg, State)        -> exit(normal);
cmd("rmdir" ++ Cs, S, Neg, State) -> rmdir(S, Neg, State, Cs);
cmd("rm" ++ Cs, S, Neg, State)    -> delete(S, Neg, State, Cs);
cmd("?", S, Neg, State)           -> help(S, Neg, State);
cmd(Cmd, S, Neg, State)           -> 
    io:format("Unknown command: ~s~n", [Cmd]),
    State.

help(_S, _Neg, State) ->
    io:format("Commands: '?', cat, cd, get, help, ls, mkdir, quit,"
	      " put, rm, rmdir~n",[]),
    State.

cd(S, Neg, {Pdu, Cwd}, Dir0) ->
    {Pdu, cwd(Cwd, rm_space(Dir0))}.

cwd(Cwd, [$.,$.])           -> rm_last_dir(Cwd);
cwd(Cwd, [$.,$.,$/|T])      -> cwd(rm_last_dir(Cwd), T);
cwd(Cwd, [$/,$/ | T] = Dir) -> swap(Dir, $/, $\\);
cwd(Cwd, [$/ | T] = Dir)    -> swap([$/|Dir], $/, $\\);
cwd(Cwd, Dir)               -> Cwd ++ swap(Dir, $/, $\\).

rm_last_dir(P) ->
    rm_until(tl(lists:reverse(P)), $\\).

%%% Shouldn't reach end of list !
rm_until([C,C] = P, C) -> P;
rm_until([H|_] = P, H) -> lists:reverse(P);
rm_until([_|T], C)     -> rm_until(T, C).

swap([H], H, N)   -> [N];
swap([H|T], H, N) -> [N|swap(T, H, N)];
swap([H|T], C, N) -> [H|swap(T, C, N)];
swap([], _, N)    -> [N].  


cat(S, Neg, State, Fname) ->
    get_file(S, Neg, State, Fname, 
	     fun(B, F) -> io:format("~s~n", [binary_to_list(B)]) end).

fetch(S, Neg, State, Fname) ->
    get_file(S, Neg, State, Fname, 
	     fun(B, F) -> file:write_file(F,B) end).

get_file(S, Neg, {Pdu0, Cwd}, Fname0, Fun) ->
    File = rm_space(Fname0),
    Fname = to_ucs2(Neg, Cwd ++ File),
    Pdu1 = esmb:open_file_ro(S, Pdu0, Fname),
    Finfo = #file_info{name = Fname, size = Pdu1#smbpdu.file_size},
    io:format("Reading....~n", []),
    {ok, Bin} = esmb:read_file(S, Pdu1, Finfo),
    io:format("Read ~p bytes~n", [size(Bin)]),
    Fun(Bin, File),
    Pdu = esmb:close_file(S, Pdu1),
    {Pdu, Cwd}.


store(S, Neg, {Pdu0, Cwd}, Fname0) ->
    File = rm_space(Fname0),
    {ok, Bin} = file:read_file(File),
    Fname = to_ucs2(Neg, Cwd ++ File),
    Pdu1 = esmb:open_file_rw(S, Pdu0, Fname),
    Finfo = #file_info{name = Fname, data = [Bin]},
    io:format("Writing....~n", []),
    Res = esmb:write_file(S, Neg, Pdu1, Finfo),
    io:format("Wrote, res=~p~n", [Res]),
    Pdu = esmb:close_file(S, Pdu1),
    {Pdu, Cwd}.


mkdir(S, Neg, {Pdu0, Cwd}, Fname0) ->
    Fname = rm_space(Fname0),
    Pdu1 = esmb:mkdir(S, Pdu0, to_ucs2(Neg, Cwd ++ Fname)),
    Pdu = esmb:close_file(S, Pdu1),
    {Pdu, Cwd}.

rmdir(S, Neg, {Pdu0, Cwd}, Dir0) ->
    Dir = rm_space(Dir0),
    Pdu = esmb:rmdir(S, Pdu0, to_ucs2(Neg, Cwd ++ Dir)),
    {Pdu, Cwd}.

delete(S, Neg, {Pdu0, Cwd}, File0) ->
    File = rm_space(File0),
    Pdu = esmb:delete_file(S, Pdu0, to_ucs2(Neg, Cwd ++ File)),
    {Pdu, Cwd}.


ls(S, Neg, {Pdu, Cwd} = State) ->
    Finfo = esmb:list_dir(S, Pdu, to_ucs2(Neg, Cwd  ++ "*")),
    print_file_info(Neg, Finfo),
    State.

print_file_info(Neg, L) ->
    F = fun(X) ->
		io:format("~-20s ~-20s SIZE ~-15w IS ~p~n", 
			  [b2l(to_iso8859_1(Neg, X#file_info.name)),
			   dt(X#file_info.date_time),
			   X#file_info.size,
			   check_attr(X#file_info.attr)])
	end,
    lists:foreach(F, L).

dt(X) ->
    {Y,M,D} = X#dt.last_access_date,
    {H,I,S} = X#dt.last_access_time,
    i2l(Y) ++ "-" ++ two(M) ++ "-" ++ two(D) ++ " " ++
	two(H) ++ ":" ++ two(I) ++ ":" ++ two(S).

two(I) ->
    case i2l(I) of
	[X] -> [$0,X];
	X   -> X
    end.

i2l(I) when integer(I) -> integer_to_list(I);
i2l(L) when list(L)    -> L.


check_attr(A) ->
    check_attr(A, [dir,hidden]).

check_attr(A, [dir | T]) when ?IS_DIR(A) ->
    [dir | check_attr(A, T)];
check_attr(A, [hidden | T]) when ?IS_HIDDEN(A) ->
    [hidden | check_attr(A, T)];
check_attr(A, [_ | T])  ->
    check_attr(A, T);
check_attr(_A, [])  ->
    [].


read_line(Cwd) ->
    rm_last_char(io:get_line(list_to_atom(swap(Cwd, $\\, $/) ++ "> "))).

get_passwd() ->
    rm_last_char(io:get_line('Password: ')).

rm_last_char([H])   -> [];
rm_last_char([H|T]) -> [H|rm_last_char(T)].

rm_space([$\s|T]) -> rm_space(T);
rm_space(Cs)      -> Cs.

%%% Extract the Host and Share part
host_share([$\s|T])   -> 
    host_share(T);
host_share("//" ++ T) -> 
    {Host, Share} = eat_until(T, $/),
    {Host, Share}.

eat_until(Cs, X) ->
    eat_until(Cs, X, []).

eat_until([X|T] = Cs, X, Acc) -> {lists:reverse(Acc), Cs};
eat_until([H|T], X, Acc)      -> eat_until(T, X, [H|Acc]).
    
l2b(L) when list(L)   -> list_to_binary(L);
l2b(B) when binary(B) -> B.

b2l(B) when binary(B) -> binary_to_list(B);
b2l(L) when list(L)   -> L.
