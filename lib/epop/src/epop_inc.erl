-module(epop_inc).
-author('tobbe@serc.rmit.edu.au').
%%%---------------------------------------------------------------------
%%% File    : epop_inc.erl
%%% Created : 16 Jul 1998 by tobbe@serc.rmit.edu.au
%%% Function: Incorporate mail from the maildrop(s).
%%%           According to the ~/.epop file we will incorporate
%%%           mail from a local file base maildrop and/or from
%%%           a remote POP3 server.
%%% ====================================================================
%%% The contents of this file are subject to the Erlang Public License
%%% License, Version 1.0, (the "License"); you may not use this file
%%% except in compliance with the License. You may obtain a copy of the
%%% License at http://www.eddieware.org/EPL
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is epop-2-3
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%
%%%---------------------------------------------------------------------
-vc('$Id$ ').
-export([start/0]).
%% Internal
-export([init/0]).

-import(error_logger,[info_msg/1,info_msg/2,error_msg/1,error_msg/2]).

remote_default_interval() -> 15. % minutes
local_default_interval()  -> 30. % seconds

retries() -> 6. % Number of retries to grab the maildrop lock

start() ->
    spawn(?MODULE,init,[]).

init() ->
    timer:start(),
    epop:initrc(),
    Ri = remote_interval(),
    Li = local_interval(),
    loop(Ri,Li).

loop(Ri,Li) ->
    receive
	do_local_update ->
	    local(),
	    timer:send_after(Li*1000,do_local_update),
	    loop(Ri,Li);
	do_remote_update ->
	    remote(),
	    timer:send_after(Ri*1000,do_remote_update),
	    loop(Ri,Li);
	_ ->
	    loop(Ri,Li)
    end.


remote_interval() ->
    case epop:read_const(epop_remote_interval) of
	Min when integer(Min), Min>=5 ->
	    remote(),
	    Sec = Min*60,
	    info_msg("epop_inc: epop_remote_interval is ~w seconds~n",[Sec]),
	    timer:send_after(Sec*1000,do_remote_update),
	    Sec;
	Min when integer(Min), Min<5 ->
	    remote(),
	    Sec = remote_default_interval()*60,
	    info_msg("epop_inc: Too small epop_remote_interval specified, "
		     "default setting is ~w seconds~n",[Sec]),
	    timer:send_after(Sec*1000,do_remote_update),
	    Sec;
	_ ->
	    info_msg("epop_inc: No (or wrong) epop_remote_interval specified, "
		     "no remote retrieval will be done !~n"),
	    infinity
    end.

local_interval() ->
    case epop:read_const(epop_local_interval) of
	Sec when integer(Sec), Sec>=15 ->
	    local(),
	    info_msg("epop_inc: epop_local_interval is ~w seconds~n",[Sec]),
	    timer:send_after(Sec*1000,do_local_update),
	    Sec;
	Sec when integer(Sec), Sec<15 ->
	    local(),
	    Default = local_default_interval(),
	    info_msg("epop_inc: Too short epop_local_interval specified, "
		     "default setting is ~s sec~n",[Default]),
	    timer:send_after(Default*1000,do_local_update),
	    Sec;
	_ ->
	    info_msg("epop_inc: No (or wrong) epop_local_interval specified, "
		     "no local retrieval will be done !~n"),
	    infinity
    end.


%% ----------------------------------------------------
%% Incorporate mail from a remote POP3 server according
%% to the specifications in the ~/.epop file.
%% ----------------------------------------------------

remote() ->
    case epop:read_const(epop_remote_users) of
	MailDrops when list(MailDrops) ->
	    F = fun({User,PopUser,PopAdr,Passwd,Port,Opts}) ->
			remote(User,PopUser,PopAdr,Passwd,Port,Opts)
		end,
	    lists:foreach(F,MailDrops);
	undefined   -> {error,no_maildrop_specified}
    end.

remote(User,PopUser,PopAdr,Passwd,Port,Opts) ->
    case catch remote_2(User,PopUser,PopAdr,Passwd,Port,Opts) of
	{'EXIT',Reason} ->
	    error_msg("epop_inc: Remote user: ~s got error reason: ~p~n",
		      [PopUser,Reason]);
	{error,Reason}  ->
	    error_msg("epop_inc: Remote user: ~s got error reason: ~p~n",
		      [PopUser,Reason]);
	Else -> Else
    end.
    
remote_2(User,PopUser,PopAdr,Passwd,Port,Opts) ->
    PopAddress = PopUser ++ "@" ++ PopAdr,
    Options = [{port,Port}|connect_options(Opts)],
    case epop:connect(PopAddress,Passwd,Options) of
	{ok,S} ->
	    Del = delete_p(Opts),
	    SMTP = smtp_p(Opts),
	    check_mail(User,S,Del,SMTP,PopAddress);
	Else ->
	    Else
    end.

check_mail(User,S,Del,SMTP,PopAddress) ->
    case epop:stat(S) of
	{ok,{N,_}} when N>0 ->
	    R = epop:scan(S),
	    Mail_s = any_mail_txt(N),
	    info_msg("epop_inc: Retreiving ~w ~s for: ~s~n",
				  [N,Mail_s,PopAddress]),
	    get_mail(User,S,Del,SMTP,R);
	{ok,{0,_}}  ->
	    info_msg("epop_inc: No mail to retreive for: ~s~n",
				  [PopAddress]),
	    epop:quit(S);
	Else ->
	    epop:quit(S)
    end.

any_mail_txt(N) when length(N) > 1 -> "mails";
any_mail_txt(N)                    -> "mail".
    
get_mail(User,S,Del,SMTP,{ok,{N,Z}}) ->
    get_all_mails(User,S,Del,SMTP,[{N,Z}]);
get_mail(User,S,Del,SMTP,{ok,List}) when list(List) ->
    get_all_mails(User,S,Del,SMTP,List);
get_mail(User,S,Del,SMTP,Else) ->
    epop:quit(S).

get_all_mails(User,S,Del,SMTP,L) ->
    F = fun({N,_}) ->
		case epop:retrieve(S,N) of
		    {ok,Msg} ->
			store_mail(User,[Msg]),
			del_mail(S,N,Del),
			smtp_deliver(SMTP,Msg);
		    Else ->
			error_msg("epop_inc: Retrieve of message ~w failed.",
				  [N])
		end
	end,
    lists:foreach(F,L),
    epop:quit(S).

del_mail(S,N,true)  -> epop:delete(S,N);
del_mail(_,_,false) -> true.

smtp_deliver({smtp,User,Host},Msg) ->
    epop_smtp_client:async_deliver(User,Host,Msg,[{snoop,true}]); % NB: REMOVE SNOOP !!
smtp_deliver(_,_) -> false.

connect_options([{snoop,Bool}|T]) -> [{snoop,Bool}|connect_options(T)];
connect_options([apop|T])         -> [apop|connect_options(T)];
connect_options([_|T])            -> connect_options(T);
connect_options([])               -> [].

%% Default is to delete the mails from the remote server.
delete_p(L) when list(L) ->
    case lists:member(copy,L) of
	true  -> false;
	false -> true
    end.
	
%% Check if we shall deliver mail to a SMTP server
smtp_p(L) when list(L) ->
    case lists:keysearch(smtp,1,L) of
	{value,SMTP} -> SMTP;
	_            -> false
    end.
	
%% -----------------------------------------------------
%% Incoporate mail from the local (file based) maildrop
%% according to the specifications in the ~/.epop file.
%% -----------------------------------------------------

local() ->
    case epop:read_const(epop_local_maildrops) of
	MailDrops when list(MailDrops) ->
	    check_local_mail(MailDrops);
	undefined   ->
	    error_msg("epop_inc: No maildrop specified !~n"),
	    {error,no_maildrop_specified}
    end.

check_local_mail(MailDrops) ->
    before_access(),
    F = fun({User,MailDrop}) ->
		check_maildrop(User,MailDrop)
	end,
    lists:foreach(F,MailDrops),
    after_access().

%% If needed, run a program (e.g 'movemail') before 
%% accessing the maildrop
before_access() -> 
    case epop:read_const(epop_before) of
	Before when list(Before) -> os:cmd(Before);
	_                        -> false
    end.

%% If needed, run a program (e.g to remove the local 
%% mail spool) after accessing the maildrop.
after_access() -> 
    case epop:read_const(epop_after) of
	After when list(After) -> os:cmd(After);
	_                      -> false
    end.

check_maildrop(User,MailDrop) ->
   {ok,Fdata} = file:file_info(MailDrop),
    case Fdata of
        {0,_,_,_,_,_,_} ->
	    false;
        {Size,_,_,_,_,_,_} when Size > 0 ->
            file(User,MailDrop)
    end.


file(User,MailDrop) when list(User), list(MailDrop) ->
    case catch file_2(User,MailDrop) of
	{'EXIT',Reason} ->
	    error_msg("epop_inc: Accessing local maildrop: ~s , "
		      "got error reason: ~p~n",[MailDrop,Reason]);
	{error,Reason}  ->
	    error_msg("epop_inc: Accessing local maildrop: ~s , "
		      "got error reason: ~p~n",[MailDrop,Reason]);
	Else -> Else
    end.

file_2(User,MailDrop) ->
    mk_tmp_lockfile(User,MailDrop).

%% ----------------------------------------------------------
%% The strategy is to create a temporary lockfile and then
%% create a hard link to it from the real maildrop lockfile.
%% This is a trick to make NFS behave synchronously. If the
%% creation of the link fails, then retry a couple of times
%% before giving up.

mk_tmp_lockfile(User,MailDrop) ->
    TmpLockFile = tmp_lockfile(User),
    case file_exist(TmpLockFile) of
	true ->
	    info_msg("epop_inc: Lockfile already in use~n"),
	    throw({error,lockfile_in_use});
	false ->
	    Tlf = open_file(TmpLockFile, [write,raw]),
	    close_file(Tlf),
	    Res = (catch mk_lockfile(User,MailDrop,TmpLockFile,lockfile(User))),
	    file:delete(TmpLockFile),
	    Res
    end.

mk_lockfile(User,MailDrop,TmpLockFile,LockFile) ->
    mk_lockfile(User,MailDrop,TmpLockFile,LockFile,retries()).

mk_lockfile(User,MailDrop,TmpLockFile,LockFile,0) ->
    info_msg("epop_inc: Failed to lock maildrop.~n"),
    throw({error,lockfile_failed});
mk_lockfile(User,MailDrop,TmpLockFile,LockFile,N) when N>0 ->
    case os:cmd(ln_cmd(TmpLockFile,LockFile)) of
	"" -> % success
	    Res = (catch inc_maildrop(User,MailDrop,TmpLockFile,LockFile)),
	    file:delete(LockFile),
	    Res;
	Else -> % failure
	    sleep(10),
	    mk_lockfile(User,MailDrop,TmpLockFile,LockFile,N-1)
    end.

inc_maildrop(User,MailDrop,TmpLockFile,LockFile) ->
    case file:read_file(MailDrop) of
	{ok,Bin} ->
	    truncate_file(MailDrop),
	    deliver(User,TmpLockFile,Bin);
	Else ->
	    throw(Else)
    end.

deliver(User,TmpLockFile,Bin) ->
    case split_mail(binary_to_list(Bin)) of
	[] -> ok;
	Ms ->
	    N = length(Ms),
	    info_msg("epop_inc: Storing ~w ~s from local maildrop of user: ~s~n",
		     [N,any_mail_txt(N),User]),
	    store_mail(User,Ms)
    end.

%% Split the maildrop into separate mails by
%% looking for a line that begins with 'From '

split_mail(Ms) ->
    split_mail(Ms,[]).

split_mail([$F,$r,$o,$m,$ |Ms],MailAcc) ->
    {Line,Mrest} = rest_of_line(Ms),
    split_mail(Mrest,MailAcc,[[$F,$r,$o,$m,$ |Line]]);
split_mail([],MailAcc) ->
    lists:reverse(MailAcc).

split_mail([$F,$r,$o,$m,$ |Ms],MailAcc,LineAcc) ->
    {Line,Mrest} = rest_of_line(Ms),
    split_mail(Mrest,[lists:flatten(lists:reverse(LineAcc))|MailAcc],
	       [[$F,$r,$o,$m,$ |Line]]);
split_mail(Ms,MailAcc,LineAcc) when Ms =/= [] ->
    {Line,Mrest} = rest_of_line(Ms),
    split_mail(Mrest,MailAcc,[Line|LineAcc]);
split_mail([],MailAcc,LineAcc) ->
    lists:reverse([lists:flatten(lists:reverse(LineAcc))|MailAcc]).


rest_of_line(L) ->  rest_of_line(L,[]).

rest_of_line([$\n|T],Acc) -> {lists:reverse([$\n|Acc]),T};
rest_of_line([H|T],Acc)   -> rest_of_line(T,[H|Acc]);
rest_of_line([],Acc)      -> {lists:reverse(Acc),[]}.
    
%% NB: Debug routine to test split_mail/1
print_mail([H|T]) ->
    F = fun(M) -> io:fwrite("~s",[M]) end,
    lists:foreach(F,H),
    io:fwrite("~n############~n",[]),
    print_mail(T);
print_mail([]) ->
    true.


ln_cmd(Tlf,Lf) ->
    "/usr/bin/ln -f " ++ Tlf ++ " " ++ Lf.

tmp_lockfile(User) -> mk_lockfile(User,".epoplock").

lockfile(User) -> mk_lockfile(User,".lock").

mk_lockfile(User,Ext) ->
    case epop:read_const(epop_local_maildrops) of
	Mdrops when list(Mdrops) ->
	    case lists:keysearch(User,1,Mdrops) of
		false ->
		    error_msg("epop_inc: No maildrop defined for user: ~p !~n",
			      [User]),
		    throw({error,no_maildrop_defined});
		{value,{_,Md}} -> Md ++ Ext
	    end;
	_ ->
	    error_msg("epop_inc: No maildrop defined for user: ~p !~n",
		      [User]),
	    throw({error,no_maildrop_defined})
    end.


%% ---------------------
%% Some general routines
%% ---------------------

store_mail(User,[]) -> true;
store_mail(User,[Mail|Ms]) ->
    MsgID = own_message_id(),
    check_result(MsgID,Mail,epop:store_mail(User,MsgID,Mail)),
    store_mail(User,Ms).

check_result(_,_,ok) -> true;
check_result(MsgID,Mail,_) ->
    %% This should hopefully never happend !!
    error_msg("epop_inc: >>> PANIC !! <<< could not store mail, "
			   "dumping to /tmp/~s !~n",
	      [MsgID]),
    file:write_file("/tmp/" ++ MsgID,list_to_binary(Mail)).

own_message_id() ->
    {M,S,U} = now(),
    {Y,N,D} = date(),
    integer_to_list(Y) ++
    integer_to_list(N) ++
    integer_to_list(D) ++
    integer_to_list(M) ++
    integer_to_list(S) ++ 
    integer_to_list(U).

truncate_file(Fname) ->
    Fd = open_file(Fname,[write,raw]),
    file:truncate(Fd),
    close_file(Fd).

open_file(Fname,Opts) ->			    
    case file:open(Fname,Opts) of
	{ok,Fd} -> Fd;
	Else    -> throw(Else)
    end.
    
close_file(Fd) -> file:close(Fd).

file_exist(Fname) ->
    case file:read_file_info(Fname) of
	{ok,Finfo} -> true;
	_          -> false
    end.

sleep(Sec) -> receive after Sec*1000 -> true end.
