-module(epop_dets).
-author('tobbe@serc.rmit.edu.au').
%%%---------------------------------------------------------------------
%%% File    : epop_dets.erl
%%% Created : 4 Mar 1998 by tobbe@serc.rmit.edu.au
%%% Function: A simple disk based database for the epop module.
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
%%% ====================================================================
%%%           The system administration tables have the following format
%%%             sys_auth  -  {User,Passwd}                  (type: set)
%%%             sys_stat  -  {User,NumOfMsgs,TotNumOfBytes} (type: set)
%%%
%%%           The user tables have the following format:
%%%             User_scan  -  {User,MsgID,SizeOfMsg}        (type: bag)
%%%             User_mail  -  {MsgID,Size,Mail}             (type: set)
%%%---------------------------------------------------------------------
-vc('$Id$ ').
-export([start/0,stop/0]).
-export([get_passwd/1,stat/1,scan_list/1,get_mail/2,
	 lock_maildrop/1,unlock_maildrop/1,delete_mail/3]).
-export([setup/0,setup/1,add_user/2,rm_user/1,change_passwd/3,msg_limit/2]).
-export([store_mail/3,check_limits/2]).
-export([init/1,loop/3]).
-export([panic/3]).

-import(epop, [lcase/1]).
-import(epop_db, [db/0,db_in/3,db_out/2,db_replace/3,db_filter/2,db_delete/2]).
-import(error_logger,[info_msg/1,error_msg/1,error_msg/2]).

-define(SERVER_NAME, epop_dets_server).

-define(DEFAULT_MAXNUM,   10000).  % No more than 10000 messages in maildrop
-define(DEFAULT_MAXSIZE,  1000000).% Maildrop cannot be larger than 1MB

-define(MAXNUM_KEY,           {limit,maxnum}).
-define(MAXSIZE_KEY,          {limit,maxsize}).
-define(LOCK_KEY(User),       {l2a(User),lock}).
-define(SCAN_KEY(User),       {l2a(User),scan}).
-define(MAIL_KEY(User),       {l2a(User),mail}).


%% ------------------
%% Exported functions
%% ------------------

start() -> start(self()).

start(Epop) when pid(Epop) ->
    case whereis(?SERVER_NAME) of
	Pid when pid(Pid) -> {ok,Pid};
	_ ->
	    Pid = spawn_link(?MODULE, init, [Epop]),
	    register(?SERVER_NAME,Pid),
	    {ok,Pid}
    end.

%% --------------------
%% POP3 server commands
%% --------------------

stat(User)                   -> call(stat,User).
scan_list(User)              -> call(scan_list,User).
get_mail(User,MsgID)         -> call(get_mail,{User,MsgID}).
get_passwd(User)             -> call(get_passwd,User).
delete_mail(User,MsgID,Size) -> call(delete_mail,{User,MsgID,Size}).
lock_maildrop(User)          -> link_call(lock_maildrop,User).
unlock_maildrop(User)        -> unlink_call(unlock_maildrop,User).

%% ------------------------------
%% System administration commands
%% ------------------------------

%% ----------------------------------
%% Create user accounts for the users
%% specified in the .epop file

setup() ->
    epop:initrc(),
    case epop:read_const(epop_users) of
	Users when list(Users) ->
	    F = fun({User,Passwd,Opts}) -> add_user(User,Passwd) end,
	    lists:foreach(F,Users),
	    ok;
	undefined ->
	    {error,no_users_defined}
    end.

%% Setup a specified user.

setup(User) when list(User) ->
    epop:initrc(),
    case epop:read_const(epop_users) of
	Users when list(Users) ->
	    case lists:keysearch(User,1,Users) of
		{value,{_,Passwd,_}} -> add_user(User,Passwd);
		_ -> {error,user_not_found}
	    end;
	undefined ->
	    {error,no_users_defined}
    end.
	    
add_user(User,Passwd) when list(User),list(Passwd) ->
    Luser = lcase(User),
    call(add_user,{User,Passwd}).

rm_user(User) when list(User) ->
    Luser = lcase(User),
    call(rm_user,User).

change_passwd(User,NewPasswd,NewPasswd) when list(User),list(NewPasswd) ->
    Luser = lcase(User),
    call(change_passwd,{User,NewPasswd}).

msg_limit(NumOfMsg,MaxTotSize) when integer(NumOfMsg),integer(MaxTotSize) ->
    call(msg_limit,{NumOfMsg,MaxTotSize}).

stop() ->
    call(stop,now).

%% --------------------
%% SMTP server commands
%% --------------------

store_mail(User,MsgID,Mail) ->
    case check_limits(User,length(Mail)) of
	ok   -> call(store_mail,{User,MsgID,Mail});
	Else -> Else
    end.

check_limits(User,Size)     -> call(check_limits,{User,Size}).

%% ----------
%% The Server
%% ----------

init(Epop) ->
    process_flag(trap_exit,true),
    epop:initrc(),
    {ok,Dir} = epop_dir(),
    Db = open_systables(Dir,db()),
    loop(Epop,Dir,Db).

epop_dir() ->
    case epop:read_const(epop_mail_dir) of
	undefined   ->
	    error_msg("epop_dets: No mail directory found !~n"),
	    {error,no_epop_dir};
	EpopDir ->
	    {ok,EpopDir}
    end.

%dbg() ->
%    {_,H} = process_info(self(),heap_size),
%    {_,S} = process_info(self(),stack_size),
%    info_msg("epop_dets(dbg): stack=~w heap=~w~n",[S,H]).
    
loop(Epop,Dir,Db) ->
    receive
	{From,Tag,Msg} when pid(From) ->
	    NewDb = dispatch(From,Tag,Msg,Dir,Db),
	    %%dbg(),
	    ?MODULE:loop(Epop,Dir,NewDb);
	{'EXIT',Epop,Reason} ->
	    shutdown(Dir,Db),
	    exit(Reason);
	{'EXIT',Pid,Reason} ->
	    NewDb = possibly_unlock_maildrop(Db,Pid),
	    ?MODULE:loop(Epop,Dir,NewDb);
	XX ->
	    error_msg("epop_dets: Unexpected msg, loop got: ~p~n",[XX]),
	    ?MODULE:loop(Epop,Dir,Db)
    end.


dispatch(From,stat,Data,_,Db) ->
    stat(From,Data,Db);
dispatch(From,scan_list,Data,_,Db) ->
    scan_list(From,Data,Db);
dispatch(From,get_mail,Data,_,Db) ->
    get_mail(From,Data,Db);
dispatch(From,get_passwd,Data,_,Db) ->
    get_passwd(From,Data,Db);
dispatch(From,delete_mail,Data,_,Db) ->
    do_delete_mail(From,Data,Db);
dispatch(From,lock_maildrop,Data,Dir,Db) ->
    lock_md(From,Data,Dir,Db);
dispatch(From,unlock_maildrop,Data,_,Db) ->
    unlock_maildrop(From,Data,Db);
dispatch(From,add_user,Data,Dir,Db) ->
    define_user(From,Data,Dir,Db);
dispatch(From,rm_user,Data,_,Db) ->
    remove_user(From,Data,Db);
dispatch(From,change_passwd,Data,_,Db) ->
    change_pw(From,Data,Db);
dispatch(From,msg_limit,Data,_,Db) ->
    set_msg_limit(From,Data,Db);
dispatch(From,store_mail,Data,Dir,Db) ->
    store_mail(From,Data,Dir,Db);
dispatch(From,check_limits,Data,Dir,Db) ->
    check_limits(From,Data,Db);
dispatch(From,stop,Data,Dir,Db) ->
    shutdown(Dir,Db),
    return(From,stop,ok),
    exit(normal);
dispatch(From,Tag,Msg,Dir,Db) ->
    error_msg("epop_dets: Intenal error, unknown tag "
	      "in dispatch: ~p~n",[Tag]),
    Db. % Unknown tag

possibly_unlock_maildrop(Db,Pid) ->
    case db_out(Db,Pid) of
	{ok,User} -> unlock_md(User,Db);
	_         -> Db
    end.

%% Close all open tables
shutdown(Dir,Db) ->
    info_msg("epop_dets: Closing all maildrops...~n"),
    close_systables(Db),
    NewDb = unlock_maildrops(Db),
    info_msg("epop_dets: All maildrops are now closed !!~n"),
    NewDb.
    
unlock_maildrops(Db) ->
    F = fun({User,lock},Acc) -> unlock_md(User,Db);
	   (_,Acc)           -> Acc
	end,
    lists:foldl(F,Db,Db).

%% ----------
%% POP3 stuff
%% ----------

%% --------------
%% A STAT request

stat(From,User,Db) ->
    case catch do_stat(User,Db) of
	{ok,Result} ->
	    return(From,stat,{ok,Result});
	_ ->
	    return(From,stat,{error,stat})
    end,
    Db.

do_stat(User,Db) ->
    SysStat = get_sysstat(Db),
    [{_,Nmsg,Tbytes}] = dets:lookup(SysStat,User),
    {ok,{Nmsg,Tbytes}}.

%% -------------------
%% Produce a scan list

scan_list(From,User,Db) ->
    case catch do_scan_list(User,Db) of
	{ok,Result} ->
	    return(From,scan_list,{ok,Result});
	_ ->
	    return(From,scan_list,{error,scan_list})
    end,
    Db.

do_scan_list(User,Db) ->
    {ok,{S,_}} = db_out(Db,?SCAN_KEY(User)),
    ScanList = dets:match(S,{User,'$1','$2'}),
    {ok,ScanList}.

%% ----------------
%% Get the password

get_passwd(From,User,Db) ->
    case catch sysauth_get(Db,User) of
	{ok,{_,Passwd}} -> return(From,get_passwd,{ok,Passwd});
	_               -> return(From,get_passwd,{error,get_passwd})
    end,
    Db.
    
%% ----------------------
%% Get the specified mail

get_mail(From,{User,MsgID},Db) ->
    case catch do_get_mail(User,MsgID,Db) of
	{ok,Result} ->
	    return(From,get_mail,{ok,Result});
	_ ->
	    return(From,get_mail,{error,get_mail})
    end,
    Db.

do_get_mail(User,MsgID,Db) ->
    {ok,{M,_}} = db_out(Db,?MAIL_KEY(User)),
    case dets:lookup(M,MsgID) of
	[{_,Size,Mail}] ->
	    {ok,{Mail,Size}};
	_ ->
	    {error,not_found}
    end.

%% -------------------------
%% Delete the specified mail

do_delete_mail(From,{User,MsgID,Size},Db) ->
    case catch do_delete_mail_2(User,MsgID,Size,Db) of
	ok -> return(From,delete_mail,ok);
	_  -> return(From,delete_mail,{error,delete_mail})
    end,
    Db.

do_delete_mail_2(User,MsgID,Size,Db) ->
    {ok,{M,_}} = db_out(Db,?MAIL_KEY(User)),
    dets:delete(M,MsgID),
    {ok,{S,_}} = db_out(Db,?SCAN_KEY(User)),
    dets:match_delete(S,{User,MsgID,'$1'}),
    SysStat = get_sysstat(Db),
    [{_,Nmsg,Tbytes}] = dets:lookup(SysStat,User),
    NewNmsg = max(Nmsg-1,0),
    NewTbytes = if (NewNmsg == 0) -> 0; true -> max(Tbytes-Size,0) end,
    dets:insert(SysStat,{User,NewNmsg,NewTbytes}),
    ok.

%% Protect ourself from complete fuckup 
%% during development and testing...
max(N,M) when N>=M -> N;
max(N,M) when N<M  -> M.
		  
%% ----------------------------------------------------------
%% Lock maildrop. When locked, the session Db contains
%% the following user specific data: {User,Pid}, {Pid,User},
%% {ScanKey,{DetsFd,Fname}}, {MailKey,{DetsFd,Fname}},
%% {LockKey,Bool}. The scan- and mail key data refers to the
%% users dets tables and the lock key just verifies that
%% the user has locked the maildrop. The (User,Pid) are there
%% so that we can deallocate the lock in case of error.

lock_md(From,User,Dir,Db) ->
    case db_out(Db,?LOCK_KEY(User)) of
	{ok,true}  ->
	    return(From,lock_maildrop,{error,already_locked}),
	    Db;
	_ ->
	    case catch do_lock_md(From,User,Dir,Db) of
		{ok,Db2} ->
		    %% 10 Mar 1999 by tnt@home.se
		    %% We have to return a scan-list here to
		    %% freeze the current status of the maildrop.
		    %% The client process should only work with
		    %% this freezed view, this to avoid interference
		    %% with new incoming mails.
		    %%
		    %% A triple nested case :-( Don't try this at home !!
		    case catch do_scan_list(User,Db2) of
			{ok,Result} ->
			    return(From,lock_maildrop,{ok,Result});
			_ ->
			    return(From,lock_maildrop,{error,lock_scan_list})
		    end,
		    Db2;
		_ ->
		    return(From,lock_maildrop,{error,lock_maildrop}),
		    Db
	    end
    end.

do_lock_md(From,User,Dir,Db) ->
    {ok,S,Sfname} = open_user_scan(User,Dir),
    {ok,M,Mfname} = open_user_mail(User,Dir),
    Db2 = db_in(db_in(db_in(db_in(db_in(Db,From,User),
				  User,From),
			    ?LOCK_KEY(User),true),
		      ?SCAN_KEY(User),{S,Sfname}),
		?MAIL_KEY(User),{M,Mfname}),
    {ok,Db2}.

%% ---------------
%% Unlock maildrop

unlock_maildrop(From,User,Db) ->
    case catch unlock_md(User,Db) of
	{'EXIT',_} -> return(From,unlock_maildrop,{error,unlock_maildrop}),Db;
	Db2        -> return(From,unlock_maildrop,ok),Db2

    end.

unlock_md(User,Db) ->
    {ok,{S,_}} = db_out(Db,?SCAN_KEY(User)),
    {ok,{M,_}} = db_out(Db,?MAIL_KEY(User)),
    unlock_user(Db,User,S,M).
    
%% --------------------------------------------------------
%% Close the dets file-desc and clear the session database.

unlock_user(Db,User,S,M) ->
    dets:close(S),
    dets:close(M),
    unlock_db(Db,User).

unlock_db(Db,User) ->
    %% Remove the (User,Pid) data from Db 
    F = fun({U,P}) when U==User,pid(P) -> false;
	   ({P,U}) when U==User,pid(P) -> false;
	   (_) -> true
	end,
    db_filter(db_delete(db_delete(db_delete(Db,?LOCK_KEY(User)),
				  ?SCAN_KEY(User)),
			?MAIL_KEY(User)),
	      F).
    

%% ---------------------------
%% System administration stuff
%% ---------------------------

%% -----------------
%% Define a new user

define_user(From,{User,Pw},Dir,Db) ->
    case already_defined(Db,User) of
	true  ->
	    return(From,add_user,{error,already_defined});
	false ->
	    sysauth_insert(Db,User,Pw),
	    sysstat_insert(Db,User,0,0),
	    define_user_tables(From,User,Dir),
	    return(From,add_user,ok)
    end,
    Db.

already_defined(Db,User) ->
    case sysauth_get(Db,User) of
	{ok,_} -> true;
	_      -> false
    end.

%% -------------
%% Remove a user

remove_user(From,User,Db) ->
    case already_defined(Db,User) of
	true  ->
	    case catch remove_user(User,Db) of
		{ok,Db2} ->
		    return(From,rm_user,ok),
		    Db2;
		_ ->
		    return(From,rm_user,{error,remove_failed}),
		    Db
	    end;
	false ->
	    return(From,rm_user,{error,not_found}),
	    Db
    end.

%% ------------------------------------------
%% We don't care if the user is active or not

remove_user(User,Db) ->
    %% Close and delete the user files.
    remove_user_files(User,Db),
    %% Remove the user from the sys tables
    Db2 = remove_sys_files(User,Db),
    {ok,Db2}.

remove_sys_files(User,Db) ->
    catch begin
	      Sa = get_sysauth(Db),
	      dets:delete(Sa,User),
	      Ss = get_sysstat(Db),
	      dets:delete(Ss,User),
	      del_sysauth(del_sysstat(Db))
	  end.

remove_user_files(User,Db) ->
    catch begin
	      {ok,{Sd,Sfname}} = db_out(Db,?SCAN_KEY(User)),
	      {ok,{Md,Mfname}} = db_out(Db,?MAIL_KEY(User)),
	      unlock_user(Db,User,Sd,Md),
	      file:delete(Sfname),
	      file:delete(Mfname)
	  end.

%% -----------------------
%% Change a users password

change_pw(From,{User,Passwd},Db) ->
    sysauth_insert(Db,User,Passwd),
    return(From,change_passwd,ok),
    Db.


%% ---------------------------------------------
%% Set some system limits for the maximum number
%% of messages allowed in the maildrop, and the
%% maximum size of the maildrop.

set_msg_limit(From,{MaxNum,MaxSize},Db) ->
    Db2 = msg_limit(Db,MaxNum,MaxSize),
    return(From,msg_limit,ok),
    Db2.

msg_limit(Db,MaxNum,MaxSize) ->
    %% Let us use the sys_auth table for this...
    SysAuth = get_sysauth(Db),
    dets:insert(SysAuth,{?MAXNUM_KEY,MaxNum}),
    dets:insert(SysAuth,{?MAXSIZE_KEY,MaxSize}),
    db_replace(db_replace(Db,?MAXNUM_KEY,MaxNum),
	       ?MAXSIZE_KEY,MaxSize).
	    
check_limits(From,{User,Size},Db) ->
    catch do_check_limits(From,User,Size,Db),
    Db.

do_check_limits(From,User,Size,Db) ->
    SysStat = get_sysstat(Db),
    [{_,Nmsg,Tbytes}] = dets:lookup(SysStat,User),
    case dets:lookup(SysStat,?MAXNUM_KEY) of
	[{_,MaxNum}] ->
	    case  dets:lookup(SysStat,?MAXSIZE_KEY) of
		[{_,MaxSize}] ->
		    if (Nmsg<MaxNum) ->
			    if ((Tbytes+Size) =< MaxSize) ->
				    return(From,check_limits,ok);
			       true ->
				    error_msg("epop_dets: Max size of maildrop "
					      "exceeded for user: ~s~n",
					      [User]),
				    return(From,check_limits,{error,
							      maxsize_exceeded})
			    end;
		       true ->
			    error_msg("epop_dets: Max number of mails in "
				      "maildrop exceeded for user: ~s~n",
				      [User]),
			    return(From,check_limits,{error,maxnum_exceeded})
		    end;
		_ -> return(From,check_limits,ok) % no limit set
	    end;
	_ -> return(From,check_limits,ok) % no limit set
    end.

%% -------------
%% SMTP commands
%% -------------

store_mail(From,{User,MsgID,Mail},Dir,Db) ->
    case catch store_the_mail(User,MsgID,Mail,Dir,Db) of
	{'EXIT',Reason} ->
	    error_msg("epop_dets: Failed to store message, "
		      "User=~s , MsgID=~s~n", [User,MsgID]),
	    return(From,store_mail,{error,store_failed});
	ok ->
	    return(From,store_mail,ok),
	    epop_notify:notify(User)
    end,
    Db.

store_the_mail(User,MsgID,Mail,Dir,Db) ->
    Size = length(Mail),
    case inc_sysstat(Db,User,Size) of
	ok ->
	    {ok,S,_} = open_user_scan(User,Dir),
	    {ok,M,_} = open_user_mail(User,Dir),
	    dets:insert(S,{User,MsgID,Size}),
	    dets:insert(M,{MsgID,Size,Mail}),
	    dets:close(S),
	    dets:close(M),
	    ok;
	{error,Reason} ->
	    {error,Reason}
    end.

%% ----------------------------
%% Manipulating the user tables
%% ----------------------------

define_user_tables(From,User,Dir) ->
    {ok,S,_} = open_user_scan(User,Dir),
    dets:close(S),
    {ok,M,_} = open_user_mail(User,Dir),
    dets:close(M).

open_user_scan(User,Dir) -> open_user(User,Dir,"_scan",bag).
open_user_mail(User,Dir) -> open_user(User,Dir,"_mail",set).

open_user(User,Dir,Suffix,Type) ->
    Name = User ++ Suffix,
    Fname = Dir ++ "/" ++ Name,
    {ok,D} = dets:open_file(l2a(Name),[{file,Fname},{type,Type}]),
    {ok,D,Fname}.

%% ---------------------------------------------
%% Manipulating the system administration tables
%% ---------------------------------------------

open_systables(Dir,Db) ->
    msg_limit(open_sysstat(Dir,open_sysauth(Dir,Db)),
	      ?DEFAULT_MAXNUM,?DEFAULT_MAXSIZE).

open_sysauth(Dir,Db) -> open_sys(Dir,Db,sys_auth).
open_sysstat(Dir,Db) -> open_sys(Dir,Db,sys_stat).

open_sys(Dir,Db,TabName) ->
    {ok,Name} = dets:open_file(TabName,[{file,Dir ++ "/" ++ a2l(TabName)}]),
    db_in(Db,TabName,Name).

close_systables(Db) ->
    close_sysstat(close_sysauth(Db)).

close_sysauth(Db) -> close_sys(Db,sys_auth).
close_sysstat(Db) -> close_sys(Db,sys_stat).

close_sys(Db,TabName) ->
    {ok,DetsName} = db_out(Db,TabName),
    dets:close(DetsName),
    db_delete(Db,TabName).

get_sysauth(Db) ->
    {ok,SysAuth} = db_out(Db,sys_auth),
    SysAuth.

get_sysstat(Db) ->
    {ok,SysStat} = db_out(Db,sys_stat),
    SysStat.

del_sysauth(Db) -> db_delete(Db,sys_auth).

del_sysstat(Db) -> db_delete(Db,sys_stat).

sysstat_insert(Db,User,Nmsg,Tbytes) ->
    SysStat = get_sysstat(Db),
    dets:insert(SysStat,{User,Nmsg,Tbytes}).

%% NB: We don't check here that the limits has been exceeded.
%% That should already have been done elsewere.
inc_sysstat(Db,User,Size) ->
    SysStat = get_sysstat(Db),
    case dets:lookup(SysStat,User) of
	[{_,Nmsg,Tbytes}] ->
	    dets:insert(SysStat,{User,Nmsg+1,Tbytes+Size}),
	    ok;
	_ -> 
	    {error,inc_sysstat}
    end.

sysauth_insert(Db,User,Passwd) ->
    SysAuth = get_sysauth(Db),
    dets:insert(SysAuth,{User,Passwd}).

sysauth_get(Db,User) ->
    SysAuth = get_sysauth(Db),
    case dets:lookup(SysAuth,User) of
	[Entry] -> {ok,Entry};
	_       -> {error,not_found}
    end.

%% --------------------------
%% Send and receive functions
%% --------------------------

%% ---------------
%% Client routines

link_call(Tag,Data) ->
    link(whereis(?SERVER_NAME)),
    call(Tag,Data).

unlink_call(Tag,Data) ->
    unlink(whereis(?SERVER_NAME)),
    call(Tag,Data).

call(Tag,Data) ->
    send(?SERVER_NAME,{self(),Tag,Data}),
    wait(Tag).

send(To,Msg) -> To ! Msg.

wait(Tag) -> receive {?SERVER_NAME,Tag,Answer} -> Answer end.

%% --------------
%% Server routine

return(To,Tag,Msg) -> To ! {?SERVER_NAME,Tag,Msg}.


%% ----------
%% Misc stuff
%% ----------

l2a(List) when list(List) -> list_to_atom(List);
l2a(Atom) when atom(Atom) -> Atom.

a2l(List) when list(List) -> List;
a2l(Atom) when atom(Atom) -> atom_to_list(Atom).

%% ------------------------------------------------------
%% Recover from disaster. Read each message from maildrop
%% and dump it into another directory, one file per mail.

panic(User,MailDir,DumpDir) ->
    {ok,S,_} = open_user_scan(User,MailDir),
    {ok,M,_} = open_user_mail(User,MailDir),
    ScanList = dets:match(S,{User,'$1','$2'}),  % {User, MsgID, SizeOfMsg}
    Res = (catch panic_dump(ScanList,M,DumpDir)),
    dets:close(S),
    dets:close(M),
    Res.

panic_dump(ScanList,M,DumpDir) ->
    F = fun([MsgId,_]) ->
		case dets:lookup(M,MsgId) of
		    [{_,Size,Mail}] -> panic_write(MsgId,Mail,DumpDir);
		    _ -> false
		end
	end,
    lists:foreach(F,ScanList),
    ok.

panic_write(MsgId,Mail,DumpDir) ->
    file:write_file(DumpDir ++ "/" ++ MsgId, list_to_binary(Mail)).
	       

