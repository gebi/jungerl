-module(epop_server).
-author('tobbe@serc.rmit.edu.au').
%%%---------------------------------------------------------------------
%%% File    : epop_server.erl 
%%% Created : 3 Mar 1998 by tobbe@serc.rmit.edu.au
%%% Function: A POP3 server according to RFC-1939, and RFC-2449.
%%%           It also implements an optional extension 
%%%           for notification handling. By default this
%%%           extension is shut off.
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
-export([start/0,stop/0,version/0]).
-export([apop_timestamp/0,authorize/3]).
-export([init/1,listener/2,worker/2]).

-import(epop,[initrc/0,read_const/1,recv_sl/1,recv_sl/2,server_port/0]).
-import(error_logger,[info_msg/2,error_msg/1,error_msg/2]).
-import(epop_db, [db/0,db_in/3,db_out/2,db_replace/3,db_filter/2,
		  db_foreach/2,db_member/2,db_member/3]).

-define(SERVER_NAME, epop_server).
-define(BYTE_STUFF_CHUNK, 1024).                % Amount of bytes to be byte 
						% stuffed before being sent.

version() ->
    [_,Rev|_] = string:tokens("$Revision$ "," "),Rev.

%% Greeting message sent to client
greeting() -> "EPOP (version " ++ version() ++ ") server ready".


start() -> 
    case whereis(?SERVER_NAME) of
	Pid when pid(Pid) -> ok;
	_ ->
	    Pid = spawn(?MODULE, init, [self()]),
	    register(?SERVER_NAME,Pid),
	    receive {Pid,Msg} -> Msg end
    end.

stop() ->
    ?SERVER_NAME ! {self(),stop},
    receive stopped -> ok end.
	  
%% -----------------
%% The Listen server
%% -----------------

init(From) ->
    process_flag(trap_exit,true),
    initrc(),
    activate_logfile(),
    Lport = server_port(),
    case epop_dets:start() of
	{ok,DbPid} ->
	    startup(From,Lport);
	Else ->
	    error_msg("epop: Could not start the database !~n"),
	    From ! {self(),Else},
	    exit(Else)
    end.

startup(From,Lport) ->
    case setup_listen_sock(Lport) of
	{ok,Lsock} ->
	    Accept = do_accept(Lsock),
	    From ! {self(),ok},
	    epop_notify:start(),
	    %% Ok, we're in business, let's start to incorporate mail.
	    epop_inc:start(), 
	    listener(Lsock,Accept);
	{error,Reason} ->
	    From ! {self(),{error,Reason}}
    end.

activate_logfile() ->
    case read_const(epop_log_dir) of
	Dir when list(Dir) ->
	    {Y,M,D} = date(),
	    LogFile = lists:concat([Dir ++ "/logfile.",Y,"-",M,"-",D]),
	    case error_logger:logfile({open,LogFile}) of
		ok -> error_logger:tty(false);
		_  -> error_msg("epop: Could not open logfile !")
	    end;
	_ ->
	    error_msg("epop: No Epop Log Directory specified !"),
	    exit(epop_log_dir)
    end.

setup_listen_sock(Lport) ->
    Opts =  [{packet,raw},{reuseaddr,true},{active,false}],
    case catch gen_tcp:listen(Lport,Opts) of
	{ok, Lsock}     -> {ok,Lsock};
	{error, Reason} -> {error,Reason};
	{'EXIT',Reason} -> {error,Reason}
    end.

listener(Lsock,Accept) ->
    receive
	{Accept,connected} ->
	    ?MODULE:listener(Lsock,do_accept(Lsock));
	{'EXIT',Accept,_} ->
	    ?MODULE:listener(Lsock,do_accept(Lsock));
	{From,stop} when pid(From) ->
	    epop_dets:stop(),
	    epop_notify:stop(),
	    error_logger:logfile(close),
	    From ! stopped,
	    exit(normal);
	_ ->
	    ?MODULE:listener(Lsock,Accept) 
    end.

do_accept(Lsock) ->
    spawn_link(?MODULE,worker,[self(),Lsock]).
    

%% ------------------
%% The Accept process
%% ------------------

worker(Listener,Lsock) ->
    initrc(),
    case gen_tcp:accept(Lsock) of
	{ok,S} ->
	    Listener ! {self(),connected},
	    Peer = peer(S),
	    info_msg("epop: Connected by ~s~n",[Peer]),
	    authenticate(S);
	Else ->
	    exit(Else)
    end.

authenticate(S) ->
    AuthMethod = which_auth_method(),
    NewDb = case AuthMethod of
		upass ->
		    send_msg(S,upass_ready),
		    db();
		apop ->
		    TS = apop_timestamp(),
		    send_msg(S,{apop_ready,TS}),
		    db_in(db(),banner_timestamp,TS)
	    end,
    authorize(S,NewDb,AuthMethod).

peer(S) ->
    case inet:peername(S) of
	{ok,{{B3,B2,B1,B0},Port}} ->
	    lists:concat([B3,".",B2,".",B1,".",B0]);
	_ -> "unknown peer"
    end.

%% -------------------------------
%% The Quit state (not in the RFC)
%% -------------------------------

quit(S,Db) ->
    {ok,User} = db_out(Db,user),
    Peer = peer(S),
    send_msg(S,{signoff,User}),
    unlock_maildrop(User),
    info_msg("epop: Signed off user: ~p , closing connection to: ~s~n",
	     [User,Peer]),
    gen_tcp:close(S),
    exit(normal).
    

%% -------------------
%% The Authorize state
%% -------------------

%% -------------------------------------------------------------
%% The session database consists of a collection
%% of {Key,Value} tuples. Valid keys so far is:
%%
%%   {user,UserName}       -  The name of the user
%%   {passwd,PassWord}     -  The user password
%%   {banner_timestamp,TS} -  In case of APOP authentication
%%   {{deleted,N},true}    -  A delete-message mark
%%   {scan_list,ScanList}  -  A scan list, where ScanList is a
%%                           sorted list of {MsgID,Size} tuples

authorize(S,Db,upass) -> goto_auth(S,Db);
authorize(S,Db,apop)  -> goto_apop_auth(S,Db).

goto_auth(S,Db) ->
    auth(S,recv_sl(S),Db).

goto_auth(S,Cc,Db) ->
    auth(S,recv_sl(S,Cc),Db).

auth(S,{[$U,$S,$E,$R|T],Cc},Db) -> % USER
    auth_name(S,T,Cc,Db);
auth(S,{[$C,$A,$P,$A|T],Cc},Db) -> % CAPA (RFC-2449)
    send_capabilities(S,Db),
    auth(S,recv_sl(S,Cc),Db);
auth(S,{[$Q,$U,$I,$T|T],Cc},Db) -> % QUIT
    quit(S,Db);
auth(S,_,Db)               -> % Unexpected
    ok.

auth_name(S,Sname,Cc,Db) ->
    case regexp:match(Sname,"[^ \\r\\n]+") of
	{match,Start,Length} ->
	    Name = string:substr(Sname,Start,Length),
	    is_user_ok(S,Name,Cc,Db);
	_ ->
	    send_msg(S,{userwrong,Sname}),
	    goto_auth(S,Cc,Db)
    end.

is_user_ok(S,Name,Cc,Db) ->
    case get_passwd(Name) of
	{ok,Passwd} ->
	    send_msg(S,{userok,Name}),
	    NewDb = db_in(db_in(Db,user,Name),passwd,Passwd),
	    auth_pass(S,recv_sl(S,Cc),NewDb);
	_ ->
	    send_msg(S,{userwrong,Name}),
	    goto_auth(S,Cc,Db)
    end.

auth_pass(S,{[$P,$A,$S,$S,$ |T],Cc},Db) ->
    %% NB: Spaces are valid in the password string
    case regexp:match(T,"[^\\r\\n]+") of
	{match,Start,Length} ->
	    Passwd = string:substr(T,Start,Length),
	    is_passwd_ok(S,Passwd,Cc,Db);
	_ ->
	    send_msg(S,passwdwrong),
	    goto_auth(S,Cc,Db)
    end.
	    
is_passwd_ok(S,Passwd,Cc,Db) ->
    case db_member(Db,passwd,Passwd) of
	true ->
	    set_lock(S,Cc,Db);
	_ ->
	    send_msg(S,passwdwrong),
	    goto_auth(S,Cc,Db)
    end.

set_lock(S,Cc,Db) ->
    {ok,User} = db_out(Db,user),
    case lock_maildrop(User) of
	{ok,ScanList} ->
	    send_msg(S,maildroplocked),
	    %%
	    %% We store the scan list locally for this session. 
	    %% It is important to freeze the view of the maildrop
	    %% at this stage to avoid interference with new 
	    %% incoming mails. The lock protects us from the
	    %% case where someone else tries to delete one of
	    %% our own mails.
	    %%
	    transaction(S,Cc,insert_scan_list(Db,ScanList));
	_ ->
    	    send_msg(S,lockfailed),
	    goto_auth(S,Cc,Db)
    end.


which_auth_method() ->
    case read_const(epop_auth) of
	upass -> upass;
	apop  -> apop;
	Else  ->
	    error_msg("epop_server: No (or wrong) authentication method: ~p , "
		      "server terminating...~n",[Else]),
	    exit(epop_auth)
    end.

%% ----------
%% APOP stuff

goto_apop_auth(S,Db) ->
    apop_auth(S,recv_sl(S),Db).

goto_apop_auth(S,Cc,Db) ->
    apop_auth(S,recv_sl(S,Cc),Db).

apop_auth(S,{[$A,$P,$O,$P|T],Cc},Db) -> % APOP
    apop_auth_name(S,T,Cc,Db);
apop_auth(S,{[$C,$A,$P,$A|T],Cc},Db) -> % CAPA (RFC-2449)
    send_capabilities(S,Db),
    apop_auth(S,recv_sl(S,Cc),Db);
apop_auth(S,{[$Q,$U,$I,$T|T],Cc},Db) -> % QUIT
    quit(S,Db);
apop_auth(S,_,Db)                    -> % Unexpected
    ok.

apop_auth_name(S,Sname,Cc,Db) ->
    case string:tokens(Sname," \r\n") of
	[User,Digest] ->
	    apop_is_user_ok(S,User,Digest,Cc,Db);
	_ ->
	    send_msg(S,unexpected),
	    goto_apop_auth(S,Cc,Db)
    end.

apop_is_user_ok(S,Name,Digest,Cc,Db) ->
    case get_passwd(Name) of
	{ok,Passwd} ->
	    apop_is_digest_ok(S,Name,Digest,Passwd,Cc,Db);
	_ ->
	    send_msg(S,{userwrong,Name}),
	    goto_apop_auth(S,Cc,Db)
    end.

apop_is_digest_ok(S,Name,Digest,Passwd,Cc,Db) ->
    {ok,TS} = db_out(Db,banner_timestamp),
    case catch epop_md5:string(TS ++ Passwd) of
	Digest when list(Digest) ->
	    NewDb = db_in(db_in(Db,user,Name),passwd,Passwd),
	    set_lock(S,Cc,NewDb);
	X ->
	    send_msg(S,no_permission),
	    goto_apop_auth(S,Cc,Db)
    end.

apop_timestamp() ->
    HostName = epop:hostname(),
    Spid = lists:concat(string:tokens(pid_to_list(self()),"<.>")),
    {Ms,S,Us} = now(),
    Clock = lists:concat([Ms,S,Us]),
    "<" ++ Spid ++ "." ++ Clock ++ "@" ++ HostName ++ ">".


%% ---------------------
%% The transaction state
%% ---------------------

transaction(S,Cc,Db) ->
    goto_trans(S,Cc,Db).

goto_trans(S,Cc,Db) ->
    %%dbg(),
    trans(S,recv_sl(S,Cc),Db).

%dbg() ->
%    {_,H} = process_info(self(),heap_size),
%    {_,S} = process_info(self(),stack_size),
%    info_msg("epop_server(dbg): stack=~w heap=~w~n",[S,H]).
    

trans(S,{[$S,$T,$A,$T|_],Cc},Db) -> % STAT
    stat_reply(S,Cc,Db);
trans(S,{[$L,$I,$S,$T|T],Cc},Db) -> % LIST
    list_reply(S,T,Cc,Db);
trans(S,{[$R,$E,$T,$R|T],Cc},Db) -> % RETR
    retr_reply(S,T,Cc,Db);
trans(S,{[$D,$E,$L,$E|T],Cc},Db) -> % DELE
    dele_reply(S,T,Cc,Db);
trans(S,{[$N,$O,$O,$P|_],Cc},Db) -> % NOOP
    noop_reply(S,Cc,Db);
trans(S,{[$R,$S,$E,$T|_],Cc},Db) -> % RSET
    rset_reply(S,Cc,Db);
trans(S,{[$Q,$U,$I,$T|_],Cc},Db) -> % QUIT
    update(S,Cc,Db);
trans(S,{[$U,$I,$D,$L|T],Cc},Db) -> % UIDL
    uidl_reply(S,T,Cc,Db);
trans(S,{[$T,$O,$P|T],Cc},Db)    -> % TOP
    top_reply(S,T,Cc,Db);
trans(S,{[$N,$T,$F,$Y|T],Cc},Db) -> % NTFY
    ntfy_reply(S,T,Cc,Db);
trans(S,{[$C,$A,$P,$A|T],Cc},Db) -> % CAPA (RFC-2449)
    send_capabilities(S,Db),
    goto_trans(S,Cc,Db);
trans(S,{_,Cc},Db)               -> % Unexpected
    unexpected_reply(S,Cc,Db).

%% -----------------------
%% Reply to a STAT request
%% -----------------------

stat_reply(S,Cc,Db) ->
    {ok,User} = db_out(Db,user),
    {ok,{N,M}}  = do_stat(User),
    send_msg(S,{statreply,N,M}),
    goto_trans(S,Cc,Db).

%% -----------------------
%% Reply to a LIST request
%% -----------------------

list_reply(S,Smsg,Cc,Db) ->
    NoArg = regexp:match(Smsg,"[ ]*\r\n"),
    Arg   = regexp:match(Smsg,"[ ]*[0-9]+[ ]*\r\n"),
    do_list_reply(S,Smsg,Cc,Db,NoArg,Arg).

%% No argument given !
do_list_reply(S,Smsg,Cc,Db,{match,1,_},_) ->
    case scan_list(Db) of
	{ok,[]} ->
	    send_msg(S,listnomsg);
	{ok,ScanList} ->
	    send_msg(S,listbegin),
	    send_scan_list(S,Db,ScanList),
	    send_msg(S,terminateoctet)
    end,
    goto_trans(S,Cc,Db);
%% A numeric argument given !
do_list_reply(S,Smsg,Cc,Db,_,{match,1,Len}) when Len==length(Smsg) ->
    {match,Start,Length} = regexp:first_match(Smsg,"[0-9]+"),
    Num = l2i(string:substr(Smsg,Start,Length)),
    case db_member(Db,{deleted,Num},true) of
	true  -> send_msg(S,nosuchmsg);
	false ->
	    case scan_list(Db,Num) of
		{ok,[_MsgID,Size]} ->
		    %% NB: Add with 2 for the termination octet
		    send_msg(S,{listmulti,Num,Size+2});
		_ ->
		    send_msg(S,nosuchmsg)
	    end
    end,
    goto_trans(S,Cc,Db);
%% A fulty argument given !
do_list_reply(S,_,Cc,Db,_,_) ->
    send_msg(S,nosuchmsg),
    goto_trans(S,Cc,Db).

%% ------------------------------------------
%% Number the scan list before sending it and
%% do not send messages marked for deletion.

send_scan_list(S,Db,ScanList) ->
    send_scan_list(S,Db,ScanList,1).

send_scan_list(S,Db,[[_MsgID,Size]|T],N) ->
    %%    case db_member(Db,{deleted,N},true) of
    case already_deleted(N,Db) of
	true  -> false; 
	false ->
	    %% NB: Add with 2 for the termination octet
	    send_msg(S,{listmulti,N,Size+2})
    end,
    send_scan_list(S,Db,T,N+1);
send_scan_list(_,_,[],_) ->
    true.

scan_list(Db) ->
    db_out(Db,scan_list).

scan_list(Db,N) ->
    case db_out(Db,scan_list) of
	{ok,ScanList} when length(ScanList)<N ->
	    {error,nosuchmsg};
	{ok,ScanList} when length(ScanList)>=N ->
	    {ok,lists:nth(N,ScanList)};
	_ ->
	    {error,nosuchmsg}
    end.

%% -----------------------
%% Reply to a RETR request
%% -----------------------

retr_reply(S,Smsg,Cc,Db) ->
    do_retr_reply(S,Smsg,Cc,Db,regexp:match(Smsg,"[ ]*[0-9]+[ ]*\r\n")).

%% A numeric argument given !
do_retr_reply(S,Smsg,Cc,Db,{match,1,Len}) when Len==length(Smsg) ->
    {match,Start,Length} = regexp:first_match(Smsg,"[0-9]+"),
    Num = l2i(string:substr(Smsg,Start,Length)),
    case get_mail(Num,Db) of
	{ok,{Mail,Size}} ->
	    %% NB: Add with 2 for the termination octet
	    send_msg(S,{retrreply,Size+2}),
	    send_the_mail(S,Mail),
	    send_msg(S,terminateoctet),
	    goto_trans(S,Cc,set_download_delete_mark(Db,Num));
	_ ->
	    send_msg(S,nosuchmsg),
	    goto_trans(S,Cc,Db)
    end;
%% ERROR: No argument given !!
do_retr_reply(S,_,Cc,Db,_) ->
    send_msg(S,nosuchmsg),
    goto_trans(S,Cc,Db).

set_download_delete_mark(Db,Num) ->
    case download_delete_p() of
	true  -> do_delete(Num,Db);
	false -> Db
    end.

download_delete_p() ->
    case read_const(epop_download_delete) of
	true -> true;
	_    -> false
    end.

send_the_mail(S,Mail) ->
    case byte_stuff_it(Mail) of
	{[],M} -> send_msg(S,{mailchunk,M});
	{R,M}  -> 
	    send_msg(S,{mailchunk,M}),
	    send_the_mail(S,R)
    end.

byte_stuff_it(Mail) -> 
    byte_stuff_it(Mail,[],?BYTE_STUFF_CHUNK).

%% Byte stuff every occasion of: '\n.' into '\n..'
%% Do it in specified chunks, but always on a
%% newline boundary.
byte_stuff_it([$\n,$.|Mail],Acc,N) when N =< 0 ->
    {[$.,$.|Mail],lists:reverse([$\n|Acc])};
byte_stuff_it([$\n|Mail],Acc,N) when N =< 0 ->
    {Mail,lists:reverse([$\n|Acc])};
byte_stuff_it([$\n,$.|Mail],Acc,N) ->
    byte_stuff_it(Mail,[$.,$.,$\n|Acc],N-3);
byte_stuff_it([H|T],Acc,N) ->
    byte_stuff_it(T,[H|Acc],N-1);
byte_stuff_it([],Acc,_) ->
    {[],lists:reverse(Acc)}.

get_mail(Num,Db) ->
    case scan_list(Db,Num) of
	{ok,[MsgID,_Size]} ->
	    {ok,User} = db_out(Db,user),
	    get_the_mail(User,MsgID);
	_ ->
	    {error,nosuchmsg}
    end.

get_the_mail(User,MsgID) ->
    case epop_dets:get_mail(User,MsgID) of
	{ok,{Mail,Size}} ->
	    {ok,{Mail,Size}};
	_                -> {error,nosuchmsg}
    end.


%% ----------------------
%% Reply to a TOP request
%% ----------------------

%% NB: We *should* separate the header and body with 
%% a blank line, and return the number of Lines in
%% the body. 

top_reply(S,Smsg,Cc,Db) ->
    case string:tokens(Smsg," \r\n") of
	[MsgId,Lines|_] ->
	    M = (catch list_to_integer(MsgId)),
	    N = (catch list_to_integer(Lines)),
	    if N>0, integer(N), integer(M) ->
		    do_top_reply(S,Smsg,Cc,Db,M,N);
	       true ->
		    top_error(S,Cc,Db)
	    end;
	_ ->
	    top_error(S,Cc,Db)
    end.

%% ERROR: Wrong arguments !!
top_error(S,Cc,Db) ->
    send_msg(S,nosuchmsg),
    goto_trans(S,Cc,Db).

%% A numeric argument given !
do_top_reply(S,Smsg,Cc,Db,Num,Lines) ->
    case get_mail(Num,Db) of
	{ok,{Mail,Size}} ->
	    %% NB: Add with 2 for the termination octet
	    {Tmail,Tsize} = trunc(Mail,Size,Lines),
	    send_msg(S,{retrreply,Tsize+2}),
	    send_the_mail(S,Tmail),
	    send_msg(S,terminateoctet),
	    goto_trans(S,Cc,set_download_delete_mark(Db,Num));
	_ ->
	    send_msg(S,nosuchmsg),
	    goto_trans(S,Cc,Db)
    end.

trunc(Mail,Size,Lines) ->
    case string:tokens(Mail,"\n") of
	Ls when length(Ls) =< Lines ->
	    {Mail,Size};
	Ls -> 
	    Fl = first(Ls,Lines),
	    Tm = lists:append(Fl),
	    {Tm,length(Tm)}
    end.

first(_,0)              -> [];
first([H|T],N) when N>0 -> [H|first(T,N-1)];
first([],_)             -> [].

%% -----------------------
%% Reply to a DELE request
%% -----------------------

dele_reply(S,Smsg,Cc,Db) ->
    do_dele_reply(S,Smsg,Cc,Db,regexp:match(Smsg,"[ ]*[0-9]+[ ]*\r\n")).

%% A numeric argument given !
do_dele_reply(S,Smsg,Cc,Db,{match,1,Len}) when Len==length(Smsg) ->
    {match,Start,Length} = regexp:first_match(Smsg,"[0-9]+"),
    Num = l2i(string:substr(Smsg,Start,Length)),
    NewDb = mark_as_deleted(S,Num,Db),
    goto_trans(S,Cc,NewDb);
do_dele_reply(S,Smsg,Cc,Db,_) ->
    send_msg(S,nosuchmsg),
    goto_trans(S,Cc,Db).

mark_as_deleted(S,N,Db) ->
    case ok_to_delete(S,N,Db) of
	true  ->
	    NewDb = do_delete(N,Db),
	    send_msg(S,{deleted,N}),
	    NewDb;
	{false,Reply} ->
	    send_msg(S,Reply),
	    Db
    end.

do_delete(N,Db) ->
    db_in(Db,{deleted,N},true).

ok_to_delete(S,N,Db) ->
    Already = already_deleted(N,Db),
    Valid   = valid_mailno(N,Db),
    if Already==true ->
	    %% NB: This is deliberate !
	    {false,{deleted,N}};
       Valid==false  ->
	    {false,nosuchmsg};
       true ->
	    true
    end.

already_deleted(N,Db) ->
    case db_out(Db,{deleted,N}) of
	{ok,true} -> true;
	_         -> false
    end.

valid_mailno(N,Db) ->
    case scan_list(Db) of
	{ok,ScanList} when N=<length(ScanList) -> true;
	_ -> false
    end.

	    

%% -----------------------
%% Reply to a NOOP request
%% -----------------------

noop_reply(S,Cc,Db) ->
    send_msg(S,ok),
    goto_trans(S,Cc,Db).

%% -----------------------
%% Reply to a RSET request
%% -----------------------

rset_reply(S,Cc,Db) ->
    Db2 = rm_delete_mark(Db),
    NewDb = rm_notification_mark(Db2),
    send_msg(S,ok),
    goto_trans(S,Cc,NewDb).

rm_delete_mark(Db) ->
    F = fun({{deleted,_},_}) -> false;
	   (_) -> true
	end,
    db_filter(Db,F).

rm_notification_mark(Db) ->
    F = fun({{notified,_},_}) -> false;
	   (_) -> true
	end,
    db_filter(Db,F).

%% -----------------------
%% Reply to a UIDL request
%% -----------------------

uidl_reply(S,Smsg,Cc,Db) ->
    NoArg = regexp:match(Smsg,"[ ]*\r\n"),
    Arg   = regexp:match(Smsg,"[ ]*[0-9]+[ ]*\r\n"),
    do_uidl_reply(S,Smsg,Cc,Db,NoArg,Arg).

%% No argument given !
do_uidl_reply(S,Smsg,Cc,Db,{match,1,_},_) ->
    do_uidl_reply_2(S,Cc,Db);
%% A numeric argument given !
do_uidl_reply(S,Smsg,Cc,Db,_,{match,1,Len}) when Len==length(Smsg) ->
    {match,Start,Length} = regexp:first_match(Smsg,"[0-9]+"),
    Num = l2i(string:substr(Smsg,Start,Length)),
    do_uidl_reply_2(S,Cc,Db,Num);
%% A fulty argument given !
do_uidl_reply(S,Smsg,Cc,Db,_,_) ->
    send_msg(S,nosuchmsg),
    goto_trans(S,Cc,Db).

do_uidl_reply_2(S,Cc,Db) ->
    case scan_list(Db) of
	{ok,[]} ->
	    send_msg(S,listnomsg);
	{ok,ScanList} ->
	    send_msg(S,uidlbegin),
	    send_uidl_list(S,Db,ScanList),
	    send_msg(S,terminateoctet)
    end,
    goto_trans(S,Cc,Db).

do_uidl_reply_2(S,Cc,Db,Num) ->
    case already_deleted(Num,Db) of
	true  -> send_msg(S,{alreadydeleted,Num});
	false ->
	    case scan_list(Db,Num) of
		{ok,[MsgID,_Size]} -> send_msg(S,{listmulti,Num,MsgID});
		_                  -> send_msg(S,nosuchmsg)
	    end
    end,
    goto_trans(S,Cc,Db).

%% Number the uidl list before sending it and
%% do not send messages marked for deletion.

send_uidl_list(S,Db,ScanList) ->
    send_uidl_list(S,Db,ScanList,1).

send_uidl_list(S,Db,[[MsgID,_Size]|T],N) ->
    case already_deleted(N,Db) of
	true  -> false; 
	false -> send_msg(S,{uidlmulti,N,MsgID})
    end,
    send_uidl_list(S,Db,T,N+1);
send_uidl_list(_,_,[],_) ->
    true.

%% -----------------------
%% Reply to a NTFY request
%% -----------------------

ntfy_reply(S,Smsg,Cc,Db) ->
    case read_const(epop_notification) of
	true -> do_ntfy_reply(S,Smsg,Cc,Db);
	_    -> unexpected_reply(S,Cc,Db)
    end.

do_ntfy_reply(S,Smsg,Cc,Db) ->
    case parse_ntfy_msg(Smsg) of
	{Host,Port} ->
	    send_msg(S,ok),
	    NewDb = mark_notified(Host,Port,Db),
	    goto_trans(S,Cc,NewDb);
	_ ->
	    send_msg(S,no_notification),
	    goto_trans(S,Cc,Db)
    end.

parse_ntfy_msg(Smsg) ->
    case string:tokens(Smsg," \r\n") of
	[Host,PortNo] -> {Host,list_to_integer(PortNo)};
	_             -> false
    end.

mark_notified(Host,Port,Db) ->
    db_in(Db,notified,{Host,Port}).

%% To be called when kicking the notification server.
notify(Db) ->
    {ok,User} = db_out(Db,user),
    epop_notify:notify(User).

%% To be called when placing an order for notification.
order_notification(Db) ->
    {ok,Name} = db_out(Db,user),
    case db_out(Db,notified) of
	{ok,{Host,Port}} -> epop_notify:order(Name,Host,Port);
	_ -> false
    end.

%% -----------------------------
%% Reply to a Unexpected request
%% -----------------------------

unexpected_reply(S,Cc,Db) ->
    send_msg(S,unexpected),
    goto_trans(S,Cc,Db).

%% ----------------
%% The update state
%% ----------------

update(S,Cc,Db) ->
    NewDb = delete_marked_msgs(Db),
    notify(NewDb),
    order_notification(NewDb),
    quit(S,NewDb).

%% --------
%% Messages
%% --------

msg(upass_ready) ->
    "+OK " ++ greeting() ++ "\r\n";
msg({apop_ready,TS}) ->
    "+OK " ++ greeting() ++ " " ++ TS ++ "\r\n";
msg({userok,Name}) ->
    "+OK " ++ Name ++ " is a valid mailbox\r\n";
msg({userwrong,Name}) ->
    "-ERR sorry, no mailbox for " ++ Name ++ " here\r\n";
msg(passwdwrong) ->
    "-ERR invalid password\r\n";
msg(no_permission) ->
    "-ERR permission denied\r\n";
msg(lockfailed) ->
    "-ERR unable to lock maildrop\r\n";
msg(maildroplocked) ->
    "+OK maildrop locked and ready\r\n";
msg({statreply,N,M}) ->
    "+OK " ++ i2l(N) ++ " " ++ i2l(M) ++ "\r\n";
msg({listreply,N,M}) ->
    "+OK " ++ i2l(N) ++ " " ++ i2l(M) ++ "\r\n";
msg(nosuchmsg) ->
    "-ERR no such message\r\n";
msg(listbegin) ->
    "+OK scan listing follows\r\n";
msg(listnomsg) ->
    "+OK\r\n.\r\n";
msg({listmulti,N,M}) ->
    i2l(N) ++ " " ++ i2l(M) ++ "\r\n";
msg(uidlbegin) ->
    "+OK uidl listing follows\r\n";
msg({uidlmulti,N,ID}) ->
    i2l(N) ++ " " ++ ID ++ "\r\n";
msg(terminateoctet) ->
    ".\r\n";
msg({retrreply,Size}) ->
    "+OK " ++ i2l(Size) ++ " octets\r\n";
msg({retrmail,Mail}) ->
    Mail ++ "\r\n";
msg({mailchunk,Mail}) ->
    Mail;
msg({deleted,Num}) ->
    "+OK message " ++ i2l(Num) ++ " deleted\r\n";
msg({alreadydeleted,N}) ->
    "-ERR message " ++ i2l(N) ++ " already deleted\r\n";
msg(ok) ->
    "+OK\r\n";
msg(unexpected) ->
    "-ERR que ?\r\n";
msg(no_notification) ->
    "-ERR cannot serve you notification request\r\n";
msg({signoff,Name}) ->
    "+OK " ++ Name ++ " , EPOP server signing off\r\n".

send_msg(S,Msg) ->
    send(S,msg(Msg)).

send(S,Msg) ->
    gen_tcp:send(S,Msg).

%% ----------------------------------------
%% Interface towards the Epop Mail Database
%% ----------------------------------------

get_passwd(User) ->
    epop_dets:get_passwd(User).

do_stat(User) ->
    epop_dets:stat(User).

lock_maildrop(User) ->
    epop_dets:lock_maildrop(User).

unlock_maildrop(User) ->
    epop_dets:unlock_maildrop(User).

insert_scan_list(Db,ScanList) ->
    case db_member(Db,scan_list) of
	true  -> db_replace(Db,scan_list,ScanList);
	false -> db_in(Db,scan_list,ScanList)
    end.

delete_marked_msgs(Db) ->
    {ok,User} = db_out(Db,user),
    F = fun({{deleted,N},_}) ->
		case scan_list(Db,N) of
		    {ok,[MsgID,Size]} ->
			epop_dets:delete_mail(User,MsgID,Size);
		    _ ->
			true % ignore !
		end;
	   (_) -> true
	end,
    %% Delete all mails marked for deletion
    db_foreach(Db,F),
    %% Remove all the delete marks
    rm_delete_mark(Db).

%% ----------
%% Misc stuff
%% ----------

i2l(I) when integer(I) -> integer_to_list(I);
i2l(I) when list(I)    -> I.

l2i(L) when list(L)    -> list_to_integer(L);
l2i(L) when integer(L) -> L.

lcase([C|Cs]) when C>=$A,C=<$Z -> [C+32|lcase(Cs)]; % A-Z
lcase([C|Cs])                  -> [C|lcase(Cs)];
lcase([])                      -> [].

a2l(Atom) when atom(Atom) -> atom_to_list(Atom);
a2l(List) when list(List) -> List.

%% =============================================
%% HERE GOES FUNCTIONALITY SPECIFIC FOR RFC-2449
%% (POP3 Extension Mechanism)
%% =============================================

%% Return our capabilities to the client
send_capabilities(S,Db) ->
    send(S,"+OK Capability list follows\r\n"),
    F = fun(Capa) -> send(S,Capa) end,
    lists:foreach(F,capabilities()),
    send_msg(S,terminateoctet).

%% This are our current capabilities
capabilities() ->
    ["TOP\r\n",
     "USER\r\n",
     "UIDL\r\n",
     "EXPIRE " ++ expire() ++ "\r\n",
     "IMPLEMENTATION " ++ implementation() ++ "\r\n"].

expire() ->
    case download_delete_p() of
	true  -> "0";
	false -> "NEVER"
    end.

implementation() ->
    case lists:keysearch(vc,1,apply(?MODULE,module_info,[attributes])) of
	{value,{vc,[VC]}} -> a2l(VC);
	_                 -> "Epop-" ++ version()
    end.



    
    
