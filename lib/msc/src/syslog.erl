-module(syslog).
-author('tobbe@serc.rmit.edu.au').
%%%----------------------------------------------------------------------
%%% File    : syslog.erl
%%% Author  : Torbjorn Tornkvist <tobbe@serc.rmit.edu.au>
%%% Purpose : Interface to the Unix syslog facility.
%%% Created : 2 Dec 1998 by Torbjorn Tornkvist <tobbe@serc.rmit.edu.au>
%%% Function: See also the man page for syslog.conf
%%%
%%%           syslog:start() 
%%%           syslog:stop()  
%%%           syslog:send(Program, Level, Msg)
%%%           syslog:send(Facility, Program, Level, Msg)
%%%
%%% Examples: syslog:send( my_ppp, syslog:debug(), "LCP link established")
%%%           syslog:send( syslog:mail(), my_mailer, syslog:err(), Msg)
%%%
%%%----------------------------------------------------------------------
-vc('$Id$ ').
%% Exported
-export([start/0,stop/0,version/0,send/3,send/4]).
-export([emergency/0,alert/0,critical/0,error/0,warning/0,
	 notice/0,info/0,debug/0]).
-export([kern/0,user/0,mail/0,daemon/0,auth/0,syslog/0,lpr/0,
	 news/0,uucp/0,cron/0,authpriv/0,ftp/0]).
%% Internal
-export([init/0]).

-define(SERVER_NAME, syslog_server).

version() ->
    [_,Rev|_] = string:tokens("$Revision$ "," "),Rev.

start() -> 
    case whereis(?SERVER_NAME) of
	Pid when pid(Pid) -> ok;
	_ ->
	    Pid = spawn(?MODULE, init, []),
	    register(?SERVER_NAME,Pid)
    end.

stop() ->
    ?SERVER_NAME ! {self(),stop},
    receive stopped -> ok end.

send(Who,Level,Msg) when atom(Who),integer(Level),list(Msg) ->
    ?SERVER_NAME ! {send,{Who,Level,Msg}}.

send(Facility,Who,Level,Msg) when integer(Facility),atom(Who),integer(Level),list(Msg) ->
    ?SERVER_NAME ! {send,{Facility,Who,Level,Msg}}.

%% Convenient routines for specifying levels.

emergency() -> 0. % system is unusable 
alert()     -> 1. % action must be taken immediately 
critical()  -> 2. % critical conditions 
error()     -> 3. % error conditions 
warning()   -> 4. % warning conditions 
notice()    -> 5. % normal but significant condition 
info()      -> 6. % informational
debug()     -> 7. % debug-level messages 

%% Convenient routines for specifying facility codes

kern()     -> (0 bsl 3) . % kernel messages 
user()     -> (1 bsl 3) . % random user-level messages 
mail()     -> (2 bsl 3) . % mail system 
daemon()   -> (3 bsl 3) . % system daemons 
auth()     -> (4 bsl 3) . % security/authorization messages 
syslog()   -> (5 bsl 3) . % messages generated internally by syslogd 
lpr()      -> (6 bsl 3) . % line printer subsystem 
news()     -> (7 bsl 3) . % network news subsystem 
uucp()     -> (8 bsl 3) . % UUCP subsystem 
cron()     -> (9 bsl 3) . % clock daemon 
authpriv() -> (10 bsl 3). % security/authorization messages (private) 
ftp()      -> (11 bsl 3). % ftp daemon 

	  
%% ----------
%% The server
%% ----------

init() ->
    process_flag(trap_exit,true),
    {ok,S} = gen_udp:open(0),
    loop(S,local_host(),syslog_port()).

loop(S,Host,Port) ->
    receive
	{send,What} ->
	    do_send(S,Host,Port,What),
	    loop(S,Host,Port);
	{From,stop} ->
	    From ! stopped;
	_ ->
	    loop(S,Host,Port)
    end.

%% priorities/facilities are encoded into a single 32-bit 
%% quantity, where the bottom 3 bits are the priority (0-7) 
%% and the top 28 bits are the facility (0-big number).    

do_send(S,Host,Port,{Who,Level,Msg}) ->
    Packet = "<" ++ i2l(Level) ++ "> " ++ a2l(Who) ++ ": " ++ Msg ++ "\n",
    gen_udp:send(S,Host,Port,Packet);
do_send(S,Host,Port,{Facil,Who,Level,Msg}) ->
    Packet = "<" ++ i2l(Facil bor Level) ++ "> " ++ a2l(Who) ++ ": " ++ Msg ++ "\n",
    gen_udp:send(S,Host,Port,Packet).

local_host() ->
    {ok,Hname} = inet:gethostname(),
    Hname.

syslog_port() -> 514.

i2l(Int) -> integer_to_list(Int).

a2l(Atom) -> atom_to_list(Atom).

