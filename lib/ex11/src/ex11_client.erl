-module(ex11_client).
-author('tnt@home.se').
%%%---------------------------------------------------------------------
%%% Created : 14 Feb 1999 by tnt@home.se
%%% Function: The X11 client (which talks to the X11 server).
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
%%% The Original Code is x11-0-1
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1999, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): Vlad Dumitrescu, vlad_dumitrescu@hotmail.com.
%%%
%%%
%%% Modified: 23 Feb 1998 by tony@cslab.ericsson.se
%%%             To make it work under Windows. Now makes
%%%             use of ex11_xauth:filename/0.
%%%           25 Feb 2003 by Vlad Dumitrescu
%%%             Converting to binary format
%%%
%%%---------------------------------------------------------------------
-export([start/1,req/2,synchronize/2,flush/1,lock_display/1,unlock_display/1,
	 set_display/2,get_display/1,new_window/2,new_pixmap/2,new_gc/2]).
%% Internal exports
-export([init/2]).

-import(error_logger,[info_msg/2]).

-include_lib("kernel/include/inet.hrl").
-include("ex11.hrl").

%% Some internal macros
-define(FLUSH,        true).  % Always flush request buffer
-define(BUFFER,      false).  % Buffer requests
-define(NO_LOCK,     false).  % No lock set on the display datastructure
-define(MAX_SEQNO,  16#FFFF). % Max sequence number.

%% -------------------
%% EXPORTED INTERFACE
%% -------------------

%% --------------------------------------------------
%% Start an X-client, connecting to specified server

start(Host) ->
    Pid = spawn_link(?MODULE,init,[self(),Host]),
    ack(Pid).

%% ---------------------------------------------------
%% Send a request to the X-client process. The encode
%% routine will tell us if the request will trigger
%% a reply being sent from the X-server. 
%% NB: The user pays for the encoding cost *here* !
req(X,Req) when ?IS_XLIB_HANDLE(X) ->
    case ex11_proto:encode(X,Req) of
	{ok,Msg} -> 
	    call(X,send_msg,Msg),ok;
	{ok,Msg,Result} -> 
	    call(X,send_msg,Msg),Result;
	{reply,Msg,ReplyType} -> 
	    call(X,send_msg_reply,{Msg,ReplyType}),ok;
	{reply,Msg,ReplyType,Result} -> 
	    call(X,send_msg_reply,{Msg,ReplyType}),Result;
	Error -> 
	    Error
    end.

%% --------------------------------------------------------
%% Tell X client whether request shall be buffered or not.

synchronize(X,true) when ?IS_XLIB_HANDLE(X) -> 
    call(X,set_sync,?FLUSH);
synchronize(X,false) when ?IS_XLIB_HANDLE(X) -> 
    call(X,set_sync,?BUFFER).

%% ----------------------
%% Flush request buffer.

flush(X) when ?IS_XLIB_HANDLE(X) -> 
    call(X,flush).

%% -------------------------------------------------
%% When allocating Id's, the encoding routine
%% (which is running in the user process) has
%% to update the Display data structure. This
%% is done in-place by writing in the Db. 
%% Before that can be done, a lock on the Display
%% data structure has to be obtained.

lock_display(X) when ?IS_XLIB_HANDLE(X) ->
    call(X,lock_display).

unlock_display(X) when ?IS_XLIB_HANDLE(X) ->
    call(X,unlock_display).

set_display(X,Dpy) when ?IS_XLIB_HANDLE(X),?IS_DISPLAY(Dpy) ->
    ex11_db:set_display(X,Dpy),ok.

get_display(X)  when ?IS_XLIB_HANDLE(X) ->
    {ok,ex11_db:get_display(X)}.

%% -----------------------------------------------
%% During encoding, new Id's may be created,
%% which we may have to be able to refer to.
%% For example, a window with a process to be 
%% able to deliver future events. This is done
%% in-place by writing straight into the Db.

new_window(X,Wid) when ?IS_XLIB_HANDLE(X) ->
    ex11_db:put_window(X#xlib.db,Wid,self()).

new_pixmap(X,Pix) when ?IS_XLIB_HANDLE(X) ->
    ex11_db:put_pixmap(X#xlib.db,Pix,self()).

new_gc(X,Cid) when ?IS_XLIB_HANDLE(X) ->
    ex11_db:put_gc(X#xlib.db,Cid,self()).


%% ---------------
%% INTERNAL STUFF
%% ---------------

%% ------------------------
%% Initialize process loop    

init(From,Host) ->
    process_flag(trap_exit,true),
    Cookie = case ex11_xauth:filename() of
		 "" -> "";
		 FName ->
		     case ex11_xauth:read(FName) of
			 {error,_} -> "";
			 {ok,Xauth} ->
			     case ex11_xauth:host2cookie(Host,Xauth) of
				 {true,XCookie} -> XCookie;
				 false -> ""
			     end
		     end
	     end,
    case setup_connection(Host,Cookie) of
	{ok,Dpy}     -> 
	    %%?PRINT_DISPLAY(Dpy),
	    run(From,Dpy);
	{error,Emsg} -> 
	    error_logger:error_msg("~w: Setup failed reason: ~s~n",
				   [?MODULE,Emsg])
    end.

%% -----------------------------------------------------
%% loop(Display,       % The main data structure.
%%      Db,            % Session database.
%%      OutBuffer,     % Buffered output requests.
%%      Continuation,  % To continue decode from where
%%                     % we ran out of input last time.
%%      Sync,          % Shall we buffer output or not
%%      Lock)          % Lock display: ?NO_LOCK | LockQueue
%% -----------------------------------------------------

run(From,Dpy) ->
    Db = ex11_db:init(),
    ex11_db:set_display(Db,Dpy),
    send(From,{self(),{ok,#xlib{pid=self(),user=From,db=Db}}}),
    loop(Dpy,Db,queue:new(),?NO_CONT,1,?BUFFER,?NO_LOCK).

loop(Dpy,Db,Out,Cont,SeqNo,Sync,Lock) ->
    receive

	{From,send_msg,Msg} when Sync == ?FLUSH ->
	    %%io:format("SENDING REQUEST: ~w~n",[binary_to_list(Msg)]),
	    NewSeqNo = send_request(Dpy,SeqNo,Msg),
	    return(From,ok),
	    loop(Dpy,Db,Out,Cont,NewSeqNo,Sync,Lock);

	{From,send_msg,Msg} when Sync == ?BUFFER ->
	    %%io:format("BUFFERING REQUEST: ~w~n",[binary_to_list(Msg)]),
	    return(From,ok),
	    loop(Dpy,Db,queue:in(Msg,Out),Cont,SeqNo,Sync,Lock);

	{From,send_msg_reply,{Msg,ReplyType}} ->
	    %% Always flush buffer when we are expecting a reply
	    SeqNo1 = flush_buffer(Dpy,Out,SeqNo),
	    NewSeqNo = send_reply_request(Dpy,Db,SeqNo1,From,Msg,ReplyType),
	    return(From,ok),
	    loop(Dpy,Db,queue:in(Msg,Out),Cont,NewSeqNo,Sync,Lock);

	{From,flush} ->
	    %%io:format("FLUSHING BUFFER !~n"),
	    NewSeqNo = flush_buffer(Dpy,Out,SeqNo),
	    return(From,ok),
	    loop(Dpy,Db,queue:new(),Cont,NewSeqNo,Sync,Lock);

	{From,set_display,NewDpy} ->
	    return(From,ok),
	    loop(NewDpy,Db,Out,Cont,SeqNo,Sync,Lock);

	{From,get_display} ->
	    return(From,{ok,Dpy}),
	    loop(Dpy,Db,Out,Cont,SeqNo,Sync,Lock);

	{From,lock_display} ->
	    if (Lock == ?NO_LOCK) -> 
		    return(From,ok),
		    loop(Dpy,Db,Out,Cont,SeqNo,Sync,queue:new());
	       true ->
		    loop(Dpy,Db,Out,Cont,SeqNo,Sync,queue:in(From,Lock))
	    end;

	{From,unlock_display} when Lock == ?NO_LOCK ->
	    return(From,ok),
	    info_msg("Pid: ~w unlocking display when "
		     "already unlocked ~~n",[From]),
	    loop(Dpy,Db,Out,Cont,SeqNo,Sync,Lock);

	{From,unlock_display} when Lock =/= ?NO_LOCK ->
	    return(From,ok),
	    NewLock = case queue:out(Lock) of
			  {{value,Pid},Lck} -> return(Pid,ok),Lck;
			  {empty,Lck}       -> ?NO_LOCK
		      end,
	    loop(Dpy,Db,Out,Cont,SeqNo,Sync,NewLock);

	{From,set_sync,?FLUSH} ->
	    NewSeqNo = flush_buffer(Dpy,Out,SeqNo),
	    return(From,ok),
	    loop(Dpy,Db,queue:new(),Cont,NewSeqNo,?FLUSH,Lock);

	{From,set_sync,?BUFFER} ->
	    return(From,ok),
	    loop(Dpy,Db,Out,Cont,SeqNo,?BUFFER,Lock);

	{tcp,Sock,Data} ->
	    dispatch(Dpy,Db,Out,Cont,SeqNo,Sync,Lock,Data);

	{tcp_closed,Sock} ->
	    exit(normal);

	{'EXIT',Pid,Reason} ->
	    NewDb= analyse_exit(Db,Pid,Reason),
	    loop(Dpy,NewDb,Out,Cont,SeqNo,Sync,Lock);

	XX ->
	    io:format("GOT: ~w~n",[XX]),
	    loop(Dpy,Db,Out,Cont,SeqNo,Sync,Lock)
    end.

%% Should unmap/destroy any window controlled 
%% by this process, and if the top level process
%% then terminate this X client.
analyse_exit(Db,Pid,Reason) ->
    io:format("SHOULD ANALYSE EXIT OF PID: ~w HERE !~n",[Pid]),
    Db.

%% --------------------------------------------
%% Send all buffered messages to the X server.
%% Return the new sequence number.

flush_buffer(Dpy,Out,SeqNo) ->
    do_flush_buffer(Dpy,queue:out(Out),SeqNo).

do_flush_buffer(_,{empty,_},SeqNo) -> 
    SeqNo;
do_flush_buffer(Dpy,{{value,Msg},Buf},SeqNo) ->
    NewSeqNo = send_request(Dpy,SeqNo,Msg),
    do_flush_buffer(Dpy,queue:out(Buf),NewSeqNo).

%% ---------------------------------------------------
%% Deal with incoming messages. 
%% NB: Decode may return a continuation if we didn't
%% have enough data to be able to decode the message.

dispatch(Dpy,Db,Out,Cont,SeqNo,Sync,Lock,Data) ->
    case decode(Db,Cont,Data)of
	M when ?NEED_MORE(M) -> 
	    loop(Dpy,Db,Out,M,SeqNo,Sync,Lock);
	{event,Event,Rest} ->
	    deliver(Db,Event),
	    dispatch(Dpy,Db,Out,?NO_CONT,SeqNo,Sync,Lock,Rest);
	{reply,{Pid,Reply},Rest} when pid(Pid) ->
	    send(Pid,{ex11,Reply}),
	    dispatch(Dpy,Db,Out,?NO_CONT,SeqNo,Sync,Lock,Rest);
	{error,E,Rest} when ?ERROR(E) ->
	    ?PRINT_ERROR(E),  
	    dispatch(Dpy,Db,Out,?NO_CONT,SeqNo,Sync,Lock,Rest);
	Else ->
	    io:format("~w:dispatch got ~w~n",[?MODULE,Else]),
	    loop(Dpy,Db,Out,?NO_CONT,SeqNo,Sync,Lock)
    end.

decode(Db,?NO_CONT,Data)           -> 
    ex11_proto:decode(Db,Data);
decode(Db,C,Data) when ?IS_CONT(C) -> 
    ?CONTINUE(C,{Db,Data}).

%% -----------------------------------------
%% Deliver an event to the correct process

deliver(Db,E) when ?IS_EXPOSE_EVENT(E) -> 
    deliver_event(Db,E#expose.window,E);
deliver(Db,E) when ?IS_MAP_NOTIFY_EVENT(E) ->
    deliver_event(Db,E#map_notify.window,E);
deliver(Db,E) when ?IS_REPARENT_NOTIFY_EVENT(E) ->
    deliver_event(Db,E#reparent_notify.window,E);
deliver(Db,E) when ?IS_CONFIGURE_NOTIFY_EVENT(E) ->
    deliver_event(Db,E#configure_notify.window,E);
deliver(Db,E) ->
    info_msg("Unexpected event, couldn't deliver: ~w~n",[E]).

deliver_event(Db,Id,Event) ->
    case ex11_db:get_id(Db,Id) of
	{ok,{Pid,_}} when pid(Pid) -> send(Pid,{ex11,Event});
	_ -> info_msg("Unable to deliver event: ~w~n",[Event])
    end.

%% --------------------------------------------------
%% Setup connection to the X-server.
%% See p.113 in the X Protocol standard X11 Rel-6.4

setup_connection(Host,Cookie) ->
    Opts = [{packet,raw},binary],
    case gen_tcp:connect(Host,6000,Opts) of
	{ok,Fd} -> 
	    Res=gen_tcp:send(Fd,ex11_proto:setup_connection(Cookie)),
	    get_connect_reply(Fd);
	Error -> exit(Error)
    end.

get_connect_reply(Fd) ->
    Rec = recv(Fd),
    case Rec of
	<<0, Emsg/binary>>  -> 
	    {error,ex11_proto:decode_error(Emsg)};
	<<2,_/binary>> -> 
	    {error,"authentication required"};
	<<1,_,Msg/binary>> -> 
	    ex11_proto:decode_connect_reply(Fd,Msg)
    end.

%% ----------------------------------------------------
%% Send a request to the X-server. 

send_request(Dpy,SeqNo,Msg) ->
    gen_tcp:send(Dpy#display.fd, Msg),
    bump_seqno(SeqNo).

%% Also, if the request will initiate a reply from the
%% X-server, then store the sequence number so that we 
%% are able to match the reply when it arrives.

send_reply_request(Dpy,Db,SeqNo,From,Msg,ReplyType) ->
    ex11_db:store_req(Db,SeqNo,From,ReplyType),
    send_request(Dpy,SeqNo,Msg).

bump_seqno(SeqNo) -> (SeqNo + 1) rem ?MAX_SEQNO.

%% --------------
%% Misc routines

recv(Fd) ->
    receive
	{tcp,Fd,Data} -> 
	    Data
    end.


call(X,What) ->
    Pid = X#xlib.pid,
    Pid ! {self(),What},
    ack(Pid).

call(X,What,Data) ->
    Pid = X#xlib.pid,
    Pid ! {self(),What,Data},
    ack(Pid).

ack(Pid) when pid(Pid) -> receive {Pid,Ack} -> Ack end.

return(To,Msg) -> send(To,{self(),Msg}).

send(To,Msg) -> To ! Msg.
