%%% File    : ssh_cm.erl
%%% Author  : Tony Rogvall <tony@bulldog>
%%% Description : SSH connection protocol manager
%%% Created : 23 Aug 2004 by Tony Rogvall <tony@bulldog>

-module(ssh_cm).

-include("../include/ssh.hrl").
-include("../include/ssh_connect.hrl").

-define(DEFAULT_PACKET_SIZE, 32768).
-define(DEFAULT_WINDOW_SIZE, 2*?DEFAULT_PACKET_SIZE).


-import(lists, [reverse/1, foreach/2]).

-compile(export_all).

-export([stop/1]).
-export([start/2, start/3, start/4]).
-export([start_link/2, start_link/3, start_link/4]).
-export([dist_start/1, dist_start/2]).

-define(DBG_SSHMSG, true).
-define(DBG_SSHCM,  true).


-record(channel,
	{
	  type,          %% "session", "x11", "forwarded-tcpip", "direct-tcpip"
	  sys,           %% "none", "shell", "exec" "subsystem"
	  user,          %% "user" process id (default to cm user)
	  user_ack = false,   %% user want ack packet when data is sent

	  local_id,           %% local channel id

	  recv_window_size,
	  recv_packet_size,
	  recv_eof = false,
	  recv_close = false,

	  remote_id,          %% remote channel id
	  send_window_size,
	  send_packet_size,
	  sent_eof = false,
	  sent_close = false,
	  send_buf = []
	 }).

-record(state,
	{
	  channels = []
	 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% API functions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop(CM) ->
    call(CM, stop).


start(Host, Opts) ->
    start(undefined, Host, Opts).
start(Name, Host, Opts) ->
    start(Name, Host, ?SSH_DEFAULT_PORT, Opts).
start(Name, Host, Port, Opts) ->
    Pid = spawn(?MODULE, connect_init,
		[self(),Name,false,Host,Port,Opts]),
    Ref = erlang:monitor(process, Pid),
    receive
	{'DOWN', Ref, _, _, Reason} ->
	    {error, Reason};
	{Pid, Reply} ->
	    erlang:demonitor(Ref),
	    Reply
    end.

start_link(Host, Opts) ->
    start_link(undefined, Host, Opts).
start_link(Name, Host, Opts) ->
    start_link(Name, Host, ?SSH_DEFAULT_PORT, Opts).

start_link(Name, Host, Port, Opts) ->
    Pid = spawn(?MODULE, connect_init,
		[self(),Name,true,Host,Port,Opts]),
    Ref = erlang:monitor(process, Pid),
    receive
	{'DOWN', Ref, _, _, Reason} ->
	    {error, Reason};
	{Pid, Reply} ->
	    erlang:demonitor(Ref),
	    Reply
    end.
%%
%% special ssh distribution version
%%
dist_start(Node) ->
    Opts1 = case init:get_argument('ssh_password') of
	       {ok, [[Passwd]]} -> [{password, Passwd}];
	       error -> []
	   end,
    Opts2 = case init:get_argument('ssh_user') of
		{ok, [[User]]} -> [{user, User}];
		error -> []
	    end,
    dist_start(Node, Opts1++Opts2).
    
dist_start(Node, Opts) when atom(Node), list(Opts) ->
    case string:tokens(atom_to_list(Node), "@") of
	[_, "ssh:"++Host] ->
	    CMHost = list_to_atom(Host),
	    case whereis(CMHost) of
		undefined ->
		    start(CMHost, Host, Opts);
		Pid ->
		    {ok,Pid};
		_ ->
		    {error, einval}
	    end;
	_ ->
	    {error, einval}
    end;
dist_start(_, _) ->
    {error, einval}.



i(CM) ->
    i(CM, all).

i(CM, User) ->
    case info(CM, User) of
	{ok, Cs} ->
	    Cs1 = lists:keysort(#channel.user, Cs),
	    foreach(
	      fun(C) ->
		      io:format("~10p ~w ~s/~s ~w/~w ~w/~w\n",
				[C#channel.user,
				 C#channel.local_id,
				 C#channel.type, C#channel.sys,
				 C#channel.recv_window_size,
				 C#channel.recv_packet_size,
				 C#channel.send_window_size,
				 C#channel.send_packet_size])
	      end, Cs1);
	Error ->
	    Error
    end.

    

info(CM) ->
    info(CM, all).

info(CM, User) ->
    call(CM, {info, User}).


%% CM Client commands
session_open(CM) ->
    session_open(CM, ?DEFAULT_WINDOW_SIZE, ?DEFAULT_PACKET_SIZE).

session_open(CM, InitialWindowSize, MaxPacketSize) ->
    CM ! {ssh_cm, self(), {open, self(), "session", 
			   InitialWindowSize, MaxPacketSize, <<>>}},
    receive
	{ssh_cm, CM, {open, Channel}} ->
	    {ok, Channel};
	{ssh_cm, CM, {open_error, Reason, Descr, Lang}} ->
	    {error, Descr}
    end.

direct_tcpip(CM, RemoteHost, RemotePort, OrigIP, OrigPort) ->
    direct_tcpip(CM, RemoteHost, RemotePort, OrigIP, OrigPort,
		 ?DEFAULT_WINDOW_SIZE, ?DEFAULT_PACKET_SIZE).

direct_tcpip(CM, RemoteIP, RemotePort, OrigIP, OrigPort,
	     InitialWindowSize, MaxPacketSize) ->
    case {encode_ip(RemoteIP), encode_ip(OrigIP)} of
	{false, _} -> {error, einval};
	{_, false} -> {error, einval};
	{RIP, OIP} ->
	    CM ! {ssh_cm, self(), {open, self(), "direct-tcpip", 
				   InitialWindowSize, MaxPacketSize,
				   [?string(RIP), ?uint32(RemotePort),
				    ?string(OIP), ?uint32(OrigPort)] }},
	    receive
		{ssh_cm, CM, {open, Channel}} ->
		    {ok, Channel};
		{ssh_cm, CM, {open_error, Reason, Descr, Lang}} ->
		    {error, Descr}
	    end
    end.

	     
tcpip_forward(CM, BindIP, BindPort) ->
    case encode_ip(BindIP) of
	false -> {error, einval};
	IPStr ->
	    global_request(CM, "tcpip-forward", true,
			   [?string(IPStr),
			    ?uint32(BindPort)])
    end.

cancel_tcpip_forward(CM, BindIP, Port) ->
    case encode_ip(BindIP) of
	false -> {error, einval};
	IPStr ->
	    global_request(CM, "cancel-tcpip-forward", true,
			   [?string(IPStr),
			    ?uint32(Port)])
    end.
    

open_pty(CM, Channel) ->
    open_pty(CM, Channel, os:getenv("TERM"), 80, 24, []).

open_pty(CM, Channel, Term, Width, Height, PtyOpts) ->
    open_pty(CM, Channel, Term, Width, Height, 0, 0, PtyOpts).


open_pty(CM, Channel, Term, Width, Height, PixWidth, PixHeight, PtyOpts) ->
    request(CM, Channel, "pty-req", true, 
	    [?string(Term),
	     ?uint32(Width), ?uint32(Height),
	     ?uint32(0),?uint32(0),
	     encode_pty_opts(PtyOpts)]).

setenv(CM, Channel, Var, Value) ->
    request(CM, Channel, "env", true, [?string(Var), ?string(Value)]).

shell(CM, Channel) ->
    request(CM, Channel, "shell", true, <<>>).

exec(CM, Channel, Command) ->
    request(CM, Channel, "exec", true, [?string(Command)]).

subsystem(CM, Channel, SubSystem) ->
    request(CM, Channel, "subsystem", true, [?string(SubSystem)]).

winch(CM, Channel, Width, Height) ->
    winch(CM, Channel, Width, Height, 0, 0).
winch(CM, Channel, Width, Height, PixWidth, PixHeight) ->
    request(CM, Channel, "window-change", false, 
	    [?uint32(Width), ?uint32(Height),
	     ?uint32(PixWidth), ?uint32(PixHeight)]).

signal(CM, Channel, Sig) ->
    request(CM, Channel, "signal", false,
	    [?string(Sig)]).

attach(CM) ->
    call(CM, {attach, self()}).

detach(CM) ->
    call(CM, {detach, self()}).


renegotiate(CM) ->
    renegotiate(CM,[]).
renegotiate(CM,Opts) ->
    CM ! {ssh_cm, self(), {renegotiate,Opts}}.


%% Setup user ack on data messages (i.e signal when the data has been sent)
set_user_ack(CM, Channel, Ack) ->
    call(CM, {set_user_ack, Channel, Ack}).

set_user(CM, Channel, User) ->
    call(CM, {set_user, Channel, User}).

send_window(CM, Channel) ->
    call(CM, {send_window, Channel}).

recv_window(CM, Channel) ->
    call(CM, {recv_window, Channel}).

adjust_window(CM, Channel, Bytes) ->
    CM ! {ssh_cm, self(), {adjust_window, Channel, Bytes}}.

close(CM, Channel) ->
    CM ! {ssh_cm, self(), {close, Channel}}.

send_eof(CM, Channel) ->
    CM ! {ssh_cm, self(), {eof, Channel}}.


send(CM, Channel, Data) ->
    CM ! {ssh_cm, self(), {data, Channel, 0, Data}}.

send(CM, Channel, Type, Data) ->
    CM ! {ssh_cm, self(), {data, Channel, Type, Data}}.

send_ack(CM, Channel, Data) ->
    send_ack(CM, Channel, 0, Data, infinity).

send_ack(CM, Channel, Type, Data) ->
    send_ack(CM, Channel, Type, Data, infinity).

send_ack(CM, Channel, Type, Data, Timeout) ->
    send(CM, Channel, Type, Data),
    receive
	{ssh_cm, CM, {ack, Channel}} ->
	    ok
    after Timeout ->
	    {error, timeout}
    end.

call(CM, Request) ->
    Ref = make_ref(),
    CM ! {cm_call, self(), Ref, Request},
    receive
	{cm_reply, Ref, Reply} ->
	    Reply
    end.

request(CM, Channel, Type, Reply, Data) ->    
    CM ! {ssh_cm, self(), {request, Channel, Type, Reply, Data}},
    if Reply == true ->
	    receive
		{ssh_cm, CM, {success, Channel}} ->
		    ok;
		{ssh_cm, CM, {failure, Channel}} ->
		    error
	    end;
       true ->
	    ok
    end.

global_request(CM, Type, Reply, Data) ->
    CM ! {ssh_cm, self(), {global_request,self(),Type,Reply,Data}},
    if Reply == true ->
	    receive
		{ssh_cm, CM, {success, Channel}} ->
		    ok;
		{ssh_cm, CM, {failure, Channel}} ->
		    error
	    end;
       true ->
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% CM command encode/decode table
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


connect_messages() ->
    [ {ssh_msg_global_request, ?SSH_MSG_GLOBAL_REQUEST,
       [string, 
	boolean,
	'...']},

      {ssh_msg_request_success, ?SSH_MSG_REQUEST_SUCCESS,
       ['...']},

      {ssh_msg_request_failure, ?SSH_MSG_REQUEST_FAILURE,
       []},
      
      {ssh_msg_channel_open, ?SSH_MSG_CHANNEL_OPEN,
       [string,
	uint32,
	uint32,
	uint32,
	'...']},

      {ssh_msg_channel_open_confirmation, ?SSH_MSG_CHANNEL_OPEN_CONFIRMATION,
       [uint32,
	uint32,
	uint32,
	uint32,
	'...']},

      {ssh_msg_channel_open_failure, ?SSH_MSG_CHANNEL_OPEN_FAILURE,
       [uint32,
	uint32,
	string,
	string]},

      {ssh_msg_channel_window_adjust, ?SSH_MSG_CHANNEL_WINDOW_ADJUST,
       [uint32,
	uint32]},

      {ssh_msg_channel_data, ?SSH_MSG_CHANNEL_DATA,
       [uint32,
	binary]},

      {ssh_msg_channel_extended_data, ?SSH_MSG_CHANNEL_EXTENDED_DATA,
       [uint32,
	uint32,
	binary]},

      {ssh_msg_channel_eof, ?SSH_MSG_CHANNEL_EOF,
       [uint32]},

      {ssh_msg_channel_close, ?SSH_MSG_CHANNEL_CLOSE,
       [uint32]},

      {ssh_msg_channel_request, ?SSH_MSG_CHANNEL_REQUEST,
       [uint32,
	string,
	boolean,
	'...']},

      {ssh_msg_channel_success, ?SSH_MSG_CHANNEL_SUCCESS,
       [uint32]},

      {ssh_msg_channel_failure, ?SSH_MSG_CHANNEL_FAILURE,
       [uint32]}
     ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% CM server functions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% CM server 
connect_init(User, Name, Link, Host, Port, Opts) ->
    case connect_register(Name) of
	ok ->
	    case ssh_proto:connect(Host, Port, Opts) of
		{ok, SSH} ->
		    case user_auth(SSH,Opts) of
			ok ->
			    SSH ! {ssh_install, connect_messages()},
			    process_flag(trap_exit, true),
			    if Link == true ->
				    link(User);
			       true -> ok
			    end,
			    User ! {self(), {ok, self()}},
			    add_user(User),  %% add inital user
			    CTab = ets:new(cm_tab, 
					   [set,{keypos,#channel.local_id}]),
			    cm_loop(SSH, CTab);
			Error ->
			    ssh_proto:disconnect(SSH, ?SSH_DISCONNECT_BY_APPLICATION),
			    User ! {self(), Error}
		    end;
		Error ->
		    User ! {self(), Error}
	    end;
	Error ->
	    User ! {self(), Error}
    end.

%% Register the cm
connect_register(undefined) ->
    ok;
connect_register(Name) when atom(Name) ->
    case catch register(Name, self()) of
	{'EXIT', _} -> 
	    {error, einval};
	true ->
	    ok
    end;
connect_register(_) ->
    {error, einval}.


    


cm_loop(SSH, CTab) ->
    receive
	{ssh_msg, SSH, Msg} ->
	    ?dbg(?DBG_SSHMSG, "cm_loop<~p>: ssh_msg ~p\n", 
		 [SSH, Msg]),
	    ssh_message(SSH, CTab, Msg),
	    ?MODULE:cm_loop(SSH, CTab);

	{ssh_cm, Sender, Msg} ->
	    ?dbg(?DBG_SSHCM, "cm_loop<~p>: sender=~p, ssh_cm ~p\n", 
		 [SSH, Sender, Msg]),
	    %% only allow attached users (+ initial user)
	    case is_user(Sender) of
		false -> 
		    ignore;
		true ->
		    cm_message(SSH, CTab, Msg)
	    end,
	    ?MODULE:cm_loop(SSH, CTab);

	{cm_call, Caller, Ref, stop} ->
	    Caller ! {cm_reply, Ref, ok},
	    foreach(
	      fun(User) ->
		      down_user(SSH, User, CTab)
	      end, get(users)),
	    ssh_proto:disconnect(SSH, 0),
	    exit(normal);
	
	{cm_call, Caller, Ref, Request} ->
	    Reply = cm_call(Request, SSH, CTab),
	    Caller ! {cm_reply, Ref, Reply},
	    ?MODULE:cm_loop(SSH, CTab);

	{'EXIT', SSH, Reason} ->
	    ?dbg(true, "SSH_CM ~p EXIT ~p\n", [SSH, Reason]),
	    ok;

	{'EXIT', Pid, Reason} ->
	    ?dbg(true, "Pid ~p EXIT ~p\n", [Pid, Reason]),
	    cm_loop(SSH, CTab);
	
	{'DOWN', Ref, process, Pid, Reason} ->
	    ?dbg(true, "Pid ~p DOWN ~p\n", [Pid, Reason]),
	    down_user(SSH, Pid, CTab),
	    ?MODULE:cm_loop(SSH, CTab)
    end.


cm_call({attach, User}, SSH, CTab) ->
    case add_user(User) of
	ok ->
	    {ok, self()};
	Error ->
	    Error
    end;
cm_call({detach, User}, SSH, CTab) ->
    del_user(User,SSH);
cm_call({send_window, Channel}, SSH, CTab) ->
    case ets:lookup(CTab, Channel) of
	[C] ->
	    {ok, {C#channel.send_window_size,
		  C#channel.send_packet_size}};
	[] -> 
	    {error, einval}
    end;
cm_call({recv_window, Channel}, SSH, CTab) ->
    case ets:lookup(CTab, Channel) of
	[C] ->
	    {ok, {C#channel.recv_window_size,
		  C#channel.recv_packet_size}};
	[] -> 
	    {error, einval}
    end;
cm_call({set_user, Channel, User}, SSH, CTab) ->
    case is_user(User) of
	false -> {error, einval};
	true ->
	    case ets:lookup(CTab, Channel) of
		[C] ->
		    ets:insert(CTab, C#channel { user = User }),
		    ok;
		[] -> 
		    {error, einval}
	    end
    end;
cm_call({set_user_ack, Channel,Ack}, SSH, CTab) ->
    case ets:lookup(CTab, Channel) of
	[C] ->
	    ets:insert(CTab, C#channel { user_ack = Ack }),
	    ok;
	[] -> 
	    {error, einval}
    end;
cm_call({info,User}, SSH, CTab) ->
    {ok, 
     ets:foldl(
       fun(C, Acc) when User == all; C#channel.user == User ->
	       [C | Acc];
	  (_, Acc) ->
	       Acc
       end, [], CTab)};
    
cm_call(_, _, CTab) ->
    {error, bad_call}.


ssh_message(SSH, CTab, Msg) ->
    case Msg of
	#ssh_msg_channel_open_confirmation { recipient_channel = Channel,
					     sender_channel = RID,
					     initial_window_size = WindowSz,
					     maximum_packet_size = PacketSz
					    } ->
	    with_channel(CTab, Channel,
			 fun(C) ->
				 if C#channel.remote_id == undefined ->
					 send_user(C, {open, Channel}),
					 ets:insert(CTab, C#channel { remote_id = RID,
								      send_window_size = WindowSz,
								      send_packet_size = PacketSz });
				    true ->
					 ignore
				 end
			 end);

	#ssh_msg_channel_open_failure { recipient_channel = Channel,
					reason = Reason,
					description = Descr,
					lang = Lang } ->
	    with_channel(CTab, Channel,
			 fun(C) ->
				 ets:delete(CTab, Channel),
				 send_user(C, {open_error,Reason,Descr,Lang})
			 end);

	#ssh_msg_channel_success { recipient_channel = Channel } ->
	    send_user(CTab, Channel, {success, Channel});

	#ssh_msg_channel_failure { recipient_channel = Channel} ->
	    send_user(CTab, Channel, {failure, Channel});
	    
	#ssh_msg_channel_eof { recipient_channel = Channel} ->
	    with_channel(CTab, Channel,
			 fun(C) ->
				 send_user(C, {eof, Channel}),
				 if C#channel.sent_eof == true ->
					 send_user(C, {closed, Channel}),
					 ets:delete(CTab, Channel);
				    true ->
					 ets:insert(CTab, C#channel { recv_eof = true })
				 end
			 end);

	#ssh_msg_channel_close { recipient_channel = Channel } ->
	    with_channel(CTab, Channel,
			 fun(C) ->
				 if C#channel.sent_close == false ->
					channel_close(SSH,C#channel.remote_id);
				    true -> 
					 ok
				 end,
				 send_user(C, {closed, Channel}),
				 ets:delete(CTab, Channel)
			 end);

	#ssh_msg_channel_data { recipient_channel = Channel,
				data = Data } ->
	    with_channel(CTab, Channel,
			 fun(C) ->
				 WSz = C#channel.recv_window_size - size(Data),
				 send_user(C, {data, Channel, 0, Data}),
				 ets:insert(CTab, C#channel { recv_window_size = WSz})
			 end);

	#ssh_msg_channel_extended_data { recipient_channel = Channel,
					 data_type_code = DataType,
					 data = Data} ->
	    with_channel(CTab, Channel,
			 fun(C) ->
				 WSz = C#channel.recv_window_size - size(Data),
				 send_user(C, {data, Channel, DataType, Data}),
				 ets:insert(CTab, C#channel { recv_window_size = WSz})
			 end);

	#ssh_msg_channel_window_adjust { recipient_channel = Channel,
					 bytes_to_add = Add } ->
	    with_channel(CTab, Channel, 
			 fun(C) -> 
				 update_send_window(SSH, CTab, C, Add) 
			 end);

	#ssh_msg_channel_open { channel_type = Type,
				sender_channel = RID,
				initial_window_size = RWindowSz,
				maximum_packet_size = RPacketSz,
				data = Data } ->
	    case Type of
		"forwarded-tcpip" ->
		    %% FIXME: check that we requested this !
		    %% (install a listener & user somehow)
		    <<?UINT32(ALen), Address:ALen/binary, ?UINT32(Port),
		     ?UINT32(OLen), Orig:OLen/binary, ?UINT32(OrigPort)>> = Data,
		    case get({bind,{Address,Port}}) of
			undefined ->
			    channel_open_failure(SSH, RID, 
						 ?SSH_OPEN_CONNECT_FAILED,
						 "Connection refused", "en");
			User ->
			    Channel = new_channel_id(),
			    LWindowSz = ?DEFAULT_WINDOW_SIZE,
			    LPacketSz = ?DEFAULT_PACKET_SIZE,
			    C = #channel { type = Type,
					   sys = "none",
					   user = User,
					   local_id = Channel,
					   recv_window_size = LWindowSz,
					   recv_packet_size = LPacketSz,
					   send_window_size = RWindowSz,
					   send_packet_size = RPacketSz },
			    ets:insert(CTab, C),
			    channel_open_confirmation(SSH, RID, Channel,
						      LWindowSz, LPacketSz),
			    send_user(C, {open, Channel, {forwarded_tcpip,
							  decode_ip(Address), Port,
							  decode_ip(Orig), OrigPort}})
		    end;
		_ ->
		    channel_open_failure(SSH, RID, 
					 ?SSH_OPEN_ADMINISTRATIVELY_PROHIBITED,
					 "Not allowed", "en")
	    end;

	#ssh_msg_channel_request { recipient_channel = Channel,
				   request_type = Type,
				   want_reply = WantReply,
				   data = Data } ->
	    case Type of
		"exit-status" ->
		    <<?UINT32(Status)>> = Data,
		    send_user(CTab, Channel, {exit_status,Channel,Status});
		"exit-signal" ->
		    <<?UINT32(SigLen), SigName:SigLen/binary,
		     ?BOOLEAN(Core), 
		     ?UINT32(ErrLen), Err:ErrLen/binary,
		     ?UINT32(LangLen), Lang:LangLen/binary>> = Data,
		    send_user(CTab, Channel, {exit_signal, Channel,
					      binary_to_list(SigName),
					      binary_to_list(Err),
					      binary_to_list(Lang)});
		"xon-xoff" ->
		    <<?BOOLEAN(CDo)>> = Data,
		    CanDo = if CDo == 0 -> false;
			       true -> true
			    end,
		    send_user(CTab, Channel, {xon_xoff,Channel,CanDo});
		
		"window-change" ->
		    <<?UINT32(Width),?UINT32(Height),
		     ?UINT32(PixWidth), ?UINT32(PixHeight)>> = Data,
		    send_user(CTab, Channel, {window_change,Channel,
					      Width, Height,
					      PixWidth, PixHeight});
		"signal" ->
		    <<?UINT32(SigLen), SigName:SigLen/binary>> = Data,
		    send_user(CTab, Channel, {signal,Channel,
					      binary_to_list(SigName)});
		_ ->
		    if WantReply == true ->
			    channel_failure(SSH, Channel);
		       true ->
			    ignore
		    end
	    end;
	    
	#ssh_msg_global_request { name = Type,
				  want_reply = WantReply,
				  data = Data } ->
	    if WantReply == true ->
		    request_failure(SSH);
	       true ->
		    ignore
	    end;
	

	#ssh_msg_disconnect { code = Code,
			      description = Description,
			      language = Lang } ->
	    ?dbg(true, "Disconnected: ~s\n", [Description]),
	    %% close all channels
	    ets:foldl(
	      fun(C, _) ->
		      send_user(C, {closed, C#channel.local_id})
	      end, ok, CTab),
	    ets:delete(CTab),
	    exit(disconnected);

	_ ->
	    ?dbg(true, "ssh_connection: ~p\n", [Msg]),
	    ignore
    end.

cm_message(SSH, CTab, Msg) ->
    case Msg of
	{eof, Channel} ->
	    with_channel(CTab, Channel,
			 fun(C) ->
				 channel_eof(SSH,  C#channel.remote_id),
				 ets:insert(CTab, C#channel { sent_eof = true })
			 end);

	{close, Channel} ->
	    with_channel(CTab, Channel,
			 fun(C) ->
				 channel_close(SSH,  C#channel.remote_id),
				 ets:insert(CTab, C#channel { sent_close = true })
			 end);

	{data, Channel, Type, Data} ->
	    send_data(SSH, CTab, Channel, Type, Data);

	{request, Channel, Type, WantReply, Data} ->
	    with_channel(CTab, Channel,
			 fun(C) ->
				 update_sys(CTab, C, Type),
				 channel_request(SSH,C#channel.remote_id,
						 Type,WantReply,Data)
			 end);

	{adjust_window, Channel, Bytes} ->
	    with_channel(CTab, Channel,
			 fun(C) ->
				 WSz = C#channel.recv_window_size + Bytes,
				 channel_adjust_window(SSH, C#channel.remote_id, Bytes),
				 ets:insert(CTab, C#channel { recv_window_size = WSz})
			 end);

	{global_request, User, Type, WantReply, Data} ->
	    case Type of
		"tcpip-forward" ->
		    <<?UINT32(IPLen), IP:IPLen/binary, ?UINT32(Port)>> = Data,
		    add_user(User),  %% auto attach user
		    put({bind, {IP,Port}}, User),
		    send_global_request(SSH,Type,WantReply,Data);
		"cancel-tcpip-forward" ->
		    <<?UINT32(IPLen), IP:IPLen/binary, ?UINT32(Port)>> = Data,
		    %% note can not erase user!
		    erase({bind, {IP,Port}}),
		    send_global_request(SSH,Type,WantReply,Data);
		_ ->
		    send_global_request(SSH,Type,WantReply,Data)
	    end;


	{open, User, Type, InitialWindowSize, MaxPacketSize, Data} ->
	    add_user(User), %% auto attach user 
	    Channel = new_channel_id(),
	    channel_open(SSH, Type, Channel, InitialWindowSize, MaxPacketSize, Data),
	    C = #channel { type = Type,
			   sys = "none",
			   user = User,
			   local_id = Channel,
			   recv_window_size = InitialWindowSize,
			   recv_packet_size = MaxPacketSize },
	    ets:insert(CTab, C);

	{renegotiate,Opts} ->
	    SSH ! {ssh_renegotiate, false, Opts};

	_ ->
	    ?dbg(true, "ssh_connection: ~p\n", [Msg]),
	    ignore
    end.


update_sys(CTab, C, Type) ->
    case Type of
	"subsystem" ->
	    ets:insert(CTab, C#channel { sys = "subsystem" });
	"exec" -> 
	    ets:insert(CTab, C#channel { sys = "subsystem" });
	"shell" -> 
	    ets:insert(CTab, C#channel { sys = "shell" });
	_ ->
	    ok
    end.


user_auth(SSH, Opts) ->
    case ssh_proto:service_request(SSH, "ssh-userauth") of
	ok ->
	    ssh_userauth:auth(SSH, "ssh-connection", Opts);
	Error ->
	    Error
    end.


%% Allocate channel ID 
new_channel_id() ->
    ID = case get(channel_id) of
	     undefined -> 0;
	     I -> I
	 end,
    put(channel_id, ID+1),
    ID.

%% User is down
down_user(SSH, User, CTab) ->
    case is_user(User) of
	true ->
	    ets:foldl(
	      fun(C, _) when C#channel.user == User ->
		      channel_close(SSH,  C#channel.remote_id),
		      ets:delete(CTab, C#channel.local_id);
		 (C, _) ->
		      ok
	      end, ok, CTab),
	    del_user(User,SSH);
	false ->
	    ok
    end.
    

%% Add a user
add_user(User) ->
    case get({ref, User}) of
	undefined ->
	    Ref = erlang:monitor(process, User),
	    put({attached, Ref}, User), %% Ref -> User
	    put({ref, User}, Ref),      %% User -> Ref
	    UserList = case get(users) of
			   undefined -> [];
			   UL -> UL
		       end,
	    put(users, [User | UserList]),
	    ok;
	Ref ->
	    ok
    end.

del_user(User,SSH) ->
    ?dbg(true, "del user: ~p\n",[User]),
    case get({ref, User}) of
	undefined ->
	    {error, einval};
	Ref ->
	    erlang:demonitor(Ref),
	    erase({attached,Ref}),
	    erase({ref,User}),
	    UserList = get(users) -- [User],
	    ?dbg(true, "del user list: ~p\n",[UserList]),
	    del_bind(get_keys(User)),
	    put(users, UserList),
	    %% exit if no more users and we are unregistered
	    if UserList == [] ->
		    case process_info(self(), registered_name) of
			[] ->
			    ssh_proto:disconnect(SSH, 0),
			    ok;
			{registered_name,Name} ->
			    ok
		    end;
	       true ->
		    ok
	    end
    end.

del_bind(undefined) ->
    ok;
del_bind([{bind,{IP,Port}} | Ks]) ->
    erase({bind,{IP,Port}}),
    del_bind(Ks);
del_bind([_ | Ks]) -> del_bind(Ks);
del_bind([]) -> ok.


is_user(User) ->
    case get({ref, User}) of
	undefined -> false;
	Ref -> true
    end.
	    

%% Send ssh_cm messages to the 'user'
send_user(C, Msg) when record(C, channel) ->
    C#channel.user ! {ssh_cm, self(), Msg}.

send_user(CTab, Channel, Msg) ->
    case ets:lookup(CTab, Channel) of
	[C] ->
	    send_user(C, Msg);
	[] ->
	    ignore
    end.

with_channel(CTab, Channel, Fun) ->
    case ets:lookup(CTab, Channel) of
	[C] ->
	    Fun(C);
	[] ->
	    ignore
    end.

    

    

%% Update the send window with Data
%% adjust output window
%%
%% buffer is on form [{DataType,UserAck,User,Data}]
%% DataType = 0   regular data
%%            1   stderr data
%% UserAck = true if "User" wants ack when data was sent
%% Data = io-list
%%
send_data(SSH, CTab, LID, DataType, Data0) ->
    case ets:lookup(CTab, LID) of
	[C] ->
	    Data = if binary(Data0) -> Data0;
		      list(Data0) -> list_to_binary(Data0)
		   end,
	    send_window(SSH,CTab,C, DataType,
			C#channel.user_ack, C#channel.user,
			Data);
	[] ->
	    ignore
    end.

update_send_window(SSH, CTab, C, Bytes) ->
    WSz0 = C#channel.send_window_size,
    send_window(SSH, CTab, C#channel { send_window_size = WSz0+Bytes},
		0, false, undefined, <<>>).


send_window(SSH, CTab, C, DataType, UserAck, User, Data) ->
    foreach(
      fun({Type,Ack,Usr,Data1}) ->
	      channel_data(SSH, C#channel.remote_id, Type, Data1),
	      if Ack == true ->
		      Usr ! {ssh_cm, self(), {ack, C#channel.local_id}};
		 true ->
		      ok
	      end
      end, remove_from_send_window(CTab, C, DataType, UserAck, User, Data)).


%% Get data from the send buffer 
%% each buffer sent must be less than packet size
remove_from_send_window(CTab, C, DataType, UserAck, User, Data) ->
    Buf0 = if Data == <<>> ->
		   C#channel.send_buf;
	      true ->
		   C#channel.send_buf ++ [{DataType,UserAck,User,Data}]
	   end,
    {Buf1,NewSz,Buf2} = get_window(Buf0, 
				   C#channel.send_packet_size,
				   C#channel.send_window_size),
    ets:insert(CTab, C#channel { send_window_size = NewSz,
				 send_buf = Buf2}),
    Buf1.

get_window(Bs, PSz, WSz) ->
    get_window(Bs, PSz, WSz, []).

get_window(Bs, PSz, 0, Acc) ->
    {reverse(Acc), 0, Bs};
get_window([B0 = {DataType,UserAck,User,Bin} | Bs], PSz, WSz, Acc) ->
    BSz = size(Bin),
    if BSz =< WSz ->  %% will fit into window
	    if BSz =< PSz ->  %% will fit into a packet
		    get_window(Bs, PSz, WSz-BSz, [B0|Acc]);
	       true -> %% split into packet size
		    <<Bin1:PSz/binary, Bin2/binary>> = Bin,
		    get_window([setelement(4, B0, Bin2) | Bs],
			       PSz, WSz-PSz, 
			       [{DataType,false,undefined,Bin1}|Acc])
	    end;
       WSz =< PSz ->  %% use rest of window
	    <<Bin1:WSz/binary, Bin2/binary>> = Bin,
	    get_window([setelement(4, B0, Bin2) | Bs],
		       PSz, WSz-WSz, 
		       [{DataType,false,undefined,Bin1}|Acc]);
       true -> %% use packet size
	    <<Bin1:PSz/binary, Bin2/binary>> = Bin,
	    get_window([setelement(4, B0, Bin2) | Bs],
		       PSz, WSz-PSz, 
		       [{DataType,false,undefined,Bin1}|Acc])
    end;
get_window([], PSz, WSz, Acc) ->
    {reverse(Acc), WSz, []}.


%%
%% CHANNEL Commands
%%
channel_eof(SSH, Channel) ->
    SSH ! {ssh_msg, self(), 
	   #ssh_msg_channel_eof { recipient_channel = Channel }}.

channel_close(SSH, Channel) ->
    SSH ! {ssh_msg, self(), 
	   #ssh_msg_channel_close { recipient_channel = Channel }}.

channel_success(SSH, Channel) ->
    SSH ! {ssh_msg, self(),
	   #ssh_msg_channel_success { recipient_channel = Channel }}.

channel_failure(SSH, Channel) ->
    SSH ! {ssh_msg, self(),
	   #ssh_msg_channel_failure { recipient_channel = Channel }}.


channel_adjust_window(SSH, Channel, Bytes) ->
    SSH ! {ssh_msg, self(), 
	   #ssh_msg_channel_window_adjust { recipient_channel = Channel,
					    bytes_to_add = Bytes }}.


channel_data(SSH, Channel, 0, Data) ->
    SSH ! {ssh_msg, self(),
	   #ssh_msg_channel_data { recipient_channel = Channel,
				   data = Data }};
channel_data(SSH, Channel, Type, Data) ->
    SSH ! {ssh_msg, self(),
	   #ssh_msg_channel_extended_data { recipient_channel = Channel,
					    data_type_code = Type,
					    data = Data }}.

channel_open(SSH, Type, Channel, WindowSize, MaxPacketSize, Data) ->
    SSH ! {ssh_msg, self(), 
	   #ssh_msg_channel_open { channel_type = Type,
				   sender_channel = Channel,
				   initial_window_size = WindowSize,
				   maximum_packet_size = MaxPacketSize,
				   data = Data
				  }}.

channel_open_confirmation(SSH, RID, LID, WindowSize, PacketSize) ->
    SSH ! {ssh_msg, self(),
	   #ssh_msg_channel_open_confirmation { recipient_channel = RID,
						sender_channel = LID,
						initial_window_size = WindowSize,
						maximum_packet_size = PacketSize}}.

channel_open_failure(SSH, RID, Reason, Description, Lang) ->
    SSH ! {ssh_msg, self(),
	   #ssh_msg_channel_open_failure { recipient_channel = RID,
					   reason = Reason,
					   description = Description,
					   lang = Lang }}.
					   
    

channel_request(SSH, Channel, Type, WantReply, Data) ->
    SSH ! {ssh_msg, self(),
	   #ssh_msg_channel_request { recipient_channel = Channel,
				      request_type = Type,
				      want_reply = WantReply,
				      data = Data }}.

send_global_request(SSH, Type, WantReply, Data) ->
    SSH ! {ssh_msg, self(),
	   #ssh_msg_global_request { name = Type,
				     want_reply = WantReply,
				     data = Data }}.

request_failure(SSH) ->
    SSH ! {ssh_msg, self(), #ssh_msg_request_failure {}}.

request_success(SSH,Data) ->
    SSH ! {ssh_msg, self(), #ssh_msg_request_success { data=Data }}.


decode_pty_opts(<<?TTY_OP_END>>) ->		     
    [];
decode_pty_opts(<<Code, ?UINT32(Value), Tail/binary>>) ->
    Op = case Code of
	     ?VINTR -> vintr;
	     ?VQUIT -> vquit;
	     ?VERASE -> verase;
	     ?VKILL -> vkill;
	     ?VEOF -> veof;
	     ?VEOL -> veol;
	     ?VEOL2 -> veol2;
	     ?VSTART -> vstart;
	     ?VSTOP -> vstop;
	     ?VSUSP -> vsusp;
	     ?VDSUSP -> vdsusp;
	     ?VREPRINT -> vreprint;
	     ?VWERASE -> vwerase;
	     ?VLNEXT -> vlnext;
	     ?VFLUSH -> vflush;
	     ?VSWTCH -> vswtch;
	     ?VSTATUS -> vstatus;
	     ?VDISCARD -> vdiscard;
	     ?IGNPAR -> ignpar;
	     ?PARMRK -> parmrk;
	     ?INPCK -> inpck;
	     ?ISTRIP -> istrip;
	     ?INLCR -> inlcr;
	     ?IGNCR -> igncr;
	     ?ICRNL -> icrnl;
	     ?IUCLC -> iuclc;
	     ?IXON -> ixon;
	     ?IXANY -> ixany;
	     ?IXOFF -> ixoff;
	     ?IMAXBEL -> imaxbel;
	     ?ISIG -> isig;
	     ?ICANON -> icanon;
	     ?XCASE -> xcase;
	     ?ECHO -> echo;
	     ?ECHOE -> echoe;
	     ?ECHOK -> echok;
	     ?ECHONL -> echonl;
	     ?NOFLSH -> noflsh;
	     ?TOSTOP -> tostop;
	     ?IEXTEN -> iexten;
	     ?ECHOCTL -> echoctl;
	     ?ECHOKE -> echoke;
	     ?PENDIN -> pendin;
	     ?OPOST -> opost;
	     ?OLCUC -> olcuc;
	     ?ONLCR -> onlcr;
	     ?OCRNL -> ocrnl;
	     ?ONOCR -> onocr;
	     ?ONLRET -> onlret;
	     ?CS7 -> cs7;
	     ?CS8 -> cs8;
	     ?PARENB -> parenb;
	     ?PARODD -> parodd;
	     ?TTY_OP_ISPEED -> tty_op_ispeed;
	     ?TTY_OP_OSPEED -> tty_op_ospeed
	 end,    
    [{Op, Value} | decode_pty_opts(Tail)].



encode_pty_opts([{Opt,Value} | Opts]) ->
    Code = case Opt of
	       vintr -> ?VINTR;
	       vquit -> ?VQUIT;
	       verase -> ?VERASE;
	       vkill -> ?VKILL;
	       veof -> ?VEOF;
	       veol -> ?VEOL;
	       veol2 -> ?VEOL2;
	       vstart -> ?VSTART;
	       vstop -> ?VSTOP;
	       vsusp -> ?VSUSP;
	       vdsusp -> ?VDSUSP;
	       vreprint -> ?VREPRINT;
	       vwerase -> ?VWERASE;
	       vlnext -> ?VLNEXT;
	       vflush -> ?VFLUSH;
	       vswtch -> ?VSWTCH;
	       vstatus -> ?VSTATUS;
	       vdiscard -> ?VDISCARD;
	       ignpar -> ?IGNPAR;
	       parmrk -> ?PARMRK;
	       inpck -> ?INPCK;
	       istrip -> ?ISTRIP;
	       inlcr -> ?INLCR;
	       igncr -> ?IGNCR;
	       icrnl -> ?ICRNL;
	       iuclc -> ?IUCLC;
	       ixon -> ?IXON;
	       ixany -> ?IXANY;
	       ixoff -> ?IXOFF;
	       imaxbel -> ?IMAXBEL;
	       isig -> ?ISIG;
	       icanon -> ?ICANON;
	       xcase -> ?XCASE;
	       echo -> ?ECHO;
	       echoe -> ?ECHOE;
	       echok -> ?ECHOK;
	       echonl -> ?ECHONL;
	       noflsh -> ?NOFLSH;
	       tostop -> ?TOSTOP;
	       iexten -> ?IEXTEN;
	       echoctl -> ?ECHOCTL;
	       echoke -> ?ECHOKE;
	       pendin -> ?PENDIN;
	       opost -> ?OPOST;
	       olcuc -> ?OLCUC;
	       onlcr -> ?ONLCR;
	       ocrnl -> ?OCRNL;
	       onocr -> ?ONOCR;
	       onlret -> ?ONLRET;
	       cs7 -> ?CS7;
	       cs8 -> ?CS8;
	       parenb -> ?PARENB;
	       parodd -> ?PARODD;
	       tty_op_ispeed -> ?TTY_OP_ISPEED;
	       tty_op_ospeed -> ?TTY_OP_OSPEED
	   end,
    [Code, ?uint32(Value) | encode_pty_opts(Opts)];
encode_pty_opts([]) -> 
    [?TTY_OP_END].


decode_ip(Addr) when binary(Addr) ->
    decode_ip(binary_to_list(Addr));
decode_ip(Addr) when tuple(Addr) ->
    Addr;
decode_ip(Addr) when list(Addr) ->
    case inet_parse:address(Addr) of
	{error,_} -> Addr;
	{ok,A}    -> A
    end.

%% return string() | false
encode_ip(Addr) when tuple(Addr) ->
    case catch inet_parse:ntoa(Addr) of
	{'EXIT',_} -> false;
	A -> A
    end;
encode_ip(Addr) when list(Addr) ->
    case inet_parse:address(Addr) of
	{ok, _} -> Addr;
	Error ->
	    case inet:getaddr(Addr, inet) of
		{ok, A} ->
		    inet_parse:ntoa(A);
		Error -> false
	    end
    end.

