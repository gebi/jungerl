%%% File    : ssh_dist.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : SSH distribution
%%% Created : 29 Aug 2004 by Tony Rogvall <tony@bix.hemma.se>

-module(ssh_dist).

%% Handles the connection setup phase with other Erlang nodes.

-export([listen/1, accept/1, accept_connection/5,
	 setup/5, close/1, select/1, is_node_name/1]).

%% internal exports

-export([accept_loop/2,do_accept/6,do_setup/6, getstat/1,tick/1]).

-export([get_node_connect_opts/0, get_node_connect_opts/1]).

-import(error_logger,[error_msg/2]).

-include_lib("kernel/src/net_address.hrl").


-define(to_port(Socket, Data),
	case inet_tcp:send(Socket, Data) of
	    {error, closed} ->
		self() ! {tcp_closed, Socket},
	        {error, closed};
	    R ->
	        R
        end).

-undef(debug).

-include_lib("kernel/src/dist.hrl").
-include_lib("kernel/src/dist_util.hrl").

-undef(trace).
-define(trace(Fmt, As),
	io:format((Fmt), (As))).


-record(tick, {read = 0,
	       write = 0,
	       tick = 0,
	       ticked = 0
	       }).

%% ------------------------------------------------------------
%%  Select this protocol based on node name
%%  select(Node) => Bool
%%  Node names MUST have the form <name> @ ssh: <hostname>
%% ------------------------------------------------------------

select(Node) ->
    case string:tokens(atom_to_list(Node), "@") of
	[_, "ssh:"++Host] ->
	    case (catch inet:getaddr(Host,inet)) of
		{ok,_} -> true;
		_ -> false
	    end;
	_ -> false
    end.

%% ------------------------------------------------------------
%% Create the listen socket, i.e. the port that this erlang
%% node is accessible through.
%% ------------------------------------------------------------

listen(Name) ->
    case do_listen([{active, false}, {packet,2}]) of
	{ok, Socket} ->
	    TcpAddress = get_tcp_address(Socket),
	    {_,Port} = TcpAddress#net_address.address,
	    {ok, Creation} = erl_epmd:register_node(Name, Port),
	    {ok, {Socket, TcpAddress, Creation}};
	Error ->
	    Error
    end.

do_listen(Options0) ->
    {First,Last} = case application:get_env(kernel,inet_dist_listen_min) of
		       {ok,N} when integer(N) ->
			   case application:get_env(kernel,
						    inet_dist_listen_max) of
			       {ok,M} when integer(M) ->
				   {N,M};
			       _ ->
				   {N,N}
			   end;
		       _ ->
			   {0,0}
		   end,
    Options = case application:get_env(kernel, inet_dist_use_interface) of
		   {ok, Ip} ->
		       [{ip, Ip} | Options0];
		   _ ->
		       Options0
	       end,
    do_listen(First, Last, Options).

do_listen(First,Last,_) when First > Last ->
    {error,eaddrinuse};
do_listen(First,Last,Options) ->
    case inet_tcp:listen(First, Options) of
	{error, eaddrinuse} ->
	    do_listen(First+1,Last,Options);
	Other ->
	    Other
    end.
	    

%% ------------------------------------------------------------
%% Accepts new connection attempts from other Erlang nodes.
%% ------------------------------------------------------------

accept(Listen) ->
    spawn_link(?MODULE, accept_loop, [self(), Listen]).

accept_loop(Kernel, Listen) ->
    process_flag(priority, max),
    case inet_tcp:accept(Listen) of
	{ok, Socket} ->
	    Kernel ! {accept,self(),Socket,inet,tcp},
	    controller(Kernel, Socket),
	    accept_loop(Kernel, Listen);
	Error ->
	    exit(Error)
    end.

controller(Kernel, Socket) ->
    receive
	{Kernel, controller, Pid} ->
	    flush_controller(Pid, Socket),
	    inet_tcp:controlling_process(Socket, Pid),
	    flush_controller(Pid, Socket),
	    Pid ! {self(), controller};
	{Kernel, unsupported_protocol} ->
	    exit(unsupported_protocol)
    end.

flush_controller(Pid, Socket) ->
    receive
	{tcp, Socket, Data} ->
	    Pid ! {tcp, Socket, Data},
	    flush_controller(Pid, Socket);
	{tcp_closed, Socket} ->
	    Pid ! {tcp_closed, Socket},
	    flush_controller(Pid, Socket)
    after 0 ->
	    ok
    end.

%% ------------------------------------------------------------
%% Accepts a new connection attempt from another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    spawn_link(?MODULE, do_accept,
	       [self(), AcceptPid, Socket, MyNode,
		Allowed, SetupTime]).

do_accept(Kernel, AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    process_flag(priority, max),
    receive
	{AcceptPid, controller} ->
	    Timer = dist_util:start_timer(SetupTime),
	    case check_ip(Socket) of
		true ->
		    HSData = #hs_data{
		      kernel_pid = Kernel,
		      this_node = MyNode,
		      socket = Socket,
		      timer = Timer,
		      this_flags = 0,
		      allowed = Allowed,
		      f_send = fun(S,D) -> inet_tcp:send(S,D) end,
		      f_recv = fun(S,N,T) -> inet_tcp:recv(S,N,T) 
			       end,
		      f_setopts_pre_nodeup = 
		      fun(S) ->
			      inet:setopts(S, 
					   [{active, false},
					    {packet, 4},
					    nodelay()])
		      end,
		      f_setopts_post_nodeup = 
		      fun(S) ->
			      inet:setopts(S, 
					   [{active, true},
					    {deliver, port},
					    {packet, 4},
					    nodelay()])
		      end,
		      f_getll = fun(S) ->
					inet:getll(S)
				end,
		      f_address = fun get_remote_id/2,
		      mf_tick = {?MODULE, tick},
		      mf_getstat = {?MODULE,getstat}
		     },
		    dist_util:handshake_other_started(HSData);
		{false,IP} ->
		    error_msg("** Connection attempt from "
			      "disallowed IP ~w ** ~n", [IP]),
		    ?shutdown(no_node)
	    end
    end.


%% we may not always want the nodelay behaviour
%% for performance reasons

nodelay() ->
    case application:get_env(kernel, dist_nodelay) of
	undefined ->
	    {nodelay, true};
	{ok, true} ->
	    {nodelay, true};
	{ok, false} ->
	    {nodelay, false};
	_ ->
	    {nodelay, true}
    end.
	    

%% ------------------------------------------------------------
%% Get remote information about a Socket.
%% ------------------------------------------------------------
get_remote_id(Socket, Node) ->
    case inet:peername(Socket) of
	{ok, Address} ->
	    [_, "ssh:"++Host] = string:tokens(atom_to_list(Node), "@"),
	    #net_address {address = Address,
			  host = Host,
			  protocol = tcp,
			  family = inet };
	{error, _Reason} ->
	    ?shutdown(no_node)
    end.

%% ------------------------------------------------------------
%% Setup a new connection to another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

get_node_connect_opts() ->
    get_node_connect_opts(node()).

get_node_connect_opts(A) when atom(A) ->
    get_node_connect_opts(atom_to_list(A));
get_node_connect_opts(L) ->
    case string:tokens(L, "@") of
	[_,"nohost"] ->
	    [];
	[_,"ssh:"++H] ->
	    case inet:getaddr(H, inet) of
		{ok, Ip} ->
		    [{ifaddr, Ip}];
		_ ->
		    []
	    end;
	_ ->
	    []
    end.

setup(Node, Type, MyNode, LongOrShortNames,SetupTime) ->
    spawn_link(?MODULE, do_setup, [self(),
				   Node,
				   Type,
				   MyNode,
				   LongOrShortNames,
				   SetupTime]).

do_setup(Kernel, Node, Type, MyNode, LongOrShortNames,SetupTime) ->
    process_flag(priority, max),
    ?trace("~p~n",[{ssh_dist,self(),setup,Node}]),
    [Name, "ssh:"++Address] = splitnode(Node, LongOrShortNames),
    case inet:getaddr(Address, inet) of
	{ok, Ip} ->
	    Timer = dist_util:start_timer(SetupTime),
	    case ssh_epmd:port_please(Node) of
		{port, TcpPort, Version} ->
		    ?trace("port_please(~p) -> version ~p~n", 
			   [Node,Version]),
		    dist_util:reset_timer(Timer),
		    case connect(Node, TcpPort, 
				 [{active, false}, 
				  {packet,2}|
				  get_node_connect_opts(MyNode)
				 ]) of
			{ok, Socket} ->
			    HSData = #hs_data{
			      kernel_pid = Kernel,
			      other_node = Node,
			      this_node = MyNode,
			      socket = Socket,
			      timer = Timer,
			      this_flags = 0,
			      other_version = Version,
			      f_send = fun(S,D) -> 
					       inet_tcp:send(S,D) 
				       end,
			      f_recv = fun(S,N,T) -> 
					       inet_tcp:recv(S,N,T) 
				       end,
			      f_setopts_pre_nodeup = 
			      fun(S) ->
				      inet:setopts
					(S, 
					 [{active, false},
					  {packet, 4},
					  nodelay()])
			      end,
			      f_setopts_post_nodeup = 
			      fun(S) ->
				      inet:setopts
					(S, 
					 [{active, true},
					  {deliver, port},
					  {packet, 4},
					  nodelay()])
			      end,
			      f_getll = fun(S) ->
						inet:getll(S)
					end,
			      f_address = 
			      fun(_,_) ->
				      #net_address {
				   address = {Ip,TcpPort},
				   host = Address,
				   protocol = tcp,
				   family = inet}
			      end,
			      mf_tick = {?MODULE, tick},
			      mf_getstat = {?MODULE,getstat},
			      request_type = Type
			     },
			    dist_util:handshake_we_started(HSData);
			_ ->
			    %% Other Node may have closed since 
			    %% port_please !
			    ?trace("other node (~p) "
				   "closed since port_please.~n", 
				   [Node]),
			    ?shutdown(Node)
		    end;
		_ ->
		    ?trace("port_please (~p) "
			   "failed.~n", [Node]),
		    ?shutdown(Node)
	    end;
	_Other ->
	    ?trace("inet_getaddr(~p) "
		   "failed (~p).~n", [Node,_Other]),
	    ?shutdown(Node)
    end.

%%
%% Close a socket.
%%
close(Socket) ->
    inet_tcp:close(Socket).

connect(Node, RemotePort, Opts) ->
    case ssh_cm:dist_start(Node) of
	{ok,CM} ->
	    ?trace("ssh_cm: dist_start OK ~p\n", [Node]),
	    case ssh_tcp:start(CM) of
		{ok, Tcp} ->
		    ?trace("ssh_tcp: started ~p\n", [Tcp]),
		    case ssh_tcp:forward(Tcp, {127,0,0,1}, 0,
					 {127,0,0,1}, RemotePort) of
			{ok,LocalPort} ->
			    ?trace("ssh_tcp: foward OK ~p\n", [LocalPort]),
			    gen_tcp:connect({127,0,0,1}, LocalPort,Opts);
			Error ->
			    ?trace("ssh_tcp: forward ERROR ~p\n", [Error]),
			    Error
		    end;
		Error ->
		    Error
	    end;
	Error ->
	    ?trace("ssh_cm: dist_start ERROR ~p\n", [Error]),
	    Error
    end.
    

%% If Node is illegal terminate the connection setup!!
splitnode(Node, LongOrShortNames) ->
    case string:tokens(atom_to_list(Node), "@") of
	[Name|Tail] when Tail /= [] ->
	    Host = lists:append(Tail),
	    case string:tokens(Host, ".") of
		[_] when LongOrShortNames == longnames ->
		    error_msg("** System running to use "
			      "fully qualified "
			      "hostnames **~n"
			      "** Hostname ~s is illegal **~n",
			      [Host]),
		    ?shutdown(Node);
		L when length(L) > 1, LongOrShortNames == shortnames ->
		    error_msg("** System NOT running to use fully qualified "
			      "hostnames **~n"
			      "** Hostname ~s is illegal **~n",
			      [Host]),
		    ?shutdown(Node);
		_ ->
		    [Name, Host]
	    end;
	[_] ->
	    error_msg("** Nodename ~p illegal, no '@' character **~n",
		      [Node]),
	    ?shutdown(Node);
	_ ->
	    error_msg("** Nodename ~p illegal **~n", [Node]),
	    ?shutdown(Node)
    end.

%% ------------------------------------------------------------
%% Fetch local information about a Socket.
%% ------------------------------------------------------------
get_tcp_address(Socket) ->
    {ok, Address} = inet:sockname(Socket),
    {ok, Host} = inet:gethostname(),
    #net_address {
		  address = Address,
		  host = Host,
		  protocol = tcp,
		  family = inet
		 }.

%% ------------------------------------------------------------
%% Do only accept new connection attempts from nodes at our
%% own LAN, if the check_ip environment parameter is true.
%% ------------------------------------------------------------
check_ip(Socket) ->
    case application:get_env(check_ip) of
	{ok, true} ->
	    case get_ifs(Socket) of
		{ok, IFs, IP} ->
		    check_ip(IFs, IP);
		_ ->
		    ?shutdown(no_node)
	    end;
	_ ->
	    true
    end.

get_ifs(Socket) ->
    case inet:peername(Socket) of
	{ok, {IP, _}} ->
	    case inet:getif(Socket) of
		{ok, IFs} -> {ok, IFs, IP};
		Error     -> Error
	    end;
	Error ->
	    Error
    end.

check_ip([{OwnIP, _, Netmask}|IFs], PeerIP) ->
    case {mask(Netmask, PeerIP), mask(Netmask, OwnIP)} of
	{M, M} -> true;
	_      -> check_ip(IFs, PeerIP)
    end;
check_ip([], PeerIP) ->
    {false, PeerIP}.
    
mask({M1,M2,M3,M4}, {IP1,IP2,IP3,IP4}) ->
    {M1 band IP1,
     M2 band IP2,
     M3 band IP3,
     M4 band IP4}.

is_node_name(Node) when atom(Node) ->
    case string:tokens(atom_to_list(Node), "@") of
	[_, _Host] -> true;
	_ -> false
    end;
is_node_name(_Node) ->
    false.
tick(Sock) ->
    ?to_port(Sock,[]).
getstat(Socket) ->
    case inet:getstat(Socket, [recv_cnt, send_cnt, send_pend]) of
	{ok, Stat} ->
	    split_stat(Stat,0,0,0);
	Error ->
	    Error
    end.

split_stat([{recv_cnt, R}|Stat], _, W, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_cnt, W}|Stat], R, _, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_pend, P}|Stat], R, W, _) ->
    split_stat(Stat, R, W, P);
split_stat([], R, W, P) ->
    {ok, R, W, P}.


