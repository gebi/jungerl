%%% File    : ssh_proto.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : SSH Protocol for erlang
%%% Created : 16 Aug 2004 by Tony Rogvall <tony@bix.hemma.se>

-module(ssh_proto).

-compile(export_all).

-import(lists, [reverse/1, map/2, foreach/2, foldl/3]).

-include("../include/ssh.hrl").

-export([connect/1, connect/2]).
-export([listen/1, listen/2]).
-export([debug/2, debug/3, debug/4]).
-export([ignore/2]).
-export([disconnect/2, disconnect/3, disconnect/4]).

-export([client_init/4, server_init/3]).

%% io wrappers
-export([yes_no/2, read_password/2, read_line/2]).

-define(DBG_ALG,     false).
-define(DBG_KEX,     true).
-define(DBG_CRYPTO,  false).
-define(DBG_PACKET,  false).
-define(DBG_MESSAGE, true).
-define(DBG_MAC,     false).

-record(alg,
	{
	  kex,
	  hkey,
	  send_mac,
	  recv_mac,
	  encrypt,
	  decrypt,
	  compress,
	  decompress,
	  c_lng,
	  s_lng
	 }).
	  
-record(ssh,
	{
	  role,         %% client | server
	  peer,         %% string version of peer address 

	  c_vsn,        %% client version {Major,Minor}
	  s_vsn,        %% server version {Major,Minor}

	  c_version,    %% client version string
	  s_version,    %% server version string

	  c_keyinit,    %% binary payload of kexinit packet
	  s_keyinit,    %% binary payload of kexinit packet

	  algorithms,   %% new algorithms (SSH_MSG_KEXINIT)
	  
	  kex,          %% key exchange algorithm
	  hkey,         %% host key algorithm
	  key_cb,       %% Private/Public key callback module
	  io_cb,        %% Interaction callback module

	  send_mac=none, %% send MAC algorithm
	  send_mac_key,  %% key used in send MAC algorithm
	  send_mac_size = 0,

	  recv_mac=none, %% recv MAC algorithm
	  recv_mac_key,  %% key used in recv MAC algorithm
	  recv_mac_size = 0,

	  encrypt = none,       %% encrypt algorithm
	  encrypt_keys,         %% encrypt keys
	  encrypt_block_size = 8,

	  decrypt = none,       %% decrypt algorithm
	  decrypt_keys,         %% decrypt keys
	  decrypt_block_size = 8,

	  compress = none,
	  decompress = none,

	  c_lng=none,   %% client to server languages
	  s_lng=none,   %% server to client languages

	  user_ack    = true,   %% client
	  timeout     = infinity,

	  shared_secret,        %% K from key exchange
	  exchanged_hash,       %% H from key exchange
	  session_id,           %% same as FIRST exchanged_hash
	  
	  opts = []
	 }).


transport_messages() ->
    [ {ssh_msg_disconnect, ?SSH_MSG_DISCONNECT, 
       [uint32,string,string]},
      
      {ssh_msg_ignore, ?SSH_MSG_IGNORE,
       [string]},

      {ssh_msg_unimplemented, ?SSH_MSG_UNIMPLEMENTED,
       [uint32]},

      {ssh_msg_debug, ?SSH_MSG_DEBUG,
       [boolean, string, string]},

      {ssh_msg_service_request, ?SSH_MSG_SERVICE_REQUEST,
       [string]},

      {ssh_msg_service_accept, ?SSH_MSG_SERVICE_ACCEPT,
       [string]},

      {ssh_msg_kexinit, ?SSH_MSG_KEXINIT,
       [cookie,
	name_list, name_list, 
	name_list, name_list, 
	name_list, name_list,
	name_list, name_list,
	name_list, name_list,
	boolean, 
	uint32]},

      {ssh_msg_newkeys, ?SSH_MSG_NEWKEYS,
       []}
     ].


kexdh_messages() ->
    [ {ssh_msg_kexdh_init, ?SSH_MSG_KEXDH_INIT,
       [mpint]},
      
      {ssh_msg_kexdh_reply, ?SSH_MSG_KEXDH_REPLY,
       [binary, mpint, binary]}
     ].


kex_dh_gex_messages() ->
    [ {ssh_msg_kex_dh_gex_request, ?SSH_MSG_KEX_DH_GEX_REQUEST,
       [uint32, uint32, uint32]},

      {ssh_msg_kex_dh_gex_request_old, ?SSH_MSG_KEX_DH_GEX_REQUEST_OLD,
       [uint32]},
      
      {ssh_msg_kex_dh_gex_group, ?SSH_MSG_KEX_DH_GEX_GROUP,
       [mpint, mpint]},

      {ssh_msg_kex_dh_gex_init, ?SSH_MSG_KEX_DH_GEX_INIT,
       [mpint]},
      
      {ssh_msg_kex_dh_gex_reply, ?SSH_MSG_KEX_DH_GEX_REPLY,
       [binary, mping, binary]}
     ].

yes_no(SSH, Prompt) when pid(SSH) ->
    {ok, CB} = call(SSH, {get_cb, io}),
    CB:yes_no(Prompt);
yes_no(SSH, Prompt) when record(SSH,ssh) ->
    (SSH#ssh.io_cb):yes_no(Prompt).

read_password(SSH, Prompt) when pid(SSH) ->
    {ok, CB} = call(SSH, {get_cb, io}),
    CB:read_password(Prompt);
read_password(SSH, Prompt) when record(SSH,ssh) ->
    (SSH#ssh.io_cb):read_password(Prompt).

read_line(SSH, Prompt) when pid(SSH) ->
    {ok, CB} = call(SSH, {get_cb, io}),
    CB:read_line(Prompt);
read_line(SSH, Prompt) when record(SSH,ssh) ->
    (SSH#ssh.io_cb):read_line(Prompt).

call(SSH, Req) ->
    Ref = make_ref(),
    SSH ! {ssh_call, [self()|Ref], Req},
    receive
	{Ref, Reply} ->
	    Reply
    end.

connect(Host) ->
    connect(Host, []).

connect(Host, Opts) ->
    connect(Host, 22, Opts).

connect(Host,Port,Opts) ->
    Pid = spawn_link(?MODULE, client_init, [self(), Host, Port, Opts]),
    receive
	{Pid, Reply} ->
	    Reply
    end.

listen(Port) ->
    listen(Port, []).

listen(Port, Opts) ->
    listen(any, Port, Opts).

listen(Addr, Port, Opts) ->
    spawn_link(?MODULE, server_init, [Addr, Port, Opts]).

debug(SSH, Message) ->
    debug(SSH, true, Message, "en").

debug(SSH, Message, Lang) ->
    debug(SSH, true, Message, Lang).

debug(SSH, Display, Message, Lang) ->
    SSH ! {ssh_msg, self(), #ssh_msg_debug { always_display = Display,
					     message = Message,
					     language = Lang }}.

ignore(SSH, Data) ->
    SSH ! {ssh_msg, self(), #ssh_msg_ignore { data = Data }}.

disconnect(SSH, Code) ->
    disconnect(SSH, Code, "", "").

disconnect(SSH, Code, Msg) ->
    disconnect(SSH, Code, "", "").

disconnect(SSH, Code, Msg, Lang) ->
    SSH ! {ssh_msg, self(), #ssh_msg_disconnect { code = Code,
						  description = Msg,
						  language = Lang }}.


service_request(SSH, Name) ->
    SSH ! {ssh_msg, self(), #ssh_msg_service_request { name = Name}},
    receive
	{ssh_msg, SSH, R} when record(R, ssh_msg_service_accept) ->
	    ok;
	{ssh_msg, SSH, R} when record(R, ssh_msg_disconnect) ->
	    {error, R};
	Other ->
	    {error, Other}
    end.    
    
    
client_init(User, Host, Port, Opts) ->
    IfAddr = getopt(ifaddr, Opts, any),
    Tmo    = getopt(connect_tmo, Opts, infinity),
    case gen_tcp:connect(Host, Port, [{packet,line},
				      {active,once},
				      {ifaddr,IfAddr}], Tmo) of
	{ok, S} ->
	    SSH = ssh_init(S, client, Opts),
	    Peer = if tuple(Host) -> inet_parse:ntoa(Host);
		      atom(Host) -> atom_to_list(Host);
		      list(Host) -> Host
		   end,
	    client_hello(S, User, SSH#ssh { peer = Peer });
	Error ->
	    User ! {self(), Error}
    end.


server_init(Addr, Port, Opts) ->
    Serv = fun(S) ->
		   User = spawn_link(?MODULE,server_sys_init, [self()]),
		   SSH = ssh_init(S, server, Opts),
		   server_hello(S, User, SSH)
	   end,
    ssh_tcp_wrap:server(Port, [{packet,line},{active,once},
			       {ifaddr,Addr},{reuseaddr,true}],
			Serv).


%%
%% Subsystem dispatcher!!!
%%
server_sys_init(SSH) ->
    receive
	{ok, SSH} -> %% wait for connection to be ready
	    server_sys(SSH)
    end.

server_sys(SSH) ->
    receive
	{ssh_msg, SSH, Msg} ->
	    io:format("SSH: server_sys: got message ~p\n", [Msg]),
	    server_sys(SSH);
	Other ->
	    io:format("SSH: server_sys: got message ~p\n", [Other]),
	    server_sys(SSH)
    end.

%%
%% Initialize basic ssh system
%%
ssh_init(S, Role, Opts) ->
    ssh_bits:install_messages(transport_messages()),
    crypto:start(),
    {A,B,C} = erlang:now(),
    random:seed(A, B, C),
    put(send_sequence, 0),
    put(recv_sequence, 0),
    case Role of
	client ->
	    Vsn = getopt(vsn, Opts, {2,0}),
	    Version = format_version(Vsn),
	    send_version(S, Version),
	    #ssh { role = Role,
		   c_vsn = Vsn,
		   c_version=Version,
		   key_cb = getopt(key_cb, Opts, ssh_file),
		   io_cb = getopt(io_cb, Opts, ssh_io),
		   opts = Opts };
	server  ->
	    Vsn = getopt(vsn, Opts, {1,99}),
	    Version = format_version(Vsn),
	    send_version(S, Version),
	    #ssh { role = Role,
		   s_vsn = Vsn,
		   s_version=Version,
		   key_cb = getopt(key_cb, Opts, ssh_file),
		   io_cb = getopt(io_cb, Opts, ssh_io),
		   opts = Opts  }
    end.

ssh_getopt(Opt, SSH, Default) ->
    getopt(Opt, SSH#ssh.opts, Default).

getopt(Opt, Opts, Default) ->
    case lists:keysearch(Opt, 1, Opts) of
	false -> Default;
	{value,{_,Val}} -> Val
    end.

ssh_setopts(NewOpts, SSH) ->
    Opts = setopts(NewOpts, SSH#ssh.opts),
    SSH#ssh { opts = Opts }.


setopts(NewOpts, Opts) ->
    foldl(fun(Opt, Acc) -> setopt(Opt, Acc) end, Opts, NewOpts).

setopt({Opt,Value}, Opts) ->
    case lists:keysearch(Opt, 1, Opts) of
	false -> 
	    [{Opt,Value} | Opts];
	{value,_} ->
	    lists:keyreplace(Opt, 1, Opts, {Opt,Value})
    end.
	    
    

format_version({Major,Minor}) ->
    "SSH-"++integer_to_list(Major)++"."++integer_to_list(Minor)++"-Erlang".
    

%% choose algorithms
kex_init(SSH) ->
    Random = ssh_bits:random(16),
    case SSH#ssh.role of
	client ->
	    #ssh_msg_kexinit { 
	  cookie = Random,
	  kex_algorithms = ["diffie-hellman-group1-sha1"],
	  server_host_key_algorithms = ["ssh-rsa", "ssh-dss"],
	  encryption_algorithms_client_to_server = ["3des-cbc"],
	  encryption_algorithms_server_to_client = ["3des-cbc"],
	  mac_algorithms_client_to_server = ["hmac-sha1"],
	  mac_algorithms_server_to_client = ["hmac-sha1"],
	  compression_algorithms_client_to_server = ["none","zlib"],  
	  compression_algorithms_server_to_client = ["none","zlib"],
	  languages_client_to_server = [],
	  languages_server_to_client = []
	 };
	server ->
	    #ssh_msg_kexinit {
	  cookie = Random,
	  kex_algorithms = ["diffie-hellman-group1-sha1"],
	  server_host_key_algorithms = ["ssh-dss"],
	  encryption_algorithms_client_to_server = ["3des-cbc"],
	  encryption_algorithms_server_to_client = ["3des-cbc"],
	  mac_algorithms_client_to_server = ["hmac-sha1"],
	  mac_algorithms_server_to_client = ["hmac-sha1"],
	  compression_algorithms_client_to_server = ["none","zlib"],  
	  compression_algorithms_server_to_client = ["none","zlib"],
	  languages_client_to_server = [],
	  languages_server_to_client = []
	 }
    end.


server_hello(S, User, SSH) ->
    receive
	{tcp, S, V = "SSH-"++_} ->
	    Version = trim_tail(V),
	    ?dbg(true, "client version: ~p\n",[Version]),
	    case string:tokens(Version, "-") of
		[_, "2.0" | _] ->
		    negotiate(S, User, SSH#ssh { c_vsn = {2,0},
						 c_version = Version});
		[_, "1.3" | _] ->
		    negotiate(S, User, SSH#ssh { c_vsn = {1,3}, 
						 c_version = Version});
		[_, "1.5" | _] ->
		    negotiate(S, User, SSH#ssh { c_vsn = {1,5}, 
						 c_version = Version});
		_ ->
		    exit(unknown_version)
	    end;
	{tcp, S, Line} ->
	    ?dbg(true, "info: ~p\n", [Line]),
	    inet:setopts(S, [{active, once}]),
	    server_hello(S, User, SSH)
    after 5000 ->
	    ?dbg(true, "timeout 5s\n", []),
	    gen_tcp:close(S),
	    {error, timeout}
    end.

client_hello(S, User, SSH) ->
    receive
	{tcp, S, V = "SSH-"++_} ->
	    Version = trim_tail(V),
	    ?dbg(true, "server version: ~p\n",[Version]),
	    case string:tokens(Version, "-") of
		[_, "2.0" | _] ->
		    negotiate(S, User, SSH#ssh { s_vsn = {2,0},
						 s_version = Version });
		[_, "1.99" | _] -> %% compatible server
		    negotiate(S, User, SSH#ssh { s_vsn = {2,0}, 
						 s_version = Version });
		[_, "1.3" | _] ->
		    negotiate(S, User, SSH#ssh { s_vsn = {1,3}, 
						 s_version = Version });
		[_, "1.5" | _] ->
		    negotiate(S, User, SSH#ssh { s_vsn = {1,5}, 
						 s_version = Version });
		_ ->
		    exit(unknown_version)
	    end;
	{tcp, S, Line} ->
	    ?dbg(true, "info: ~p\n", [Line]),
	    inet:setopts(S, [{active, once}]),
	    client_hello(S, User, SSH)
    after 5000 ->
	    ?dbg(true, "timeout 5s\n", []),
	    gen_tcp:close(S),
	    {error, timeout}
    end.

%% Determine the version and algorithms
negotiate(S, User, SSH) ->
    inet:setopts(S, [{packet,0},{mode,binary}]),
    send_negotiate(S, User, SSH, true).

%% We start re-negotiate
send_negotiate(S, User, SSH, UserAck) ->
    SendAlg = kex_init(SSH),
    {ok, SendPdu} = send_algorithms(S, SSH, SendAlg),
    {ok, {RecvPdu,RecvAlg}} = recv_algorithms(S, SSH),
    kex_negotiate(S, User, SSH, UserAck, SendAlg, SendPdu, RecvAlg, RecvPdu).


%% Other side started re-negotiate
recv_negotiate(S, User, SSH, RecvAlg, UserAck) ->
    RecvPdu = ssh_bits:encode(RecvAlg),
    SendAlg = kex_init(SSH),
    {ok, SendPdu} = send_algorithms(S, SSH, SendAlg),
    send_msg(S, SSH, SendAlg),
    kex_negotiate(S, User, SSH, UserAck, SendAlg, SendPdu, RecvAlg, RecvPdu).

%% Select algorithms
kex_negotiate(S, User, SSH, UserAck, SendAlg, SendPdu, RecvAlg, RecvPdu) ->
    case SSH#ssh.role of
	client ->
	    SSH1 = SSH#ssh { c_keyinit = SendPdu, s_keyinit = RecvPdu },
	    case select_algorithm(SSH1, #alg {}, SendAlg, RecvAlg) of
		{ok, SSH2} ->
		    ALG = SSH2#ssh.algorithms,
		    case client_kex(S, SSH2, ALG#alg.kex) of
			{ok, SSH3} ->
			    newkeys(S, User, SSH3, UserAck);
			Error ->
			    kexfailed(S, User, UserAck, Error)
		    end;
		Error ->
		    kexfailed(S, User, UserAck, Error)
	    end;

	server ->
	    SSH1 = SSH#ssh { c_keyinit = RecvPdu, s_keyinit = SendPdu }, 
	    case select_algorithm(SSH1, #alg {}, RecvAlg, SendAlg) of
		{ok,SSH2} ->
		    ALG = SSH2#ssh.algorithms,
		    case server_kex(S, SSH2, ALG#alg.kex) of
			{ok, SSH3} ->
			    newkeys(S, User, SSH3, UserAck);
			Error ->
			    kexfailed(S, User, UserAck, Error)
		    end;
		Error ->
		    kexfailed(S, User, UserAck, Error)
	    end
    end.
    
newkeys(S, User, SSH, UserAck) ->
    %% Send new keys and wait for newkeys
    send_msg(S, SSH, #ssh_msg_newkeys {}),
    case recv_msg(S, SSH) of
	{ok, M} when record(M, ssh_msg_newkeys) ->
	    SSH1 = install_alg(SSH),
	    if UserAck == true ->
		    User ! {self(), {ok, self()}},
		    inet:setopts(S, [{active, once}]),
		    ssh_main(S, User, SSH1);
	       true ->
		    inet:setopts(S, [{active, once}]),
		    ssh_main(S, User, SSH1)
	    end;
	{ok,_} ->
	    {error, bad_message};
	Error ->
	    Error
    end.



client_kex(S, SSH, 'diffie-hellman-group1-sha1') ->
    ssh_bits:install_messages(kexdh_messages()),
    {G,P} = dh_group1(),
    {Private, Public} = dh_gen_key(G,P,1024),
    ?dbg(?DBG_KEX, "public: ~.16B\n", [Public]),
    send_msg(S, SSH, #ssh_msg_kexdh_init { e = Public }),
    case recv_msg(S, SSH) of
	{ok, R} when record(R, ssh_msg_kexdh_reply) ->
	    K_S = R#ssh_msg_kexdh_reply.public_host_key,
	    F = R#ssh_msg_kexdh_reply.f,
	    K = ssh_math:ipow(F, Private, P),
	    H = kex_h(SSH, K_S, Public, F, K),
	    H_SIG = R#ssh_msg_kexdh_reply.h_sig,
	    ?dbg(?DBG_KEX, "shared_secret: ~s\n", [fmt_binary(K, 16, 4)]),
	    ?dbg(?DBG_KEX, "hash: ~s\n", [fmt_binary(H, 16, 4)]),
	    case verify_host_key(S, SSH, K_S, H, H_SIG) of
		ok ->
		    {ok, SSH#ssh { shared_secret  = K,
				   exchanged_hash = H,
				   session_id = H }};
		Error ->
		    Error
	    end;
	{ok,_} ->
	    {error, bad_message};
	Error ->
	    Error
    end;
client_kex(S, SSH, 'diffie-hellman-group-exchange-sha1') ->
    ssh_bits:install_messages(kex_dh_gex_messages()),
    Min = 512,
    NBits = 1024,
    Max = 4096,
    send_msg(S, SSH, #ssh_msg_kex_dh_gex_request { min = Min,
						   n   = NBits,
						   max = Max }),
    case recv_msg(S, SSH) of
	{ok, RG} when record(RG, ssh_msg_kex_dh_gex_group) ->
	    P = RG#ssh_msg_kex_dh_gex_group.p,
	    G = RG#ssh_msg_kex_dh_gex_group.g,
	    {Private, Public} = dh_gen_key(G,P, 1024),
	    ?dbg(?DBG_KEX, "public: ~.16B\n", [Public]),
	    send_msg(S, SSH, #ssh_msg_kex_dh_gex_init { e = Public }),
	    case recv_msg(S, SSH) of
		{ok, R} when record(R, ssh_msg_kex_dh_gex_reply) ->
		    K_S = R#ssh_msg_kex_dh_gex_reply.public_host_key,
		    F = R#ssh_msg_kex_dh_gex_reply.f,
		    K = ssh_math:ipow(F, Private, P),
		    H = kex_h(SSH, K_S, Min, NBits, Max, P, G, Public, F, K),
		    H_SIG = R#ssh_msg_kex_dh_gex_reply.h_sig,
		    ?dbg(?DBG_KEX, "shared_secret: ~s\n",
			 [fmt_binary(K, 16, 4)]),
		    ?dbg(?DBG_KEX, "hash: ~s\n", 
			 [fmt_binary(H, 16, 4)]),
		    case verify_host_key(S, SSH, K_S, H, H_SIG) of
			ok ->
			    {ok,  SSH#ssh { shared_secret  = K,
					    exchanged_hash = H,
					    session_id = H }};
			Error ->
			    Error
		    end;
		{ok,_} ->
		    {error, bad_message};
		Error ->
		    Error
	    end;
	{ok,_} ->
	    {error, bad_message};
	Error ->
	    Error
    end;
client_kex(S, SSH, Kex) ->
    {error, {bad_kex_algorithm, Kex}}.


server_kex(S, SSH, 'diffie-hellman-group1-sha1') ->
    ssh_bits:install_messages(kexdh_messages()),
    {G,P} = dh_group1(),
    {Private, Public} = dh_gen_key(G,P,1024),
    ?dbg(?DBG_KEX, "public: ~.16B\n", [Public]),
    case recv_msg(S, SSH) of
	{ok, R} when record(R, ssh_msg_kexdh_init) ->
	    E = R#ssh_msg_kexdh_init.e,
	    K = ssh_math:ipow(E, Private, P),
	    {Key,K_S} = get_host_key(SSH),
	    H = kex_h(SSH, K_S, E, Public, K),
	    H_SIG = sign_host_key(S, SSH, Key, H),
	    send_msg(S, SSH,
		     #ssh_msg_kexdh_reply { public_host_key = K_S,
					    f = Public,
					    h_sig = H_SIG
					   }),
	    ?dbg(?DBG_KEX, "shared_secret: ~s\n", [fmt_binary(K, 16, 4)]),
	    ?dbg(?DBG_KEX, "hash: ~s\n", [fmt_binary(H, 16, 4)]),
	    {ok, SSH#ssh { shared_secret = K,
			   exchanged_hash = H,
			   session_id = H }};
	{ok,_} ->
	    {error, bad_message};
	Error ->
	    Error
    end;
server_kex(S, SSH, 'diffie-hellman-group-exchange-sha1') ->
    ssh_bits:install_messages(kex_dh_gex_messages()),
    R0 = recv_msg(S, SSH),
    #ssh_msg_kex_dh_gex_request { min = Min,
				  n   = NBits,
				  max = Max } = R0,
    {G,P} = dh_group1(), %% FIX ME!!!
    send_msg(S, SSH, #ssh_msg_kex_dh_gex_group { p = P, g = G }),
    {Private, Public} = dh_gen_key(G,P,1024),
    ?dbg(?DBG_KEX, "public: ~.16B\n", [Public]),
    case recv_msg(S, SSH) of
	{ok, R} when record(R, ssh_msg_kex_dh_gex_init) ->
	    E = R#ssh_msg_kex_dh_gex_init.e,
	    K = ssh_math:ipow(E, Private, P),
	    {Key,K_S} = get_host_key(SSH),
	    H = kex_h(SSH, K_S, Min, NBits, Max, P, G, E, Public, K),
	    H_SIG = sign_host_key(S, SSH, Key, H),
	    send_msg(S, SSH,
		     #ssh_msg_kex_dh_gex_reply { public_host_key = K_S,
						 f = Public,
						 h_sig = H_SIG
						}),
	    ?dbg(?DBG_KEX, "shared_secret: ~s\n", [fmt_binary(K, 16, 4)]),
	    ?dbg(?DBG_KEX, "hash: ~s\n", [fmt_binary(H, 16, 4)]),
	    {ok, SSH#ssh { shared_secret = K,
			   exchanged_hash = H,
			   session_id = H }};
	{ok,_} ->
	    {error, bad_message};
	Error ->
	    Error
    end;
server_kex(S, SSH, Kex) ->
    {error, {bad_kex_algorithm, Kex}}.



ssh_main(S, User, SSH) ->
    receive
	{tcp, S, Data} ->
	    %% This is a lazy way of gettting events without block
	    ?dbg(?DBG_PACKET, "UNRECEIVE: ~w BYTES\n", [size(Data)]),
	    gen_tcp:unrecv(S, Data),
	    case recv_msg(S, SSH) of
		{ok, M} when record(M, ssh_msg_unimplemented) ->
		    ?dbg(true, "UNIMPLEMENTED: ~p\n",
			 [M#ssh_msg_unimplemented.sequence]),
		    inet:setopts(S, [{active, once}]),
		    ssh_main(S, User, SSH);
		{ok,M} when record(M, ssh_msg_disconnect) ->
		    User ! {ssh_msg, self(), M},
		    ?dbg(true, "DISCONNECT: ~w ~s\n",
			 [M#ssh_msg_disconnect.code,
			  M#ssh_msg_disconnect.description]),
		    gen_tcp:close(S);

		{ok,M} when record(M, ssh_msg_kexinit) ->
		    recv_negotiate(S, User, SSH, M, false);

		{ok,M} ->
		    User ! {ssh_msg, self(), M},
		    inet:setopts(S, [{active, once}]),
		    ssh_main(S, User, SSH);
		{error, unimplemented} ->
		    send_msg(S, SSH, 
			     #ssh_msg_unimplemented { sequence =
						      get(recv_sequence)-1}),
		    inet:setopts(S, [{active, once}]),
		    ssh_main(S, User, SSH);
		{error, Other} ->
		    inet:setopts(S, [{active, once}]),
		    %% send disconnect!
		    ssh_main(S, User, SSH)
	    end;

	{tcp_closed, S} ->
	    User ! {ssh_msg, self(),
		    #ssh_msg_disconnect { code=?SSH_DISCONNECT_CONNECTION_LOST,
					  description = "Connection closed",
					  language = "" }},
	    gen_tcp:close(S), %% CHECK ME, is this needed ?
	    ok;

	{ssh_msg, User, Msg} ->
	    send_msg(S, SSH, Msg),
	    if record(Msg, ssh_msg_disconnect) ->
		    ok;
	       true ->
		    ssh_main(S, User, SSH)
	    end;

	{ssh_install, Table} ->
	    ssh_bits:install_messages(Table),
	    ssh_main(S, User, SSH);

	{ssh_uninstall, Table} ->
	    ssh_bits:uninstall_messages(Table),
	    ssh_main(S, User, SSH);

	{ssh_renegotiate, UserAck, Opts} ->
	    send_negotiate(S, User, ssh_setopts(Opts,SSH), UserAck);

	{ssh_call, From, Req} ->
	    ?dbg(true, "Call: ~p from ~p\n", [Req,From]),
	    SSH1 = handle_call(Req, From, SSH),
	    ssh_main(S, User, SSH1);

	Other ->
	    ?dbg(true, "ssh_loop: got ~p\n", [Other]),
	    ssh_main(S, User, SSH)
    end.
%%
%% Handle call's to ssh_proto
%%
handle_call({get_cb,io}, From, SSH) ->
    reply(From, {ok, SSH#ssh.io_cb}),
    SSH;
handle_call({get_cb,key}, From, SSH) ->
    reply(From, {ok, SSH#ssh.key_cb}),
    SSH;
handle_call(Other, From, SSH) ->
    reply(From, {error, bad_call}),
    SSH.

reply([Pid|Ref], Reply) ->
    ?dbg(true, "Reply: ~p\n", [Reply]),
    Pid ! {Ref, Reply}.


%%
%% The host key should be read from storage
%%
get_host_key(SSH) ->
    Mod = SSH#ssh.key_cb,
    Scope = getopt(key_scope, SSH#ssh.opts, user),
    ALG = SSH#ssh.algorithms,
    case ALG#alg.hkey of
	'ssh-rsa' ->
	    case Mod:private_host_rsa_key(Scope) of
		{ok,Key=#ssh_key { public={N,E}} } ->
		    {Key,
		     ssh_bits:encode(["ssh-rsa",E,N],[string,mpint,mpint])};
		Error ->
		    exit(Error)
	    end;
	'ssh-dss' ->
	    case Mod:private_host_dsa_key(Scope) of
		{ok,Key=#ssh_key { public={P,Q,G,Y}}} ->
		    {Key, ssh_bits:encode(["ssh-dss",P,Q,G,Y],
					  [string,mpint,mpint,mpint,mpint])};
		Error ->
		    exit(Error)
	    end;
	_ ->
	    exit({error, bad_key_type})
    end.

sign_host_key(S, SSH, Private, H) ->
    ALG = SSH#ssh.algorithms,
    case ALG#alg.hkey of
	'ssh-rsa' ->
	    case catch ssh_rsa:sign(Private, H) of
		{'EXIT', Reason} ->
		    io:format("SIGN FAILED: ~p\n", [Reason]),
		    {error, Reason};
		SIG ->
		    ssh_bits:encode(["ssh-rsa",SIG],[string,binary])
	    end;
	'ssh-dss' ->
	    case catch ssh_dsa:sign(Private, H) of
		{'EXIT', Reason} ->
		    io:format("SIGN FAILED: ~p\n", [Reason]),
		    {error, Reason};
		SIG ->
		    ssh_bits:encode(["ssh-dss",SIG],[string,binary])
	    end;
	_ ->
	    {error, bad_host_key_algorithm}
    end.    

verify_host_key(S, SSH, K_S, H, H_SIG) ->
    ALG = SSH#ssh.algorithms,
    case ALG#alg.hkey of
	'ssh-rsa' ->
	    case ssh_bits:decode(K_S,[string,mpint,mpint]) of
		["ssh-rsa", E, N] ->
		    ["ssh-rsa",SIG] = ssh_bits:decode(H_SIG,[string,binary]),
		    Public = #ssh_key { type=rsa, public={N,E} },
		    case catch ssh_rsa:verify(Public, H, SIG) of
			{'EXIT', Reason} ->
			    io:format("VERIFY FAILED: ~p\n", [Reason]),
			    {error, bad_signature};
			ok ->
			    known_host_key(SSH, Public)
		    end;
		_ ->
		    {error, bad_format}
	    end;
	'ssh-dss' ->
	    case ssh_bits:decode(K_S,[string,mpint,mpint,mpint,mpint]) of
		["ssh-dss",P,Q,G,Y] ->
		    ["ssh-dss",SIG] = ssh_bits:decode(H_SIG,[string,binary]),
		    Public = #ssh_key { type=dsa, public={P,Q,G,Y} },
		    case catch ssh_dsa:verify(Public, H, SIG) of
			{'EXIT', Reason} ->
			    io:format("VERIFY FAILED: ~p\n", [Reason]),
			    {error, bad_signature};
			ok ->
			    known_host_key(SSH, Public)
		    end;
		_ ->
		    {error, bad_host_key_format}
	    end;
	_ ->
	    {error, bad_host_key_algorithm}
    end.

known_host_key(SSH, Public) ->
    case (SSH#ssh.key_cb):lookup_host_key(SSH#ssh.peer) of
	{ok, Public} ->
	    ok;
	{ok, BadPublic} ->
	    {error, bad_public_key};
	{error, not_found} ->
	    case (SSH#ssh.io_cb):yes_no("New host "++SSH#ssh.peer++" accept") of
		yes ->
		    (SSH#ssh.key_cb):add_host_key(SSH#ssh.peer, Public),
		    ok;
		no ->
		    {error, rejected}
	    end
    end.


	    
send_algorithms(S, SSH, KexInit) ->
    Payload = ssh_bits:encode(KexInit),
    ?dbg(?DBG_MESSAGE, "SEND_MSG: ~70p\n", [KexInit]),
    Res = send_packet(S, SSH, Payload),
    {Res,Payload}.
		       

recv_algorithms(S, SSH) ->
    case recv_packet(S, SSH) of
	{ok,Packet} ->
	    case ssh_bits:decode(Packet) of
		{ok, R} ->
		    ?dbg(?DBG_MESSAGE, "RECV_MSG: ~70p\n", [R]),
		    {ok,{Packet, R}};
		Error ->
		    Error
	    end;
	Error ->
	    ?dbg(?DBG_MESSAGE, "RECV_MSG: ~p\n", [Error]),
	    Error
    end.

%%   Each of the algorithm strings MUST be a comma-separated list of
%%   algorithm names (see ''Algorithm Naming'' in [SSH-ARCH]).  Each
%%   supported (allowed) algorithm MUST be listed in order of preference.
%%
%%   The first algorithm in each list MUST be the preferred (guessed)
%%   algorithm.  Each string MUST contain at least one algorithm name.

select_algorithm(SSH, ALG, C, S) ->
    %% find out the selected algorithm
    C_Enc = select(C#ssh_msg_kexinit.encryption_algorithms_client_to_server,
		   S#ssh_msg_kexinit.encryption_algorithms_client_to_server),

    C_Mac = select(C#ssh_msg_kexinit.mac_algorithms_client_to_server,
		   S#ssh_msg_kexinit.mac_algorithms_client_to_server),

    C_Cmp = select(C#ssh_msg_kexinit.compression_algorithms_client_to_server,
		   S#ssh_msg_kexinit.compression_algorithms_client_to_server),

    C_Lng = select(C#ssh_msg_kexinit.languages_client_to_server,
		   S#ssh_msg_kexinit.languages_client_to_server),

    S_Enc = select(C#ssh_msg_kexinit.encryption_algorithms_server_to_client,
		   S#ssh_msg_kexinit.encryption_algorithms_server_to_client),

    S_Mac = select(C#ssh_msg_kexinit.mac_algorithms_server_to_client,
		   S#ssh_msg_kexinit.mac_algorithms_server_to_client),

    S_Cmp = select(C#ssh_msg_kexinit.compression_algorithms_server_to_client,
		   S#ssh_msg_kexinit.compression_algorithms_server_to_client),

    S_Lng = select(C#ssh_msg_kexinit.languages_server_to_client,
		   S#ssh_msg_kexinit.languages_server_to_client),

    HKey = select_all(C#ssh_msg_kexinit.server_host_key_algorithms,
		      S#ssh_msg_kexinit.server_host_key_algorithms),
    HK = case HKey of
	     [] -> undefined;
	     [HK0|_] -> HK0
	 end,
    %% Fixme verify Kex against HKey list and algorithms
    
    Kex = select(C#ssh_msg_kexinit.kex_algorithms,
		 S#ssh_msg_kexinit.kex_algorithms),

    ALG1 = ALG#alg { kex = Kex, hkey = HK },

    ALG2 = save_alg(SSH#ssh.role, 
		   ALG1,
		   [{c_enc, C_Enc},
		    {c_mac, C_Mac},
		    {c_cmp, C_Cmp},
		    {c_lng, C_Lng},
		    {s_enc, S_Enc},
		    {s_mac, S_Mac},
		    {s_cmp, S_Cmp},
		    {s_lng, S_Lng}]),
    {ok, SSH#ssh { algorithms = ALG2 }}.


save_alg(Role, ALG, [{Key,A} | As]) ->
    if A == undefined ->
	    save_alg(Role, ALG, As);
       true ->
	    case Key of
		c_enc ->
		    case Role of
			client ->
			    save_alg(Role,ALG#alg { encrypt = A }, As);
			server ->
			    save_alg(Role,ALG#alg { decrypt = A }, As)
		    end;

		s_enc -> 
		    case Role of
			server -> 
			    save_alg(Role,ALG#alg { encrypt = A }, As);
			client ->
			    save_alg(Role,ALG#alg { decrypt = A }, As)
		    end;

		c_mac ->
		    case Role of
			client ->
			    save_alg(Role,ALG#alg { send_mac=A }, As);
			server ->
			    save_alg(Role,ALG#alg { recv_mac=A }, As)
		    end;

		s_mac -> 
		    case Role of
			server -> 
			    save_alg(Role,ALG#alg { send_mac = A }, As); 
			client ->
			    save_alg(Role,ALG#alg { recv_mac = A }, As)
		    end;

		c_cmp -> 
		    case Role of
			client ->
			    save_alg(Role,ALG#alg { compress = A }, As);
			server ->
			    save_alg(Role,ALG#alg { decompress = A }, As)
		    end;
			    
		s_cmp -> 
		    case Role of
			server ->
			    save_alg(Role, ALG#alg { compress = A }, As);
			client ->
			    save_alg(Role, ALG#alg { decompress = A }, As)
		    end;
		c_lng -> save_alg(Role, ALG#alg { c_lng = A }, As);
		s_lng -> save_alg(Role, ALG#alg { s_lng = A }, As)
	    end
    end;
save_alg(Role, ALG, []) ->
    ALG.

install_alg(SSH) ->
    SSH1 = alg_final(SSH),
    SSH2 = alg_setup(SSH1),
    alg_init(SSH2).

alg_setup(SSH) ->
    ALG = SSH#ssh.algorithms,
    ?dbg(?DBG_ALG, "ALG: setup ~p\n", [ALG]),
    SSH#ssh { kex       = ALG#alg.kex,
	      hkey      = ALG#alg.hkey,
	      encrypt = ALG#alg.encrypt,
	      decrypt = ALG#alg.decrypt,
	      send_mac = ALG#alg.send_mac,
	      send_mac_size = mac_digest_size(ALG#alg.send_mac),
	      recv_mac = ALG#alg.recv_mac,
	      recv_mac_size = mac_digest_size(ALG#alg.recv_mac),
	      compress = ALG#alg.compress,
	      decompress = ALG#alg.decompress,
	      c_lng = ALG#alg.c_lng,
	      s_lng = ALG#alg.s_lng,
	      algorithms = undefined
	      }.

alg_init(SSH0) ->
    ?dbg(?DBG_ALG, "ALG: init\n", []),
    {ok,SSH1} = send_mac_init(SSH0),
    {ok,SSH2} = recv_mac_init(SSH1),
    {ok,SSH3} = encrypt_init(SSH2),
    {ok,SSH4} = decrypt_init(SSH3),
    {ok,SSH5} = compress_init(SSH4),
    {ok,SSH6} = decompress_init(SSH5),
    SSH6.

alg_final(SSH0) ->
    ?dbg(?DBG_ALG, "ALG: final\n", []),
    {ok,SSH1} = send_mac_final(SSH0),
    {ok,SSH2} = recv_mac_final(SSH1),
    {ok,SSH3} = encrypt_final(SSH2),
    {ok,SSH4} = decrypt_final(SSH3),
    {ok,SSH5} = compress_final(SSH4),
    {ok,SSH6} = decompress_final(SSH5),
    SSH6.



select_all(CL, SL) ->
    A = CL -- SL,  %% algortihms only used by client
    %% algorithms used by client and server (client pref)
    map(fun(ALG) -> list_to_atom(ALG) end, (CL -- A)).

select([], []) ->
    none;
select(CL, SL) ->
    C = case select_all(CL,SL) of
	    [] -> undefined;
	    [ALG|_] -> ALG
	end,
    ?dbg(?DBG_ALG, "ALG: select: ~p ~p = ~p\n", [CL, SL, C]),
    C.
	    
send_version(S, Version) ->
    gen_tcp:send(S, [Version,"\r\n"]).


send_msg(S, SSH, Record) ->
    ?dbg(?DBG_MESSAGE, "SEND_MSG: ~70p\n", [Record]),
    Bin = ssh_bits:encode(Record),
    send_packet(S, SSH, Bin).


%%
%% TotalLen = 4 + 1 + size(Data) + size(Padding)
%% PaddingLen = TotalLen - (size(Data)+4+1)
%% 
send_packet(S, SSH, Data0) when binary(Data0) ->
    {ok,Data} = compress(SSH, Data0),
    BlockSize = SSH#ssh.encrypt_block_size,
    PL = (BlockSize - ((4 + 1 + size(Data)) rem BlockSize)) rem BlockSize,
    PaddingLen = if PL <  4 -> PL+BlockSize;
		    true -> PL
		 end,
    Padding = ssh_bits:random(PaddingLen),
    PacketLen = 1 + PaddingLen + size(Data),
    Packet = <<?UINT32(PacketLen),?BYTE(PaddingLen), 
	      Data/binary, Padding/binary>>,
    EncPacket = encrypt(SSH, Packet),
    Seq = get(send_sequence),
    MAC = send_mac(SSH, Packet, Seq),
    ?dbg(?DBG_PACKET, "SEND_PACKET:~w len=~p,payload=~p,padding=~p,mac=~p\n",
	 [Seq, PacketLen, size(Data), PaddingLen, MAC]),
    Res = gen_tcp:send(S, [EncPacket, MAC]),
    put(send_sequence, (Seq+1) band 16#ffffffff),
    Res.


recv_msg(S, SSH) ->
    case recv_packet(S, SSH) of
	{ok, Packet} ->
	    case ssh_bits:decode(Packet) of
		{ok, M} when record(M, ssh_msg_debug) ->
		    if M#ssh_msg_debug.always_display == true ->
			    io:format("DEBUG: ~p\n",
				      [M#ssh_msg_debug.message]);
		       true ->
			    ?dbg(true, "DEBUG: ~p\n",
				 [M#ssh_msg_debug.message])
		    end,
		    inet:setopts(S, [{active, once}]),
		    recv_msg(S, SSH);
		{ok, M} when record(M, ssh_msg_ignore) ->
		    inet:setopts(S, [{active, once}]),
		    recv_msg(S, SSH);

		{ok, Msg} ->
		    ?dbg(?DBG_MESSAGE, "RECV_MSG: ~70p\n", [Msg]),
		    {ok, Msg};
		Error ->
		    %% Fixme (send disconnect...)
		    Error
	    end;
	Error ->
	    ?dbg(?DBG_MESSAGE, "RECV_MSG: ~70p\n", [Error]),
	    Error
    end.

%% receive ONE packet
recv_packet(S, SSH) ->
    BlockSize = SSH#ssh.decrypt_block_size,
    case gen_tcp:recv(S, BlockSize) of
	{ok, EncData0} ->
	    Data0 = decrypt(SSH, EncData0),
	    <<?UINT32(PacketLen), _/binary>> = Data0,
	    if PacketLen < 5; PacketLen > ?SSH_MAX_PACKET_SIZE ->
		    terminate(S, SSH, ?SSH_DISCONNECT_PROTOCOL_ERROR,
			      "Bad packet length "++
			      integer_to_list(PacketLen));
	       true ->
		    case gen_tcp:recv(S, (PacketLen - BlockSize)+4) of
			{ok, EncData1} ->
			    Data1 = decrypt(SSH, EncData1),
			    Data = <<Data0/binary, Data1/binary>>,
			    recv_packet_data(S, SSH, PacketLen, Data);
			Error ->
			    Error
		    end
	    end;
	Error ->
	    Error
    end.

recv_packet_data(S, SSH, PacketLen, Data) ->
    Seq = get(recv_sequence),
    Res = valid_mac(SSH, S, Data, Seq),
    put(recv_sequence, (Seq+1) band 16#ffffffff),
    case Res of
	true ->
	    <<_:32, PaddingLen:8, _/binary>> = Data,
	    PayloadLen = PacketLen - PaddingLen - 1,
	    <<_:32, _:8, Payload:PayloadLen/binary, 
	     _:PaddingLen/binary>> = Data,
	    ?dbg(?DBG_PACKET, 
		 "RECV_PACKET:~w, len=~p,payload=~w,padding=~w\n", 
		 [Seq,PacketLen,PayloadLen,PaddingLen]),
	    decompress(SSH, Payload);

	false ->
	    ?dbg(?DBG_PACKET, "RECV_PACKET:~w, len=~p\n", 
		 [Seq,PacketLen]),
	    terminate(S, SSH, ?SSH_DISCONNECT_MAC_ERROR,
		      "Bad MAC #"++ integer_to_list(Seq))
    end.


kexfailed(S, User, UserAck, Error) ->
    Description =
	case Error of
	    {error, bad_message} ->
		"key exchanged failed: bad message received";
	    _ ->
		"key exchanged failed"
	end,
    M = #ssh_msg_disconnect { code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
			      description = Description,
			      language = "en"},
    if UserAck == true ->
	    User ! {self(), Error};
       true ->
	    User ! {ssh_msg, self(), M}
    end,
    gen_tcp:close(S),
    Error.



%% Send a disconnect message
terminate(S, SSH, Code, Message) ->
    M = #ssh_msg_disconnect { code=Code, 
			      description=Message,
			      language = "en" },
    send_msg(S, SSH, M),
    gen_tcp:close(S),
    {error, M}.

    

    

%% public key algorithms
%%
%%   ssh-dss              REQUIRED     sign    Raw DSS Key
%%   ssh-rsa              RECOMMENDED  sign    Raw RSA Key
%%   x509v3-sign-rsa      OPTIONAL     sign    X.509 certificates (RSA key)
%%   x509v3-sign-dss      OPTIONAL     sign    X.509 certificates (DSS key)
%%   spki-sign-rsa        OPTIONAL     sign    SPKI certificates (RSA key)
%%   spki-sign-dss        OPTIONAL     sign    SPKI certificates (DSS key)
%%   pgp-sign-rsa         OPTIONAL     sign    OpenPGP certificates (RSA key)
%%   pgp-sign-dss         OPTIONAL     sign    OpenPGP certificates (DSS key)
%%

%% key exchange
%%
%%     diffie-hellman-group1-sha1       REQUIRED
%%
%%

    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Encrytion
%%   context stored in dictionary as 'encrypt_ctx'
%%
%% chiphers
%%
%%       3des-cbc         REQUIRED          
%%       three-key 3DES in CBC mode
%%       blowfish-cbc     OPTIONAL          Blowfish in CBC mode
%%       twofish256-cbc   OPTIONAL          Twofish in CBC mode,
%%                                          with 256-bit key
%%       twofish-cbc      OPTIONAL          alias for "twofish256-cbc" (this
%%                                          is being retained for
%%                                          historical reasons)
%%       twofish192-cbc   OPTIONAL          Twofish with 192-bit key
%%       twofish128-cbc   OPTIONAL          Twofish with 128-bit key
%%       aes256-cbc       OPTIONAL          AES in CBC mode,
%%                                          with 256-bit key
%%       aes192-cbc       OPTIONAL          AES with 192-bit key
%%       aes128-cbc       RECOMMENDED       AES with 128-bit key
%%       serpent256-cbc   OPTIONAL          Serpent in CBC mode, with
%%                                          256-bit key
%%       serpent192-cbc   OPTIONAL          Serpent with 192-bit key
%%       serpent128-cbc   OPTIONAL          Serpent with 128-bit key
%%       arcfour          OPTIONAL          the ARCFOUR stream cipher
%%       idea-cbc         OPTIONAL          IDEA in CBC mode
%%       cast128-cbc      OPTIONAL          CAST-128 in CBC mode
%%       none             OPTIONAL          no encryption; NOT RECOMMENDED
%%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encrypt_init(SSH) ->
    case SSH#ssh.encrypt of
	none ->
	    {ok,SSH};
	'3des-cbc' ->
	    {IV,KD} = 
		case SSH#ssh.role of
		    client ->
			{hash(SSH, "A", 64),
			 hash(SSH, "C", 192)};
		    server ->
			{hash(SSH, "B", 64),
			 hash(SSH, "D", 192)}
		end,
	    <<K1:8/binary, K2:8/binary, K3:8/binary>> = KD,
	    put(encrypt_ctx,  IV),
	    {ok,SSH#ssh { encrypt_keys = {K1,K2,K3},
			  encrypt_block_size = 8 }};
	_ ->
	    exit({bad_algorithm,SSH#ssh.encrypt})
    end.

encrypt_final(SSH) ->
    erase(encrypt_ctx),
    {ok, SSH#ssh { encrypt = none, 
		   encrypt_keys = undefined,
		   encrypt_block_size = 8
		  }}.


encrypt(SSH, Data) ->
    case SSH#ssh.encrypt of
	none -> 
	    Data;
	'3des-cbc' ->
	    {K1,K2,K3} = SSH#ssh.encrypt_keys,
	    IV0 = get(encrypt_ctx),
	    ?dbg(?DBG_CRYPTO, "encrypt: IV=~p K1=~p, K2=~p, K3=~p\n",
		 [IV0,K1,K2,K3]),
	    Enc = crypto:des3_cbc_encrypt(K1,K2,K3,IV0,Data),
	    ?dbg(?DBG_CRYPTO, "encrypt: ~p -> ~p\n", [Data, Enc]),
	    %% Enc = list_to_binary(E0),
	    IV = crypto:des_cbc_ivec(Enc),
	    put(encrypt_ctx, IV),
	    Enc;
	_ ->
	    exit({bad_algorithm,SSH#ssh.encrypt})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Decryption
%%   context stored in dictionary as 'decrypt_ctx'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decrypt_init(SSH) ->
    case SSH#ssh.decrypt of
	none ->
	    {ok,SSH};
	'3des-cbc' ->
	    {IV,KD} = case SSH#ssh.role of
			  client ->
			      {hash(SSH, "B", 64),
			       hash(SSH, "D", 192)};
			  server -> 
			      {hash(SSH, "A", 64),
			       hash(SSH, "C", 192)}
		      end,
	    <<K1:8/binary, K2:8/binary, K3:8/binary>> = KD,
	    put(decrypt_ctx,  IV),
	    {ok,SSH#ssh{ decrypt_keys = {K1,K2,K3},
			 decrypt_block_size = 8	}};
	_ ->
	    exit({bad_algorithm,SSH#ssh.decrypt})
    end.

decrypt_final(SSH) ->
    erase(decrypt_ctx),
    {ok, SSH#ssh { decrypt = none, 
		   decrypt_keys = undefined,
		   decrypt_block_size = 8 }}.

decrypt(SSH, Data) ->
    case SSH#ssh.decrypt of
	none -> 
	    Data;
	'3des-cbc' ->
	    {K1,K2,K3} = SSH#ssh.decrypt_keys,
	    IV0 = get(decrypt_ctx),
	    ?dbg(?DBG_CRYPTO, "decrypt: IV=~p K1=~p, K2=~p, K3=~p\n",
		 [IV0,K1,K2,K3]),
	    Dec = crypto:des3_cbc_decrypt(K1,K2,K3,IV0,Data),
	    %% Enc = list_to_binary(E0),
	    ?dbg(?DBG_CRYPTO, "decrypt: ~p -> ~p\n", [Data, Dec]),
	    IV = crypto:des_cbc_ivec(Data),
	    put(decrypt_ctx, IV),
	    Dec;
	_ ->
	    exit({bad_algorithm,SSH#ssh.decrypt})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Compression
%%   context stored in dictionary as 'compress_ctx'
%%
%%     none     REQUIRED        no compression
%%     zlib     OPTIONAL        ZLIB (LZ77) compression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compress_init(SSH) ->
    compress_init(SSH, 1).
compress_init(SSH, Level) ->
    case SSH#ssh.compress of
	none ->
	    {ok,SSH};
	zlib ->
	    Z = zlib:open(),
	    case zlib:deflateInit(Z, Level) of
		ok ->
		    put(compress_ctx, Z),
		    {ok,SSH};
		Error ->
		    zlib:close(Z),
		    Error
	    end;
	_ ->
	    exit({bad_algorithm,SSH#ssh.compress})
    end.

compress_final(SSH) ->
    case SSH#ssh.compress of
	none ->
	    {ok, SSH};
	zlib ->
	    zlib:close(get(compress_ctx)),
	    erase(compress_ctx),
	    {ok, SSH#ssh { compress = none }};
	_ ->
	    exit({bad_algorithm,SSH#ssh.compress})
    end.

compress(SSH, Data) ->
    case SSH#ssh.compress of
	none ->
	    {ok,Data};
	zlib ->
	    case zlib:deflate(get(compress_ctx), Data, partial) of
		{ok, Compressed} ->
		    {ok,list_to_binary(Compressed)};
		Error ->
		    Error
	    end;
	_ ->
	    exit({bad_algorithm,SSH#ssh.compress})
    end.    

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Decompression
%%   context stored in dictionary as 'decompress_ctx'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decompress_init(SSH) ->
    case SSH#ssh.decompress of
	none ->
	    {ok,SSH};
	zlib ->
	    Z = zlib:open(),
	    case zlib:inflateInit(Z) of
		ok ->
		    put(decompress_ctx, Z),
		    {ok,SSH};
		Error ->
		    zlib:close(Z),
		    Error
	    end;
	_ ->
	    exit({bad_algorithm,SSH#ssh.decompress})
    end.

decompress_final(SSH) ->
    case SSH#ssh.decompress of
	none ->
	    {ok, SSH};
	zlib ->
	    zlib:close(get(decompress_ctx)),
	    erase(decompress_ctx),
	    {ok, SSH#ssh { decompress = none }};
	_ ->
	    exit({bad_algorithm,SSH#ssh.decompress})
    end.
    
decompress(SSH, Data) ->
    case SSH#ssh.decompress of
	none ->
	    {ok,Data};
	zlib ->
	    case zlib:inflate(get(decompress_ctx), Data, partial) of
		{ok, Decompressed} ->
		    {ok,list_to_binary(Decompressed)};
		Error ->
		    Error
	    end;
	_ ->
	    exit({bad_algorithm,SSH#ssh.decompress})
    end.

%%
%% macs
%%
%%     hmac-sha1    REQUIRED        HMAC-SHA1 (digest length = key
%%                                  length = 20)
%%     hmac-sha1-96 RECOMMENDED     first 96 bits of HMAC-SHA1 (digest
%%                                  length = 12, key length = 20)
%%     hmac-md5     OPTIONAL        HMAC-MD5 (digest length = key
%%                                  length = 16)
%%     hmac-md5-96  OPTIONAL        first 96 bits of HMAC-MD5 (digest
%%                                  length = 12, key length = 16)
%%     none         OPTIONAL        no MAC; NOT RECOMMENDED
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MAC calculation
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_mac_init(SSH) ->
    case SSH#ssh.role of
	client ->
	    Key = hash(SSH, "E", mac_key_size(SSH#ssh.send_mac)),
	    {ok, SSH#ssh { send_mac_key = Key }};
	server ->
	    Key = hash(SSH, "F", mac_key_size(SSH#ssh.send_mac)),
	    {ok, SSH#ssh { send_mac_key = Key }}
    end.

send_mac_final(SSH) ->
    {ok, SSH#ssh {  send_mac = none, send_mac_key = undefined }}.

send_mac(SSH, Data, Seq) ->
    case SSH#ssh.send_mac of
	none -> 
	    <<>>;
	'hmac-sha1' ->
	    crypto:sha_mac(SSH#ssh.send_mac_key, [<<?UINT32(Seq)>>, Data]);
	'hmac-sha1-96' ->
	    crypto:sha_mac_96(SSH#ssh.send_mac_key, [<<?UINT32(Seq)>>, Data]);
	'hmac-md5' ->
	    crypto:md5_mac(SSH#ssh.send_mac_key, [<<?UINT32(Seq)>>, Data]);
	'hmac-md5-96' ->
	    crypto:md5_mac_96(SSH#ssh.send_mac_key, [<<?UINT32(Seq)>>, Data]);
	_ ->
	    exit({bad_algorithm,SSH#ssh.send_mac})
    end.
	

recv_mac_init(SSH) ->
    case SSH#ssh.role of
	client ->
	    Key = hash(SSH, "F", mac_key_size(SSH#ssh.recv_mac)),
	    {ok, SSH#ssh { recv_mac_key = Key }};
	server ->
	    Key = hash(SSH, "E", mac_key_size(SSH#ssh.recv_mac)),
	    {ok, SSH#ssh { recv_mac_key = Key }}
    end.

recv_mac_final(SSH) ->
    {ok, SSH#ssh { recv_mac = none, recv_mac_key = undefined }}.

recv_mac(SSH, Data, Seq) ->
    case SSH#ssh.recv_mac of
	none -> 
	    <<>>;
	'hmac-sha1' ->
	    crypto:sha_mac(SSH#ssh.recv_mac_key, [<<?UINT32(Seq)>>, Data]);
	'hmac-sha1-96' ->
	    crypto:sha_mac_96(SSH#ssh.recv_mac_key, [<<?UINT32(Seq)>>, Data]);
	'hmac-md5' ->
	    crypto:md5_mac(SSH#ssh.recv_mac_key, [<<?UINT32(Seq)>>, Data]);
	'hmac-md5-96' ->
	    crypto:md5_mac_96(SSH#ssh.recv_mac_key, [<<?UINT32(Seq)>>, Data]);
	_ ->
	    exit({bad_algorithm,SSH#ssh.recv_mac})
    end.


%% return N hash bytes (HASH)
hash(SSH, Char, Bits) ->
    HASH =
	case SSH#ssh.kex of
	    'diffie-hellman-group1-sha1' ->
		fun(Data) -> crypto:sha(Data) end;
	    'diffie-hellman-group-exchange-sha1' ->
		fun(Data) -> crypto:sha(Data) end;
	    _ ->
		exit({bad_algorithm,SSH#ssh.kex})
	end,
    hash(SSH, Char, Bits, HASH).

hash(SSH, Char, 0, HASH) ->
    <<>>;
hash(SSH, Char, N, HASH) ->
    K = ssh_bits:mpint(SSH#ssh.shared_secret),
    H = SSH#ssh.exchanged_hash,
    SessionID = SSH#ssh.session_id,
    K1 = HASH([K, H, Char, SessionID]),
    Sz = N div 8,
    <<Key:Sz/binary, _/binary>> = hash(K, H, K1, N-128, HASH),
    ?dbg(?DBG_KEX, "Key ~s: ~s\n", [Char, fmt_binary(Key, 16, 4)]),
    Key.

hash(K, H, Ki, N, HASH) when N =< 0 ->
    Ki;
hash(K, H, Ki, N, HASH) ->
    Kj = HASH([K, H, Ki]),
    hash(K, H, <<Ki/binary, Kj/binary>>, N-128, HASH).
%%
%% calcuation of H (diffie-hellman-group1-sha1)
%% Must use ssh#ssh.algorithms here because new algorithms
%% are not install at this point
%%
kex_h(SSH, K_S, E, F, K) ->
    L = ssh_bits:encode([SSH#ssh.c_version, SSH#ssh.s_version,
			 SSH#ssh.c_keyinit, SSH#ssh.s_keyinit,
			 K_S, E,F,K],
			[string,string,string,string,string,
			 mpint,mpint,mpint]),
    crypto:sha(L).

kex_h(SSH, K_S, Min, NBits, Max, Prime, Gen, E, F, K) ->
    L = if Min==-1; Max==-1 ->
		Ts = [string,string,string,string,string,
		      uint32,
		      mpint,mpint,mpint,mpint,mpint],
		ssh_bits:encode([SSH#ssh.c_version,SSH#ssh.s_version,
				 SSH#ssh.c_keyinit,SSH#ssh.s_keyinit,
				 K_S, NBits, Prime, Gen, E,F,K],
				Ts);
	   true ->
		Ts = [string,string,string,string,string,
		      uint32,uint32,uint32,
		      mpint,mpint,mpint,mpint,mpint],
		ssh_bits:encode([SSH#ssh.c_version,SSH#ssh.s_version,
				 SSH#ssh.c_keyinit,SSH#ssh.s_keyinit,
				 K_S, Min, NBits, Max,
				 Prime, Gen, E,F,K], Ts)
	end,
    crypto:sha(L).
    
    


mac_key_size('hmac-sha1')    -> 20*8;
mac_key_size('hmac-sha1-96') -> 20*8;
mac_key_size('hmac-md5')     -> 16*8;
mac_key_size('hmac-md5-96')  -> 16*8;
mac_key_size(none) -> 0;
mac_key_size(_) -> exit(bad_algoritm).

mac_digest_size('hmac-sha1')    -> 20;
mac_digest_size('hmac-sha1-96') -> 12;
mac_digest_size('hmac-md5')    -> 20;
mac_digest_size('hmac-md5-96') -> 12;
mac_digest_size(none) -> 0;
mac_digest_size(_) -> exit(bad_algoritm).

integrity_char(send, client) -> "E";
integrity_char(recv, server) -> "E";
integrity_char(send, server) -> "F";
integrity_char(recv, client) -> "F".
    
valid_mac(SSH, S, Data, Seq) ->
    if SSH#ssh.recv_mac_size == 0 ->
	    true;
       true ->
	    {ok,MAC0} = gen_tcp:recv(S, SSH#ssh.recv_mac_size),
	    ?dbg(?DBG_MAC, "~p: MAC0=~p\n", [Seq, MAC0]),
	    MAC1 = recv_mac(SSH, Data, Seq),
	    ?dbg(?DBG_MAC, "~p: MAC1=~p\n", [Seq, MAC1]),
	     MAC0 == MAC1
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Diffie-Hellman utils
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dh_group1() ->
    {2, 16#FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE65381FFFFFFFFFFFFFFFF}.

dh_gen_key(G,P, Bits) ->
    Q = (P-1) div 2,
    Private = ssh_bits:irandom(ssh_bits:isize(P)-1, 1, 1),
    Public = ssh_math:ipow(G, Private, P),
    {Private,Public}.

trim(Str) ->
    reverse(trim_head(reverse(trim_head(Str)))).

trim_tail(Str) ->
    reverse(trim_head(reverse(Str))).

trim_head([$\s|Cs]) -> trim_head(Cs);
trim_head([$\t|Cs]) -> trim_head(Cs);
trim_head([$\n|Cs]) -> trim_head(Cs);
trim_head([$\r|Cs]) -> trim_head(Cs);
trim_head(Cs) -> Cs.
%%
%% DEBUG utils
%% Format integers and binaries as hex blocks
%%
fmt_binary(B) ->
    fmt_binary(B, 0, 0).

fmt_binary(B, BlockSize) ->
    fmt_binary(B, BlockSize, 0).

fmt_binary(B, BlockSize, GroupSize) ->
    fmt_block(fmt_bin(B), BlockSize, GroupSize).

fmt_block(Bin, BlockSize, GroupSize) ->
    fmt_block(Bin, BlockSize, 0, GroupSize).
    

fmt_block(Bin, 0, I, G) ->
    binary_to_list(Bin);
fmt_block(Bin, Sz, G, G) when G =/= 0 ->
    ["\n" | fmt_block(Bin, Sz, 0, G)];
fmt_block(Bin, Sz, I, G) ->
    case Bin of
	<<Block:Sz/binary, Tail/binary>> ->
	    if Tail == <<>> ->
		    [binary_to_list(Block)];
	       true ->
		    [binary_to_list(Block), " " | fmt_block(Tail, Sz, I+1, G)]
	    end;
	<<>> ->
	    [];
	_ -> 
	    [binary_to_list(Bin)]
    end.

%% Format integer or binary as hex
fmt_bin(X) when integer(X) ->
    list_to_binary(io_lib:format("~.16B", [X]));
fmt_bin(X) when binary(X) ->
    Sz = size(X)*8,
    <<Y:Sz/unsigned-big>> = X,
    Fmt = "~"++integer_to_list(size(X)*2)++".16.0B",
    list_to_binary(io_lib:format(Fmt, [Y])).


    
    
