%%
%% SSH definitions
%%

-define(SSH_DEFAULT_PORT, 22).
-define(SSH_MAX_PACKET_SIZE, (256*1024)).

-define(FALSE, 0).
-define(TRUE,  1).
%% basic binary constructors
-define(BOOLEAN(X),  X:8/unsigned-big-integer).
-define(BYTE(X),     X:8/unsigned-big-integer).
-define(UINT16(X),   X:16/unsigned-big-integer).
-define(UINT32(X),   X:32/unsigned-big-integer).
-define(UINT64(X),   X:64/unsigned-big-integer).
-define(STRING(X),   ?UINT32((size(X))), (X)/binary).

%% building macors
-define(boolean(X),
	case X of
	    true -> <<?BOOLEAN(1)>>;
	    false -> (<<?BOOLEAN(0)>>)
	end).

-define(byte(X),   << ?BYTE(X) >> ).
-define(uint16(X), << ?UINT16(X) >> ).
-define(uint32(X), << ?UINT32(X) >> ).
-define(uint64(X), << ?UINT64(X) >> ).

-define(string(X),
	if list(X) ->
		(fun(__B) -> (<<?STRING(__B)>>) end)(list_to_binary(X));
	   atom(X) ->
		(fun(__B) -> (<<?STRING(__B)>>) end)(list_to_binary(atom_to_list(X)));
	   binary(X) ->
		(<<?STRING(X)>>)
	end).

-define(binary(X),
	<<?STRING(X)>>).

-ifdef(debug).
-define(dbg(Debug, Fmt, As),
	if (Debug) == true ->
		io:format((Fmt), (As));
	   true ->
		ok
	end).
-else.
-define(dbg(Debug, Fmt, As), ok).
-endif.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% BASIC transport messages
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SSH_MSG_DISCONNECT,             1).
-define(SSH_MSG_IGNORE,                 2).
-define(SSH_MSG_UNIMPLEMENTED,          3).
-define(SSH_MSG_DEBUG,                  4).
-define(SSH_MSG_SERVICE_REQUEST,        5).
-define(SSH_MSG_SERVICE_ACCEPT,         6).

-define(SSH_MSG_KEXINIT,                20).
-define(SSH_MSG_NEWKEYS,                21).


-record(ssh_msg_disconnect,
	{
	  code,         %% uint32
	  description,  %% string
	  language      %% string
	 }).

-record(ssh_msg_ignore,
	{
	  data          %% string
	 }).

-record(ssh_msg_unimplemented,
	{
	  sequence     %% uint32
	 }).

-record(ssh_msg_debug,
	{
	  always_display,  %% boolean
	  message,         %% string
	  language         %% string
	 }).


-record(ssh_msg_service_request,
	{
	  name     %% string (service name)
	 }).

-record(ssh_msg_service_accept,
	{
	  name     %% string
	 }).

-record(ssh_msg_kexinit,
	{
	  cookie,                                   %% random(16)
	  kex_algorithms,                           %% string
	  server_host_key_algorithms,               %% string    
	  encryption_algorithms_client_to_server,   %% string    
	  encryption_algorithms_server_to_client,   %% string    
	  mac_algorithms_client_to_server,          %% string
	  mac_algorithms_server_to_client,          %% string    
	  compression_algorithms_client_to_server,  %% string
	  compression_algorithms_server_to_client,  %% string
	  languages_client_to_server,               %% string
	  languages_server_to_client,               %% string
	  first_kex_packet_follows=false,           %% boolean
	  %% (reserved for future extension)
	  reserved=0                                %% uint32=0
	 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% KEY DH messages
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% diffie-hellman-group1-sha1
-define(SSH_MSG_KEXDH_INIT,  30).
-define(SSH_MSG_KEXDH_REPLY,  31).

-record(ssh_msg_kexdh_init,
	{
	  e  %% mpint
	 }).

-record(ssh_msg_kexdh_reply,
	{
	  public_host_key,  %% string (K_S)
	  f,                %% mpint
	  h_sig             %% string, signature of H
	 }).

-record(ssh_msg_newkeys,
	{}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% KEY DH GEX messages
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% diffie-hellman-group-exchange-sha1
-define(SSH_MSG_KEX_DH_GEX_REQUEST_OLD, 30).
-define(SSH_MSG_KEX_DH_GEX_REQUEST,     34).
-define(SSH_MSG_KEX_DH_GEX_GROUP,       31).
-define(SSH_MSG_KEX_DH_GEX_INIT,        32).
-define(SSH_MSG_KEX_DH_GEX_REPLY,       33).

-record(ssh_msg_kex_dh_gex_request,
	{
	  min,
	  n,
	  max
	 }).

-record(ssh_msg_kex_dh_gex_request_old,
	{
	  n
	 }).

-record(ssh_msg_kex_dh_gex_group,
	{
	  p,  %% prime
	  g   %% generator
	 }).

-record(ssh_msg_kex_dh_gex_init,
	{
	  e
	 }).

-record(ssh_msg_kex_dh_gex_reply,
	{
	  public_host_key,  %% string (K_S)
	  f,
	  h_sig
	 }).


-define(SSH_CIPHER_NONE, 0).
-define(SSH_CIPHER_3DES, 3).
-define(SSH_CIPHER_AUTHFILE, ?SSH_CIPHER_3DES).


-record(ssh_key,
	{
	  type,
	  public,
	  private,
	  comment = ""
	 }).

%% passed to shell program
-record(ssh_pty_params,
	{
	  terminal = "dumb",
	  width = 80,
	  height = 24,
	  pix_width = 0,
	  pix_height = 0,
	  pty_opts = []
	 }).


%% assertion macro
-define(ssh_assert(Expr, Reason),
	case Expr of
	    true -> ok;
	    _ -> exit(Reason)
	end).

%% error codes
-define(SSH_DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT,   1).
-define(SSH_DISCONNECT_PROTOCOL_ERROR,   2).
-define(SSH_DISCONNECT_KEY_EXCHANGE_FAILED,   3).
-define(SSH_DISCONNECT_RESERVED,   4).
-define(SSH_DISCONNECT_MAC_ERROR,   5).
-define(SSH_DISCONNECT_COMPRESSION_ERROR,   6).
-define(SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,   7).
-define(SSH_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED,   8).
-define(SSH_DISCONNECT_HOST_KEY_NOT_VERIFIABLE,   9).
-define(SSH_DISCONNECT_CONNECTION_LOST,  10).
-define(SSH_DISCONNECT_BY_APPLICATION,  11).
-define(SSH_DISCONNECT_TOO_MANY_CONNECTIONS,  12).
-define(SSH_DISCONNECT_AUTH_CANCELLED_BY_USER,  13).
-define(SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE,  14).
-define(SSH_DISCONNECT_ILLEGAL_USER_NAME,  15).


