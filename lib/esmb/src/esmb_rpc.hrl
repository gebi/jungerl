-ifndef(_ESMB_RPC_HRL).
-define(_ESMB_RPC_HRL, true).


%%% -------------------------------------------------------------------
%%% --- DCE/RPC specific declarations
%%% -------------------------------------------------------------------
%%% The major version number for the connection-oriented protocol is 5.
%%% 
%%% The minor version numbers for the connection-oriented protocol are 
%%% either 0 (zero) or 1.
%%% 
%%% The connection-oriented minor version number of 1 supports security 
%%% credentials larger than 1400 bytes. The state machine automatically 
%%% falls back to the minor version number 0 (zero) style of security 
%%% credentials should the server return an RPC version mismatch error.
%%% 
%%% The version number for the connectionless protocol is 4.
%%%
-define(RPC_VERSION,            5).
-define(RPC_MINOR_VERSION,      0).
%%% See p.575
-define(RPC_OP_REQUEST,         0).
-define(RPC_OP_PING,            1).
-define(RPC_OP_RESPONSE,        2).
-define(RPC_OP_FAULT,           3).
-define(RPC_OP_REJECT,          6).
-define(RPC_OP_ACK,             7).
-define(RPC_OP_BIND,           11).
-define(RPC_OP_BIND_ACK,       12).
-define(RPC_OP_BIND_NACK,      13).
%%%
%%% Packet Flags, 1 byte (as big-endian values...)
%%%
%%% 16#80  =  Object: Not set(0)
%%% 16#40  =  Maybe: Not set(0)
%%% 16#20  =  Did not execute: Not set(0)
%%% 16#10  =  Multiplex: Not set(0)
%%% 16#08  =  Reserved: Not set(0)
%%% 16#04  =  Cancel pending: Not set(0)
%%% 16#02  =  Last frag: Not set(0)
%%% 16#01  =  First frag: Not set(0)
%%%
-define(PACKET_FLAGS,        16#3).  % Last frag(1), First frag(1)
%%%
%%% Data representation, 4 bytes
%%%
%%% Byte order: Little-endian(1)  , 4 bits
%%% Character: ASCII(0)           , 4 bits
%%% Floating-point: IEEE(0)       , 1 byte
%%% Reserved: 0                   , 2 bytes
%%%                                       
-define(DATA_REPRESENTATION,  16#10000000). % A 4-byte vector !
%%%
-define(MAX_XMIT_FRAG,    4280).  % ?
-define(MAX_RECV_FRAG,    4280).  % ?
%%%
-define(DCE_VERSION,      16#1000).  % urk...
-define(DCE_VARIANT,      16#80).    % urk... DCE variant
-define(CLK_SEQ,          16#0101).  % hardcoded random number...
%%%
-define(IFACE_VER,         3).
-define(IFACE_VER_MINOR,   0).
-define(SYNTAX_VERSION,    2).


%%% -------------------------------------------------------------------
%%% --- LSA specific declarations
%%% -------------------------------------------------------------------
-define(LSA_OP_OPEN_POLICY,     0).


%%% -------------------------------------------------------------------
%%% --- Server Service specific declarations
%%% -------------------------------------------------------------------
-define(OP_SS_NETR_SHARE_ENUM,  15).


%%% -------------------------------------------------------------------
%%% --- Security Account Manager stuff
%%% -------------------------------------------------------------------
-define(OP_SAMR_CLOSE_HANDLE,                     1).
-define(OP_SAMR_LOOKUP_DOMAIN_IN_SAM_SERVER,      5).
-define(OP_SAMR_ENUMERATE_DOMAINS_IN_SAM_SERVER,  6).
-define(OP_SAMR_OPEN_DOMAIN,                      7).
-define(OP_SAMR_LOOKUP_NAMES_IN_DOMAIN,          17).
-define(OP_SAMR_LOOKUP_RIDS_IN_DOMAIN,           18).
-define(OP_SAMR_OPEN_USER,                       34).
-define(OP_SAMR_GET_GROUPS_FOR_USER,             39).
-define(OP_SAMR_CONNECT2,                        57).

-define(SAMR_ACCESS_MASK,  16#02000000).  % see ethereal...


%%% Security Identifier
%%%
%%%  http://msdn.microsoft.com/library/default.asp?url=/library/en-us/security/accctrl_38yn.asp
%%%
%%% The values in sub-auths uint32's are in *native* byteorder, not
%%% neccessarily little-endian....
%%%
-define(SID(SidRev,NumAuth,IdAuth,SubAuths,Rest),
	<<SidRev,             % Security Identifier revision number
	  NumAuth,            % Number of sub-authoraties
	  IdAuth:6/binary,    % Identifier authority
	  SubAuths:NumAuth/binary-unit:32, % Pointer to sub-auths
	  Rest/binary>>).





%%% Internal stuff

%%% "service type" (?)
-define(ST_SRVSVC,  0).
-define(ST_SAMR,    1).


-record(rpc_bind_ack, {
	  assoc_group,
	  ack_result,
	  prov_reason
	  }).

-record(rpc_bind_nack, {
	  prov_reason
	  }).

-record(rpc_response, {
	  frag_len,
	  alloc_hint,
	  call_id,
	  ctx_id,
	  cancel_count,
	  data
	  }).



-define(DIR,              16#00000000).
-define(PRINTER_QUEUE,    16#00000001).
-define(HIDDEN_DIR,       16#80000000).
-define(HIDDEN_IPC,       16#80000003).

-record(nse_entry, {
	  type,
	  name    = "",
	  comment = ""
	 }).


-record(dom_entry, {
	  index,
	  len,
	  size,
	  domain = "",
	  sid
	 }).

%%%
%%% Name Types
%%%
-define(NT_USER_NAME,             16#1).
-define(NT_DOMAIN_GROUP_NAME,     16#2).
-define(NT_DOMAIN_NAME,           16#3).
-define(NT_ALIAS_NAME,            16#4).  % Alias == Local Group
-define(NT_WELL_KNOWN_GROUP_NAME, 16#5).
-define(NT_DELETED_NAME,          16#6).
-define(NT_INVALID_NAME,          16#7).
-define(NT_UNKNOWN_NAME,          16#8).

-record(name_entry, {
	  name,
	  type,
	  rid
	 }).



-endif.
