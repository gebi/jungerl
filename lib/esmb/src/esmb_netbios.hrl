-ifndef(_ESMB_NETBIOS_HRL).
-define(_ESMB_NETBIOS_HRL, true).

%%% --------------------------------------------------------------------
%%% NetBIOS stuff
%%% --------------------------------------------------------------------

-define(NETBIOS_NS_PORT,   137).
-define(NETBIOS_DGM_PORT,  138).

-define(NETBIOS_NAME_LEN, 15).
-define(NETBIOS_SX_WORKSTATION,       16#00).  % Workstation service
-define(NETBIOS_SX_FILESERVER,        16#20).  % File server service
-define(NETBIOS_LOCAL_MASTER_BROWSER, 16#1d).  % Local Master Browser

%%% NetBIOS messages
-define(SESSION_SERVICE,           16#00).
-define(SESSION_REQUEST,           16#81).
-define(POSITIVE_SESSION_RESPONSE, 16#82).
-define(NEGATIVE_SESSION_RESPONSE, 16#83).
-define(RETARGET_SESSION_RESPONSE, 16#84).
-define(SESSION_KEEP_ALIVE,        16#85).


-define(DGM_MTYPE_DIRECT_UNIQUE,   16#10).
-define(DGM_MTYPE_DIRECT_GROUP,    16#11).
-define(DGM_MTYPE_BROADCAST,       16#12).
-define(DGM_MTYPE_ERROR,           16#13).
-define(DGM_MTYPE_QUERY_REQ,       16#14).
-define(DGM_MTYPE_POS_QUERY_RESP,  16#15).
-define(DGM_MTYPE_NEG_QUERY_RESP,  16#16).

-define(DGM_FLAGS_MORE,    16#01).    % bit(7)
-define(DGM_FLAGS_FIRST,   16#02).    % bit(6)
-define(DGM_FLAGS_BNODE,   16#00).    % bit(4,5)

-define(DGM_FLAGS_FIRST_BNODE, (?DGM_FLAGS_FIRST bor ?DGM_FLAGS_BNODE)).

-record(netbios_dgm, {
	  sock,             % A opened UDP socket
	  msg_type,         % Message type
	  flags,            %
	  dgm_id,           % 
	  src_ip,           % Source IP
	  src_port,         % Source port
	  src_name,
	  dst_name,
	  data              % The Data as a binary.
	 }).


-define(NS_QUERY_REQ,   0).
-define(NS_QUERY_RESP,  1).

%%%
%%% NBT: NetBIOS over TCP/IP
%%%
-record(nbt, {             
	  src_ip,          % Source IP address
	  op,              % Type of operation
	  tid,             % Transaction ID
	  name             % NetBIOS name in message
	  }).


-endif.
