-ifndef(_DHCP_LIB).
-define(_DHCP_LIB, true).


%%% DHCP operation
-define(DHCP_BOOTREQUEST,     1).
-define(DHCP_BOOTREPLY,       2).

%%% Message types
-define(DHCPDISCOVER,     1).
-define(DHCPOFFER,        2).
-define(DHCPREQUEST,      3).
-define(DHCPDECLINE,      4).
-define(DHCPACK,          5).
-define(DHCPNAK,          6).
-define(DHCPRELEASE,      7).

%%% Option tags (see also rfc1533)
-define(DHCP_OP_PAD,              0).
-define(DHCP_OP_SUBNET_MASK,      1).
-define(DHCP_OP_ROUTERS,          3).
-define(DHCP_OP_DNS_SRVS,         6).
-define(DHCP_OP_DOMAIN_NAME,     15).
-define(DHCP_OP_BCAST_ADDR,      28).
-define(DHCP_OP_VENDOR,          43).
-define(DHCP_OP_NBNS_SRVS,       44).
-define(DHCP_OP_REQUESTED_IP,    50).
-define(DHCP_OP_LEASE_TIME,      51).
-define(DHCP_OP_MSGTYPE,         53).
-define(DHCP_OP_SRV_ID,          54).
-define(DHCP_OP_REQ_PARAMS,      55).
-define(DHCP_OP_RENEWAL_TIME,    58).
-define(DHCP_OP_REBINDING_TIME,  59).
-define(DHCP_OP_VENDOR_CLASS,    60).
-define(DHCP_OP_CLIENT_ID,       61).
-define(DHCP_OP_END,            255).

-record(dhcp, {
	  op       = ?DHCP_BOOTREQUEST,  % make it easy for clients
	  htype    = 1,     % 10mb ethernet
	  hlen     = 6,     % 10mb ethernet
	  hops     = 0,
	  xid      = 0,
	  secs     = 0,
	  flags    = 0,
	  ciaddr   = 0,
	  yiaddr   = 0,
	  siaddr   = 0,
	  giaddr   = 0,
	  chaddr   = <<>>,
	  sname    = <<>>,
	  file     = <<>>,
	  options  = [],   % list of {Tag, Value}
	  msg_type = 0
	  }).

%%%
%%% NB: If the 'giaddr' is zero; then we will try and
%%%     figure out what IP address we should use. We
%%%     do that by connecting to the server and then
%%%     using the IP address our end of the socket got.
%%%
-record(dhcp_alloc, {
	  srv_ips = [],     % List of DHCP server IP addresses
	  giaddr  = 0,      % Relay IP address
	  cb_mod,           % Callback module (for trace and release)
	  cb_data,          % Opaque callback data
	  v_class = "",     % Vendor class string
	  sock_opts = [],   % List of socket options
	  requested_ops = [] % List of requestes option codes
	 }).

-endif.
