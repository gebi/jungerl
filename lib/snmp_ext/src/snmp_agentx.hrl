%%%-------------------------------------------------------------------
%%% File    : snmp_agentx.hrl
%%% Author  :  <mbj@bluetail.com>
%%% Description : 
%%% Created :  9 Nov 2003 by  <mbj@bluetail.com>
%%%-------------------------------------------------------------------

-include_lib("snmp/include/snmp_types.hrl").

-define(AX_VSN_1, 1).

-define(AX_OPEN,               1).
-define(AX_CLOSE,              2).
-define(AX_REGISTER,           3).
-define(AX_UNREGISTER,         4).
-define(AX_GET,                5).
-define(AX_GET_NEXT,           6).
-define(AX_GET_BULK,           7).
-define(AX_TEST_SET,           8).
-define(AX_COMMIT_SET,         9).
-define(AX_UNDO_SET,          10).
-define(AX_CLEANUP_SET,       11).
-define(AX_NOTIFY,            12).
-define(AX_PING,              13).
-define(AX_INDEX_ALLOCATE,    14).
-define(AX_INDEX_DEALLOCATE,  15).
-define(AX_ADD_AGENT_CAPS,    16).
-define(AX_REMOVE_AGENT_CAPS, 17).
-define(AX_RESPONSE,          18).


-define(AX_F_INSTANCE_REGISTRATION,    1). % bit 0
-define(AX_F_NEW_INDEX,                2). % bit 1
-define(AX_F_ANY_INDEX,                4). % bit 2
-define(AX_F_NON_DEFAULT_CTXT,         8). % bit 3
-define(AX_F_NETWORK_BYTE_ORDER,      16). % bit 4

-define(AX_INTEGER,             2).
-define(AX_OCTET_STRING,        4).
-define(AX_NULL,                5).
-define(AX_OBJECT_IDENTIFIER,   6).
-define(AX_IPADDRESS,          64).
-define(AX_COUNTER32,          65).
-define(AX_GAUGE32,            66).
-define(AX_TIMETICKS,          67).
-define(AX_OPAQUE,             68).
-define(AX_COUNTER64,          70).
-define(AX_NOSUCHOBJECT,      128).
-define(AX_NOSUCHINSTANCE,    129).
-define(AX_ENDOFMIBVIEW,      130).

-define(AX_REASON_OTHER,          1).
-define(AX_REASON_PARSE_ERROR,    2).
-define(AX_REASON_PROTOCOL_ERROR, 3).
-define(AX_REASON_TIMEOUTS,       4).
-define(AX_REASON_SHUTDOWN,       5).
-define(AX_REASON_BY_MANAGER,     6).


-record(sr, {
	  oid1,
	  included = 0,
	  oid2 = []
	 }).   

-record(ax_msg, {
	  hdr,
	  pdu
	 }).

-record(ax_hdr, {
	  version        = ?AX_VSN_1,
	  type,                     % must not be set by application
	  flags          = 0,       % must not be set by application
	  sessionID,
	  transactionID,
	  packetID
	 }).

-record(ax_open, {
	  timeout = 0,
	  id,
	  descr = ""
	 }).

-record(ax_close, {
	  reason = ?AX_REASON_OTHER
	 }).

-record(ax_register, {
	  context     = "",
	  timeout     = 0,
	  priority    = 127,
	  range_subid = 0,
	  subtree,
	  upper_bound = 0,
	  instance_registration = false
	 }).

-record(ax_unregister, {
	  context     = "",
	  priority    = 127,
	  range_subid = 0,
	  subtree,
	  upper_bound = 0
	 }).

-record(ax_get, {
	  context = "",
	  oids
	 }).

-record(ax_get_next, {
	  context = "",
	  srs
	 }).

-record(ax_get_bulk, {
	  context         = "",
	  non_repeaters,
	  max_repetitions,
	  srs
	 }).

-record(ax_test_set, {
	  context = "",
	  vbs
	 }).

-record(ax_notify, {
	  context = "",
	  vbs
	 }).

-record(ax_ping, {
	  context = ""
	 }).

-record(ax_index_allocate, {
	  context = "",
	  vbs,
	  new_index = false,
	  any_index = false
	 }).

-record(ax_index_deallocate, {
	  context = "",
	  vbs
	 }).

-record(ax_add_agent_caps, {
	  context = "",
	  id,
	  descr = ""
	 }).

-record(ax_remove_agent_caps, {
	  context = "",
	  id
	 }).

-record(ax_response, {
	  sysUpTime,
	  error,
	  index
	 }).

			      
