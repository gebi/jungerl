-ifndef(_ERADIUS_LIB).
-define(_ERADIUS_LIB , true).
%%%-------------------------------------------------------------------
%%% File        : oaml_radius_lib.hrl
%%% Author      : Martin Bjorklund <mbj@bluetail.com>
%%% Description : Definitions for RADIUS
%%% Created     :  7 Oct 2002 by Martin Bjorklund <mbj@bluetail.com>
%%%
%%% $Id$
%%%-------------------------------------------------------------------

-define(BYTE, integer-unit:8).    % Nice syntactic sugar...

%%- cmds
-define(RAccess_Request,         1).
-define(RAccess_Accept,          2).
-define(RAccess_Reject,          3).
-define(RAccounting_Request,     4).
-define(RAccounting_Response,    5).
-define(RAccess_Challenge,      11).

%%- attribs
-define(RUser_Name,              1).
-define(RUser_Passwd,            2).
-define(RNAS_Ip_Address,         4).
-define(RReply_Msg,             18).
-define(RState,                 24).
-define(RClass,                 25).
-define(RVendor_Specific,       26).
-define(RStatus_Type,           40).
-define(RSession_Id,            44).
-define(RSession_Time,          46).
-define(RTerminate_Cause,       49).

%%- attribute values
-define(RStatus_Type_Start,      1).
-define(RStatus_Type_Stop,       2).
-define(RStatus_Type_Update,     3).  % Interim-Update
-define(RStatus_Type_On,         7).
-define(RStatus_Type_Off,        8).

%%- Set radius accounting attributes
-define(ACC_ATTR(Key,Val), {Key,Val}).

%%- Terminate Cause values
-define(RTCUser_Request,           1).
-define(RTCIdle_Timeout,           4).
-define(RTCAdmin_Reset,            6).
-define(RTCAdmin_Reboot,           7).
-define(RTCNAS_Reboot,            11).


%%- Radius accounting server info.
-record( radacct , {
	   servers,      % a list of [Ip,Port,Secret] tripplets
	   timeout}).    % timeout in seconds


-record(rad_pdu, {
	  reqid,
	  authenticator,
	  cmd              % one of the records below
	 }).

-record(rad_request, {
	  user,
	  passwd = <<>>,
	  nas_ip,
	  state = <<>>
	 }).

-record(rad_accept, {
	  user,
	  vendor_specifics = [],
	  attribs = []
	 }).

-record(rad_challenge, {
	  state,
	  reply_msgs = []  % list of binaries
	 }).

-record(rad_reject, {
	  reply_msgs = []  % list of binaries
	 }).

-record(rad_accreq, {      % accounting request
	  status_type,
	  session_time,
	  login_time,      % erlang:now/0
	  logout_time,     % erlang:now/0
	  session_id,
	  vend_attrs = [], % list_of( {VendorId, list_of( {Id, Val} ) } )
	  std_attrs  = [], % list_of( {Id, Val} )
	  user,
	  nas_ip,
	  servers,         % overrides the #radacct{} content
	  timeout = 5000,  % -- "" --  , default is 5 seconds
	  term_cause}).

-record(rad_accresp, {}).  % accounting response


-endif.
