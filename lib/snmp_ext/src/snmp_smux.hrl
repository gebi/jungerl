%%%-------------------------------------------------------------------
%%% File    : snmp_smux.hrl
%%% Author  :  <mbj@bluetail.com>
%%% Description : 
%%% Created :  3 Nov 2003 by  <mbj@bluetail.com>
%%%-------------------------------------------------------------------

-define(smux_version_1, 0).

-record(smux_open, {version,
		    identity,  % oid() 
		    description, % string()
		    password   % string()
		   }).


-define(close_goingDown, 0).
-define(close_unsupportedVersion, 1).
-define(close_packetFormat, 2).
-define(close_protocolError, 3).
-define(close_internalError, 4).
-define(close_authenticationFailure, 5).

-record(smux_close, {code
		     }).

-define(rreq_delete, 0).
-define(rreq_readOnly, 1).
-define(rreq_readWrite, 2).

-record(smux_rreq, {subtree,   % oid()
		    priority,  % -1..2147483647
		    operation  % delete | readOnly | readWrite
		   }).

-define(rrsp_failure, -1).

-record(smux_rrsp, {code       % failure(-1)
		    }).

-define(sout_commit, 0).
-define(sout_rollback, 1).

-record(smux_sout, {code       % commit | rollback
		    }).
