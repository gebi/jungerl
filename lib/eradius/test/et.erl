-module(et).
%%%----------------------------------------------------------------------
%%% File    : et.erl
%%% Author  : Torbjorn Tornkvist <tobbe@bluetail.com>
%%% Purpose : eradius test code
%%% Created : 25 Sep 2003 by Torbjorn Tornkvist <tobbe@bluetail.com>
%%%----------------------------------------------------------------------
-export([local/0, duva/1, acc/0]).
-export([go/5]).

-include_lib("kernel/include/inet.hrl").
-include("eradius.hrl").
-include("eradius_lib.hrl").
-include("eradius_dict.hrl").
-include("dictionary_alteon.hrl").

-import(eradius_acc, 
	[set_user/2, set_nas_ip_address/1, set_nas_ip_address/2,
	 set_login_time/1, set_logout_time/1, set_session_id/2, new/0,
	 set_radacct/1, set_attr/3, set_vend_attrs/2, acc_update/1,
	 set_servers/2, set_timeout/2, set_login_time/2, set_vendor_id/2,
	 set_logout_time/2, set_tc_ureq/1, set_tc_itimeout/1,
	 set_tc_areset/1, set_tc_areboot/1, set_tc_nasreboot/1]).


%%% Radius shortcuts
local() -> 
    go({127,0,0,1}, "tobbe", "qwe123", "qwe123", {127,0,0,1}).

duva(Passwd) ->
    go({192,168,128,1}, "support", Passwd, Passwd, {192,168,128,32}).

%%% Server for Radius accounting
radacct_servers() ->
    %%
    %% list_of( [IP, Port, SharedSecret] )
    %%
    [[{192,168,128,51}, 1813, "mortuta42"]].


%%% --------------------------------
%%% Radius authentication test case.
%%% --------------------------------

go(IP, User, Passwd, Shared, NasIP) ->
    TraceFun = fun(_E,Str,Args) ->
		       io:format(Str,Args),
		       io:nl()
	       end,
    E = #eradius{servers = [[IP, 1812, Shared]],
		 user = User,
		 passwd = Passwd,
		 tracefun = TraceFun,
		 nas_ip_address = NasIP},
    eradius:start(),
    eradius:load_tables(["dictionary",
			 "dictionary_alteon",
			 "dictionary_ascend"]),
    print_result(eradius:auth(E)).

print_result({accept, Attributes}) ->
    io:format("Got 'Accept' with attributes: ~p~n",[Attributes]),
    pa(Attributes);
print_result({reject, Attributes}) ->
    io:format("Got 'Reject' with attributes: ~p~n",[Attributes]),
    pa(Attributes);
print_result(Res) ->
    io:format("Got: ~p~n",[Res]).

pa([{K, V} | As]) ->
    case eradius_dict:lookup(K) of
	[A] ->
	    io:format("     ~s = ~p~n",[A#attribute.name, 
					to_list(V, A#attribute.type)]);
	_ -> 
	    io:format("  <not found in dictionary>: ~p~n", [{K,V}])
    end,
    pa(As);
pa([]) ->
    true.


%%% --------------------------------
%%% Radius accounting test case.
%%% --------------------------------

%%% Reasons for session termination
-define(REASON_LOGOUT,      1).
-define(REASON_TIMEOUT,     2).
-define(REASON_RESET,       3).
-define(REASON_REBOOT,      4).
-define(REASON_TERMINATE,   5).


acc() ->
    eradius:start(),
    eradius_acc:start(),
    eradius:load_tables(["dictionary",
			 "dictionary_alteon"]),
    User = "tobbe",
    SessionId = 42,
    R = acc_start(User, SessionId),
    Login = R#rad_accreq.login_time,
    sleep(10),
    VendAttrs = [{?Alteon, [{?Alteon_ASA_Audit_Trail, "This is a test!"}]}],
    acc_update(User, SessionId, VendAttrs),
    sleep(10),
    acc_stop(User, SessionId, Login, ?REASON_LOGOUT).


acc_start(User, SessId) ->
    Srvs = radacct_servers(),
    NasIP = nas_ip_address(),
    A = eradius_acc:new(),
    R = set_session_id(
	  set_user(
	    set_servers(
	      set_nas_ip_address(
		set_login_time(A),
		NasIP),
	      Srvs),
	    User),
	  SessId),
    eradius_acc:acc_start(R),
    R.


acc_stop(User, SessId, Login, Reason) ->
    Srvs = radacct_servers(),
    NasIP = nas_ip_address(),
    Logout = erlang:now(),
    A = eradius_acc:new(),
    R = set_stop_reason(
	  set_logout_time(
	    set_login_time(
	      set_session_id(
		set_user(
		  set_servers(
		    set_nas_ip_address(A, NasIP),
		    Srvs),
		  User),
		SessId),
	      Login),
	    Logout),
	  Reason),
    eradius_acc:acc_stop(R),
    R.

acc_update(User, SessId, VendAttrs) ->
    Srvs = radacct_servers(),
    NasIP = nas_ip_address(),
    A = eradius_acc:new(),
    R = set_vend_attrs(
	  set_session_id(
	    set_user(
	      set_servers(
		set_nas_ip_address(A, NasIP),
		Srvs),
	      User),
	    SessId),
	  VendAttrs),
    eradius_acc:acc_update(R),
    R.


set_stop_reason(R, ?REASON_LOGOUT)    -> set_tc_ureq(R);
set_stop_reason(R, ?REASON_TIMEOUT)   -> set_tc_itimeout(R);
set_stop_reason(R, ?REASON_RESET)     -> set_tc_areset(R);
set_stop_reason(R, ?REASON_REBOOT)    -> set_tc_areboot(R);
set_stop_reason(R, ?REASON_TERMINATE) -> set_tc_nasreboot(R).

%%% -----------------
%%% Misc. stuff
%%% -----------------

%%% Our own IP address
nas_ip_address() ->
    case catch inet:gethostbyname(element(2,inet:gethostname())) of
	{ok,H} when record(H,hostent) ->
	    hd(H#hostent.h_addr_list);
	_ ->
	    io:format("WARNING: failed to get local IP address!~n",[]),
	    "192.168.42.42"
    end.

sleep(Sec) ->
    receive after Sec*1000 -> true end.
    

to_list(B, string)  -> binary_to_list(B);
to_list(B, octets)  -> B;
to_list(B, integer) -> b2i(B);
to_list(B, ipaddr)  -> b2ip(B);
to_list(D, date)    -> D.  % FIXME !
		
b2i(<<I:32>>)          -> I;
b2i(<<I:16>>)          -> I;
b2i(<<I:8>>)           -> I;
b2i(I) when integer(I) -> I.

b2ip(<<A:8,B:8,C:8,D:8>>) -> {A,B,C,D};
b2ip({A,B,C,D})           -> {A,B,C,D}.
    
    
