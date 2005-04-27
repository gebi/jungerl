%%% --------------------------------------------------------------------
%%% File    : esmb.erl
%%% Created : 10 Dec 2003 by Torbjorn Tornkvist <tobbe@bluetail.com>
%%%
%%% @doc Implementation of the NetBIOS/SMB protocol.
%%%     
%%%      <p>The <em>esmb</em> library makes it possible to communicate
%%%      with SMB servers. In particular Windows servers but also
%%%      other implementations of SMB such as
%%%       <a href="http://www.samba.org/">Samba</a>.
%%%      With <em>esmb</em> it is possible to list, create, remove 
%%%      directories and download, upload, delete files. It is also possible
%%%      to list shares, which uses DCE/RPC running over SMB.
%%%      A number DCE/RPC operations ({@link esmb_rpc}) are supported,
%%%      mainly to make it possible to retrieve which groups
%%%      a certain user belongs to.</p>
%%%
%%%      <h2>How to setup a SMB session</h2>
%%%
%%%      <p>When we want to perform a file share operation on a SMB server
%%%      we need to setup a SMB session. We begin by doing a TCP-connect 
%%%      to the SMB server on the standard port 139. This is followed by 
%%%      the following four steps:</p>
%%%
%%%      <p><dl>
%%%      <dt>NetBIOS session setup</dt>
%%%      <dd><p>
%%%         After a TCP connection is established a NetBIOS session is 
%%%         established between the client and the server. 
%%%      </p></dd>
%%%
%%%      <dt>Protocol negotiation</dt>
%%%      <dd><p>
%%%         The SMB protocol allows for the client and the server to 
%%%         negotiate for a compatible protocol version. 
%%%         The result will for example affect how authentication will be done. 
%%%         Another important parameter to be negotiated is whether to use Unicode 
%%%         or not, or if signing of SMB packets should be done.
%%%      </p></dd>
%%%
%%%      <dt>Authentication</dt>
%%%      <dd><p>
%%%         This is done by sending a Session-Setup SMB request, containing the 
%%%         Username and Password.  Depending on what was negotiated earlier the 
%%%         password may be in cleartext or encrypted. Also, depending on the 
%%%         protocol version used, there are two ways of encrypting the password.
%%%      </p></dd>
%%%
%%%      <dt>Resource request</dt>
%%%      <dd><p>
%%%         The client sends a SMB Tree-Connect request to the server containing 
%%%         the name of the disk share the client wants to access. As a result, 
%%%         the server will return a tree identifier (TID) that the client need 
%%%         to provide in all the succeeding protocol operations.
%%%      </p></dd></dl></p>
%%%      <p>
%%%      We are now ready to start issuing the SMB protocol messages that
%%%      represents the file system operations we want to perform.
%%%      </p>
%%%
%%%      <h2>How to use the ESMB library</h2>
%%%
%%%      <p>The esmb library is (per definition) stateless. That means
%%%      that the user has to maintain the esmb state which consists of
%%%      the Socket returned from {@link connect/1} and the Pdu record
%%%      that is returned from all the other API functions. The content
%%%      of the Pdu record contains dynamic information. So it is crucial
%%%      that the latest returned Pdu record is the one used in the next call
%%%      to an API function. This is extra important in case <em>signing
%%%      of SMB messages</em> is negotiated since a counter then is incremented
%%%      for each SMB message sent.
%%%      </p>
%%%      
%%%
%%% @author  Torbjörn Törnkvist <tobbe@bluetail.com>
%%% @reference "CIFS Technical Reference", from 
%%%    <a href="http://www.snia.org/">www.snia.org</a>
%%% @end
%%%
%%% $Id$
%%% --------------------------------------------------------------------
-module(esmb).
-export([ucase/1, lcase/1, check_dir/3, bin2ip/1,
	 connect/1, connect/2, connect/3, close/1, error_p/1,
	 user_logon/3, user_logon/4, emsg/3, get_user_groups/4,
	 tree_connect/4, tree_connect_ipc/4, tree_connect/5, 
	 list_dir/3, called/1, negotiate/1, sign_p/1, ssn/1,
	 open_file_ro/3, open_file_rw/3, stream_read_file/3,
	 read_file/3, mkdir/3, rmdir/3, is_ok/2, unicode_p/1,
	 astart/0, istart/0, ustart/0, start/0, to_ucs2_and_null/2,
	 client/2, aclient/2, iclient/2, uclient/2, to_ucs2/2,
	 close_file/2, close_file/3, write_file/4, write_file/3, 
	 hexprint/1, b2l/1, do_ls/3, mailslot_browse_transaction/2,
	 delete_file/3, caller/0, named_pipe_transaction/4,
	 named_pipe_transaction/3, ucs2_to_ascii/1, ip2bin/1,
	 exit_if_error/2, list_shares/3, list_shares/5, 
	 dec_transaction_req/2, list_server_rap/3,
	 list_shares_old/3, list_shares_old/5, l/3, parse_uri/1,
	 smb_read_andx_pdu/2, decode_smb_response/2]).

-export([b2l/1, ip2bin/1, ip2str/1, lcase/1, sizeof/1]).

-export([mac_test/0,mac_test2/1,nt_key_test/2,lm_key_test/2]).

-import(esmb_netbios, [called_name/1, calling_name/1, nbss_session_request/3,
		       nbss_session_service/2, nbss_datagram_service/2]).


-include("esmb_lib.hrl").
-include("esmb_rpc.hrl").



-define(sdbg(S,A), true).
%%-define(sdbg(S,A), ?elog(S,A)).

%%% ==============================================
%%% ====== T Y P E  D E C L A R A T I O N S ======
%%% ==============================================
%%%
%%% @type pdu(). The smbpdu{} record.
%%%
%%% @type neg(). The smb_negotiate_res{} record.
%%%
%%% @type finfo(). A list of file_info{} records.
%%%
%%% @type exception(). Raises a throw(Error) exception.
%%%
%%% @type continuation(). A continuation to be called as: Cont().
%%%
%%% @type bstring() = string() | binary()
%%%


%%% @hidden

start()->
    iconv:start(),
    md4:start().

%%%---------------------------------------------------------------------
%%% Interface to esmb_client:start/N
%%% Example, run it as:
%%%
%%%  erl -pa ./ebin -noshell -s esmb astart   % ASCII capable
%%%  erl -pa ./ebin -noshell -s esmb istart   % ISO-8859-1 capable
%%%
%%% or in a utf-8 Xterm (xterm -en utf8):
%%%
%%%  erl -pa ./ebin -noshell -s esmb ustart   % UTF-8 capable
%%% 
%%%---------------------------------------------------------------------
%%% @hidden
astart() -> aclient("//pungmes/tobbe", "tobbe"). % for testing !!
%%% @hidden
istart() -> iclient("//pungmes/tobbe", "tobbe"). % for testing !!
%%% @hidden
ustart() -> uclient("//pungmes/tobbe", "tobbe"). % for testing !!

%%% @hidden
client(Path, User)  -> iclient(Path, User).

%%% @hidden
aclient(Path, User) -> esmb_client:astart(Path, User).
%%% @hidden
iclient(Path, User) -> esmb_client:istart(Path, User).
%%% @hidden
uclient(Path, User) -> esmb_client:ustart(Path, User).

	    
%%%
%%% SMB URI's "according" to: draft-crhertel-smb-url-08.txt
%%% Or rather:
%%%
%%%  smb://[[[authdomain;]user@]host[:port]/[share/[path/][file]][?context]
%%%
parse_uri("smb://" ++ T) ->
    catch parse_uri(T, #user{}).

parse_uri(Str, U) ->
    case parse_uri_part(Str) of
	{Tok, $;, Rest} ->
	    parse_uri(Rest, U#user{auth_domain = Tok});
	{Tok, $@, Rest} ->
	    parse_uri(Rest, U#user{name = Tok});
	{Tok, $:, Rest} ->
	    parse_port(Rest, U#user{host = Tok});
	{Tok, $/, Rest} ->
	    parse_share(Rest, U#user{host = Tok});
	Host ->
	    U#user{host = Host}
    end.

parse_uri_part(Str) ->
    parse_uri_part(Str, []).

parse_uri_part([$;|T], Acc) ->
    {lists:reverse(Acc), $;, T};
parse_uri_part([$@|T], Acc) ->
    {lists:reverse(Acc), $@, T};
parse_uri_part([$:|T], Acc) ->
    {lists:reverse(Acc), $:, T};
parse_uri_part([$/|T], Acc) ->
    {lists:reverse(Acc), $/, T};
parse_uri_part([H|T], Acc) ->
    parse_uri_part(T, [H|Acc]);
parse_uri_part([], Acc) ->
    lists:reverse(Acc).

parse_port(Str, U) ->
    parse_port(Str, U, []).

parse_port([$/|T], U, Acc) ->
    parse_share(T, U#user{port = l2i_x(lists:reverse(Acc))});
parse_port([H|T], U, Acc) ->
    parse_port(T, U, [H|Acc]);
parse_port([], U, Acc) ->
    U#user{port = l2i_x(lists:reverse(Acc))}.

parse_share(Str, U) ->
    parse_share(Str, U, []).

parse_share([$/|T], U, Acc) ->
    parse_path(T, U#user{share = lists:reverse(Acc)});
parse_share([H|T], U, Acc) ->
    parse_share(T, U, [H|Acc]);
parse_share([], U, Acc) ->
    U#user{share = lists:reverse(Acc)}.

parse_path(Str, U) ->
    parse_path(Str, U, []).

parse_path([$?|T], U, Acc) ->
    U#user{path = lists:reverse(Acc), context = T};
parse_path([H|T], U, Acc) ->
    parse_path(T, U, [H|Acc]);
parse_path([], U, Acc) ->
    U#user{path = lists:reverse(Acc)}.


l2i_x(Str) ->
    case catch list_to_integer(Str) of
	I when integer(I) -> I;
	_ -> throw({error, "Port is not an integer value"})
    end.


%%%---------------------------------------------------------------------
%%% Holy cow ! It turns out ugly already from the beginning.
%%% 
%%% "list shares" is using the Lanman Remote API protocol
%%% which runs on top of SMB. So right now we do some
%%% special treatment for just this message.
%%%
%%% See also: <http://www.thursby.com/CIFS/rap/>
%%%
%%%---------------------------------------------------------------------
-define(DOLLAR, [36]).   

%%% @hidden
l(Host, User, Passwd) ->
    case catch list_shares(Host, User, Passwd) of
	{ok, Res} ->
	    io:format("\n~-15.s ~s~n",["SHARE","TYPE"]),
	    io:format("~s~n",[string:copies("-",30)]),
	    F = fun(X) -> io:format("~-15.s ~s~n",
				   [b2l(X#share_info.name),
				   share_type(X#share_info.type)])
		end,
	    lists:foreach(F, Res),
	    io:nl();
	Else ->
	    Else
    end.

share_type(?SHARETYPE_DISKTREE) -> "Directory tree";
share_type(?SHARETYPE_PRINTQ)   -> "Printer queue";
share_type(?SHARETYPE_DEVICE)   -> "Comm. device";
share_type(?SHARETYPE_IPC)      -> "IPC".
    

%%% 
%%% List Shares - The new (DCE/RPC) way of doing it !
%%%
%%% @spec list_shares(Host::hostip(), User::bstring(), 
%%%                   Passwd::bstring()) ->
%%%            {ok, Shares::shares()} | {error, Emsg::string()}
%%%
%%% @type hostip(). A hostname or IP address as a string or an IP-tuple.
%%% @type shares(). A list of #share_info{} records.
%%%
%%% @doc Returns a list of shares available from a specified <em>Host</em>.
%%%      This function is using the DCE/RPC method which is the preferred
%%%      way compared to the old Lanman/RPC method (which has a 12 character
%%%      restriction on the length of the share names).
%%%
%%% @see list_shares_old/3
%%% @end
%%%
list_shares(Host, User, Passwd) ->
    list_shares(Host, User, Passwd, ?DEFAULT_WORKGROUP, []).

%%% 
%%% @spec list_shares(Host::hostip(), User::bstring(), 
%%%                   Passwd::bstring(), Wgrp::string(),
%%%                   SockOpts::list()) ->
%%%            {ok, Shares::shares()} | {error, Emsg::string()}
%%%
%%% @doc Works as {@link list_shares/3} but takes two extra arguments.
%%%      The <em>SockOpts</em> argument takes socket options to be passed
%%%      to the ''gen_tcp:connect/3'' function.
%%% @end
%%%
list_shares(Host0, User, Passwd, Workgroup, SockOpts) ->
    case catch list_shares0(Host0, User, Passwd, Workgroup, SockOpts) of
	{'EXIT', Reason} -> 
	    {error, "list_shares internal"};
	Else -> 
	    Else
    end.

list_shares0(Host0, User, Passwd, Workgroup, SockOpts) ->
    Host = h2s(Host0),
    case connect(Host, SockOpts) of
	{ok,S,Neg} ->
	    U = #user{pw = Passwd, name = User, primary_domain = Workgroup},
	    Pdu0 = user_logon(S, Neg, U),
	    exit_if_error(Pdu0, "Login failed"),
	    if (Pdu0#smbpdu.samba2 == true) or (Pdu0#smbpdu.netware == true) ->
		    %% Samba-2 and NetWare doesn't support DCE/RPC !!
		    %% Do it the old way...
		    esmb:close(S),
		    list_shares_old(Host, User, Passwd, Workgroup, SockOpts);
	       true ->
		    IPC = "\\\\"++Host++"\\IPC"++[$$],
		    Path1 = to_ucs2(unicode_p(Neg), IPC),
		    Pdu1 = tree_connect(S, Neg, Pdu0, Path1, 
					?SERVICE_NAMED_PIPE),
		    exit_if_error(Pdu1, "Tree connect failed"),
		    Res = (catch do_list_shares(S, Pdu1, Host)),
		    close(S),
		    Res
	    end;
	Else ->
	    Else
    end.

%%% Debug function
do_ls(S, Pdu, Host) ->
    if (Pdu#smbpdu.samba2 == true) or (Pdu#smbpdu.netware == true) ->
	    {Req, Pdu1} = smb_list_shares_pdu(Pdu),
	    decode_list_shares_response(Req, nbss_session_service(S, Pdu1));
       true ->
	    do_list_shares(S, Pdu, Host)
    end.


do_list_shares(S, Pdu, Host) ->
    {ok,Pdu1} = esmb_rpc:open_srvsvc_pipe(S, Pdu),
    exit_if_error(Pdu1, "Open file, failed"),
    %%
    {ok,Pdu2} = esmb_rpc:rpc_bind(S, Pdu1, ?ST_SRVSVC),
    {ok,Nse,Pdu3} = esmb_rpc:rpc_netr_share_enum(S, Pdu2, Host),
    Pdu4 = close_file(S, Pdu3, Pdu1#smbpdu.fid),
    F = fun(E) -> 
		#share_info{name = ucs2_to_ascii(E#nse_entry.name),
			    type = E#nse_entry.type}
	end,
    XX = lists:map(F,Nse),
    {ok, XX}.


%%%
%%% The old (LANMAN RPC) way of doing it !
%%%
%%% @spec list_shares_old(Host::hostip(), User::bstring(), 
%%%                       Passwd::bstring()) ->
%%%            {ok, Shares::shares()} | {error, Emsg::string()}
%%%
%%% @doc Returns a list of shares available from a specified <em>Host</em>.
%%%      This function is using the Lanman RPC method which restricts the 
%%%      length of the share names to a maximum of 12 characters.
%%%
%%% @see list_shares/3
%%% @end
%%%
list_shares_old(Host, User, Passwd) ->
    list_shares_old(Host, User, Passwd, ?DEFAULT_WORKGROUP, []).

%%% @spec list_shares_old(Host::hostip(), User::bstring(), 
%%%                       Passwd::bstring(), Wgrp::string(),
%%%                       SockOpts::list()) ->
%%%            {ok, Shares::shares()} | {error, Emsg::string()}
%%%
%%% @doc Works as {@link list_shares_old/3} but takes two extra arguments.
%%%      The <em>SockOpts</em> argument takes socket options to be passed
%%%      to the ''gen_tcp:connect/3'' function.
%%% @end
%%%
list_shares_old(Host0, User, Passwd, Workgroup, SockOpts) ->
    Host = h2s(Host0),
    case connect(Host, SockOpts) of
	{ok,S,Neg} ->
	    U = #user{pw = Passwd, name = User, primary_domain = Workgroup},
	    Pdu0 = user_logon(S, Neg, U),
	    exit_if_error(Pdu0, "Login failed"),
	    Path = "\\\\"++Host++"\\IPC" ++ [$$], 
	    Path1 = to_ucs2(unicode_p(Neg), Path),
	    Pdu1 = tree_connect(S, Neg, Pdu0, Path1, ?SERVICE_ANY_TYPE),
	    exit_if_error(Pdu1, "Tree connect failed"),
	    {Req, Pdu2} = smb_list_shares_pdu(Pdu1),
	    decode_list_shares_response(Req, nbss_session_service(S, Pdu2));
	Else ->
	    Else
    end.


h2s({A,B,C,D}) -> 
    lists:flatten(io_lib:format("~w.~w.~w.~w", [A,B,C,D]));
h2s(Host) when list(Host) -> 
    Host.

decode_list_shares_response(Req, {ok, _, ResPdu}) ->
    Res = safe_dec_smb(Req, ResPdu), % NB: may throw(Pdu)
    <<TotParamCount:16/little,
     TotDataCount:16/little,
     0:16/little,
     ParamCount:16/little,
     ParamOffset:16/little,
     ParamDisplacement:16/little,
     DataCount:16/little,
     DataOffset:16/little,
     DataDisplacement:16/little,
     SetupCount,
    _/binary>> = Res#smbpdu.wp,
    if (Res#smbpdu.netware == true) ->
	    <<Status:16/little,       % success(0), access_denied(5)
	     Convert:16/little,       
	     EntryCount:16/little,    % # of entries returned
	     AvailEntries:16/little,  % # of available entries
	     _:16,                    % what the f*uck is this ??
	     Entries/binary>> = Res#smbpdu.bf;
       true ->
	    <<_,                      % what the f*uck is this ??
	     Status:16/little,        % success(0), access_denied(5)
	     Convert:16/little,       
	     EntryCount:16/little,    % # of entries returned
	     AvailEntries:16/little,  % # of available entries
	     Entries/binary>> = Res#smbpdu.bf
    end,
    if (Status == ?SUCCESS) ->
	    warning(EntryCount, AvailEntries, 
		    "<Warning>: Buffer too small to fit all entries !"),
	    {ok, parse_entries(EntryCount, Entries)};
       true ->
	    {error, list_shares}
    end.

parse_entries(0, _) -> 
    [];
parse_entries(N, <<ShareName:13/binary,
		  _CstrNull,
		  ShareType:16/little,
		  _CharPtr:4/binary,
		  Rest/binary>>) -> 
    [#share_info{name = l2b(rm_trailing_nulls(b2l(ShareName))),
		 type = ShareType}|
     parse_entries(N-1, Rest)].

rm_trailing_nulls(Str) ->
    rm_nulls(lists:reverse(Str)).

rm_nulls([0|T]) -> rm_nulls(T);    
rm_nulls(L)     -> lists:reverse(L).
       
warning(X,X,_) -> true;
warning(_,_,S) -> io:format("~s~n", [S]).


%%% ---

decode_list_servers_response(Req, {ok, _, ResPdu}) ->
    Res = safe_dec_smb(Req, ResPdu), % NB: may throw(Pdu)
    <<TotParamCount:16/little,
     TotDataCount:16/little,
     0:16/little,
     ParamCount:16/little,
     ParamOffset:16/little,
     ParamDisplacement:16/little,
     DataCount:16/little,
     DataOffset:16/little,
     DataDisplacement:16/little,
     SetupCount,
    _/binary>> = Res#smbpdu.wp,
    if (Res#smbpdu.netware == true) ->
	    <<Status:16/little,       % success(0), access_denied(5)
	     Convert:16/little,       
	     EntryCount:16/little,    % # of entries returned
	     AvailEntries:16/little,  % # of available entries
	     _:16,                    % what the f*ck is this ??
	     Entries/binary>> = Res#smbpdu.bf;
       true ->
	    <<_,                      % what the f*ck is this ??
	     Status:16/little,        % success(0), access_denied(5)
	     Convert:16/little,       
	     EntryCount:16/little,    % # of entries returned
	     AvailEntries:16/little,  % # of available entries
	     Entries/binary>> = Res#smbpdu.bf
    end,
    if (Status == ?SUCCESS) ->
	    warning(EntryCount, AvailEntries, 
		    "<Warning>: Buffer too small to fit all entries !"),
	    {ok, parse_srv_entries(EntryCount, Entries)};
       true ->
	    {error, "list servers"}
    end.

%%%
%%% FIXME , we ignore the comment strings for now...
%%%
parse_srv_entries(0, _) -> 
    [];
parse_srv_entries(N, <<ServerName:16/binary,
		      OsMajorVers,
		      OsMinorVers,
		      ServerType:32/little,
		      _CharPtr:4/binary,
		      Rest/binary>>) -> 
    [#server_info{name = rm_trailing_nulls(b2l(ServerName)),
		  type = ServerType}|
     parse_srv_entries(N-1, Rest)].



%%% @spec list_server_rap(Host::hostip(), Wgrp::string(),
%%%                       SockOpts::list()) ->
%%%            {ok, Servers::servers()} | {error, Emsg::string()}
%%%
%%% @doc 
%%% @end
%%%
list_server_rap(Host0, Workgroup, SockOpts) ->
    Host = h2s(Host0),
    case connect(Host, SockOpts) of
	{ok,S,Neg0} ->
	    Neg = Neg0#smb_negotiate_res{security_mode = 0},
	    U = #user{pw = "", name = "", primary_domain = Workgroup},
	    Pdu0 = user_logon(S, Neg, U),
	    exit_if_error(Pdu0, "Login failed"),
	    Path = "\\\\"++Host++"\\IPC" ++ [$$], 
	    Path1 = to_ucs2(unicode_p(Neg), Path),
	    Pdu1 = tree_connect(S, Neg, Pdu0, Path1, ?SERVICE_ANY_TYPE),
	    exit_if_error(Pdu1, "Tree connect failed"),
	    {Req, Pdu2} = smb_list_servers_pdu(Pdu1, Workgroup),
	    decode_list_servers_response(Req, nbss_session_service(S, Pdu2));
	Else ->
	    Else
    end.


%%% 
%%% Get User Groups
%%%
%%% @spec get_user_groups(S::socket(), InReq::pdu(),
%%%                       User::bstring(), IpStr::string()) -> 
%%%                  {ok, Groups::list(), pdu()} | {error, pdu()}
%%%
%%% @doc This function retrieves, from the server in <em>IpStr</em>,
%%%      the groups the specified <em>User</em> belongs to
%%% @end
%%%
get_user_groups(S, Pdu0, User, IpStr) ->
    case catch gugs(S, Pdu0, User, IpStr) of
	{ok, _Groups, _Pdu1} = Result -> Result;
	{error, Emsg} -> 
	    {error, Pdu0#smbpdu{eclass = ?INTERNAL,
				emsg = Emsg}};
	{'EXIT', _Reason} ->
	    {error, Pdu0#smbpdu{eclass = ?INTERNAL,
				emsg = "Internal DCE/RPC error"}}
    end.

gugs(S, Pdu0, User, IpStr) ->	
    {ok,Pdu1} = esmb_rpc:open_samr_pipe(S, Pdu0),
    {ok,Pdu2} = esmb_rpc:rpc_bind(S, Pdu1, ?ST_SAMR),
    {ok,Res,Pdu3} = do_gugs(S, Pdu2, User, IpStr),
    Pdu4 = esmb:close_file(S, Pdu3, Pdu1#smbpdu.fid),
    {ok,Res,Pdu4}.

do_gugs(S, Pdu0, User, IpStr) ->
    {ok,CtxHandle1,Pdu1} = esmb_rpc:rpc_samr_connect(S, Pdu0, IpStr),
    {ok,Doms1,Pdu2} = esmb_rpc:rpc_samr_enum_doms(S, Pdu1, CtxHandle1),
    {ok,Doms2,Pdu3} = esmb_rpc:rpc_samr_lookup_doms(S,Pdu2,CtxHandle1,Doms1),
    {ok,Pdu4} = esmb_rpc:rpc_samr_close(S, Pdu3, CtxHandle1),
    {ok,CtxHandle2,Pdu5} = esmb_rpc:rpc_samr_connect(S, Pdu4, IpStr),
    %% NB: Doms2 will be just a single record at the moment, FIXME !!
    {ok,CtxHandle3,Pdu6} = esmb_rpc:rpc_samr_open_domain(S,Pdu5,IpStr,
							 CtxHandle2,Doms2),
    Name = to_ucs2(unicode_p(Pdu6), b2l(User)),
    {ok,Rid,Type,Pdu7} = esmb_rpc:rpc_samr_lookup_names(S, Pdu6, IpStr, 
							CtxHandle3, Name),
    {ok,CtxHandleU,Pdu8} = esmb_rpc:rpc_samr_open_user(S, Pdu7, IpStr, 
						       CtxHandle3, Name, Rid),
    {ok,RidList,Pdu9} = esmb_rpc:rpc_samr_user_groups(S, Pdu8, 
						      IpStr, CtxHandleU),
    {ok,Pdu10} = esmb_rpc:rpc_samr_close(S, Pdu9, CtxHandleU),
    {ok,Ns,Pdu11} = esmb_rpc:rpc_samr_lookup_rids(S, Pdu10, IpStr, 
						  CtxHandle3, RidList),
    {ok,Pdu12} = esmb_rpc:rpc_samr_close(S, Pdu11, CtxHandle3),
    {ok,Pdu14} = esmb_rpc:rpc_samr_close(S, Pdu12, CtxHandle2),
    {ok,names(Ns),Pdu14}.


names(L) when list(L) -> 
    F = fun(E) -> b2l(ucs2_to_ascii(E#name_entry.name))
	end,
    lists:map(F,L);
names(E) ->
    names([E]).


%%% 
%%% Mailslot Browse Transaction
%%% 
%%%
%%% @spec mailslot_browse_transaction(Dgm::netbios_dgm(), InReq::pdu(), Rpc::binary()) -> 
%%%                  {ok, binary(), pdu()} | exception()
%%%
%%% @doc This function takes an DCE/RPC packet (<em>RPC</em>), which
%%%      conforms to the <em>Win32 Mailslot Browse</em> protocol
%%%      and transmits it, embedded in an SMB-Transaction message,
%%%      over a NetBIOS-DGM UDP socket.
%%% @end
%%%
mailslot_browse_transaction(Dgm, Rpc) when binary(Rpc) ->
    Pipe = "\\MAILSLOT\\BROWSE",
    Path = to_ucs2_and_null(false, Pipe),  % Don't use Unicode
    mailslot_browse_transaction(Dgm, Rpc, Path).

%%% @private
mailslot_browse_transaction(Dgm, Rpc, PipeName) 
  when binary(PipeName), binary(Rpc) ->
    {_Req, Pdu} = smb_mailslot_browse_transaction(PipeName, Rpc),
    nbss_datagram_service(Dgm, Pdu).


smb_mailslot_browse_transaction(PipeName, Rpc) ->
    {Wc,Wp} = wp_mailslot_browse(size(PipeName), size(Rpc)),
    Bf = bf_mailslot_browse(PipeName),
    Rec = #smbpdu{
      cmd = ?SMB_COM_TRANSACTION,
      wc  = Wc,
      wp  = Wp,
      bc  = size(Bf) + size(Rpc),
      bf  = Bf},
    enc_smb(Rec, Rpc).

wp_mailslot_browse(NameLen, DataSize) ->
    %% How to compute the offset values (no.of bytes):
    %% ParamOffset = ?SMB_HEADER_LEN + ThisLen + WordCount + ByteCount + Pad + length(TransName)
    %%             = ?SMB_HEADER_LEN + 1 + 16*2 + 3 + 1 + NameLen
    %%             = ?SMB_HEADER_LEN + 37 + NameLen
    %%
    ParamOffset = ?SMB_HEADER_LEN + 37 + NameLen,
    ParamLen    = 0, 
    DataOffset  = ParamOffset + ParamLen,
    ParamCount  = DataOffset - ParamOffset,
    {17,                         % WordCount = 14+3
     <<ParamCount:16/little,     % Total parameter bytes sent
      DataSize:16/little,        % Total data bytes sent
      0:16/little,               % Max parameter bytes to return
      ?MAX_BUFFER_SIZE:16/little,% Max data bytes to return
      0,                         % Max setup words to return
      0,                         % reserved
      0:16/little,               % Flags
      1000:32/little,            % timeout = 1000 mSec = 1 Sec
      0:16/little,               % reserved2
      ParamLen:16/little,        % Parameter bytes sent this buffer
      ParamOffset:16/little,     % Offset (from header start) to parameters
      DataSize:16/little,        % Data bytes sent this buffer
      DataOffset:16/little,      % Offset (from header start) to data
      3,                         % Count of setup words
      0,                         % reserved3 (pad above to word boundary)
      ?SUBCMD_TRANSACT_WRITE_MAILSLOT:16/little,
      1:16/little,               % Priority
      2:16/little                % Class == Unreliable & Broadcast
      >>}.

bf_mailslot_browse(PipeName) ->
    list_to_binary([PipeName    % Name of transaction
		    ]).


%%% ---------------------
%%% 
%%% Named Pipe Transaction
%%% 
%%%
%%% @spec named_pipe_transaction(S::socket(), InReq::pdu(), RPC::binary()) -> 
%%%                  {ok, binary(), pdu()} | exception()
%%%
%%% @doc This function takes an DCE/RPC packet (<em>RPC</em>), which
%%%      conforms to the <em>Win32 TransactNamedPipe()</em> protocol
%%%      and transmits it, embedded in an SMB-Transaction message.
%%% @end
%%%
named_pipe_transaction(S, InReq, Rpc) when binary(Rpc) ->
    Pipe = "\\PIPE\\",
    Path = to_ucs2_and_null(unicode_p(InReq), Pipe),
    named_pipe_transaction(S, InReq, Rpc, Path).

%%% @private
named_pipe_transaction(S, InReq, Rpc, PipeName) 
  when binary(PipeName), binary(Rpc) ->
    {Req, Pdu} = smb_named_pipe_transaction(InReq, PipeName, Rpc),
    decode_smb_response(Req, nbss_session_service(S, Pdu)).


smb_named_pipe_transaction(InReq, PipeName, Rpc) ->
    {Wc,Wp} = wp_named_pipe(InReq#smbpdu.fid, size(PipeName), size(Rpc)),
    Bf = bf_named_pipe(PipeName),
    Rec = ?CP_PDU(InReq)#smbpdu{
		   cmd = ?SMB_COM_TRANSACTION,
		   wc  = Wc,
		   wp  = Wp,
		   bc  = size(Bf) + size(Rpc),
		   bf  = Bf},
    enc_smb(Rec, Rpc).

wp_named_pipe(Fid, NameLen, DataSize) ->
    %% How to compute the offset values (no.of bytes):
    %% ParamOffset = ?SMB_HEADER_LEN + ThisLen + WordCount + ByteCount + Pad + length(TransName)
    %%             = ?SMB_HEADER_LEN + 1 + 16*2 + 2 + 1 + NameLen
    %%             = ?SMB_HEADER_LEN + 36 + NameLen
    %%
    ParamOffset = ?SMB_HEADER_LEN + 36 + NameLen,
    ParamLen    = 0, 
    DataOffset  = ParamOffset + ParamLen,
    ParamCount  = DataOffset - ParamOffset,
    {16,                         % WordCount = 14+2
     <<ParamCount:16/little,     % Total parameter bytes sent
      DataSize:16/little,        % Total data bytes sent
      0:16/little,               % Max parameter bytes to return
      ?MAX_BUFFER_SIZE:16/little,% Max data bytes to return
      0,                         % Max setup words to return
      0,                         % reserved
      0:16/little,               % Flags
      0:32/little,               % timeout , 0 = return immediately
      0:16/little,               % reserved2
      ParamLen:16/little,        % Parameter bytes sent this buffer
      ParamOffset:16/little,     % Offset (from header start) to parameters
      DataSize:16/little,        % Data bytes sent this buffer
      DataOffset:16/little,      % Offset (from header start) to data
      2,                         % Count of setup words
      0,                         % reserved3 (pad above to word boundary)
      ?SUBCMD_TRANSACT_NM_PIPE:16/little,
      Fid:16/little
      >>}.

bf_named_pipe(PipeName) ->
    list_to_binary([0,          % Pad
		    PipeName    % Name of transaction
		    ]).


%%% ---------------------

smb_list_shares_pdu(InReq) ->
    {Wc,Wp} = wp_list_shares(),
    Bf = bf_list_shares("\\PIPE\\LANMAN"),
    Rec = ?CP_PDU(InReq)#smbpdu{
		   cmd = ?SMB_COM_TRANSACTION,
		   wc  = Wc,
		   wp  = Wp,
		   bc  = size(Bf),
		   bf  = Bf},
    enc_smb(Rec).


wp_list_shares() ->
    %% How to compute the offset values (no.of bytes):
    %% ParamOffset = ?SMB_HEADER_LEN + ThisLen + WordCount + ByteCount + length(TransName) + NullByte
    %%             = ?SMB_HEADER_LEN + 1 + 14*2 + 2 + 12 + 1
    %%             = ?SMB_HEADER_LEN + 44
    %% ParamLen    = <Length of the Lanman RAP block below>
    %% DataOffset  = ParamOffset + ParamLen + NullByte
    %%             = ParamOffset + ParamLen
    ParamOffset = ?SMB_HEADER_LEN + 44,
    ParamLen    = 19, % The Lanman RAP block
    DataOffset  = ParamOffset + ParamLen,
    ParamCount  = DataOffset - ParamOffset,
    {14,                         % WordCount = 14
     <<ParamCount:16/little,     % Total parameter bytes sent
      0:16/little,               % Total data bytes sent
      10:16/little,              % Max parameter bytes to return
      ?MAX_BUFFER_SIZE:16/little,% Max data bytes to return
      0,                         % Max setup words to return
      0,                         % reserved
      0:16/little,               % Flags
      0:32/little,               % timeout , 0 = return immediately
      0:16/little,               % reserved2
      ParamLen:16/little,        % Parameter bytes sent this buffer
      ParamOffset:16/little,     % Offset (from header start) to parameters
      0:16/little,               % Data bytes sent this buffer
      DataOffset:16/little,      % Offset (from header start) to data
      0,                         % Count of setup words
      0                          % reserved3 (pad above to word boundary)
      >>}.

bf_list_shares(TransName) ->
    list_to_binary([TransName,0, % Name of transaction
		    %% --- Start of Parameter Block (19 bytes) ---
		    <<0:16/little>>,% Function Code: NetShareEnum(0)
		    "WrLeh",0,   % Parameter Descriptor
		    "B13BWz",0,  % Return Descriptor
		    <<1:16/little>>, % Detail level
		    <<?MAX_BUFFER_SIZE:16/little>> % Receive buffer size
		    ]).


%%% ---------------------

smb_list_servers_pdu(InReq, Workgroup) ->
    {Wc,Wp} = wp_list_servers(sizeof(Workgroup)),
    Bf = bf_list_servers("\\PIPE\\LANMAN", Workgroup),
    Rec = ?CP_PDU(InReq)#smbpdu{
		   cmd = ?SMB_COM_TRANSACTION,
		   wc  = Wc,
		   wp  = Wp,
		   bc  = size(Bf),
		   bf  = Bf},
    enc_smb(Rec).

wp_list_servers(WgrpLen) ->
    %% How to compute the offset values (no.of bytes):
    %% ParamOffset = ?SMB_HEADER_LEN + ThisLen + WordCount + ByteCount + length(TransName) + NullByte
    %%             = ?SMB_HEADER_LEN + 1 + 14*2 + 2 + 12 + 1
    %%             = ?SMB_HEADER_LEN + 44
    %% ParamLen    = <Length of the Lanman RAP block below>
    %% DataOffset  = ParamOffset + ParamLen + NullByte
    %%             = ParamOffset + ParamLen
    ParamOffset = ?SMB_HEADER_LEN + 44,
    ParamCount  = ParamLen = 27 + WgrpLen,
    DataOffset  = ParamOffset + ParamLen,
    {14,                         % WordCount = 14
     <<ParamCount:16/little,     % Total parameter bytes sent
      0:16/little,               % Total data bytes sent
      10:16/little,              % Max parameter bytes to return
      ?MAX_BUFFER_SIZE:16/little,% Max data bytes to return
      0,                         % Max setup words to return
      0,                         % reserved
      0:16/little,               % Flags
      0:32/little,               % timeout , 0 = return immediately
      0:16/little,               % reserved2
      ParamCount:16/little,      % Parameter bytes sent this buffer
      ParamOffset:16/little,     % Offset (from header start) to parameters
      0:16/little,               % Data bytes sent this buffer
      DataOffset:16/little,      % Offset (from header start) to data
      0,                         % Count of setup words
      0                          % reserved3 (pad above to word boundary)
      >>}.

bf_list_servers(TransName, Workgroup) ->
    list_to_binary([TransName,0, % Name of transaction
		    %% --- Start of Parameter Block (19 bytes) ---
		    <<16#0068:16/little>>,% Function Code: NetServerEnum2
		    "WrLehDz",0,   % Parameter Descriptor
		    "B16BBDz",0,   % Return Descriptor
		    <<1:16/little>>, % Detail level
		    <<?MAX_BUFFER_SIZE:16/little>>, % Receive buffer size
		    <<?SV_TYPE_ALL:32/little>>, % ServerType
		    Workgroup,0    % Name of Wgrp to list
		    ]).



%%%
%%% @spec connect(Host::string()) ->
%%%          {ok, socket(), pdu()} | {error, Reason}
%%%
%%% @doc Will call {@link connect/2}.
%%%
connect(Host) -> connect(Host, []).

%%%
%%% @spec connect(Host::string(), SockOpts::list()) ->
%%%          {ok, socket(), pdu()} | {error, Reason}
%%%
%%% @doc Will call {@link connect/3}.
%%%
connect(Host, SockOpts) -> connect(Host, SockOpts, ?PORT).

%%%
%%% @spec connect(Host::string(), SockOpts::list(), Port::integer()) ->
%%%          {ok, socket(), pdu()} | {error, Reason}
%%%
%%% @doc Setup a socket, initiate an NBSS session and 
%%%      negotiate the protocol dialect. This function will
%%%      automatically call the {@link negotiate/1} function
%%%      as soon as the socket has been connected.
%%% @end
%%%
connect(Host, SockOpts, Port) ->
    Opts = [binary, {packet, 0}|SockOpts],
    case gen_tcp:connect(Host, Port, Opts) of
	{ok,S} ->
	    case nbss_session_request(S, "*SMBSERVER", caller()) of
		{ok,_} ->
		    {ok, S, negotiate(S)};
		_ ->
		    {error, nbss_session_request}
	    end;
	Else ->
	    Else
    end.

%%%
%%% @spec close(socket()) -> ok
%%%
%%% @doc Close a socket opened with {@link connect/3}.
%%% @end
%%%
close(S) ->
    gen_tcp:close(S),
    ok.


%%%
%%% @spec write_file(S::socket(), N::neg(), 
%%%                  P::pdu(), I::finfo()) ->
%%%                   {ok, Written::integer(), pdu()} | {error, pdu()}
%%%
%%% @doc Writes a file, which has already been opened with
%%%      {@link open_file_rw/3}. Returns the number of bytes written.
%%% @end
%%%
write_file(S, InReq, Finfo) ->
    write_file(S, InReq#smbpdu.neg, InReq, Finfo).

write_file(S, Neg, InReq, Finfo) ->
    write_file(S, Neg, InReq, Finfo, list_to_binary(Finfo#file_info.data), 0).

-define(LT_BUFSIZE(Neg, Bin), 
	(Neg#smb_negotiate_res.max_buffer_size >= size(Bin))).

write_file(S, Neg, InReq, Finfo, Bin, Written) when ?LT_BUFSIZE(Neg,Bin) -> 
    {Req1, Pdu} = smb_write_andx_pdu(InReq, Finfo, Bin, Written),
    case decode_smb_response(Req1, nbss_session_service(S, Pdu)) of
	{ok, Wrote, Req2} ->
	    {ok, Wrote + Written, Req2};
	_ ->
	    {error, Req1#smbpdu{eclass = ?INTERNAL, emsg = "write_file"}}
    end;
write_file(S, Neg, InReq, Finfo, Bin, Written) ->
    {B1,B2} = split_binary(Bin, Neg#smb_negotiate_res.max_buffer_size - ?HEADER_SIZE),
    case write_file(S, Neg, InReq, Finfo, B1, Written) of
	{ok, Wrote, Res} ->
	    write_file(S, Neg, Res, Finfo, B2, Wrote + Written);
	Else ->
	    Else
    end.
	

-define(STREAM_READ,  true).
-define(READ_ALL,     false).

%%%
%%% @spec stream_read_file(S::socket(), InReq::pdu(), I::finfo()) ->
%%%           {ok, binary(), pdu()} | {more, binary(), pdu()} | {error, string()}
%%%
%%% @doc A streaming version of {@link read_file/3}. It returns
%%%      a received chunk of data + a continuation if more data
%%%      is expected. To retrieve the next chunk of data call:
%%%      ``(P#smbpdu.cont)()''
%%% @end
%%%
stream_read_file(S, InReq, Finfo) ->
    read_file(S, InReq, Finfo, ?STREAM_READ, []).

%%%
%%% @spec read_file(S::socket(), InReq::pdu(), I::finfo()) ->
%%%                   {ok, binary(), pdu()} | {error, pdu()}
%%%
%%% @doc Reads a file, which has already been opened with
%%%      {@link open_file_ro/3}. Returns the content if the
%%%      file as a binary. The file and the amount of data to
%%%      read are specified with a finfo() record. In case of 
%%%      error, you can obtain the error code and error message
%%%      using the {@link error_p/1} function.
%%% <br/>Example:
%%% ```
%%%  ...
%%%  Pdu1 = esmb:open_file_ro(S, Pdu0, Fname),
%%%  Finfo = #file_info{name = Fname, size = Pdu1#smbpdu.file_size},
%%%  {ok, Bin, Pdu2} = esmb:read_file(S, Pdu1, Finfo),
%%%  ...
%%% '''
%%% @end
%%%
read_file(S, InReq, Finfo) ->
    read_file(S, InReq, Finfo, ?READ_ALL, []).

-define(READ_ENOUGH(F), (F#file_info.size =< F#file_info.data_len)).
-define(MORE_TO_READ(F), (F#file_info.size > F#file_info.data_len)).

read_file(S, InReq, Finfo, ?STREAM_READ, Acc) when ?READ_ENOUGH(Finfo) ->
    Data = trim_binary(concat_binary(lists:reverse(Acc)), 
		       Finfo#file_info.data_len - Finfo#file_info.size),
    {ok, Data, InReq};
read_file(S, InReq, Finfo, ?READ_ALL, Acc) when ?READ_ENOUGH(Finfo) ->
    Data = trim_binary(concat_binary(lists:reverse(Acc)), 
		       Finfo#file_info.size),
    {ok, Data, InReq};
read_file(S, InReq, Finfo, Rtype, Acc) when ?MORE_TO_READ(Finfo) ->
    {Req, Pdu} = smb_read_andx_pdu(InReq, Finfo),
    case decode_smb_response(Req, nbss_session_service(S, Pdu)) of
	{ok, Res, Data} -> 
	    Dlen = Finfo#file_info.data_len + size(Data),
	    NewFinfo = Finfo#file_info{data_len = Dlen},
	    if (Rtype == ?STREAM_READ) ->
		    Cont = fun() ->
				   read_file(S, 
					     Res, 
					     NewFinfo,
					     Rtype,
					     Acc)
			   end,
		    {more, Data, Res#smbpdu{cont = Cont}};
	        true ->
		    read_file(S, 
			      Res, 
			      NewFinfo, 
			      Rtype,
			      [Data | Acc])
	    end;
	_ ->
	    {error, Req#smbpdu{eclass = ?INTERNAL, emsg = "decoding_read_andx"}}
    end.

trim_binary(Bin, Size) when size(Bin) > Size -> 
    element(1, split_binary(Bin, Size));
trim_binary(Bin, _) -> 
    Bin.
    

%%%
%%% @spec open_file_ro(S::socket(), InReq::pdu(), 
%%%                    Path::bstring()) -> pdu()
%%%
%%% @doc Open file as Read-Only. The returned pdu() record
%%%      will contain the file descriptor (FID).
%%% @end
%%%
open_file_ro(S, InReq, Path) ->
    {Req, Pdu} = smb_open_file_ro_pdu(InReq, Path),
    decode_smb_response(Req, nbss_session_service(S, Pdu)).
	

%%%
%%% @spec open_file_rw(S::socket(), InReq::pdu(), 
%%%                    Path::bstring()) -> pdu()
%%%
%%% @doc Open file as Read-Write. The returned pdu() record
%%%      will contain the file descriptor (FID).
%%% @end
%%%
open_file_rw(S, InReq, Path) ->
    {Req, Pdu} = smb_open_file_rw_pdu(InReq, Path),
    decode_smb_response(Req, nbss_session_service(S, Pdu)).


%%%
%%% @spec close_file(S::socket(), Pdu::pdu()) -> pdu()
%%%
%%% @doc Close file corresponding to the ''Pdu#smbpdu.fid'' filedescriptor.
%%%      This will clear the file descriptor (<em>FID</em>),
%%%      in the returned pdu(). Note that care has to be taken
%%%      to make sure the right FID is used.
%%% 
%%% @see close_file/3
%%% @end
%%%
close_file(S, Pdu) ->
    close_file(S, Pdu, Pdu#smbpdu.fid).

%%%
%%% @spec close_file(S::socket(), InReq::pdu(), Fid::integer()) -> pdu()
%%%
%%% @doc Close file. Will clear the file descriptor (<em>FID</em>),
%%%      in the returned pdu(). Note that care has to be taken
%%%      to make sure the right FID is used. See the example below:
%%% ```
%%%  ...
%%%  Pdu1 = open_file_rw(S, Pdu0, PathA),
%%%  ...
%%%  Pdu5 = open_file_rw(S, Pdu4, PathB),
%%%  ...
%%%  Pdu8 = close_file(S, Pdu7, Pdu5#smbpdu.fid),
%%%  Pdu9 = close_file(S, Pdu8, Pdu1#smbpdu.fid),
%%%  ...
%%% '''
%%%      In the example above, we are first closing the file leading
%%%      to <em>PathB</em> by using the FID returned from corresponding 
%%%      open operation.
%%% @end
%%%
close_file(S, InReq, Fid) ->
    {Req, Pdu} = smb_close_file_pdu(InReq),
    decode_smb_response(Req#smbpdu{fid=undefined}, nbss_session_service(S,Pdu)).

%%%
%%% @spec mkdir(S::socket(), InReq::pdu(),
%%%             Path::bstring()) -> pdu()
%%%
%%% @doc Create a directory according to <em>Path</em>. Returns a
%%%      file descriptor (FID) in the returned pdu(), which needs 
%%%      to be closed with close_file/N .
%%% @end
%%%
mkdir(S, InReq, Path) ->
    {Req, Pdu} = smb_open_dir_pdu(InReq, Path),
    decode_smb_response(Req, nbss_session_service(S, Pdu)).

%%%
%%% @spec rmdir(S::socket(), InReq::pdu(),
%%%             Path::bstring()) -> pdu()
%%%
%%% @doc Remove a directory according to <em>Path</em>.
%%% @end
%%%
rmdir(S, InReq, Path) ->
    {Req, Pdu} = smb_delete_dir_pdu(InReq, Path),
    decode_smb_response(Req, nbss_session_service(S, Pdu)).
    
%%%
%%% @spec delete_file(S::socket(), InReq::pdu(),
%%%                   Path::bstring()) -> pdu()
%%%
%%% @doc Delete the file according to <em>Path</em>.
%%% @end
%%%
delete_file(S, InReq, Path) ->
    {Req, Pdu} = smb_delete_file_pdu(InReq, Path),
    decode_smb_response(Req, nbss_session_service(S, Pdu)).
    

%%%
%%% @spec list_dir(S::socket(), InReq::pdu(),
%%%                Path::bstring()) -> pdu()
%%%
%%% @doc List the content of the directory pointed to by <em>Path</em>.
%%%      The returned pdu() record contains a list of of finfo() 
%%%      records. 
%%% <br/>Example: ``P#smbpdu.finfo''
%%% @end
%%%
list_dir(S, InReq, Path) ->
    {Req, Pdu} = smb_trans2_find_first2_pdu(InReq, Path),
    case decode_smb_response(Req, nbss_session_service(S, Pdu)) of
	{Req2, X} when X#find_result.eos == true -> 
	    Req2#smbpdu{finfo = X#find_result.finfo};
	{Req2, X} -> 
	    list_dir_cont(S, Req2, Path, X#find_result.sid, X#find_result.finfo)
    end.

list_dir_cont(S, InReq, Path, Sid, Finfo) ->
    {Req, Pdu} = smb_trans2_find_next2_pdu(InReq, Path, Sid),
    case decode_smb_response(Req, nbss_session_service(S, Pdu)) of
	{Req2, X} when X#find_result.eos == true -> 
	    Req2#smbpdu{finfo = Finfo ++ X#find_result.finfo};
	{Req2, X} -> 
	    list_dir_cont(S, Req2, Path, Sid, Finfo ++ X#find_result.finfo)
    end.
    

%%%
%%% @spec check_dir(S::socket(), InReq::pdu(),
%%%                Path::bstring()) -> pdu()
%%%
%%% @doc Verify that the <em>Path</em> exists and is a directory.
%%%      Returns a pdu() record where the error code and class
%%%      indicates if the check was successful or not.
%%% @end
%%%
check_dir(S, InReq, Path) ->
    {Req, Pdu} = smb_check_directory_pdu(InReq, Path),
    decode_smb_response(Req, nbss_session_service(S, Pdu)).


%%%
%%% @spec tree_connect(S::socket(), Neg::neg(), InReq::pdu(),
%%%                    Path::bstring()) -> pdu()
%%%
%%% @doc Connect ourselves to the disk share to which
%%%      we want to perform some operations. 
%%%
%%% @see tree_connect/5
%%% @end
%%%
tree_connect(S, Neg, InReq, Path) ->
    tree_connect(S, Neg, InReq, Path, ?SERVICE_DISK_SHARE).

%%%
%%% @spec tree_connect(S::socket(), Neg::neg(), InReq::pdu(),
%%%                    Path::bstring(), Service::integer()) -> pdu()
%%%
%%% @doc Connect ourselves to the resource to which
%%%      we want to perform some operations. The <em>Service</em>
%%%      component indicates the type of resource.
%%% @end
%%%
tree_connect(S, Neg, InReq, Path, Service) ->
    {Req, Pdu} = smb_tree_connect_andx_pdu(Neg, InReq, Path, Service),
    decode_smb_response(Req, nbss_session_service(S, Pdu)).

%%%
%%% @spec tree_connect_ipc(S::socket(), Neg::neg(), InReq::pdu(),
%%%                        Path::bstring()) -> pdu()
%%%
%%% @doc Connect ourselves to the named pipe specified in <em>Path</em> to which
%%%      we want to perform some operations. See also tree_connect/5.
%%% @end
%%%
tree_connect_ipc(S, Neg, InReq, Path) ->
    tree_connect(S, Neg, InReq, Path, ?SERVICE_NAMED_PIPE).


%%%
%%% @spec negotiate(S::socket()) -> neg()
%%%
%%% @doc This function is the first to be called after a server has been
%%%      connected. It will send a list of dialects with which we can
%%%      communicate. Depending on what the server replies,
%%%      this will for example affect how the authentication is done 
%%%      (if done at all) and if Unicode is supported or not.
%%%
%%% @see user_logon/4
%%% @end
%%%
negotiate(S) ->
    {Req, Pdu} = smb_negotiate_pdu(),
    decode_smb_response(Req, nbss_session_service(S, Pdu)).


%%%
%%% @spec user_logon(S::socket(), Neg::neg(), 
%%%                  User::bstring(), Passwd::bstring()) -> pdu()
%%%
%%% @doc This function will further set up the session that has been established
%%%      by {@link negotiate/1}. The primary goal is to authenticate the user
%%%      in case passwords are requred by the server. Another function is to
%%%      setup the maximum message size the client can receive.
%%% @end
%%%
user_logon(S, Neg, User, Passwd) ->
    user_logon(S, Neg, #user{name=User, pw=Passwd}).

%%% @private
user_logon(S, Neg, U) ->
    {Req, Pdu} = smb_session_setup_andx_pdu(Neg, U),
    decode_smb_response(Req#smbpdu{neg = Neg}, nbss_session_service(S, Pdu)).


dbg_smb(What, Res) ->
    ?dbg("~s ---"
	"  Eclass  = ~p"
	"  Ecode   = ~p"
	"  Uid     = ~p"
	"  Tid     = ~p"
	"  Fid     = ~p"
	"  Fsize   = ~p~n",
	[What,
	Res#smbpdu.eclass,
	Res#smbpdu.ecode,
	Res#smbpdu.uid,
	Res#smbpdu.tid,
	Res#smbpdu.fid,
	Res#smbpdu.file_size
       ]).


%%% --------------------------------------------------------------------
%%% SMB decode routines
%%% --------------------------------------------------------------------

-define(IS(Req,Cmd), (Req#smbpdu.cmd == Cmd) ).
-define(SUB_CMD(Req, SubCmd), (Req#smbpdu.sub_cmd == SubCmd) ).

decode_smb_response(Req, {ok, _, ResPdu}) when  ?IS(Req, ?SMB_NEGOTIATE) ->
    Res = safe_dec_smb(Req, ResPdu),
    <<Di:16/little, B/binary>> = Res#smbpdu.wp,
    case Di of
	?PCNET_1_0 ->
	    #smb_negotiate_res{dialect_index = Di,
			       flags2 = Res#smbpdu.flags2};
	?LANMAN_1_0 ->
	    crypto:start(),
	    lanman_neg_resp(B, Res#smbpdu.bf, 
			    #smb_negotiate_res{dialect_index = Di,
					       flags2 = Res#smbpdu.flags2});
	?NT_LM_0_12 ->
	    crypto:start(),
	    ntlm_neg_resp(B, Res#smbpdu.bf, 
			  #smb_negotiate_res{dialect_index = Di,
					     flags2 = Res#smbpdu.flags2});
	_ ->
	    exit(nyi)
    end;
decode_smb_response(Req, {ok, _, ResPdu}) when  ?IS(Req, ?SMB_SESSION_SETUP_ANDX) ->
    dec_session_setup_andx(Req, ResPdu);
decode_smb_response(Req, {ok, _, ResPdu}) when  ?IS(Req, ?SMB_TREE_CONNECT_ANDX) ->
    safe_dec_smb(Req, ResPdu);
decode_smb_response(Req, {ok, _, ResPdu}) when  ?IS(Req, ?SMB_CHECK_DIRECTORY) ->
    safe_dec_smb(Req, ResPdu);
decode_smb_response(Req, {ok, _, ResPdu}) when  ?IS(Req, ?SMB_DELETE_DIRECTORY) ->
    safe_dec_smb(Req, ResPdu);
decode_smb_response(Req, {ok, _, ResPdu}) when  ?IS(Req, ?SMB_DELETE) ->
    safe_dec_smb(Req, ResPdu);
decode_smb_response(Req, {ok, _, ResPdu}) when  ?IS(Req, ?SMB_CLOSE) ->
    safe_dec_smb(Req, ResPdu);
decode_smb_response(Req, {ok, _, ResPdu}) when  ?IS(Req, ?SMB_NT_CREATE_ANDX) ->
    dec_nt_create_andx(Req, ResPdu);
decode_smb_response(Req, {ok, _, ResPdu}) when  ?IS(Req, ?SMB_READ_ANDX) ->
    dec_read_andx(Req, ResPdu);
decode_smb_response(Req, {ok, _, ResPdu}) when  ?IS(Req, ?SMB_WRITE_ANDX) ->
    dec_write_andx(Req, ResPdu);
decode_smb_response(Req, {ok, _, ResPdu}) 
  when  ?IS(Req, ?SMB_COM_TRANSACTION2), 
	?SUB_CMD(Req, ?SMB_TRANS2_FIND_FIRST2) ->
    dec_trans2_find_x2(Req, ResPdu, ?SMB_TRANS2_FIND_FIRST2);
decode_smb_response(Req, {ok, _, ResPdu}) 
  when  ?IS(Req, ?SMB_COM_TRANSACTION2), 
	?SUB_CMD(Req, ?SMB_TRANS2_FIND_NEXT2) ->
    dec_trans2_find_x2(Req, ResPdu, ?SMB_TRANS2_FIND_NEXT2);
decode_smb_response(Req, {ok, _, ResPdu}) when  ?IS(Req, ?SMB_COM_TRANSACTION) ->
    dec_transaction(Req, ResPdu).


lanman_neg_resp(<<SecurityMode:16/little,
		  MaxBufferSize:16/little,
		  _:2/binary,            % Max pending multiplexed requests
		  _:2/binary,            % Max VCs between client and server
		  _:2/binary,            % Raw modes supported
		  _:4/binary,            % Unique token identifying this session
		  _:2/binary,            % Current time at server
		  _:2/binary,            % Current date at server
		  _:2/binary,            % Current time zone at server
		  EncKeyLen:16/little,   % Encryption key length
		  _:2/binary>>,          % reserved
		  B,
		Neg) ->
    <<EncKey:EncKeyLen/binary, _/binary>> = B,
    Neg#smb_negotiate_res{security_mode   = SecurityMode,
			  max_buffer_size = MaxBufferSize,
			  encryption_key  = EncKey}.
    
ntlm_neg_resp(<<SecurityMode,
	      _:2/binary,            % Max pending outstanding requests
	      _:2/binary,            % Max VCs between client and server
	      MaxBufferSize:32/little,
	      _:4/binary,            % Max raw buffer size
	      _:4/binary,            % Unique token identifying this session
	      Cap:32/little,         % Server capabilities
	      _:4/binary,            % System (UTC) time of server (low)
	      _:4/binary,            % System (UTC) time of server (high)
	      _:2/binary,            % Time zone of server (minutes from UTC)
	      EncKeyLen>>,           % Encryption key length
	      B,
	      Neg) ->
    <<EncKey:EncKeyLen/binary, _/binary>> = B,
    Neg#smb_negotiate_res{security_mode    = SecurityMode,
			  max_buffer_size  = MaxBufferSize,
			  srv_capabilities = Cap,
			  encryption_key   = EncKey}.



%%% ---

dec_write_andx(Req, Pdu) -> 
    Res = safe_dec_smb(Req, Pdu),   
    <<?NoAndxCmd,              
     _,                         % reserved
     _:2/binary,                % offset to next command WC
     Count:16/little,           % Number of bytes written
     _:2/binary,                % Remaining (reserved)
     _:4/binary,                % reserved
     _/binary>> = Res#smbpdu.wp,
    {ok, Count, Res}.
    

%%% ---

dec_read_andx(Req, Pdu) -> 
    Res = safe_dec_smb(Req, Pdu),   
    <<?NoAndxCmd,              
     0,                         % reserved
     _:2/binary,                % offset to next command WC
     _:2/binary,                % Remaining (reserved) must be -1
     DcompactMode:16/little,    % ???
     0:16/little,               % reserved
     DataLength:16/little,      % Number of data bytes
     DataOffset:16/little,      % Offset (from header start) to data
     _:2/binary,                % High 16 bits of number of data bytes if
				% CAP_LARGE_READEX; else zero
     _/binary>> = Res#smbpdu.wp,
    <<_:DataOffset/binary,Data:DataLength/binary, _/binary>> = Pdu,
    {ok, Res, Data}.
    

%%% ---

dec_session_setup_andx(Req, Pdu) -> 
    Res = safe_dec_smb(Req, Pdu),   
    %% Check for Samba version 2 or NetWare since it requires 
    %% the old method of listing shares.
    is_netware(is_samba2(Res)).

is_samba2(Res) ->
    case regexp:first_match(b2l(Res#smbpdu.bf), "Samba 2") of
	{match, _, _} -> Res#smbpdu{samba2 = true};
	_             -> Res#smbpdu{samba2 = false}
    end.

is_netware(Res) ->
    case regexp:first_match(b2l(Res#smbpdu.bf), "NetWare") of
	{match, _, _} -> Res#smbpdu{netware = true};
	_             -> Res#smbpdu{netware = false}
    end.


    
%%% ---


dec_nt_create_andx(Req, Pdu) -> 
    Res = safe_dec_smb(Req, Pdu),
    <<?NoAndxCmd,              
     0,                         % reserved
     _:2/binary,                % offset to next command WC
     OpLockLevel,               % The oplock level granted
     Fid:16/little,             % The file ID
     CreateAction:32/little,    % The action taken (1 = file opened)
     CreationTime:8/binary,     % When file was created
     LastAccessTime:8/binary,   % When file was accessed
     LastWriteTime:8/binary,    % When file was last written
     ChangeTime:8/binary,       % When file was last changed
     _:4/binary,                % The file attributes
     _:8/binary,                % Allocation size
     EOF:8/binary,              % The file size 
     FileType:16/little,        % 0 = Disk file or directory
     _:2/binary,                % State of IPC device (e.g pipe)
     Directory,                 % Boolean (0 = Not a directory)
     _/binary>> = Res#smbpdu.wp,
    FileSize = large_integer(EOF),
    Res#smbpdu{fid       = Fid,
	       file_size = FileSize}.

%% NB: The TIME comes in units of 100 nano seconds !!
%% Gsec = (CreationTime div 10000000) + ?GREG_SEC_0_TO_1601,
%% GDT = calendar:gregorian_seconds_to_datetime(Gsec),
%% io:format("CREATION TIME: ~w~n", [GDT]),
%% Asec = (LastAccessTime div 10000000) + ?GREG_SEC_0_TO_1601,
%% ADT = calendar:gregorian_seconds_to_datetime(Asec),
%% io:format("LAST ACCESS TIME: ~w~n", [ADT]),
%% Wsec = (LastWriteTime div 10000000) + ?GREG_SEC_0_TO_1601,
%% WDT = calendar:gregorian_seconds_to_datetime(Wsec),
%% io:format("LAST WRITE TIME: ~w~n", [WDT]),
%% Csec = (ChangeTime div 10000000) + ?GREG_SEC_0_TO_1601,
%% CDT = calendar:gregorian_seconds_to_datetime(Csec),
%%io:format("LAST CHANGE TIME: ~w~n", [CDT]),

    
%%% ---

%%%
%%% This is the decoding of a transaction response.
%%%
%%% @private
dec_transaction(Req, Pdu) ->
    Res = safe_dec_smb(Req, Pdu),  % NB: may throw(Pdu)
    ?elog("%%%%%%%%%% Wp = ~p~n", [Res#smbpdu.wp]),
    <<TotParamCount:16/little,
     TotDataCount:16/little,
     _:16/little,                % reserved
     ParamCount:16/little,
     ParamOffset:16/little,
     ParamDisplacement:16/little,
     DataCount:16/little,
     DataOffset:16/little,
     DataDisplacement:16/little,
     SetupCount,
     _/binary>> = Res#smbpdu.wp,
    Bf = Res#smbpdu.bf,
    {SetupWords, B1} =
	if (SetupCount > 0) ->
		<<Xsw:SetupCount/binary,Xb1/binary>> = Bf,
		{Xsw, Xb1};
	   true ->
		{<<>>, Bf}
	end,
    %% We may have some pad bytes here between the
    %% ByteCount parameter and the actual data.
    %% Strip it off by computing how many bytes
    %% we have upto and including the ByteCount param,
    %% and subtract that from the DataOffset param.
    %% Whatever is left is the number of pad-bytes.
    Pad = DataOffset - 
	(?SMB_HEADER_LEN + 
	 1 +    % WordCount == 1 byte
	 ((Res#smbpdu.wc + SetupCount) * 2) + 
	 2),    % ByteCount == 2 bytes
    Data = if (Pad > 0) ->
		   <<SS:Pad/binary,Xdata/binary>> = B1,
		   Xdata;
	      true ->
		   B1
	   end,
    {ok, Data, Res}.

%%%    
%%% This is the decoding of a transaction request.
%%% NB: This function is used by esmb_browser.erl
%%%
dec_transaction_req(Req, Pdu) ->
    Res = safe_dec_smb(Req, Pdu),  % NB: may throw(Pdu)
    <<TotParamCount:16/little,
     TotDataCount:16/little,
     MaxParamCount:16/little,
     MaxDataCount:16/little,
     MaxSetup:16/little,
     Flags:16/little,
     Timeout:32/little,         % in mSec
     0:16,                      % reserved
     ParamLen:16/little,        % Parameter bytes sent this buffer
     ParamOffset:16/little,     % Offset (from header start) to parameters
     DataSize:16/little,        % Data bytes sent this buffer
     DataOffset:16/little,      % Offset (from header start) to data
     SetupCount,
     _/binary>> = Res#smbpdu.wp,
    Res.




%%% ---

%%% Detta suger häst...
-define(RESPONSE_DATA(SubCmd,    
		      Pad,
		      Sid,
		      SearchCount,
		      EndOfSearch,
		      EaErrorOffset,
		      LastNameOffset,
		      Buffer),
	if (SubCmd == ?SMB_TRANS2_FIND_FIRST2) ->
		<<Pad,
		Sid:16/little,            % Search handle
		SearchCount:16/little,    % # of entries returned
		EndOfSearch:16/little,    % Was last entry returned ?
		EaErrorOffset:16/little,  % Offset into EA list if EA error
		LastNameOffset:16/little, % Offset into Data holding the filename
		%%                          of the last entry, if server needs it 
		%%                          to resume search; else 0
		_/binary>> = Buffer;
	   true -> 
		%% SubCmd == ?SMB_TRANS2_FIND_NEXT2
		Sid = 0,
		<<Pad,
		SearchCount:16/little,    % # of entries returned
		EndOfSearch:16/little,    % Was last entry returned ?
		EaErrorOffset:16/little,  % Offset into EA list if EA error
		LastNameOffset:16/little, % Offset into Data holding the filename
		%%                          of the last entry, if server needs it 
		%%                          to resume search; else 0
		_/binary>> = Buffer
	end).
		
dec_trans2_find_x2(Req, Pdu, SubCmd) ->
    Res = safe_dec_smb(Req, Pdu),  % NB: may throw(Pdu)
    <<TotParamCount:16/little,
    TotDataCount:16/little,
    0:16/little,
    ParamCount:16/little,
    ParamOffset:16/little,
    ParamDisplacement:16/little,
    DataCount:16/little,
    DataOffset:16/little,
    DataDisplacement:16/little,
    _/binary>> = Res#smbpdu.wp,
    %%
    SMBheaderLen = ?SMB_HEADER_LEN,
    Offset = DataOffset - (SMBheaderLen + (Res#smbpdu.wc * 2) + ParamCount),
    ?RESPONSE_DATA(SubCmd,    
		   Pad,
		   Sid,
		   SearchCount,
		   EndOfSearch,
		   EaErrorOffset,
		   LastNameOffset,
		   Res#smbpdu.bf),
    <<_:DataOffset/binary, Data/binary>> = Pdu,
    Finfo = dec_find_file_both_dir_info(Res, Data, SearchCount),
    %%print_fd_info(Finfo),
    {Res, #find_result{sid = Sid, 
		       eos = to_bool(EndOfSearch), 
		       finfo = Finfo}}.

%%% ---


%%%
%%% The FIND_FILE_BOTH_DIRECTORY_INFO decoder
%%%
dec_find_file_both_dir_info(Req, Data, Max) ->
    Ucode = bytes_per_character(Req),
    dec_find_file_both_dir_info(Data, Ucode, Max, 1).

dec_find_file_both_dir_info(<<Offset:32/little,  % Offset to next struct
			     FileIndex:32/little, 
			     CT:8/binary,        % File creation time
			     AT:8/binary,        % File access time
			     WT:8/binary,        % File write time
			     HT:8/binary,        % File attribute change time
			     Size:8/binary,      % File size
			     AllocSize:8/binary, % Allocation size
			     Attr:32/little,     % Extended file attributes
			     Len:32/little,      % Length of filename (in bytes)
			     EaSize:32/little,   % Size of file's ext.attrs
			     ShortNameLen,       % Length of short fname (bytes)
			     _,                  % reserved
			     ShortName:24/binary,% Short name in Unicode(!)
			     Filename:Len/binary, 
			     Rest/binary>>,  Ucode, Max, I) when I =< Max ->
    Strip = Offset - (94 + Len),    % Strip off trailing crap...
    F = #file_info{name = Filename,
		   size = large_integer(Size),
		   attr = Attr,
		   date_time = dec_dt_find_file(CT, AT, WT)},
    [F | dec_find_file_both_dir_info(strip(Strip, Rest), Ucode, Max, I+1)];
dec_find_file_both_dir_info(Rest, Ucode, Max, I) when I > Max ->
    [];
dec_find_file_both_dir_info(Rest, Ucode, Max, I) ->
    ?elog("dec_find_file_both_dir_info: <ERROR> Missing file info I=~p~n",[I]),
    %%hexprint(b2l(Rest)),
    [].


%%%
%%% The FIND_FILE_DIRECTORY_INFO decoder
%%%
dec_find_file_dir_info(Req, Data, Max) ->
    Ucode = bytes_per_character(Req),
    dec_find_file_dir_info(Data, Ucode, Max, 1).

dec_find_file_dir_info(<<Offset:32/little,  % Offset to next struct
			FileIndex:32/little, 
			CT:8/binary,        % File creation time
			AT:8/binary,        % File access time
			WT:8/binary,        % File write time
			HT:8/binary,        % File attribute change time
			Size:8/binary,      % File size
			AllocSize:8/binary, % Allocation size
			Attr:32/little,     % Extended file attributes
			Len:32/little,      % Length of filename (in bytes)    
			Filename:Len/binary, 
			Rest/binary>>,  Ucode, Max, I) when I =< Max ->
    Strip = Offset - (64 + Len),    % Strip off trailing crap...
    F = #file_info{name = Filename,
		   size = large_integer(Size),
		   attr = Attr,
		   date_time = dec_dt_find_file(CT, AT, WT)},
    [F | dec_find_file_dir_info(strip(Strip, Rest), Ucode, Max, I+1)];
dec_find_file_dir_info(Rest, Ucode, Max, I) when I > Max ->
    [];
dec_find_file_dir_info(Rest, Ucode, Max, I) ->
    io:format("dec_find_file_dir_info: <ERROR> Missing file info I=~p~n",[I]),
    %%hexprint(b2l(Rest)),
    [].


strip(Strip, Rest0) when Strip>0, Strip<size(Rest0) ->
    <<_:Strip/binary, Rest/binary>> = Rest0,
    Rest;
strip(_, Rest) ->
    Rest.

print_fd_info([H|T]) ->
    X = H#file_info.date_time,
    {A,B,C} = X#dt.creation_date,
    {D,E,F} = X#dt.creation_time,
    io:format("~-20.s ~10.w  "
	      "~2.w:~2.w:~2.w "
	      "~2.s ~s ~w~n",
	      [b2l(esmb_client:ucs2_to_charset(H#file_info.name, "ASCII")), 
	       H#file_info.size,
	       D,E,F,
	       i2l(C), month(B), A]),
    print_fd_info(T);
print_fd_info([]) ->
    ok.

   
%%% 
%%% We are not using this format anymore,
%%% but we keep the code anyway.
%%%
dec_info_standard(Req, Data, Max) ->
    Ucode = bytes_per_character(Req),
    dec_info_standard(Data, Ucode, Max, 1).

dec_info_standard(<<_:4/binary, DT:12/binary, Size:32/little,
		  _:4/binary, Attr:16/little, Len, 
		  Rest0/binary>>, Ucode, Max, I) when I<Max->
    Rest1 = strip_upad(Ucode, Rest0),
    <<Filename:Len/binary, _:Ucode/binary, Rest/binary>> = Rest1,
    F = #file_info{name = Filename,
		   size = Size,
		   attr = Attr,
		   date_time = dec_dt_info_std(DT)},
    [F | dec_info_standard(Rest, Ucode, Max, I+1)];
dec_info_standard(<<Rkey:4/binary, DT:12/binary, Size:32/little,
		  _:4/binary, Attr:16/little, Len, 
		  Rest0/binary>>, Ucode, Max, Max) ->
    Rest1 = strip_upad(Ucode, Rest0),
    <<Filename:Len/binary, _:Ucode/binary, Rest/binary>> = Rest1,
    F = #file_info{name = Filename,
		   size = Size,
		   attr = Attr,
		   date_time = dec_dt_info_std(DT),
		   resume_key = Rkey},
    [F];
dec_info_standard(Data, Ucode, Max, I) ->
    io:format("dec_info_standard: <ERROR> Missing file info I=~p~n",[I]),
    [].


bytes_per_character(Pdu) when record(Pdu,smbpdu),
			      ?F2_USE_UNICODE(Pdu) -> 
    2;
bytes_per_character(Neg) when record(Neg,smb_negotiate_res),
			    ?USE_UNICODE(Neg) ->  
    2;
bytes_per_character(_) -> 
    1.


%%%
%%% In case of unicode, we have a pad byte in front !
%%%
strip_upad(1, Bin)               -> Bin;
strip_upad(2, <<_, Bin/binary>>) -> Bin.

%%%
%%% DOS - Date/Time format
%%% Year has a range 0-119 which represents 1980-2099
%%% Twoseconds has a range 0-29 representing two seconds increment
%%%
dec_dt_info_std(<<CreationDate:2/binary, CreationTime:2/binary,
		  LastAccessDate:2/binary, LastAccessTime:2/binary,
		  LastWriteDate:2/binary, LastWriteTime:2/binary>>) ->
    <<Yc:7, Mc:4, Dc:5>> = swap(CreationDate),
    <<Hc:5, Ic:6, Tc:5>> = swap(CreationTime),
    <<Ya:7, Ma:4, Da:5>> = swap(LastAccessDate),
    <<Ha:5, Ia:6, Ta:5>> = swap(LastAccessTime),
    <<Yw:7, Mw:4, Dw:5>> = swap(LastWriteDate),
    <<Hw:5, Iw:6, Tw:5>> = swap(LastWriteTime),
    #dt{creation_date    = {Yc + 1980, Mc, Dc},
	creation_time    = {Hc, Ic, Tc * 2},
	last_access_date = {Ya + 1980, Ma, Da},
	last_access_time = {Ha, Ia, Ta * 2},
	last_write_date  = {Yw + 1980, Mw, Dw},
	last_write_time  = {Hw, Iw, Tw * 2}}.

swap(<<X,Y>>) -> <<Y,X>>.
	

dec_dt_find_file(CT,AT,WT) ->
    CTime = large_integer(CT),
    ATime = large_integer(AT),
    WTime = large_integer(WT),
    %% NB: The TIME comes in units of 100 nano seconds !!
    Csec = (CTime div 10000000) + ?GREG_SEC_0_TO_1601,
    Asec = (ATime div 10000000) + ?GREG_SEC_0_TO_1601,
    Wsec = (WTime div 10000000) + ?GREG_SEC_0_TO_1601,
    {Xd,Xt} = calendar:gregorian_seconds_to_datetime(Csec),
    {Yd,Yt} = calendar:gregorian_seconds_to_datetime(Asec),
    {Zd,Zt} = calendar:gregorian_seconds_to_datetime(Wsec),
    #dt{creation_date    = Xd,
	creation_time    = Xt,
	last_access_date = Yd,
	last_access_time = Yt,
	last_write_date  = Zd,
	last_write_time  = Zt}.


large_integer(<<LowPart:32/little, HiPart:32/little>>) ->
    if (HiPart == 0) -> LowPart;
       true          -> (HiPart * 16#100000000) + LowPart
    end.


    
	
%%% ---    


safe_dec_smb(Req, Pdu) ->
    case catch dec_smb(Req, Pdu) of
	R when R#smbpdu.eclass == ?ERRNT, R#smbpdu.ecode == ?SUCCESS -> R;
	R when R#smbpdu.eclass == ?SUCCESS -> R;
	R when record(R,smbpdu)            -> throw(R);
	Else                               -> throw({error,{dec_smb,Else}})
    end.


%%%
%%% NB: It is important that we maintain the Input-Req record
%%%     so that we don't loose the sequence number counters.
%%%    
%%% NB-2: We need to increment the sequence number counter here !!
%%%
%%% @private
dec_smb(Req, 
	<<16#FF, $S, $M, $B,          % smb-header
	Cmd,
	Status:4/binary,
	Flags,
	Flags2:16/little,
	_:12/unit:8,                  % Pad (12 bytes)
	Tid:16/little, 
	Pid:16/little, 
	Uid:16/little,
	Mid:16/little,
	Wc,
	Rest/binary>>) ->
    if ((Flags2 band ?FLAGS2_NT_ERR_CODES) > 0) ->
	    <<Ecode:32/little>> = Status,
	    Eclass = ?ERRNT;
       true ->
	    <<Eclass, 
	      _,                      % zero (not used)
	      Ecode:16/little>> = Status
    end,
    <<Wp:Wc/binary-unit:16, Bc:16/little, Bf/binary>> = Rest,
    SSN = Req#smbpdu.sign_seqno,
    ?sdbg("dec_smb: new SSN=~p~n",[SSN+1]),
    Req#smbpdu{cmd    = Cmd,
	       eclass = Eclass,
	       ecode  = Ecode,
	       flags  = Flags,
	       flags2 = Flags2,
	       tid    = Tid,
	       pid    = Pid,
	       uid    = Uid,
	       mid    = Mid,
	       sign_seqno = SSN + 1,
	       wc     = Wc,
	       wp     = Wp,
	       bc     = Bc,
	       bf     = Bf}.
    
	
%%% --------------------------------------------------------------------
%%% SMB encode routines
%%% --------------------------------------------------------------------


smb_close_file_pdu(InReq) ->
    {Wc,Wp} = wp_close_file(InReq#smbpdu.fid),
    Rec = ?CP_PDU(InReq)#smbpdu{
		   cmd = ?SMB_CLOSE,
		   wc = Wc,
		   wp = Wp},
    enc_smb(Rec).

wp_close_file(Fid) ->
    {3,
     <<Fid:16/little,
     0:32/little>>}.     % Time of last write (set by local system)


%%% ---

smb_delete_file_pdu(InReq, Fname) ->
    {Wc,Wp} = wp_delete_file(),
    Bf = bf_delete_file(InReq, Fname),
    Rec = ?CP_PDU(InReq)#smbpdu{
		   cmd = ?SMB_DELETE,
		   wc = Wc,
		   wp = Wp,
		   bc  = size(Bf),
		   bf  = Bf},
    enc_smb(Rec).

wp_delete_file() ->
    {1,
     <<0:16/little>>}.   % SearchAttributes

bf_delete_file(InReq, Fname) ->
    list_to_binary([?BUF_FMT_ASCII,   % Buffer format
		    Fname, null(InReq)]).      % Filename 

%%% ---

smb_delete_dir_pdu(InReq, Dir) ->
    Bf = bf_delete_directory(InReq, Dir),
    Rec = ?CP_PDU(InReq)#smbpdu{
		   cmd = ?SMB_DELETE_DIRECTORY,
		   bc  = size(Bf),
		   bf  = Bf},
    enc_smb(Rec).

bf_delete_directory(InReq, Dir) ->
    list_to_binary([?BUF_FMT_ASCII,   % Buffer format
		    Dir, null(InReq)]).        % Dir path

%%% ---

smb_write_andx_pdu(InReq, Finfo, Data, Written) ->
    DataLen = size(Data),
    FileOffset = Finfo#file_info.offset + Written,
    {Wc,Wp} = wp_write_andx(InReq#smbpdu.fid, FileOffset, DataLen),
    Rec = ?CP_PDU(InReq)#smbpdu{
		   cmd = ?SMB_WRITE_ANDX,
		   wc = Wc,
		   wp = Wp,
		   bc = DataLen,
		   bf = Data},  
    enc_smb(Rec).

wp_write_andx(Fid, FileOffset, Dlen) ->
    Remaining = 0,
    %% How to compute the offset values (no.of bytes):
    %% ParamOffset = ?SMB_HEADER_LEN + ThisLen + WordCount + ByteCount 
    %%             = ?SMB_HEADER_LEN + 1 + 12*2 + 2
    %%             = ?SMB_HEADER_LEN + 27
    Offset = ?SMB_HEADER_LEN + 27,
    {12,                        % (ThisLen)
     <<?NoAndxCmd,              
     0,                         % reserved
     0:16/little,               % offset to next command WC
     Fid:16/little,             % File handle
     FileOffset:32/little,      % Offset in file to begin write
     0:32/little,               % reserved
     0:16/little,               % Write mode: 0 = write through
     Remaining:16/little,       % Bytes remaining to satisfy request
     0:16/little,               % DataLengthHigh, zero if not CAP_LARGE_WRITEX
     Dlen:16/little,            % Number of data bytes in buffer (>=0)
     Offset:16/little>>}.       % Offset to data bytes


%%% ---

%%% Note that function also is used when fetching DCE/RPC fragments !!

smb_read_andx_pdu(InReq, Finfo) ->
    {Wc,Wp} = wp_read_andx(InReq#smbpdu.fid, 
			   Finfo#file_info.data_len,
			   Finfo#file_info.max_count,
			   Finfo#file_info.min_count),
    Rec = ?CP_PDU(InReq)#smbpdu{
		   cmd = ?SMB_READ_ANDX,
		   wc = Wc,
		   wp = Wp},
    enc_smb(Rec).

wp_read_andx(Fid, Offset, MaxCount, MinCount) ->
    {10,
     <<?NoAndxCmd,              
     0,                         % reserved
     0:16/little,               % offset to next command WC
     Fid:16/little,             % File handle
     Offset:32/little,          % Offset in file to begin read
     MaxCount:16/little,        % Max number of bytes to return
     MinCount:16/little,        % Reserved for obsolescent requests
     0:32/little,               % High 16 buts of MaxXount if CAP_LARGE READX;
                                % else must be zero
     0:16/little>>}.            % Remaining, obsolescent requests

%%% ---

-record(candx, {
	  type = file,     % file | dir
	  mode = ro}).     % ro | rw
	  

smb_open_file_ro_pdu(InReq, Path) ->
    smb_nt_create_andx_pdu(InReq, Path, #candx{type=file,mode=ro}).

smb_open_file_rw_pdu(InReq, Path) ->
    smb_nt_create_andx_pdu(InReq, Path, #candx{type=file,mode=rw}).

smb_open_dir_pdu(InReq, Path) ->
    smb_nt_create_andx_pdu(InReq, Path, #candx{type=dir}).


smb_nt_create_andx_pdu(InReq, Path, Opts) ->
    {Wc,Wp} = wp_nt_create_andx(sizeof(Path), Opts),
    Bf = bf_nt_create_andx(InReq, Path),
    Rec = ?CP_PDU(InReq)#smbpdu{
		   cmd = ?SMB_NT_CREATE_ANDX,
		   wc = Wc,
		   wp = Wp,
		   bc  = size(Bf),
		   bf  = Bf},
    enc_smb(Rec).

wp_nt_create_andx(NameLen, Opts) ->
    OpLock = oplock(Opts),
    AccessMask = access_mask(Opts),
    FileAttrs = file_attributes(Opts),
    ShareAccess = share_access(Opts),
    CreateOptions = create_options(Opts),
    CreateDisp = create_dispositions(Opts),
    {24,
     <<?NoAndxCmd,              
      0,                         % reserved
      0:16/little,               % offset to next command WC
      0,                         % reserved
      NameLen:16/little,         % Length of Name[] in bytes
      OpLock:32/little,          % Oplock Flags
      0:32/little,               % RootDirectoryFid
      AccessMask:32/little,      % Desired access
      0:32/little,               % Allocaton size MSW
      0:32/little,               %                LSW (64 bit)
      FileAttrs:32/little,       % File attributes
      ShareAccess:32/little,     % Type of share access
      CreateDisp:32/little,      % Create disposition
      CreateOptions:32/little,   % Create options
      ?SECURITY_IDENTIFICATION:32/little, % Security QOS info (?)
      0>>}.                      % Security tracking mode flag (?)

%%%
%%% When a string is passed in Unicode format, it must be
%%% word-aligned with respect to the beginning of the SMB !!
%%% Should the string not naturally fall on a two-byte boundary,
%%% a null byte of padding will be inserted, and the Unicode
%%% string will begin at the next address.
%%%

bf_nt_create_andx(InReq, Name) when ?F2_USE_UNICODE(InReq) ->
    list_to_binary([0,                   % pad !
		    Name, null(InReq)]); % filename
bf_nt_create_andx(InReq, Name) ->
    list_to_binary([                     % don't pad !
		    Name, null(InReq)]). % filename


oplock(_) -> ?NO_OPLOCK.

access_mask(X) when X#candx.mode == ro -> ?AM_READ;
access_mask(X) when X#candx.mode == rw -> ?AM_READ bor ?AM_WRITE.

file_attributes(X) when X#candx.type == file -> ?FILE_ATTR_NORMAL;
file_attributes(X) when X#candx.type == dir  -> ?FILE_ATTR_DIR.

share_access(X) when X#candx.type == file -> ?FILE_SHARE_READ;
share_access(X) when X#candx.type == dir  -> ?FILE_SHARE_RW.

create_dispositions(X) when X#candx.type == file,
			    X#candx.mode == rw   -> ?FILE_OWRITE_IF;
create_dispositions(X) when X#candx.type == file -> ?FILE_OPEN;
create_dispositions(X) when X#candx.type == dir  -> ?FILE_CREATE.

create_options(X) when X#candx.type == file -> ?FILE_OPEN_OPTIONS;
create_options(X) when X#candx.type == dir  -> ?DIR_OPEN_OPTIONS.


%%% ---

smb_trans2_find_next2_pdu(InReq, Path, Sid) ->
    {Wc,Wp} = wp_trans2_find_x2(?SMB_TRANS2_FIND_NEXT2, 
				sizeof(Path), length(null(InReq))),
    Bf = bf_trans2_find_next2(InReq, Path, Sid),
    Rec = ?CP_PDU(InReq)#smbpdu{
		   cmd = ?SMB_COM_TRANSACTION2,
		   wc  = Wc,
		   wp  = Wp,
		   bc  = size(Bf),
		   bf  = Bf,
		   sub_cmd = ?SMB_TRANS2_FIND_NEXT2},
    enc_smb(Rec).

bf_trans2_find_next2(InReq, Path, Sid) ->
    %% Flags ::= DoNotClose,CloseAtEndOfSearch,ReturnResumeKey,ContinueSearch,NoBackupIntent
    %% ContinueSearch,Resume,CloseAtEOS,
    Flags = <<16#000e:16/little>>,
    list_to_binary([0,           % Must be null
		    0,0,         % Pad to SHORT or LONG
		    %% --- Start of Parameter Block (12 bytes) ---
		    <<Sid:16/little>>, % Search attribute or SID
		    <<512:16/little>>, % Max # of entries returned
		    <<?SMB_FIND_FILE_BOTH_DIRECTORY_INFO:16/little>>, % What info to return in the result
		    <<0:32/little>>,   % Resume key
		    Flags,             % Flags
		    Path, null(InReq)  % Search pattern
		    ]).

%%% ---

smb_trans2_find_first2_pdu(InReq, Path) ->
    {Wc,Wp} = wp_trans2_find_x2(?SMB_TRANS2_FIND_FIRST2, 
				sizeof(Path), length(null(InReq))),
    Bf = bf_trans2_find_first2(InReq, Path),
    Rec = ?CP_PDU(InReq)#smbpdu{
		   cmd = ?SMB_COM_TRANSACTION2,
		   wc  = Wc,
		   wp  = Wp,
		   bc  = size(Bf),
		   bf  = Bf,
		   sub_cmd = ?SMB_TRANS2_FIND_FIRST2},
    enc_smb(Rec).


wp_trans2_find_x2(SubCmd, PathLen, NullLen) ->
    %% How to compute the offset values (no.of bytes):
    %% ParamOffset = ?SMB_HEADER_LEN + ThisLen + WordCount + ByteCount + Pad
    %%             = ?SMB_HEADER_LEN + 30 + 1 + 2 + 3
    %%             = ?SMB_HEADER_LEN + 36
    %% ParamLen    = ParameterBytesLen + PathLen + NullByteLen
    %%             = 12 + PathLen + NullByteLen
    %% DataOffset  = ParamOffset + ParamLen
    %%
    ParamOffset = ?SMB_HEADER_LEN + 36,
    ParamLen    = 12 + PathLen + NullLen,
    DataOffset  = ParamOffset + ParamLen,
    ParamCount  = DataOffset - ParamOffset,
    {15,                         % WordCount = 14 + SetupCount
     <<ParamCount:16/little,     % Total parameter bytes sent
      0:16/little,               % Total data bytes sent
      10:16/little,              % Max parameter bytes to return
      4096:16/little,            % Max data bytes to return
      0,                         % Max setup words to return
      0,                         % reserved
      0:16/little,               % Flags
      0:32/little,               % timeout , 0 = return immediately
      0:16/little,               % reserved2
      ParamLen:16/little,        % Parameter bytes sent this buffer
      ParamOffset:16/little,     % Offset (from header start) to parameters
      0:16/little,               % Data bytes sent this buffer
      DataOffset:16/little,      % Offset (from header start) to data
      1,                         % Count of setup words
      0,                         % reserved3 (pad above to word boundary)
      SubCmd:16/little           % Setup words (# = Setup word count)
      >>}.

bf_trans2_find_first2(InReq, Path) ->
    %% Sa ::= Archive,Directory,Volume,System,Hidden,ReadOnly
    %% Include: Hidden, System and Directory in search result
    Sa = <<16#0016:16/little>>,
    %% Flags ::= DoNotClose,CloseAtEndOfSearch,ReturnResumeKey,ContinueSearch,NoBackupIntent
    %% Resume,CloseAtEOS
    Flags = <<16#0006:16/little>>,
    list_to_binary([0,           % Must be null
		    0,0,         % Pad to SHORT or LONG
		    %% --- Start of Parameter Block (12 bytes) ---
		    Sa,          % Search attribute
		    <<512:16/little>>, % Max # of entries returned
		    Flags,       % Flags
		    <<?SMB_FIND_FILE_BOTH_DIRECTORY_INFO:16/little>>, % What info to return in the result
		    <<0:32/little>>,  % Storage type
		    Path, null(InReq) % Search pattern
		    ]).

sizeof(B) when binary(B) -> size(B);
sizeof(L) when list(L)   -> length(L).


%%% ---

smb_check_directory_pdu(InReq, Path) ->
    Bf = bf_check_directory(InReq, Path),
    Rec = ?CP_PDU(InReq)#smbpdu{
		   cmd = ?SMB_CHECK_DIRECTORY,
		   bc  = size(Bf),
		   bf  = Bf},
    enc_smb(Rec).

bf_check_directory(InReq, Path) ->
    list_to_binary([?BUF_FMT_ASCII,      % Buffer format
		    Path, null(InReq)]). % Dir path

%%% ---
    
smb_tree_connect_andx_pdu(Neg, InReq, Path, Service) ->
    {Wc,Wp} = wp_tree_connect_andx(Neg),
    Bf = bf_tree_connect_andx(Neg, Path, Service),
    Rec = ?CP_PDU(InReq)#smbpdu{
		   cmd = ?SMB_TREE_CONNECT_ANDX,
		   flags2 = flags2(Neg),
		   wc = Wc,
		   wp = Wp,
		   bc  = size(Bf),
		   bf  = Bf},
    enc_smb(Rec).

wp_tree_connect_andx(Neg) ->
    {4,
     <<?NoAndxCmd,              
      0,                         % reserved
      0:16/little,               % offset to next command WC
      0:16/little,               % Flags
      1:16/little>>}.            % PasswordLength (incl. NULL)

bf_tree_connect_andx(Neg, Path, Service) ->
    list_to_binary([0,                    % password
		    Path, null(Neg),      % filesystem 
		    Service, 0]).         % service (never in Unicode !)


null(Neg) when record(Neg,smb_negotiate_res),
	       ?USE_UNICODE(Neg) -> 
    [0,0];
null(Pdu) when record(Pdu,smbpdu),
	       ?F2_USE_UNICODE(Pdu) -> 
    [0,0];
null(_) -> 
    [0].

%%% ---

%%%
%%% If the dialect is earlier than "NTLM 0.12" then the client computes
%%% the response using the "LM session key". If the dialect is "NTLM 0.12"
%%% then the client may compute the response either using the "LM session key",
%%% or the "NT session key", or both. The server may choose to refuse
%%% responses computed using the "LM session key".
%%%
smb_session_setup_andx_pdu(Neg, U) when ?PRE_DOS_LANMAN_2_1(Neg) ->
    {Passwd, PwLen, MacKey}   = enc_lm_passwd(Neg, U#user.pw),
    {Wc,Wp} = wp_session_setup_andx(Neg, U, PwLen),
    Bf = bf_session_setup_andx(Neg, U, Passwd),
    Rec = #smbpdu{cmd = ?SMB_SESSION_SETUP_ANDX,
		  pid = mypid(),
		  mid = 1,
		  flags2 = flags2(Neg),
		  mac_key = MacKey,
		  wc = Wc,
		  wp = Wp,
		  bc  = size(Bf),
		  bf  = Bf},
    enc_smb(Rec);
%%%
smb_session_setup_andx_pdu(Neg, U) when ?NTLM_0_12(Neg),
					?USE_UNICODE(Neg) ->
    {RN, RNlen, MacKey} = enc_nt_passwd(Neg, U#user.pw, U#user.charset),
    {Wc,Wp} = wp_session_setup_andx(Neg, U, 0, RNlen), 
    Bf = bf_session_setup_andx(Neg, U, RN),
    Rec = #smbpdu{cmd = ?SMB_SESSION_SETUP_ANDX,
		  pid = mypid(),
		  mid = 1,
		  flags2 = flags2(Neg),
		  signatures = signatures_p(Neg),
		  mac_key = MacKey,
		  wc = Wc,
		  wp = Wp,
		  bc  = size(Bf),
		  bf  = Bf},
    enc_smb(Rec);
%%%
smb_session_setup_andx_pdu(Neg, U) when ?NTLM_0_12(Neg),
					?USE_UNICODE_ANYWAY(Neg) ->
    {RN, RNlen, MacKey} = enc_nt_passwd(Neg, U#user.pw, U#user.charset),
    {Wc,Wp} = wp_session_setup_andx(Neg, U, 0, RNlen), 
    Bf = bf_session_setup_andx(Neg, U, RN),
    Rec = #smbpdu{cmd = ?SMB_SESSION_SETUP_ANDX,
		  pid = mypid(),
		  mid = 1,
		  flags2 = flags2(Neg),
		  signatures = signatures_p(Neg),
		  mac_key = MacKey,
		  wc = Wc,
		  wp = Wp,
		  bc  = size(Bf),
		  bf  = Bf},
    enc_smb(Rec);
%%%
smb_session_setup_andx_pdu(Neg, U) when ?NTLM_0_12(Neg) ->
    {Passwd, PwLen, MacKey} = enc_lm_passwd(Neg, U#user.pw),
    {Wc,Wp} = wp_session_setup_andx(Neg, U, PwLen, 0),
    Bf = bf_session_setup_andx(Neg, U, Passwd),
    Rec = #smbpdu{cmd = ?SMB_SESSION_SETUP_ANDX,
		  pid = mypid(),
		  mid = 1,
		  flags2 = flags2(Neg),
		  signatures = signatures_p(Neg),
		  mac_key = MacKey,
		  wc = Wc,
		  wp = Wp,
		  bc  = size(Bf),
		  bf  = Bf},
    enc_smb(Rec).

flags2(Neg) when ?USE_UNICODE(Neg) -> ?FLAGS2_NTLM;
flags2(_)                          -> ?FLAGS2_LONG_NAMES.

signatures_p(Neg) when ?USE_SIGNATURES(Neg) -> true;
signatures_p(_)                             -> false.

wp_session_setup_andx(Neg, U, PwLen) ->
    {10,
     <<?NoAndxCmd,              
      0,                         % reserved
      0:16/little,               % offset to next command WC
      ?MAX_BUFFER_SIZE:16/little,
      ?MaxMpxCount:16/little,   
      ?VcNumber:16/little,
      0:32/little,               % session key
      PwLen:16/little,
      0:32/little>>}.            % reserved
%%%    
wp_session_setup_andx(Neg, U, PwLen, UPwLen) ->
    {13,
     <<?NoAndxCmd,              
      0,                         % reserved
      0:16/little,               % offset to next command WC
      ?MAX_BUFFER_SIZE:16/little,
      ?MaxMpxCount:16/little,   
      ?VcNumber:16/little,
      0:32/little,               % session key
      PwLen:16/little,           % ANSI password length
      UPwLen:16/little,          % UNICODE password length
      0:32/little,               % reserved
      ?CAP_UNICODE:32/little>>}. % client capabilities


bf_session_setup_andx(Neg, U, Passwd) when ?USE_UNICODE(Neg) ->
    {ok, Cd}    = iconv:open(?CSET_UCS2, ?CSET_ASCII),
    {ok, Uname} = iconv:conv(Cd, l2b(U#user.name)),
    {ok, Udom}  = iconv:conv(Cd, l2b(U#user.primary_domain)),
    {ok, Unos}  = iconv:conv(Cd, l2b(U#user.native_os)),
    {ok, Ulan}  = iconv:conv(Cd, l2b(U#user.native_lanman)),
    iconv:close(Cd),
    list_to_binary([Passwd,
		    [0],         % if Unicode, pad to even byte boundary
		    Uname,[0,0],
		    Udom,[0,0],
		    Unos,[0,0],
		    Ulan,[0,0]]);
%%%
bf_session_setup_andx(Neg, U, Passwd) ->
    list_to_binary([Passwd,
		    U#user.name,[0],
		    U#user.primary_domain,[0],
		    U#user.native_os,[0],
		    U#user.native_lanman,[0]]).



%%%
%%% Compute the LM-challenge-response and the MAC-Key
%%%
%%% NB: The notion of the 'Session-Key' is somewhat confusing.
%%%     What the SNIA doc call 'S16' (the partial MacKey) is
%%%     what we call (in Samba terms) the Session-Key.
%%%
enc_lm_passwd(Neg, Passwd) when ?CORE_PROTOCOL(Neg) ->
    {Passwd, sizeof(Passwd), Passwd};
enc_lm_passwd(Neg, Passwd) when ?USE_ENCRYPTION(Neg) -> 
    EncKey = Neg#smb_negotiate_res.encryption_key,
    RN = lm_challenge_response(Passwd, EncKey),
    LMsessKey = lm_session_key(Passwd),
    MacKey = concat(LMsessKey, RN),
    {RN, sizeof(RN), MacKey};
enc_lm_passwd(Neg, Passwd) ->
    {Passwd, sizeof(Passwd), Passwd}.

lm_session_key(Passwd) when list(Passwd) ->
    lm_session_key(l2b(Passwd));
lm_session_key(Passwd) when binary(Passwd) ->
    concat(head(s16x(Passwd), 8), zeros(8)).


%%%
%%% Compute the NT-challenge-response and the MAC-Key
%%%
%%% NB: The NTLM Session Key is the MD4 of the MD4 of the 
%%%     Unicode password. The SNIA doc is wrong when it says 
%%%     there's only one MD4 (see also C.Hertel's book).
%%%
enc_nt_passwd(Neg, Passwd, Cset) when ?NTLM_0_12(Neg), 
				      ?USE_ENCRYPTION(Neg) ->
    {ok, Cd} = iconv:open(?CSET_UCS2, Cset),
    {ok, UCS2pw} = iconv:conv(Cd, l2b(Passwd)),
    iconv:close(Cd),
    EncKey = Neg#smb_negotiate_res.encryption_key,
    RN = nt_challenge_response(UCS2pw, EncKey),
    {ok, X} = md4:digest(UCS2pw),
    {ok, NTsessKey} = md4:digest(X),
    MacKey = concat(NTsessKey, RN),
    {RN, sizeof(RN), MacKey};
enc_nt_passwd(_, Passwd, _) ->
    %% FIXME What does this mean ?
    {Passwd, sizeof(Passwd), Passwd}.

%%%
%%% TEST LM-SessionKey: 
%%%
%%%  smbclient //pungmes/tobbe -U tobbe -m LANMAN1
%%%
%%%   EncKey = 1cb2c4dc19d52588
%%%
%%%   Passwd = qwe123
%%%
%%%   Response = b6c89e28077ada40648149220da0ca5c9f5aa481a3f88467
%%%
%%% TEST NT-SessionKey: 
%%%
%%%  smbclient //pungmes/tobbe -U tobbe
%%%
%%%   EncKey = 9d5d78803705c22e
%%%
%%%   Passwd = qwe123
%%%
%%%   Response = 75e19308dd287c1905f73e519ee5fd41b4ebaa262ed284f4
%%%

lmtest() ->
    EncKey = <<16#1c,16#b2,16#c4,16#dc,16#19,16#d5,16#25,16#88>>,
    Resp = lm_challenge_response(<<"qwe123">>, EncKey),
    {Resp == lmtest_response(),
     Resp,
     lmtest_response()}.

lmtest_response() ->
    <<16#b6,16#c8,16#9e,16#28,16#07,16#7a,16#da,16#40,16#64,
      16#81,16#49,16#22,16#0d,16#a0,16#ca,16#5c,16#9f,16#5a,
       16#a4,16#81,16#a3,16#f8,16#84,16#67>>.
    
nttest() ->
    EncKey = <<16#9d,16#5d,16#78,16#80,16#37,16#05,16#c2,16#2e>> ,
    {ok, Cd} = iconv:open(?CSET_UCS2, ?CSET_ASCII),
    {ok, UCS2pw} = iconv:conv(Cd, l2b("qwe123")),
    iconv:close(Cd),
    Resp = nt_challenge_response(UCS2pw, EncKey),
    {Resp == nttest_response(),
     Resp,
     nttest_response()}.

nttest_response() ->
    <<16#75,16#e1,16#93,16#08,16#dd,16#28,16#7c,16#19,16#05,
     16#f7,16#3e,16#51,16#9e,16#e5,16#fd,16#41,16#b4,16#eb,
     16#aa,16#26,16#2e,16#d2,16#84,16#f4>>.
    

lm_challenge_response(Passwd, Challenge) when binary(Passwd) -> 
    ex(s21_lm_session_key(Passwd), Challenge);
lm_challenge_response(Passwd, Challenge) when list(Passwd) -> 
    lm_challenge_response(list_to_binary(Passwd), Challenge).


nt_challenge_response(Passwd, Challenge) when binary(Passwd) -> 
    ex(s21_nt_session_key(Passwd), Challenge);
nt_challenge_response(Passwd, Challenge) when list(Passwd) -> 
    nt_challenge_response(list_to_binary(Passwd), Challenge).


ex(<<K0:7/binary,K1:7/binary>>, Data) when size(Data) == 8 ->
    concat_binary([e(K0, Data),
		   e(K1, Data)]);
ex(<<K0:7/binary,K1:7/binary,K2:7/binary>>, Data) when size(Data) == 8 ->
    concat_binary([e(K0, Data),
		   e(K1, Data),
		   e(K2, Data)]);
ex(K, D)  ->
    io:format("<FATAL ERROR>: K=~p~nD=~p~n",[K,D]),
    exit("fatal_error").

e(K,D) -> 
    B = crypto:des_cbc_encrypt(s2k(K), null_vector(), D).

null_vector() -> <<0,0,0,0,0,0,0,0>>.

s21_lm_session_key(Passwd) -> 
    S16X  = s16x(Passwd),
    Zero5 = zeros(5),
    <<S16X/binary, Zero5/binary>>.

s21_nt_session_key(Passwd) -> 
    {ok, S16}  = md4:digest(Passwd),
    Zero5 = zeros(5),
    <<S16/binary, Zero5/binary>>.

%%%
%%% What does this function do ??
%%% According to the CIFS spec we should do
%%% bit reverse on each byte. But instead we
%%% do this...
%%%
%%% See libsmb/smbdes.c str_to_key(Str,Key)
%%%
%%% Here is an explanation from the samba.internals News group:
%%%
%%%   "str_to_key converts a 7 character string (7 bytes, 8 bits 
%%%    per byte, total56 bits) to a DES key (8 bytes, 7 bits per 
%%%    byte, total 56 bits). In an actual DES there is a parity 
%%%    involved in the low order bit but this is not used by smbdes."
%%%
s2k(<<S0,S1,S2,S3,S4,S5,S6>>) ->
    K0 = S0 bsr 1,
    K1 = ((S0 band 16#01) bsl 6) bor (S1 bsr 2),
    K2 = ((S1 band 16#03) bsl 5) bor (S2 bsr 3),
    K3 = ((S2 band 16#07) bsl 4) bor (S3 bsr 4),
    K4 = ((S3 band 16#0F) bsl 3) bor (S4 bsr 5),
    K5 = ((S4 band 16#1F) bsl 2) bor (S5 bsr 6),
    K6 = ((S5 band 16#3F) bsl 1) bor (S6 bsr 7),
    K7 = S6 band 16#7F,
    list_to_binary([X bsl 1 || X <- [K0,K1,K2,K3,K4,K5,K6,K7]]);
s2k(<<B0:7/binary,B1:7/binary>>) ->
    concat_binary([s2k(B0),s2k(B1)]).
    

s16x(Passwd) -> 
    ex(p14(Passwd), n8()).

p14(Passwd) when size(Passwd) =< 14 ->
    Upasswd = list_to_binary(ucase(binary_to_list(Passwd))),
    Zeros = zeros(14 - size(Passwd)),
    <<Upasswd/binary, Zeros/binary>>.

n8() -> <<16#4b,16#47,16#53,16#21,16#40,16#23,16#24,16#25>>.

zeros(N) ->
    list_to_binary(zerosN(N)).

zerosN(0)          -> [];
zerosN(N) when N>0 -> [0 | zerosN(N-1)].

%%%
%%% MAC handling
%%%
%%% The sender of a message inserts the sequence number SN
%%% into the message by putting it into the first 4 bytes
%%% of the SecuritySignature field and zeroing the last 4 bytes,
%%% computes the MAC over the entire message, then puts the MAC
%%% in the field.
%%%
%%% The receiver of a message validates the MAC by extracting 
%%% the value of the SecuritySignature field, putting its RSN 
%%% into the first 4 bytes of the SecuritySignature field and 
%%% zeroing the last 4 bytes, computing the MAC, and comparing
%%% it to the extracted value.
%%%

mac(MacKey, Data) when binary(MacKey), binary(Data) ->
    head(erlang:md5(concat(MacKey, Data)), 8).


mac_test() ->
    EncKey = l2b([16#b1,16#38,16#fb,16#e6,16#0f,16#7c,16#0c,16#2e]),
    Sign = [16#d0,16#42,16#72,16#12,16#29,16#3a,16#4a,16#e9],
    File = "/home/tobbe/Ethereal/setup.bin",
    MacKey = hej("qwe123", EncKey),
    mac_test(0, MacKey, File, Sign).

mac_test2(0) ->
    EncKey = l2b([16#0a,16#cf,16#ca,16#37,16#73,16#f5,16#d8,16#21]),
    Sign = [16#5a,16#3c,16#23,16#8b,16#8d,16#e1,16#7b,16#f7],
    File = "/home/tobbe/Ethereal/ssetup.bin",
    MacKey = hej("qwe123", EncKey),
    mac_test(0, MacKey, File, Sign);
mac_test2(1) ->
    EncKey = l2b([16#0a,16#cf,16#ca,16#37,16#73,16#f5,16#d8,16#21]),
    Sign = [16#a3,16#d9,16#e1,16#4a,16#5e,16#63,16#24,16#99],
    File = "/home/tobbe/Ethereal/tconnect.bin",
    MacKey = hej("qwe123", EncKey),
    mac_test(2, MacKey, File, Sign);
mac_test2(2) ->
    EncKey = l2b([16#0a,16#cf,16#ca,16#37,16#73,16#f5,16#d8,16#21]),
    Sign = [16#02,16#34,16#2e,16#b7,16#7a,16#ea,16#7b,16#65],
    File = "/home/tobbe/Ethereal/cdir.bin",
    MacKey = hej("qwe123", EncKey),
    mac_test(4, MacKey, File, Sign);
mac_test2(N) ->
    EncKey = l2b([16#0a,16#cf,16#ca,16#37,16#73,16#f5,16#d8,16#21]),
    Sign = [16#02,16#34,16#2e,16#b7,16#7a,16#ea,16#7b,16#65],
    File = "/home/tobbe/Ethereal/cdir.bin",
    MacKey = hej("qwe123", EncKey),
    mac_test(N, MacKey, File, Sign).


hej(Passwd, EncKey) ->
    {ok, Cd} = iconv:open(?CSET_UCS2, "ASCII"),
    {ok, UCS2pw} = iconv:conv(Cd, l2b(Passwd)),
    iconv:close(Cd),
    RN = nt_challenge_response(UCS2pw, EncKey),
    {ok, X} = md4:digest(UCS2pw),
    {ok, Y} = md4:digest(X),
    concat(Y, RN).
    


mac_test(SSN, MacKey, File, Sign) ->
    {ok,Bin} = file:read_file(File),
    io:format("READ PDU(~p): ",[size(Bin)]), hexprint(Bin),
    io:format("md5sum: ",[]), hexprint(erlang:md5(Bin)),
    %%
    <<Head:14/binary,XX:8/binary,Tail/binary>> = Bin,
    io:format("FIRST PART(~p): ",[size(Head)]), hexprint(Head),
    io:format("DEL SIGNATURE(~p): ",[size(XX)]), hexprint(XX),
    io:format("TAIL PART(~p): ",[size(Tail)]), hexprint(Tail),
    New = <<Head/binary,SSN:32/little,0:32,Tail/binary>>,
    %%
    C0 = erlang:md5_init(),
    %% copy in the first bit of the SMB header
    C11 = erlang:md5_update(C0, MacKey),     
    C1 = erlang:md5_update(C11, Head),     
    %% copy in the sequence number, instead of the signature
    C2 = erlang:md5_update(C1, <<SSN:32/little,0:32>>), 
    %% copy in the rest of the packet in, skipping the signature
    C3 = erlang:md5_update(C2, Tail), 
    %% calculate the MD5 sig
    Mac = erlang:md5_final(C3),
    %%
    io:format("ORIGINAL SIGNATURE: ",[]),hexprint(Sign),
    io:format("COMPUTED SIGNATURE: ",[]),hexprint(Mac),
    io:format("COMPUTED SIGNATURE2: ",[]),hexprint(mac(MacKey,New)),
    ok.



nt_key_test(Passwd, EncKey) ->
    XX = concat(nt_s16(Passwd),
		nt_challenge_response(Passwd, EncKey)),
    hexprint(XX),
    XX.

lm_key_test(Passwd, EncKey) when binary(Passwd) ->
    XX = concat(lm_s16(Passwd),
		lm_challenge_response(Passwd, EncKey)),
    hexprint(XX),
    XX.
	      



lm_mac_key(Neg, Passwd) when ?USE_SIGNATURES(Neg) ->
    Bpass = l2b(Passwd),
    EncKey = Neg#smb_negotiate_res.encryption_key,
    concat(lm_s16(Bpass),
	   lm_challenge_response(Bpass, EncKey));
lm_mac_key(_, _) ->
    0.

lm_s16(Passwd) -> concat(head(s16x(Passwd), 8), zeros(8)).

nt_s16(Passwd) ->
    {ok, S16} = md4:digest(Passwd),
    S16.
concat(A,B) -> concat_binary([A,B]).

head(B, N) -> 
    <<Bn:N/binary,_/binary>> = B,
    Bn.


%%%
%%% Return a sequence of bytes where each byte
%%% has reversed its bit pattern.
%%%
swab(B) when binary(B) -> 
    list_to_binary(swab(binary_to_list(B)));
swab(L) when list(L)   -> 
    F = fun(X) -> bit_rev(X) end,
    lists:map(F, L).

bit_rev(N) when N < 256, N >= 0 ->
    <<B0:1,B1:1,B2:1,B3:1,B4:1,B5:1,B6:1,B7:1>> = <<N>> ,
    <<Rev>> = <<B7:1,B6:1,B5:1,B4:1,B3:1,B2:1,B1:1,B0:1>> ,
    Rev.

%%% ---

smb_negotiate_pdu() ->
    Bf = dialects(),
    Rec = #smbpdu{cmd = ?SMB_NEGOTIATE,
		  pid = mypid(),
		  mid = 1,
		  %% Assume "NT LM 0.12" dialect with Unicode !
		  flags2 = ?FLAGS2_NTLM, 
		  bc  = size(Bf),
		  bf  = Bf},
    enc_smb(Rec).




enc_smb(Pdu) ->
    enc_smb(Pdu, <<>>).

enc_smb(Pdu, Data) when ?SIGN_SMB(Pdu) ->
    SSN    = Pdu#smbpdu.sign_seqno,
    Cmd    = Pdu#smbpdu.cmd,
    Eclass = Pdu#smbpdu.eclass,
    Ecode  = Pdu#smbpdu.ecode,
    Flags  = Pdu#smbpdu.flags,
    Flags2 = Pdu#smbpdu.flags2,
    Tid    = Pdu#smbpdu.tid,
    Pid    = Pdu#smbpdu.pid,
    Uid    = Pdu#smbpdu.uid,
    Mid    = Pdu#smbpdu.mid,
    Wc     = Pdu#smbpdu.wc,
    Wp     = Pdu#smbpdu.wp,
    Bc     = Pdu#smbpdu.bc,
    Bf     = Pdu#smbpdu.bf,
    Bin = <<16#FF, $S, $M, $B,          % smb-header
	   Cmd,
	   Eclass, 
	   0,                           % zero (not used)
	   Ecode:16/little,    
	   Flags, 
	   Flags2:16/little,
	   0:16/little,                 % PidHigh
	   SSN:32/little,               % Request sequence number
	   0:4/?BYTE,                   % zeroed initially
	   0:16/little,                 % reserved
	   Tid:16/little, 
	   Pid:16/little, 
	   Uid:16/little,
	   Mid:16/little,
	   Wc, 
	   Wp/binary,
	   Bc:16/little, 
	   Bf/binary,
	   Data/binary>>,
    Mac = mac(Pdu#smbpdu.mac_key, Bin),
    <<Head:14/binary,_:8/binary,Tail/binary>> = Bin,
    ?sdbg("SMB signing required, SSN=~p NewSSN=~p~n", [SSN,SSN+1]),
    {Pdu#smbpdu{sign_seqno = SSN + 1},
     <<Head/binary,Mac/binary,Tail/binary>>};
%%
enc_smb(Pdu, Data) ->
    ?sdbg("SMB signing NOT required !!~n", []),
    Cmd    = Pdu#smbpdu.cmd,
    Eclass = Pdu#smbpdu.eclass,
    Ecode  = Pdu#smbpdu.ecode,
    Flags  = Pdu#smbpdu.flags,
    Flags2 = Pdu#smbpdu.flags2,
    Tid    = Pdu#smbpdu.tid,
    Pid    = Pdu#smbpdu.pid,
    Uid    = Pdu#smbpdu.uid,
    Mid    = Pdu#smbpdu.mid,
    Wc     = Pdu#smbpdu.wc,
    Wp     = Pdu#smbpdu.wp,
    Bc     = Pdu#smbpdu.bc,
    Bf     = Pdu#smbpdu.bf,
    {Pdu,
     <<16#FF, $S, $M, $B,          % smb-header
      Cmd,
      Eclass, 
      0,                           % zero (not used)
      Ecode:16/little,    
      Flags, 
      Flags2:16/little,
      0:12/unit:8,                 % Pad (12 bytes)
      Tid:16/little, 
      Pid:16/little, 
      Uid:16/little,
      Mid:16/little,
      Wc, 
      Wp/binary,
      Bc:16/little, 
      Bf/binary,
      Data/binary>>}.


%%% See also the header file for dialect index.
dialects() ->
    %%<<?BUF_FMT_DIALECT, <<"PC NETWORK PROGRAM 1.0">>/binary, 0>>.
    %%<<?BUF_FMT_DIALECT, <<"PC NETWORK PROGRAM 1.0">>/binary, 0,
     %% ?BUF_FMT_DIALECT, <<"LANMAN1.0">>/binary, 0>>.
    <<?BUF_FMT_DIALECT, <<"PC NETWORK PROGRAM 1.0">>/binary, 0,
      ?BUF_FMT_DIALECT, <<"LANMAN1.0">>/binary, 0,
      ?BUF_FMT_DIALECT, <<"NT LM 0.12">>/binary, 0>>.

    
mypid() ->
    [_,Pid,_] = string:tokens(pid_to_list(self()),"."),
    list_to_integer(Pid).


%%% @private
ucase([C|Cs]) when C>=$a,C=<$z -> [C-32|ucase(Cs)]; % a-z
ucase([C|Cs])                  -> [C|ucase(Cs)];
ucase([])                      -> [].

%%% @private
lcase([C|Cs]) when C>=$A,C=<$Z -> [C+32|lcase(Cs)]; % A-Z
lcase([C|Cs])                  -> [C|lcase(Cs)];
lcase([])                      -> [].

to_bool(0) -> false;
to_bool(_) -> true.

%%% @private
hexprint(B) when binary(B) -> hexprint(b2l(B));
hexprint(L) ->
    F = fun(H, Acc) ->
		io:format("~c~s",[Acc,i2x(H)]),
		$,
	end,
    lists:foldl(F, $[, L),
    io:format("]~n",[]).

i2x(I) when I > 15 -> [x(I div 16),x(I rem 16)];
i2x(I)	           -> [$0,x(I)].

x(X) when X>=0,X=<9 -> X + $0;
x(10) -> $a;
x(11) -> $b;
x(12) -> $c;
x(13) -> $d;
x(14) -> $e;
x(15) -> $f.


sleep(Sec) ->
    receive after Sec*1000 -> true end.

%%%
%%% @spec is_ok(Pdu::pdu(), DefaultMsg::string()) -> 
%%%          ok | {error, Ecode::integer(), Emsg::string()}
%%%
%%% @doc Checks the error class/code in the <em>Pdu</em>. If no error
%%%      it returns <em>true</em>. Otherwise it returns an
%%%      error string.
%%%
%%% @see error_p/1
%%% @end
%%%
is_ok(Pdu, Dmsg) when Pdu#smbpdu.eclass == ?SUCCESS -> ok;
is_ok(Pdu, Dmsg) 
  when Pdu#smbpdu.eclass == ?ERRNT, Pdu#smbpdu.ecode == ?SUCCESS -> ok;
is_ok(Pdu, Dmsg) when record(Pdu, smbpdu) ->
    {error, emsg(Pdu#smbpdu.eclass, Pdu#smbpdu.ecode, Dmsg)}.

%%%
%%% @spec emsg(Eclass::integer(), Ecode::integer(), 
%%%            DefaultMsg::string()) ->  string()
%%%
%%% @doc Return an error string corresponding to an <em>Eclass</em>
%%%      error class and <em>Ecode</em> error code which has been
%%%      returned from an earlier SMB message transaction.
%%%      In case of an unknown error, the <em>DefaultMsg</em> is returned.
%%% @end
%%%
emsg(Eclass, Ecode, DefaultEmsg) ->
    case catch emsg(Eclass, Ecode) of
	{'EXIT', _} -> DefaultEmsg;
	Emsg        -> Emsg
    end.

%%% See p.118 in CIFS/1.0 doc.
emsg(?ERRDOS, ?ERRbadfunc)     -> "Invalid function";
emsg(?ERRDOS, ?ERRbadfile)     -> "File not found";
emsg(?ERRDOS, ?ERRbadpath)     -> "Directory invalid";
emsg(?ERRDOS, ?ERRnofids)      -> "Too many open files";
emsg(?ERRDOS, ?ERRnoaccess)    -> "Access denied";
emsg(?ERRDOS, ?ERRnoshare)     -> "Share does not exist";
emsg(?ERRDOS, ?ERRfileexist)   -> "File in operation already exists";
emsg(?ERRDOS, ?ERRdirnotempty) -> "Directory not empty";
emsg(?ERRSRV, ?ERRSbadpw)      -> "Bad password";
emsg(?ERRSRV, ?ERRSaccess)     -> "Insufficient access rights";
emsg(?ERRSRV, ?ERRSinvtid)     -> "Invalid TID (Transaction ID)";
emsg(?ERRSRV, ?ERRSinvnetname) -> "Invalid network name in tree connect";
emsg(?ERRNT,  ?ERRNTfileisdir) -> "File is directory".

%%% @private
caller() ->
    {ok, Host} = inet:gethostname(),
    ucase(Host).

%%% @private
called({A,B,C,D}) ->
    lists:flatten(io_lib:format("~w.~w.~w.~w", [A,B,C,D]));
called(Host) when list(Host) ->
    ucase(Host).


%%%
%%% @spec exit_if_error(Pdu::pdu(), DefaultMsg::string()) -> 
%%%                        true | exception()
%%%
%%% @doc Checks the error class/code in the <em>Pdu</em>. If no error
%%%      it returns <em>true</em>. Otherwise throws an exception
%%%      <em>{error, Emsg}</em> where <em>Emsg</em> will be <em>DefaultMsg</em>
%%%      unless it is a recognised error code.
%%% @end
%%%
exit_if_error(Pdu, Dmsg) when Pdu#smbpdu.eclass == ?SUCCESS -> true;
exit_if_error(Pdu, Dmsg) 
  when Pdu#smbpdu.eclass == ?ERRNT, Pdu#smbpdu.ecode == ?SUCCESS -> true;
exit_if_error(Pdu, Dmsg) ->
    Emsg = emsg(Pdu#smbpdu.eclass, Pdu#smbpdu.ecode, Dmsg),
    throw({error, Emsg}).

%%%
%%% @spec error_p(Pdu::pdu()) -> 
%%%               false | {true, Ecode::integer(), Emsg::string()}
%%%
%%% @doc Checks the error class/code in the <em>Pdu</em>. If no error
%%%      it returns <em>false</em>. Otherwise it returns a tuple
%%%      containing the error code and the error message.
%%%
%%% @see is_ok/2
%%% @end
%%%
error_p(Pdu) when Pdu#smbpdu.eclass == ?SUCCESS  -> false;
error_p(Pdu) 
  when Pdu#smbpdu.eclass == ?ERRNT, Pdu#smbpdu.ecode == ?SUCCESS -> false;
error_p(Pdu) when Pdu#smbpdu.eclass == ?INTERNAL ->
    {true, ?INTERNAL, Pdu#smbpdu.emsg};
error_p(Pdu) ->
    Emsg = emsg(Pdu#smbpdu.eclass, Pdu#smbpdu.ecode, ""),
    {true, Pdu#smbpdu.ecode, Emsg}.


%%%
%%% @spec unicode_p(Pdu::pdu() | Neg::neg()) -> false | true
%%%
%%% @doc Checks if Unicode has been negotiated or not.
%%%
%%% @end
%%%
unicode_p(Neg) when record(Neg,smb_negotiate_res),?USE_UNICODE(Neg) -> true;
unicode_p(Pdu) when record(Pdu,smbpdu),?F2_USE_UNICODE(Pdu)         -> true; 
unicode_p(_)                                                        -> false.

%%%
%%% @spec sign_p(Pdu::pdu()) -> false | true
%%%
%%% @doc Checks if SMB signing has been negotiated or not.
%%%
%%% @end
%%%
sign_p(Pdu) when record(Pdu,smbpdu),?SIGN_SMB(Pdu)  -> true; 
sign_p(_)                                           -> false.

%%%
%%% @spec ssn(Pdu::pdu()) -> integer()
%%%
%%% @doc Returns the SMB signing sequence counter.
%%%
%%% @end
%%%
ssn(Pdu) when record(Pdu,smbpdu),?SIGN_SMB(Pdu)  -> Pdu#smbpdu.sign_seqno;
ssn(_)                                           -> 0.


%%% @private
l2b(L) when list(L)   -> list_to_binary(L);
l2b(B) when binary(B) -> B.

%%% @private
b2l(B) when binary(B) -> binary_to_list(B);
b2l(L) when list(L)   -> L.


ip2str({A,B,C,D}) -> 
    lists:flatten(io_lib:format("~w.~w.~w.~w",[A,B,C,D]));
ip2str(L) when list(L) -> 
    L.

ip2bin({A,B,C,D}) -> 
    <<A,B,C,D>>;
ip2bin(L) when list(L) -> 
    [A,B,C,D] = string:tokens(L, "."),
    Ai = list_to_integer(A),
    Bi = list_to_integer(B),
    Ci = list_to_integer(C),
    Di = list_to_integer(D),
    <<Ai, Bi, Ci, Di>>;
ip2bin(B) when binary(B) -> 
    B.

bin2ip(<<A,B,C,D>>) -> {A,B,C,D}.


i2l(I) when integer(I) -> integer_to_list(I);
i2l(L) when list(L)    -> L.

month(1)  -> "Jan";
month(2)  -> "Feb";
month(3)  -> "Mar";
month(4)  -> "Apr";
month(5)  -> "May";
month(6)  -> "Jun";
month(7)  -> "Jul";
month(8)  -> "Aug";
month(9)  -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

%%% @private
to_ucs2_and_null(UnicodeP, Str) ->
    B = to_ucs2(UnicodeP, Str),
    N = null2(UnicodeP),
    <<B/binary,N/binary>>.

%%% @private
to_ucs2(UnicodeP, Str) when UnicodeP == true ->    
    {ok, Cd}   = iconv:open(?CSET_UCS2, ?CSET_ASCII),
    {ok, Ustr} = iconv:conv(Cd, Str),
    iconv:close(Cd),
    Ustr;
to_ucs2(_, Str) ->    
    l2b(Str).

null2(UnicodeP) when UnicodeP == true -> <<0,0>>;
null2(_)                              -> <<0>>.

%%% @private
ucs2_to_ascii(Ustr) ->
    ucs2_to_charset(Ustr, "ASCII").

%%% @private
ucs2_to_charset(Ustr, Cset) ->
    case iconv:open(esmb:ucase(Cset), ?CSET_UCS2LE) of
	{ok, Cd} ->
	    case iconv:conv(Cd, Ustr) of
		{ok, Res} -> 
		    iconv:close(Cd),
		    Res;
		{error, _Reason} -> 
		    iconv:close(Cd),
		    Ustr
	    end;
	{error, Reason} ->
	    ?elog("ucs2_to_charset, open failed Reason=~p , Cset=~p~n",
		[Reason, esmb:ucase(Cset)]),
	    Ustr
    end.


