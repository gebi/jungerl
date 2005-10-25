%%% File    : ssh_auth.erl
%%% Author  : Tony Rogvall <tony@bulldog>
%%% Description : SSH user authentication
%%% Created : 20 Aug 2004 by Tony Rogvall <tony@bulldog>

-module(ssh_userauth).

-vsn("$Revision$ ").

-rcsid("$Id$\n").

-compile(export_all).

-import(lists, [member/2]).

-include("../include/ssh.hrl").
-include("../include/ssh_userauth.hrl").


userauth_messages() ->
    [ {ssh_msg_userauth_request, ?SSH_MSG_USERAUTH_REQUEST,
       [string, 
	string, 
	string, 
	'...']},
      
      {ssh_msg_userauth_failure, ?SSH_MSG_USERAUTH_FAILURE,
       [string, 
	boolean]},

      {ssh_msg_userauth_success, ?SSH_MSG_USERAUTH_SUCCESS,
       []},

      {ssh_msg_userauth_banner, ?SSH_MSG_USERAUTH_BANNER,
       [string, 
	string]}].


userauth_passwd_messages() ->
    [ 
      {ssh_msg_userauth_passwd_changereq, ?SSH_MSG_USERAUTH_PASSWD_CHANGEREQ,
       [string, 
	string]}
     ].

userauth_pk_messages() ->
    [ {ssh_msg_userauth_pk_ok, ?SSH_MSG_USERAUTH_PK_OK,
       [string,binary]}
     ].

auth(SSH, Service, Opts) ->
    case user(Opts) of
	{ok, User} ->
	    case lists:keysearch(userauth, 1, Opts) of
		false ->
		    none(SSH, Service, User, Opts);
		{value, {_, ["none"]}} ->
		    none(SSH, Service, User, Opts);
		{value, {_, []}} ->
		    none(SSH, Service, User, Opts);
		{value, {_, AuthList}} ->
		    auth_list(SSH,Service,User,AuthList,Opts)
	    end;
	Error ->
	    Error
    end.

auth_list(SSH, Service, User, AuthList, Opts) ->
    ssh_proto:install(SSH, userauth_messages()),
    auth_dispatch(SSH,Service,User,AuthList,undefined,Opts).


none(SSH, Service, User, Opts) ->
    ssh_proto:install(SSH, userauth_messages()),
    SSH ! {ssh_msg, self(),
	   #ssh_msg_userauth_request { user = User,
				       service = Service,
				       method = "none",
				       data = <<>> } },
    none_reply(SSH, Service, User, Opts).

none_reply(SSH, Service, User, Opts) ->
    receive
	{ssh_msg, SSH, R} when record(R, ssh_msg_userauth_success) ->
	    ok;
	{ssh_msg, SSH, R} when record(R, ssh_msg_userauth_failure) ->
	    AuthList = list_split(R#ssh_msg_userauth_failure.authentications,
				  ","),
	    auth_dispatch(SSH, Service, User, AuthList,
			  R#ssh_msg_userauth_failure.partial_success,
			  Opts);
	{ssh_msg, SSH, R} when record(R, ssh_msg_userauth_banner) ->
	    io:format("~w", [R#ssh_msg_userauth_banner.message]),
	    none_reply(SSH, Service, User, Opts);
	{ssh_msg, SSH, R} when record(R, ssh_msg_disconnect) ->
	    {error, R}
    end.

auth_dispatch(SSH, Service, User, [Auth|AutList], PartialSuccess, Opts) ->
    case Auth of
	"publickey" ->
	    case lists:keysearch(pubkey, 1, Opts) of
		{value,{pubkey,Key}} ->
		    pubkey(SSH, User, Key);
		false ->
		    auth_dispatch(SSH, Service, User, AutList, PartialSuccess, Opts)
	    end;
	"password" ->
	    case lists:keysearch(password, 1, Opts) of
		{value,{password, Password}} ->
		    passwd(SSH, Service, User, Password);
		false ->
		    Password = ssh_proto:read_password(SSH, "ssh password: "),
		    passwd(SSH, Service, User, Password)
	    end;
	_ ->
	    auth_dispatch(SSH, Service, User, AutList, PartialSuccess, Opts)
    end;
auth_dispatch(_SSH, _Service, _User, [], _PartialSuccess, _Opts) ->
    {error, no_auth_method}.

server_auth(SSH, Opts) ->
    ssh_proto:install(SSH, userauth_messages()),
    ssh_proto:send(SSH, #ssh_msg_service_accept {name="ssh-userauth" }),
    server_auth_loop(SSH, Opts, 3+1).

server_auth_loop(_SSH, _Opts, 0) ->
    {error, to_many_attempts};
server_auth_loop(SSH, Opts, I) when I > 0 ->
    receive
	{ssh_msg, SSH, R} when record(R, ssh_msg_userauth_request) ->
	    AuthList = case lists:keysearch(userauth, 1, Opts) of
			   false -> ["publickey"];
			   {value,{_,AL}} -> AL
		       end,
	    if R#ssh_msg_userauth_request.method == "none" ->
		    Auths = list_cat(AuthList, ","),
		    R1 = #ssh_msg_userauth_failure { authentications=Auths,
						     partial_success = false },
		    ssh_proto:send(SSH,R1),
		    server_auth_loop(SSH, Opts, I-1);
	       true ->
		    Method = R#ssh_msg_userauth_request.method,
		    Res =
			case member(Method, AuthList) of
			    true ->
				case Method of
				    "password" ->
					password_check(SSH,R,Opts);
				    "publickey" ->
					publickey_check(SSH,R,Opts);
				    _ ->
					{error, auth_not_supported}
				end;
			    false ->
				io:format("Auth type ~s not supported\n",
					  [Method]),
				{error, auth_not_supported}
			end,
		    case Res of
			ok ->
			    User = R#ssh_msg_userauth_request.user,
			    Service =R#ssh_msg_userauth_request.service,
			    {ok,{User,Method,Service}};

			partial ->
			    server_auth_loop(SSH, Opts, I);
			    
			{error, Reason} ->
			    io:format("Auth error: ~p\n", [Reason]),
			    Auths = list_cat(AuthList, ","),
			    R1 = #ssh_msg_userauth_failure { authentications=Auths,
							     partial_success = false },
			    ssh_proto:send(SSH,R1),
			    server_auth_loop(SSH, Opts, I-1)
		    end
	    end
    after 10000 ->
	    {error, timeout}
    end.

    

%% Find user name
user(Opts) ->
    case lists:keysearch(user, 1, Opts) of
	{value,{user,User}} ->
	    {ok,User};
	false ->
	    case os:getenv("USER") of
		false -> 
		    {error, no_user};
		User ->
		    {ok, User}
	    end
    end.

pubkey_type(#ssh_key { type=rsa }) -> <<"ssh-rsa">>;
pubkey_type(#ssh_key { type=dsa }) -> <<"ssh-dss">>.
    

pubkey(SSH, User, Key) ->
    pubkey(SSH, "ssh-connection", User, Key).

pubkey(SSH, Service, User, Key) ->
    KeyType = pubkey_type(Key),
    KeyBlob = ssh_bits:key_to_blob(Key),
    ssh_proto:install(SSH, userauth_pk_messages()),
    SSH ! {ssh_msg, self(),
	   #ssh_msg_userauth_request { user = User,
				       service = Service,
				       method = "publickey",
				       data =
				       <<?BOOLEAN(?FALSE),
					?STRING(KeyType),
					?STRING(KeyBlob)>>}},
    pubkey_reply(SSH, Service, User, Key).


pubkey_reply(SSH, Service, User, Key) ->
    receive
	{ssh_msg, SSH, R} when record(R, ssh_msg_userauth_pk_ok) ->
	    pubkey_sign(SSH, Service, User, Key);
	{ssh_msg, SSH, R} when record(R, ssh_msg_userauth_failure) ->
	    {error, R};
	{ssh_msg, SSH, R} when record(R, ssh_msg_userauth_banner) ->
	    io:format("~w", [R#ssh_msg_userauth_banner.message]),
	    pubkey_reply(SSH, Service, User, Key);
	{ssh_msg, SSH, R} when record(R, ssh_msg_disconnect) ->
	    {error, R}
    end.

pubkey_sign(SSH, User, Key) ->
    pubkey_sign(SSH, "ssh-connection", User, Key).

pubkey_sign(SSH, Service, User, Key) ->
    ssh_proto:install(SSH, userauth_pk_messages()),
    KeyType = pubkey_type(Key),
    KeyBlob = ssh_bits:key_to_blob(Key),
    AuthType = <<"publickey">>,
    UserBin = list_to_binary(User),
    ServiceBin = list_to_binary(Service),
    Data = <<?SSH_MSG_USERAUTH_REQUEST,
	    ?STRING(UserBin),
	    ?STRING(ServiceBin),
	    ?STRING(AuthType),
	    ?BOOLEAN(?TRUE),
	    ?STRING(KeyType),
	    ?STRING(KeyBlob)>>,
    {ok,Signature} = ssh_proto:sign(SSH, Key, Data),
    SSH ! {ssh_msg, self(),
	   #ssh_msg_userauth_request { user = User,
				       service = Service,
				       method = "publickey",
				       data =
				       << ?BOOLEAN(?TRUE),
					?STRING(KeyType),
					?STRING(KeyBlob),
					?STRING(Signature) >> }},
    pubkey_sign_reply(SSH, Service, User, Key).


pubkey_sign_reply(SSH, Service, User, Key) ->
    receive
	{ssh_msg, SSH, R} when record(R, ssh_msg_userauth_success) ->
	    ok;
	{ssh_msg, SSH, R} when record(R, ssh_msg_userauth_failure) ->
	    {error, R};
	{ssh_msg, SSH, R} when record(R, ssh_msg_userauth_banner) ->
	    io:format("~w", [R#ssh_msg_userauth_banner.message]),
	    pubkey_sign_reply(SSH, Service, User, Key);
	{ssh_msg, SSH, R} when record(R, ssh_msg_disconnect) ->
	    {error, R}
    end.
			       
			       

    


%% Password authentication for ssh-connection
passwd(SSH, User, Password) ->
    passwd(SSH, "ssh-connection", User, Password).

passwd(SSH, Service, User, Password) -> 
    Pwd = list_to_binary(Password),
    ssh_proto:install(SSH, userauth_passwd_messages()),
    SSH ! {ssh_msg, self(), 
	   #ssh_msg_userauth_request { user = User,
				       service = Service,
				       method = "password",
				       data =
				       <<?BOOLEAN(?FALSE),
					?STRING(Pwd)>> }},
    passwd_reply(SSH).


passwd_reply(SSH) ->
    receive
	{ssh_msg, SSH, R} when record(R, ssh_msg_userauth_success) ->
	    ok;
	{ssh_msg, SSH, R} when record(R, ssh_msg_userauth_failure) ->
	    {error, R};
	{ssh_msg, SSH, R} when record(R, ssh_msg_userauth_banner) ->
	    io:format("~w", [R#ssh_msg_userauth_banner.message]),
	    passwd_reply(SSH);
	{ssh_msg, SSH, R} when record(R, ssh_msg_userauth_passwd_changereq) ->
	    {error, R};
	{ssh_msg, SSH, R} when record(R, ssh_msg_disconnect) ->
	    {error, R}
    end.

password_check(SSH,R, Opts) ->
    ssh_proto:install(SSH, userauth_passwd_messages()),
    case R#ssh_msg_userauth_request.data of
	<<?BOOLEAN(?FALSE), ?UINT32(Len), Pwd:Len/binary>> ->
	    User = R#ssh_msg_userauth_request.user,
	    Service =R#ssh_msg_userauth_request.service, 
	    Password = binary_to_list(Pwd),
	    case password_check(SSH,User, Service, Password, Opts) of
		ok ->
		    ok;
		error ->
		    {error, bad_auth}
	    end;
	_ ->
	    {error, bad_packet}
    end.

password_check(SSH,User, Service, Password, Opts) ->
    ssh_file:authorized_user(user, User, Service, Password).


publickey_check(SSH,R, Opts) ->
    ssh_proto:install(SSH, userauth_pk_messages()),
    case R#ssh_msg_userauth_request.data of
	<<?BOOLEAN(?FALSE), 
	 ?UINT32(Len1), KeyType:Len1/binary,
	 ?UINT32(Len2), KeyBlob:Len2/binary>> ->
	    User = R#ssh_msg_userauth_request.user,
	    Service =R#ssh_msg_userauth_request.service,
	    case ssh_file:authorized_key(user, User, Service, KeyBlob) of
		ok ->
		    R1 = #ssh_msg_userauth_pk_ok { algorithm=KeyType,
						   key_blob=KeyBlob },
		    ssh_proto:send(SSH, R1),
		    partial;
		_ ->
		    {error, bad_user}
	    end;
	<<?BOOLEAN(?TRUE), 
	 ?UINT32(Len1), KeyType:Len1/binary,
	 ?UINT32(Len2), KeyBlob:Len2/binary,
	 ?UINT32(Len3), Signature:Len3/binary>> ->
	    User = R#ssh_msg_userauth_request.user,
	    Service =R#ssh_msg_userauth_request.service,
	    case ssh_file:authorized_key(user, User, Service, KeyBlob) of
		ok ->
		    AuthType = <<"publickey">>,
		    UserBin = list_to_binary(User),
		    ServiceBin = list_to_binary(Service),

		    Data = <<?SSH_MSG_USERAUTH_REQUEST,
			    ?STRING(UserBin),
			    ?STRING(ServiceBin),
			    ?STRING(AuthType),
			    ?BOOLEAN(?TRUE),
			    ?STRING(KeyType),
			    ?STRING(KeyBlob)>>,
		    case catch ssh_bits:blob_to_key(KeyBlob) of
			{'EXIT',_} ->
			    {error, bad_key};
			Key ->
			    ssh_proto:verify(SSH,Key,Data,Signature)
		    end;
		Error ->
		    {error, bad_user}
	    end;
	_ ->
	    {error, bad_format}
    end.
    


list_split(String, Sep) ->
    string:tokens(String, Sep).

list_cat([Elem], Sep) -> Elem;
list_cat([Elem|Es],Sep) -> Elem ++ Sep ++ list_cat(Es,Sep);
list_cat([],_) -> "".
    

	

	

				       
    
    



