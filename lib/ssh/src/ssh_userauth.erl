%%% File    : ssh_auth.erl
%%% Author  : Tony Rogvall <tony@bulldog>
%%% Description : SSH user authentication
%%% Created : 20 Aug 2004 by Tony Rogvall <tony@bulldog>

-module(ssh_userauth).

-vsn("$Revision$ ").

-rcsid("$Id$\n").

-compile(export_all).

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
    SSH ! {ssh_install, userauth_messages()},
    auth_dispatch(SSH,Service,User,AuthList,undefined,Opts).


none(SSH, Service, User, Opts) ->
    SSH ! {ssh_install, userauth_messages()},
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
	    AuthList = string:tokens(R#ssh_msg_userauth_failure.authentications,
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
auth_dispatch(SSH, Service, User, [], PartialSuccess, Opts) ->
    {error, no_auth_method}.


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
    KeyBlob = ssh_bits:key_blob(Key),
    SSH ! {ssh_install, userauth_pk_messages()},
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
    SSH ! {ssh_install, userauth_pk_messages()},
    KeyType = pubkey_type(Key),
    KeyBlob = ssh_bits:key_blob(Key),
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
    SSH ! {ssh_install, userauth_passwd_messages()},
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

	

	

				       
    
    



