%%% File    : ssh_auth.erl
%%% Author  : Tony Rogvall <tony@bulldog>
%%% Description : SSH user authentication
%%% Created : 20 Aug 2004 by Tony Rogvall <tony@bulldog>

-module(ssh_userauth).

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
       []}
     ].

auth(SSH, Service, Opts) ->
    case user(Opts) of
	{ok, User} ->
	    case lists:keysearch(password, 1, Opts) of
		{value,{password, Password}} ->
		    passwd(SSH, Service, User, Password);
		false ->
		    Password = ssh_proto:read_password(SSH, "ssh password: "),
		    passwd(SSH, Service, User, Password)
	    end;
	Error -> Error
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

%% Password authentication for ssh-connection
passwd(SSH, User, Password) ->
    passwd(SSH, "ssh-connection", User, Password).

passwd(SSH, Service, User, Password) -> 
    SSH ! {ssh_install, 
	   userauth_messages() ++ userauth_passwd_messages()},
    SSH ! {ssh_msg, self(), 
	   #ssh_msg_userauth_request { user = User,
				       service = Service,
				       method = "password",
				       data =
				       <<?BOOLEAN(?FALSE),
					?STRING(list_to_binary(Password))>> }},
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

	

	

				       
    
    



