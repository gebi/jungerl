%% userauth
-define(SSH_MSG_USERAUTH_REQUEST,  50).
-define(SSH_MSG_USERAUTH_FAILURE,  51).
-define(SSH_MSG_USERAUTH_SUCCESS,  52).
-define(SSH_MSG_USERAUTH_BANNER,  53).
-define(SSH_MSG_USERAUTH_PK_OK,  60).
-define(SSH_MSG_USERAUTH_PASSWD_CHANGEREQ, 60).

-record(ssh_msg_userauth_request,
	{
	  user,     %% string
	  service,  %% string
	  method,   %% string "publickey", "password"
	  data      %% opaque
	 }).

-record(ssh_msg_userauth_failure,
	{
	  authentications,     %% string
	  partial_success      %% boolean
	 }).

-record(ssh_msg_userauth_success,
	{
	 }).

-record(ssh_msg_userauth_banner,
	{
	  message,    %% string
	  language    %% string
	 }).

-record(ssh_msg_userauth_passwd_changereq,
	{
	  prompt,     %% string
	  languge     %% string
	 }).

-record(ssh_msg_userauth_pk_ok,
	{
	  algorithm,     %% string
	  key_blob       %% string
	 }).

