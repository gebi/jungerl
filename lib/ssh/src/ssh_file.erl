%%% File    : ssh_file.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : SSH file hook
%%% Created : 31 Aug 2004 by Tony Rogvall <tony@bix.hemma.se>

-module(ssh_file).

-include("../include/ssh.hrl").
-include("PKCS-1.hrl").
-include("DSS.hrl").

-compile(export_all).

-export([public_host_dsa_key/1,private_host_dsa_key/1,
	 public_host_rsa_key/1,private_host_rsa_key/1,
	 public_host_key/1,private_host_key/1,
	 lookup_host_key/1, add_host_key/2, del_host_key/1]).

-import(lists, [reverse/1, append/1]).

%% API

public_host_dsa_key(Type) ->
    File = filename:join(ssh_dir(Type), "ssh_host_dsa_key.pub"),
    read_public_key_v2(File, "ssh-dss").

private_host_dsa_key(Type) ->
    File = filename:join(ssh_dir(Type), "ssh_host_dsa_key"),
    read_private_key_v2(File, "DSA").

public_host_rsa_key(Type) ->
    File = filename:join(ssh_dir(Type), "ssh_host_rsa_key.pub"),
    read_public_key_v2(File, "ssh-rsa").

private_host_rsa_key(Type) ->
    File = filename:join(ssh_dir(Type), "ssh_host_rsa_key"),
    read_private_key_v2(File, "RSA").

public_host_key(Type) ->
    File = filename:join(ssh_dir(Type), "ssh_host_key"),
    case read_private_key_v1(File,public) of
	{error, enoent} ->	
	    read_public_key_v1(File++".pub");
	Result ->
	    Result
    end.
	    

private_host_key(Type) ->
    File = filename:join(ssh_dir(Type), "ssh_host_key"),
    read_private_key_v1(File,private).

%% lookup_host_key
%% return {ok, Key(s)} or {error, not_found}
%%
lookup_host_key(Host) ->
    case file:open(filename:join(ssh_dir(user), "known_hosts"), [read]) of
	{ok, Fd} ->
	    Res = lookup_host_key_fd(Fd, Host),
	    file:close(Fd),
	    Res;
	Error -> Error
    end.

add_host_key(Host, Key) ->
    case file:open(filename:join(ssh_dir(user),"known_hosts"),[write,append]) of
	{ok, Fd} ->
	    Res = add_key_fd(Fd, Host, Key),
	    file:close(Fd),
	    Res;
	Error ->
	    Error
    end.

del_host_key(Host) ->
    case file:open(filename:join(ssh_dir(user),"known_hosts"),[write,read]) of
	{ok, Fd} ->
	    Res = del_key_fd(Fd, Host),
	    file:close(Fd),
	    Res;
	Error ->
	    Error
    end.


%%
%% Utils
%%
ssh_dir(user) ->
    filename:join(os:getenv("HOME"), ".ssh");
ssh_dir(system) ->
    "/etc/ssh".

read_public_key_v2(File, Type) ->
    case file:read_file(File) of
	{ok,Bin} ->
	    List = binary_to_list(Bin),
	    case lists:prefix(Type, List) of
		true ->
		    List1 = lists:nthtail(length(Type),List),
		    K_S = ssh_bits:b64_decode(List1),
		    decode_public_key_v2(K_S, Type);
		false ->
		    {error, bad_format}
	    end;
	Error ->
	    Error
    end.

decode_public_key_v2(K_S, "ssh-rsa") ->
    case ssh_bits:decode(K_S,[string,mpint,mpint]) of
	["ssh-rsa", E, N] ->
	    {ok, #ssh_key { type = rsa,
			    public = {N,E},
			    comment=""}};
	_ ->
	    {error, bad_format}
    end;
decode_public_key_v2(K_S, "ssh-dss") ->
    case ssh_bits:decode(K_S,[string,mpint,mpint,mpint,mpint]) of
	["ssh-dss",P,Q,G,Y] ->
	    {ok,#ssh_key { type = dsa,
			   public = {P,Q,G,Y}
			  }};
	A ->
	    {error, bad_format}
    end;
decode_public_key_v2(_, _) ->
    {error, bad_format}.
    

read_public_key_v1(File) ->
    case file:read_file(File) of
	{ok,Bin} ->
	    List = binary_to_list(Bin),
	    case io_lib:fread("~d ~d ~d ~s", List) of
		{ok,[Sz,E,N,Comment],_} ->
		    {ok,#ssh_key { type = rsa,
				   public ={N,E},
				   comment = Comment }};
		Error ->
		    {error, bad_format}
	    end;
	Error ->
	    Error
    end.

read_private_key_v2(File, Type) ->
    case file:read_file(File) of
	{ok,Bin} ->
	    case read_pem(binary_to_list(Bin), Type) of
		{ok,Bin1} ->
		    decode_private_key_v2(Bin1, Type);
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

decode_private_key_v2(Private,"RSA") ->
    case asn1rt:decode('PKCS-1', 'RSAPrivateKey', Private) of
	{ok,RSA} -> %% FIXME Check for two-prime version
	    {ok, #ssh_key { type = rsa,
			    public = {RSA#'RSAPrivateKey'.modulus,
				      RSA#'RSAPrivateKey'.publicExponent},
			    private = {RSA#'RSAPrivateKey'.modulus,
				       RSA#'RSAPrivateKey'.privateExponent}
			    }};
	Error ->
	    Error
    end;
decode_private_key_v2(Private, "DSA") ->
    %% io:format("private DSA: ~w\n", [Private]),
    %% Ret = (catch asn1rt_ber_bin_v2:decode(Private)),
    %% io:format("private ASN1: ~w\n", [Ret]),
    case asn1rt:decode('DSS', 'DSAPrivateKey', Private) of
	{ok,DSA} -> %% FIXME Check for two-prime version
	    {ok, #ssh_key { type = dsa,
			    public = {DSA#'DSAPrivateKey'.p,
				      DSA#'DSAPrivateKey'.q,
				      DSA#'DSAPrivateKey'.g,
				      DSA#'DSAPrivateKey'.y},
			    private= {DSA#'DSAPrivateKey'.p,
				      DSA#'DSAPrivateKey'.q,
				      DSA#'DSAPrivateKey'.g,
				      DSA#'DSAPrivateKey'.x}
			   }};
	_ ->
	    {error,bad_format}
    end.
				       
				      


%% SSH1 private key format
%%  <<"SSH PRIVATE KEY FILE FORMATE 1.1\n" 0:8 
%%    CipherNum:8, Reserved:32,
%%    NSz/uint32, N/bignum, E/bignum, Comment/string,
%%
%% [ R0:8 R1:8 R0:8 R1:8, D/bignum, IQMP/bignum, Q/bignum, P/bignum, Pad(8)]>>
%%
%% where [ ] is encrypted using des3 (ssh1 version) and
%% a posssibly empty pass phrase using md5(passphase) as key
%% 

read_private_key_v1(File, Type) ->
    case file:read_file(File) of
	{ok,<<"SSH PRIVATE KEY FILE FORMAT 1.1\n",0,
	     CipherNum,_Resereved:32,Bin/binary>>} ->
	    decode_private_key_v1(Bin, CipherNum,Type);
	{ok,_} ->
	    {error, bad_format};
	Error ->
	    Error
    end.

decode_private_key_v1(Bin, CipherNum, Type) ->
    case ssh_bits:decode(Bin,0,[uint32, bignum, bignum, string]) of
	{Offset,[NSz,N,E,Comment]} ->
	    if Type == public ->
		    {ok,#ssh_key { type=rsa,
				   public={N,E},
				   comment=Comment}};
	       Type == private ->
		    <<_:Offset/binary, Encrypted/binary>> = Bin,
		    case ssh_bits:decode(decrypt1(Encrypted, CipherNum),0,
					 [uint32, bignum, bignum, 
					  bignum, bignum,{pad,8}]) of
			{_,[_,D,IQMP,Q,P]} ->
			    {ok,#ssh_key { type=rsa,
					   public={N,E},
					   private={D,IQMP,Q,P},
					   comment=Comment}};
			_ ->
			    {error,bad_format}
		    end
	    end;
	_ ->
	    {error,bad_format}
    end.


decrypt1(Bin, CipherNum) ->
    decrypt1(Bin, CipherNum,"").

decrypt1(Bin, CipherNum, Phrase) ->
    if CipherNum == ?SSH_CIPHER_NONE; Phrase == "" ->
	    Bin;
       CipherNum == ?SSH_CIPHER_3DES ->
	    <<K1:8/binary, K2:8/binary>> = erlang:md5(Phrase),
	    K3 = K1,
	    IV = <<0,0,0,0,0,0,0,0>>,
	    Bin1 = crypto:des_cbc_decrypt(K3,IV,Bin),
	    Bin2 = crypto:des_cbc_encrypt(K2,IV,Bin1),
	    crypto:des_cbc_decrypt(K1,IV,Bin2)
    end.

encrypt1(Bin, CipherNum) ->
    encrypt1(Bin, CipherNum,"").

encrypt1(Bin, CipherNum, Phrase) ->
    if CipherNum == ?SSH_CIPHER_NONE; Phrase == "" ->
	    Bin;
       CipherNum == ?SSH_CIPHER_3DES ->
	    <<K1:8/binary, K2:8/binary>> = erlang:md5(Phrase),
	    K3 = K1,
	    IV = <<0,0,0,0,0,0,0,0>>,
	    Bin1 = crypto:des_cbc_encrypt(K1,IV,Bin),
	    Bin2 = crypto:des_cbc_decrypt(K2,IV,Bin1),
	    crypto:des_cbc_encrypt(K3,IV,Bin2)
    end.



lookup_host_key_fd(Fd, Host) ->
    case io:get_line(Fd, '') of
	eof ->
	    {error, not_found};
	Line ->
	    case string:tokens(Line, " ") of
		[HostList, Type, KeyData] ->
		    case lists:member(Host, string:tokens(HostList, ",")) of
			true ->
			    decode_public_key_v2(ssh_bits:b64_decode(KeyData),
						 Type);
			false ->
			    lookup_host_key_fd(Fd, Host)
		    end;
		_ ->
		    lookup_host_key_fd(Fd, Host)
	    end
    end.

del_key_fd(Fd, Host) ->
    del_key_fd(Fd, Host, 0, 0).

del_key_fd(Fd, Host, ReadPos0, WritePos0) ->
    case io:get_line(Fd, '') of
	eof ->
	    if ReadPos0 == WritePos0 ->
		    ok;
	       true ->
		    file:truncate(Fd)
	    end;
	Line ->
	    {ok,ReadPos1} = file:position(Fd, cur),
	    case string:tokens(Line, " ") of
		[HostList, Type, KeyData] ->
		    case lists:member(Host, string:tokens(HostList, ",")) of
			true ->
			    del_key_fd(Fd, Host, ReadPos1, WritePos0);
			false ->
			    if ReadPos0 == WritePos0 ->
				    del_key_fd(Fd, Host, ReadPos1, ReadPos1);
			       true ->
				    file:position(Fd, WritePos0),
				    file:write(Fd, Line),
				    {ok,WritePos1} = file:position(Fd,cur),
				    del_key_fd(Fd, Host, ReadPos1, WritePos1)
			    end
		    end;
		_ ->
		    if ReadPos0 == WritePos0 ->
			    del_key_fd(Fd, Host, ReadPos1, ReadPos1);
		       true ->
			    file:position(Fd, WritePos0),
			    file:write(Fd, Line),
			    {ok,WritePos1} = file:position(Fd,cur),
			    del_key_fd(Fd, Host, ReadPos1, WritePos1)
		    end		    
	    end
    end.


add_key_fd(Fd, Host, Key) ->
    case Key#ssh_key.type of
	rsa ->
	    {N,E} = Key#ssh_key.public,
	    DK = ssh_bits:b64_encode(
		   ssh_bits:encode(["ssh-rsa",E,N],
				   [string,mpint,mpint])),
	    file:write(Fd, [Host, " ssh-rsa ", DK, "\n"]);
	dsa ->
	    {P,Q,G,Y} = Key#ssh_key.public,
	    DK = ssh_bits:b64_encode(
		   ssh_bits:encode(["ssh-dss",P,Q,G,Y],
				   [string,mpint,mpint,mpint,mpint])),
	    file:write(Fd, [Host, " ssh-dss ", DK, "\n"])
    end.


read_pem(Cs, Type) ->
    case read_line(Cs) of
	{"-----BEGIN "++Rest,Cs1} ->
	    case string:tokens(Rest, " ") of
		[Type, "PRIVATE", "KEY-----"] ->
		    read_pem64(Cs1, [], Type);
		_ ->
		    {error, bad_format}
	    end;
	{"",Cs1} when Cs1 =/= "" ->
	    read_pem(Cs1,Type);
	{_,""} ->
	    {error, bad_format}
    end.

read_pem64(Cs, Acc, Type) ->
    case read_line(Cs) of
	{"-----END "++Rest,Cs1} ->
	    case string:tokens(Rest, " ") of
		[Type, "PRIVATE", "KEY-----"] ->
		    {ok,ssh_bits:b64_decode(append(reverse(Acc)))};
		Toks ->
		    %% io:format("TOKENS=~p\n", [Toks]),
		    {error, bad_format}
	    end;
	{B64, Cs1} when Cs1 =/= "" ->
	    read_pem64(Cs1, [B64|Acc], Type);
	What ->
	    {error, bad_format}
    end.


read_line(Cs) -> read_line(Cs,[]).
read_line([$\r,$\n|T], Acc) -> {reverse(Acc), T};
read_line([$\n|T], Acc) -> {reverse(Acc), T};
read_line([C|T], Acc) -> read_line(T,[C|Acc]);
read_line([], Acc) -> {reverse(Acc),[]}.
