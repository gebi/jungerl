%%% File    : ssh_dsa.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : 
%%% Created : 30 Aug 2004 by Tony Rogvall <tony@bix.hemma.se>

-module(ssh_dsa).

-compile(export_all).


-export([verify/3]).
-export([sign/2]).

-include("../include/ssh.hrl").

start() ->
    crypto:start().

sign_file(File) ->
    start(),
    {ok,Bin} = file:read_file(File),
    {ok,Key} = ssh_file:private_host_dsa_key(user),
    sign(Key, Bin).

verify_file(File, Sig) ->
    start(),
    {ok,Bin} = file:read_file(File),
    {ok,Key} = ssh_file:public_host_dsa_key(user),
    verify(Key, Bin, Sig).


sign(Private=#ssh_key { private={P,Q,G,X} },Mb) ->
    K = ssh_bits:irandom(160) rem Q,
    R = ssh_math:ipow(G, K, P) rem Q,
    Ki = ssh_math:invert(K, Q),
    <<M:160/big-unsigned-integer>> = crypto:sha(Mb),
    S = (Ki * (M + X*R)) rem Q,
    <<R:160/big-unsigned-integer, S:160/big-unsigned-integer>>.


verify(Public=#ssh_key { public={P,Q,G,Y} },Mb,Sb) ->
    <<R0:160/big-unsigned-integer, S0:160/big-unsigned-integer>> = Sb,
    ?ssh_assert(R0 >= 0 andalso R0 < Q andalso
		S0 >= 0 andalso S0 < Q, out_of_range),
    W = ssh_math:invert(S0,Q),
    <<M0:160/big-unsigned-integer>> = crypto:sha(Mb),
    U1 = (M0*W) rem Q,
    U2 = (R0*W) rem Q,
    T1 = ssh_math:ipow(G,U1,P),
    T2 = ssh_math:ipow(Y,U2,P),
    V = ((T1*T2) rem P) rem Q,
    if V == R0 ->
	    ok;
       true ->
	    exit(inconsistent)
    end.

