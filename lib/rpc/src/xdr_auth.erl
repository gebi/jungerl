%% Copyright (c) 2000 Sendmail, Inc.  All rights reserved.
%%
%% RCP authentication module
%%
%%
-module(xdr_auth).
-compile([verbose, report_errors, report_warnings, trace]).
-export([public_key/1, common_key/2, get_public_key/1, get_secret_key/1]).
-export([generate_secret_key/0, des_key/1, key_to_hex/1, hex_to_key/1]).

-define(BASE, 3).
-define(MODULUS, 16#d4a0ba0250b6fd2ec626e7efd637df76c716e22d0944b88b).


public_key(Secret) ->
    pow(?BASE, Secret, ?MODULUS).

common_key(Public, Secrete) ->
    pow(Public, Secrete, ?MODULUS).

get_public_key(User) ->
    %% read the public key for the user 'User' from key-file
    0.

get_secret_key(ID) ->
    %% read the encrypted secret key for user 'User' from key-file
    0.

generate_secret_key() ->
    {A,B,C} = erlang:now(),
    random:seed(A,B,C),
    lists:foldl(
      fun(_, Key) ->
	      (Key bsl 8) + (random:uniform(256)-1)
      end, 0, lists:seq(1,24)).
    
%%
%% Generate an 8-byte DES key from a 192 bit key
%%
des_key(Key) ->
    des_key(Key bsr 64, 8, Key band 1, []).

des_key(X, 0, _, DKs) -> DKs;
des_key(X, N, P, DKs) ->
    des_key(X bsr 8, N-1, P, [((X band 16#ff) bxor P) | DKs]).

%% X^N mod M (Fix better version)

pow(X, 1, M) -> X rem M;
pow(X, N, M) when N band 1 == 1 -> 
    Y = pow(X, N bsr 1, M),
    Y*Y*X rem M;
pow(X, N, M) ->
    Y = pow(X, N bsr 1, M),
    Y*Y rem M.


key_to_hex(X) ->
    key_to_hex(X, 24, []).

key_to_hex(X, 0, Ds) -> Ds;
key_to_hex(X, N, Ds) ->
    B = X band 16#ff,
    key_to_hex(X bsr 8, N-1, [hex((B bsr 4) band 16#f),hex(B band 16#f)|Ds]).

hex(X) when X < 10 -> X + $0;
hex(X) -> (X-10) + $a.

hex_to_key(Cs) ->
    hex_to_key(Cs, 0).

hex_to_key([C|Cs], X) when C >= $0, C =< $9 ->
    hex_to_key(Cs, (X bsl 4) + (C-$0));
hex_to_key([C|Cs], X) when C >= $a, C =< $f ->
    hex_to_key(Cs, (X bsl 4) + (C-$a)+10);
hex_to_key([C|Cs], X) when C >= $A, C =< $F ->
    hex_to_key(Cs, (X bsl 4) + (C-$A)+10);
hex_to_key([], X) -> X.
