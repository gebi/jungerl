%%% Copyright (c) 2000, 2001 Sendmail, Inc.  All rights reserved.
%%%----------------------------------------------------------------------
%%% File    : test.erl
%%% Author  : Martin Bjorklund <mbj@bluetail.com>
%%% Purpose : Test program for XDR encoding / decoding funtions.
%%% Created : 10 Nov 2000 by Martin Bjorklund <mbj@bluetail.com>
%%%----------------------------------------------------------------------
-module(test).
-author('mbj@bluetail.com').

-compile(export_all).

%% Uncomment if you run rpc-1.0
%-define(rpc_10, true).

-ifdef(rpc_10).
%% rpc-1.0
-define(DEC(E), {E, 1}).
-else.
%% rpc-1.1
-define(DEC(E), E, 0).
-endif.

all() ->
    t_enum(),
    t_primitive_struct(),    
    t_union_e(),
    t_union_i(),
    t_optional(),
    t_struct().

t_enum() ->
    M1 = 'T_ENUM_0',
    E1 = test_xdr:enc_t_enum(M1),
    {D1, _} = test_xdr:dec_t_enum(?DEC(E1)),
    M1 = D1,

    M2 = 'T_ENUM_1',
    E2 = test_xdr:enc_t_enum(M2),
    {D2, _} = test_xdr:dec_t_enum(?DEC(E2)),
    M2 = D2,
    ok.
   
t_primitive_struct() ->
    M1 = mk_prim(),
    E1 = test_xdr:enc_t_primitive_struct(M1),
    {D1, _} = test_xdr:dec_t_primitive_struct(?DEC(list_to_binary(E1))),
    M1 = D1,
    ok.
    
mk_prim() ->
    {-4711,
     4294967295,
     'T_ENUM_2',
     true,
     -42949672900,
     429496729000,
     list_to_binary([1,2,3]),
     list_to_binary([1,2,3,4]),
     list_to_binary("qwertyqwertyqwertyqwerty"),
     list_to_binary("arne")}.
    

t_union_e() ->
    M1 = {'T_ENUM_0', 42},
    E1 = test_xdr:enc_t_union_e(M1),
    {D1, _} = test_xdr:dec_t_union_e(?DEC(list_to_binary(E1))),
    M1 = D1,

    M2 = {'T_ENUM_1', mk_prim()},
    E2 = test_xdr:enc_t_union_e(M2),
    {D2, _} = test_xdr:dec_t_union_e(?DEC(list_to_binary(E2))),
    M2 = D2,
    ok.

t_union_i() ->
    M1 = {3, 42},
    E1 = test_xdr:enc_t_union_i(M1),
    {D1, _} = test_xdr:dec_t_union_i(?DEC(list_to_binary(E1))),
    M1 = D1,

    M2 = {6, mk_prim()},
    E2 = test_xdr:enc_t_union_i(M2),
    {D2, _} = test_xdr:dec_t_union_i(?DEC(list_to_binary(E2))),
    M2 = D2,
    ok.

    
t_optional() ->
    M1 = void,
    E1 = test_xdr:enc_t_optional(M1),
    {D1, _} = test_xdr:dec_t_optional(?DEC(list_to_binary([E1]))),
    M1 = D1,

    M2 = {7, {8, {9, void}}},
    E2 = test_xdr:enc_t_optional(M2),
    {D2, _} = test_xdr:dec_t_optional(?DEC(list_to_binary([E2]))),
    M2 = D2,

    ok.
    
t_struct() ->
    M1 = {[mk_prim(), mk_prim()],
	  {'T_ENUM_1', mk_prim()},
	  [mk_prim(), mk_prim(), mk_prim()]},
    E1 = test_xdr:enc_t_struct(M1),
    {D1, _} = test_xdr:dec_t_struct(?DEC(list_to_binary([E1]))),
    M1 = D1,
    ok.
	  


tc() ->	  
    timer:tc(?MODULE, loop, [10000]).

loop(0) ->
    ok;
loop(N) ->
    t_struct(),
    loop(N-1).

