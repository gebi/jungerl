%% Copyright (c) 2000, 2001 Sendmail, Inc.  All rights reserved.
%% Encode/Decode (erl.x to pure erlang terms)
%%
-module(erl2xdr).

-export([encode/1, decode/1]).

encode(X) when integer(X) -> {'INTEGER', X};
encode(X) when float(X) -> {'FLOAT', X};
encode(X) when atom(X) -> {'ATOM', atom_to_list(X)};
encode([]) -> {'NIL', void};
encode([H|T]) -> {'LIST', {encode(H), encode(T)}};
encode(T) when tuple(T) ->
    {'TUPLE', { size(T), 
	       lists:map(fun(E) -> encode(E) end, tuple_to_list(T)) }}.


decode({'INTEGER',X}) -> X;
decode({'FLOAT',X}) -> X;
decode({'NIL',_}) -> [];
decode({'ATOM',X}) -> list_to_atom(X);
decode({'LIST',{H,T}}) -> [decode(H) | decode(T)];
decode({'TUPLE',{Size,Elems}}) ->
    list_to_tuple(lists:map(fun(E) -> decode(E) end, Elems)).


