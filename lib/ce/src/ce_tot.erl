%%% BEGIN ce_tot.erl %%%
%%%
%%% ce - Miscellaneous Programming Support Libraries for Erlang/OTP
%%% Copyright (c)2003 Cat's Eye Technologies.  All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%%   Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%%
%%%   Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in
%%%   the documentation and/or other materials provided with the
%%%   distribution.
%%%
%%%   Neither the name of Cat's Eye Technologies nor the names of its
%%%   contributors may be used to endorse or promote products derived
%%%   from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
%%% OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
%%% PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
%%% OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%%% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE. 

%% @doc Tuple-of-tuples library.
%%
%% @end

-module(ce_tot).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-export([new/3]).
-export([get/3, put/4]).
-export([fold/3]).
-export([print/1]).
-export([to_lol/1, from_lol/1]).

%% @spec new(integer(), integer(), term()) -> tot()
%% @doc Constructs a new tuple of tuples of the given width and height.

new(W, H, Cell) ->
  erlang:make_tuple(H, erlang:make_tuple(W, Cell)).

%% @spec get(integer(), integer(), tot()) -> term()
%% @doc Retrieves a single term from the given X,Y location 
%% in a tuple of tuples.
  
get(X, Y, Tot) ->
  element(X, element(Y, Tot)).

%% @spec put(integer(), integer(), tot(), term()) -> tot()
%% @doc Replaces a term with a new term in a tuple of tuples.
  
put(X, Y, Tot, V) ->
  setelement(Y, Tot, setelement(X, element(Y, Tot), V)).

%% @spec print(tot()) -> ok
%% @doc Displays the tuple of tuples on the standard output.
%% Assumes that each term in the ToT is a string.

print(ToT) ->
  print(1, ToT).
print(Y, ToT) when Y =< size(ToT) ->
  print_tuple(element(Y, ToT)),
  io:fwrite("~n"),
  print(Y+1, ToT);
print(Y, ToT) -> ok.
print_tuple(T) ->
  print_tuple(1, T).
print_tuple(X, T) when X =< size(T) ->
  io:fwrite("~s", [[element(X, T)]]),
  print_tuple(X+1, T);
print_tuple(X, T) -> ok.

%% @spec fold(fun(), tot(), term()) -> term()
%% @doc Applies a fun/2 to every element of a tuple of
%% tuples, in row major order.

fold(Fun, ToT, Acc) ->
  fold(1, 1, size(element(1, ToT)), size(ToT), Fun, ToT, Acc).
fold(X, Y, MaxX, MaxY, Fun, ToT, Acc) ->
  Acc0 = Fun(ce_tot:get(X, Y, ToT, Acc)),
  case X of
    MaxX ->
      case Y of
        MaxY ->
          Acc0;
        _ ->
          fold(1, Y+1, MaxX, MaxY, Fun, ToT, Acc0)
      end;
    _ ->
      fold(X+1, Y, MaxX, MaxY, Fun, ToT, Acc0)
  end.

to_lol(ToT) ->
  LoT = tuple_to_list(ToT),
  LoL = lists:map(fun(T) -> tuple_to_list(T) end, LoT).

from_lol(LoL) ->
  LoT = lists:map(fun(L) -> list_to_tuple(L) end, LoL),
  ToT = list_to_tuple(LoT).

%%% END of ce_tot.erl %%%
