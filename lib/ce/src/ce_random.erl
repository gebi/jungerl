%%% BEGIN ce_random.erl %%%
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

%% @doc Pseudo-random numbers library.
%%
%% @end

-module(ce_random).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2002 Cat`s Eye Technologies. All rights reserved.').

-export([seed/0, pick/1, dist/1, scramble/1, d/2]).

%% @spec seed() -> ok
%% @doc Seeds the random number generator so that it will produce
%% unpredictable values.  Should be called once at the startup of
%% the process, before using random numbers.

seed() ->
  {MegaSeconds, Seconds, MicroSeconds} = now(),
  random:seed(MicroSeconds, Seconds, MegaSeconds),
  random:uniform(23),    % prime the pump - first number can be iffy
  ok.

%% @spec pick(tuple() | [term()]) -> term()
%% @doc Picks a random element from a tuple or a list (equal chance for every
%% element.)

pick(Tuple) when tuple(Tuple) ->
  pick(tuple_to_list(Tuple));
pick(List) ->
  lists:nth(random:uniform(length(List)), List).

%% @spec dist(distribution()) -> term()
%%        distribution() = [{integer(), term()}]
%% @doc Picks a random element <code>M</code> from a list of pairs
%% <code>{N, M}</code> where <code>N</code> is the
%% percentage chance of <code>M</code> being returned.

dist(List) -> dist(List, random:uniform(100)).
dist([], X) -> nil;
dist([{P,E} | T], X) when P >= X -> E;
dist([{P,E} | T], X) -> dist(T, X - P).

%% @spec scramble(tuple() | [term()]) -> tuple() | [term()]
%% @doc Randomizes the order of a tuple or list.
  
scramble(Tuple) when tuple(Tuple) ->
  list_to_tuple(scramble(tuple_to_list(Tuple)));
scramble(List) ->
  scramble(List, []).
scramble([], Acc) -> Acc;
scramble(List, Acc) ->
  S = pick(List),
  scramble(List -- [S], Acc ++ [S]).

%% @spec d(NumberOfDice::integer(), FacesPerDie::integer()) -> integer()
%% @doc Simulates rolling N dice of M faces each.

d(0, Sides) -> 0;
d(Num, 1) -> Num;
d(Num, Sides) -> d(Num, Sides, 0).
d(0, Sides, Acc) -> Acc;
d(Num, Sides, Acc) -> d(Num-1, Sides, Acc + random:uniform(Sides)).

%%% END of ce_random.erl %%%
