%%% BEGIN ce_timer.erl %%%
%%%
%%% ce - Miscellaneous Programming Support Libraries for Erlang/OTP
%%% Copyright (c)2002 Cat's Eye Technologies.  All rights reserved.
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

%% @doc Timer library.
%%
%% @end

-module(ce_timer).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2002 Cat`s Eye Technologies. All rights reserved.').

-export([sleep/1, sleep_forever/0, sleep_until_registered/1]).
-export([now_diff/2]).

%% @spec sleep(integer()) -> ok
%% @doc A function that allows for longer sleeps than does
%% timer:sleep/1.  Thanks to Ulf Wiger for this code.

-define(MAX_MSG_WAIT_TIME, 4294967295).  % Max int. for unsigned 32 bit

sleep(Time) ->
  sleep(Time, system_time()).

sleep(Time, Start) ->
  DiffTime = system_time() - Start,
  case Time - DiffTime of
    X when X > ?MAX_MSG_WAIT_TIME ->
      sleep(?MAX_MSG_WAIT_TIME),
      sleep(Time, Start);
    X when X > 0 ->
      receive
        after X ->
          ok
      end;
    _ ->
      ok
  end.

%% @spec system_time() -> integer()
%% @doc Returns the system time in milli-seconds.

system_time() ->
  {M,S,U} = erlang:now(),
  1000000000 * M + 1000 * S + (U div 1000).

%% @spec sleep_forever() -> never_returns
%% @doc Sleeps indefinately.

sleep_forever() ->
  receive
    _ -> sleep_forever()
  end.

%% @spec sleep_until_registered(atom()) -> ok
%% @doc Sleeps until the given process name is registered.

sleep_until_registered(Name) ->
  case lists:member(Name, registered()) of
    false ->
      timer:sleep(100),
      sleep_until_registered(Name);
    true ->
      ok
  end.

%% @spec now_diff(now_time(), now_time()) -> integer()
%% @doc Returns difference between two times returned from now(), in ms.
%% Thanks to Ulf Wiger for this code.

now_diff({M, S, U}, {M, S1, U1}) ->
  ((S - S1) * 1000) + ((U - U1) div 1000);
now_diff({M, S, U}, {M1, S1, U1}) ->
  ((M - M1) * 1000000 + (S - S1)) * 1000 + ((U - U1) div 1000).

%%% END of ce_timer.erl %%%
