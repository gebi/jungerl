%%% BEGIN ce_broadcaster.erl %%%
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

%% @doc Broadcaster design pattern.
%%
%% <p>Any process may elect to subscribe or unsubscribe from a broadcaster.
%% Each message sent to a broadcaster is sent to all of the broadcaster's
%% subscribers.</p>
%%
%% @end

-module(ce_broadcaster).
-vsn('JUNGERL').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2003 Cat`s Eye Technologies. All rights reserved.').

-export([start/0, loop/2]).
-export([subscribe/1, unsubscribe/1]).
-export([subscribe/2, unsubscribe/2]).

%% @spec start() -> {ok, pid()} | {error, Reason}
%% @doc Starts a broadcasting service.

start() ->
  Pid = spawn_link(?MODULE, loop, [self(), []]),
  {ok, Pid}.

%% @spec loop(Parent::pid(), Subscribers::[pid()]) -> never_returns()
%% @doc Drives the broadcasting.  This function is spawned by <code>start/0</code>
%% and should not be used by external callers.

loop(Parent, Subscribers) ->
  receive
    {?MODULE, {subscribe, Pid}} ->
      case lists:member(Pid, Subscribers) of
        true ->
          Pid ! {?MODULE, {error, already_subscribed}},
          loop(Parent, Subscribers);
        false ->
          Pid ! {?MODULE, ok},
          loop(Parent, Subscribers ++ [Pid])
      end;
    {?MODULE, {unsubscribe, Pid}} ->
      case lists:member(Pid, Subscribers) of
        false ->
          Pid ! {?MODULE, {error, not_subscribed}},
          loop(Parent, Subscribers);
        true ->
          Pid ! {?MODULE, ok},
          loop(Parent, Subscribers -- [Pid])
      end;
    Else ->
      lists:foreach(fun(Subscriber) ->
        Subscriber ! Else
      end, Subscribers),
      loop(Parent, Subscribers)
  end.

%% @spec subscribe(Broadcaster::pid()) -> ok | {error, Reason}
%% @equiv subscribe(Broadcaster, self())

subscribe(Broadcaster) ->
  subscribe(Broadcaster, self()).

%% @spec subscribe(Broadcaster::pid(), Subscriber::pid()) -> ok | {error, Reason}
%% @doc Subscribes to a broadcasting service.
  
subscribe(Broadcaster, Pid) ->
  Broadcaster ! {?MODULE, {subscribe, Pid}},
  receive
    {?MODULE, Reply} ->
      Reply
  end.

%% @spec unsubscribe(Broadcaster::pid()) -> ok | {error, Reason}
%% @equiv unsubscribe(Broadcaster, self())

unsubscribe(Broadcaster) ->
  unsubscribe(Broadcaster, self()).

%% @spec unsubscribe(Broadcaster::pid(), Unsubscriber::pid()) -> ok | {error, Reason}
%% @doc Cancels subscription to a broadcasting service.
  
unsubscribe(Broadcaster, Pid) ->
  Broadcaster ! {?MODULE, {unsubscribe, Pid}},
  receive
    {?MODULE, Reply} ->
      Reply
  end.

%%% END of ce_broadcaster.erl %%%
